use crate::Events::{Duet, Trio};
use crate::element::parse_elem_kind;
use crate::{CardIssue, Category, CoachCard, Element, MilliDD, ci_err, ci_errs};
use calamine::{Data, DataType, Range, Reader, Rows, Xls, Xlsx};
use chrono::NaiveTime;
use std::io::{Read, Seek};

type ParsedElements = (Box<[Element]>, NaiveTime, Box<[CardIssue]>);
type ParsedCard = (String, CoachCard, Box<[CardIssue]>);

fn parse_element(category: Category, element_row: &[Data]) -> (Result<Element, String>, NaiveTime) {
    // if we have a DD column, get that value and remove that column
    // from the columns to search for difficulty declarations
    let (dd, element_row) = if let Some(last_col) = element_row.last()
        && let Some(dd) = last_col.as_f64()
    {
        // if DD is negative then this will blow up later when we validate
        // the reported DD, so it does not matter if the sign is lost. Using
        // units of MilliDD should convert the DD to a whole number, but
        // we'll call round just in case we end up with 5.999 instead of 6.
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        (Some(MilliDD((dd * 1000.0).round() as u32)), &element_row[..element_row.len() - 1])
    } else {
        (None, element_row)
    };

    // get_string will return a &str but only if the underlying DataType
    // is String, otherwise it skips the cell, so the element numbers
    // would be skipped (they are stored as float).
    let mut row_cols = element_row.iter().filter_map(Data::as_string);

    let start_end_time_str = row_cols.next().unwrap_or_default();
    let mut parts = start_end_time_str.split('-');
    let val = parts.next().map(|v| "0:".to_owned() + v).unwrap_or_default();
    let start_time = NaiveTime::parse_from_str(&val, "%T").unwrap_or_default();
    let val = parts.next().map(|v| "0:".to_owned() + v).unwrap_or_default();
    let stop_time = NaiveTime::parse_from_str(&val, "%T").unwrap_or_default();

    // reports can be # ACRO-A ACRO-A <decl>
    // cards can be ACRO # ACRO-A <decl>
    // transitions won't have any element number
    let next = row_cols.next().unwrap_or_default().to_uppercase();
    let (num, etype) = match next.parse() {
        Ok(n) => (n, row_cols.next().unwrap_or_default().to_uppercase()),
        Err(_) => (row_cols.next().unwrap_or_default().parse().unwrap_or_default(), next),
    };

    let code = match etype.trim() {
        "CHOHY" => "ChoHy".to_owned(),
        "TRE" => row_cols.next().unwrap_or_default(),
        "ACRO" | "ACROBATIC" | "ACRO-A" | "ACRO-B" | "ACRO-C" | "ACRO-P" => row_cols
            .find(|x| !matches!(x.as_str(), "ACRO" | "ACRO-A" | "ACRO-B" | "ACRO-C" | "ACRO-P"))
            .unwrap_or_default(),
        "SUCONN" | "TRANS-SUCONN" => "SuConn".to_owned(),
        "HYBRID" | "REQHY" => row_cols.collect::<Vec<_>>().join(" "),
        // not really an error, but we don't want transitions to be an
        // element type since all we need to parse from them is a stop
        // time. So return an error with an empty string, and the
        // caller will filter that out
        "" | "TRANS" => return (Err(String::new()), stop_time),
        &_ => {
            return (Err(format!("Element {num}: unknown element type '{etype}'")), stop_time);
        }
    };

    match parse_elem_kind(category.event, category.free, code.as_str(), dd) {
        Ok(kind) => (Ok(Element { number: num, start_time, stop_time, kind }), stop_time),
        Err(e) => (Err(format!("Element {num}: {e}")), stop_time),
    }
}

fn parse_elements(sheet: Rows<Data>, category: Category) -> ParsedElements {
    let mut elements = Vec::new();
    let mut end_time = NaiveTime::default();
    let mut ci = Vec::new();

    for element_row in sheet {
        let (ret, stop_time) = parse_element(category, element_row);
        end_time = NaiveTime::max(stop_time, end_time);
        match ret {
            Ok(element) => elements.push(element),
            Err(e) if !e.is_empty() => ci_err(&mut ci, e),
            _ => {} // transition, ignore
        }
    }
    (elements.into(), end_time, ci.into())
}

fn parse_report(sheet: &Range<Data>) -> Box<[ParsedCard]> {
    let mut cards = Vec::new();
    let mut category = Category::default();
    let mut name = String::new();

    let mut elements_start = (0u32, 0u32);
    for (i, row) in (0u32..).zip(sheet.rows()) {
        let mut cols = row.iter().filter_map(Data::get_string);
        let first_col = cols.next().unwrap_or_default();
        if first_col == "EVENT" {
            let event_txt = cols.next().unwrap_or_default();
            category.ag = event_txt.into();
            category.free = !event_txt.contains("TECH");
            category.event = event_txt.into();
        } else if first_col == "ROUTINE #" {
            let draw = cols.next().unwrap_or_default();
            let routine_name = cols.next().unwrap_or_default();
            name = format!("{draw} {routine_name}");
        } else if first_col == "TIME" {
            elements_start = (i + 1, 0u32);
        }

        if elements_start != (0u32, 0u32) && first_col.is_empty()
            || (i as usize) == sheet.height() - 1
        {
            let elements_end = if (i as usize) == sheet.height() - 1 {
                sheet.end().unwrap_or_default()
            } else {
                (i - 1, 10u32)
            };

            let er = sheet.range(elements_start, elements_end);
            // make up a theme since this report don't include them so
            // we don't warn about that for every single acro/combo
            let (elements, end_time, ci) = parse_elements(er.rows(), category);
            cards.push((
                name,
                CoachCard { category, theme: "foo".into(), elements, end_time, iss_ver: None },
                ci,
            ));
            elements_start = (0u32, 0u32);
            name = String::new();
        }
    }
    cards.into()
}

fn parse_iss_card(name: &str, sheet: &Range<Data>) -> Box<[ParsedCard]> {
    const ISS_VER_PREFIX: &str = "ISS Coach Card Version: ";

    let mut card = CoachCard::default();

    let Some((elem_start_row, _)) = (0u32..)
        .zip(sheet.rows())
        .find(|(_, row)| row.first().is_some_and(|c| c.to_string().starts_with("0:")))
    else {
        return [(name.into(), card, ci_errs("could not find elements"))].into();
    };

    let header = sheet.range((0, 0), (elem_start_row, sheet.end().unwrap_or_default().1));
    for row in header.rows() {
        let mut cols = row.iter().filter_map(Data::get_string);
        let row_name = cols.next().unwrap_or_default();

        if row_name.starts_with("Theme") {
            card.theme = cols.next().unwrap_or_default().into();
        }
        if row_name.starts_with("Age Group") {
            card.category.ag = cols.next().unwrap_or_default().into();
        }
        if row_name.starts_with("Event") {
            let col = cols.next().unwrap_or_default();
            card.category.event = col.into();
            card.category.free = !col.to_uppercase().contains("TECH");
            if card.category.event == Duet
                && (card.theme.to_uppercase().contains(" TRIO")
                    || card.theme.to_uppercase().contains("TRIO ")
                    || card.theme.to_uppercase() == "TRIO"
                    || name.to_uppercase().contains(" TRIO")
                    || name.to_uppercase().contains("_TRIO"))
            {
                // Special case Trio since ISS does not support
                // Trios. To avoid matching on a theme that
                // has a word that contain "trio", only match if
                // "trio" is its own word.
                card.category.event = Trio;
            }
        }
    }

    // remove ISS hidden checksum column
    let elements_end = sheet.end().map(|r| (r.0, r.1 - 1)).unwrap_or_default();
    let element_range = sheet.range((elem_start_row, 0), elements_end);
    let ci;
    (card.elements, card.end_time, ci) = parse_elements(element_range.rows(), card.category);

    if let Some(col) = element_range
        .rows()
        .filter_map(|r| r.first().and_then(Data::as_string))
        .find(|c| c.starts_with(ISS_VER_PREFIX))
    {
        card.iss_ver = col.strip_prefix(ISS_VER_PREFIX).and_then(|s| s.parse().ok());
    }

    [(name.into(), card, ci)].into()
}

fn parse_sheet(name: &str, sheet: &Range<Data>) -> Box<[ParsedCard]> {
    if sheet.get((0, 0)).is_some_and(|c| c == "JUDGE #") {
        parse_report(sheet)
    } else {
        parse_iss_card(name, sheet)
    }
}

pub fn parse_excel<R: Read + Seek>(
    name: &str,
    reader: &mut R,
) -> Result<Box<[ParsedCard]>, String> {
    let sheets = if name.to_lowercase().ends_with(".xls") {
        let workbook: Result<Xls<_>, _> = calamine::open_workbook_from_rs(reader);
        match workbook {
            Ok(mut sheet) => sheet.worksheets(),
            Err(e) => return Err(format!("Failed to open workbook: {name}: {e}")),
        }
    } else {
        let workbook: Result<Xlsx<_>, _> = calamine::open_workbook_from_rs(reader);
        match workbook {
            Ok(mut sheet) => sheet.worksheets(),
            Err(e) => return Err(format!("Failed to open workbook: {name}: {e}")),
        }
    };

    let sheet = sheets.into_iter().find(|(name, _)| name != "LEGEND" && name != "Codes and Values");
    if let Some((_, sheet)) = sheet {
        Ok(parse_sheet(name, &sheet))
    } else {
        Err("Could not find worksheet".into())
    }
}
