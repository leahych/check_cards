use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
use crate::Events::{Duet, Trio};
use crate::{AgeGroups, Category, CoachCard, Element, Events, TeamAcrobatic};
use calamine::{Data, DataType, Range, Reader, Rows, Xls, Xlsx};
use chrono::NaiveTime;
use std::io::{Read, Seek};

fn parse_elements(sheet: Rows<Data>, team_event: bool) -> (Vec<Element>, NaiveTime) {
    let mut elements = vec![];
    let mut end_time = NaiveTime::default();

    for element_row in sheet {
        let mut row_cols = element_row.iter().filter(|x| !x.is_empty());

        // FUTURE use map_or_default once it is in stable
        let start_end_time_str = row_cols.next().map_or_else(String::new, ToString::to_string);
        let mut parts = start_end_time_str.split('-');
        let val = parts.next().map(|v| "0:".to_owned() + v).unwrap_or_default();
        let start_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();
        let val = parts.next().map(|v| "0:".to_owned() + v).unwrap_or_default();
        let stop_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();

        end_time = NaiveTime::max(stop_time, end_time);

        let element_type_str =
            row_cols.next().map_or_else(String::new, ToString::to_string).to_uppercase();
        let (number, element_type_str) = match element_type_str.parse() {
            Ok(num) => {
                (num, row_cols.next().map_or_else(String::new, ToString::to_string).to_uppercase())
            }
            Err(_) => (
                row_cols
                    .next()
                    .and_then(DataType::as_i64)
                    .and_then(|x| i64::try_into(x).ok())
                    .unwrap_or_default(),
                element_type_str,
            ),
        };

        // turn DD into a string because it makes implementing Eq easier
        let dd = row_cols
            .clone()
            .rev()
            .find_map(|x| if x.is_float() { x.as_string() } else { None })
            .unwrap_or_default();

        let kind = match element_type_str.as_str() {
            "CHOHY" => ChoHy,
            "TRE" => {
                let code = row_cols.next().map_or_else(String::new, ToString::to_string);
                TRE(code, dd)
            }
            "ACRO" | "ACROBATIC" | "ACRO-A" | "ACRO-B" | "ACRO-C" | "ACRO-P" => {
                if team_event {
                    //row_cols.next(); // base mark/acro type
                    let prefixes = &["ACRO-A", "ACRO-B", "ACRO-C", "ACRO-P"];
                    let code = row_cols
                        .find_map(|x| {
                            if x.is_string() && !prefixes.contains(&&*x.to_string()) {
                                x.as_string()
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default()
                        .replace('\n', "");
                    if let Ok(acro) = TeamAcrobatic::from(code.as_str()) {
                        TeamAcro(acro, dd)
                    } else {
                        // TODO log
                        continue;
                    }
                } else {
                    // filter out ACRO, cards that were originally created
                    // older versions of ISS can have "ACRO" both in the
                    // part column and in the base mark column.
                    PairAcro(
                        row_cols
                            .find_map(|x| {
                                if x.is_string() && *x != "ACRO" { x.as_string() } else { None }
                            })
                            .unwrap_or_default(),
                    )
                }
            }
            "SUCONN" | "TRANS-SUCONN" => SuConn,
            "HYBRID" | "REQHY" => {
                let decls = row_cols
                    .filter(|x| Data::is_string(x))
                    .flat_map(|x| {
                        x.to_string().split_whitespace().map(String::from).collect::<Vec<_>>()
                    })
                    .collect();
                Hybrid(decls, dd)
            }
            &_ => {
                // TODO log
                continue;
            }
        };
        elements.push(Element { number, start_time, stop_time, kind });
    }
    (elements, end_time)
}

fn parse_report(sheet: &Range<Data>) -> Vec<(String, CoachCard)> {
    let mut cards = Vec::new();
    let mut category = Category::default();
    let mut name = String::new();

    let mut elements_start = (0u32, 0u32);
    for (i, row) in (0u32..).zip(sheet.rows()) {
        let mut cols = row.iter().filter(|x| !x.is_empty());
        let first_col = cols.next().unwrap_or(&Data::Empty);
        if first_col == "EVENT" {
            let event_txt =
                cols.next().map_or_else(String::new, ToString::to_string).to_uppercase();
            category.ag = AgeGroups::from_str(event_txt.as_str());
            category.free = !event_txt.contains("TECH");
            category.event = Events::from_str(event_txt.as_str());
        } else if first_col == "ROUTINE #" {
            let draw = cols.next().map_or_else(String::new, ToString::to_string);
            let routine_name = cols.next().map_or_else(String::new, ToString::to_string);
            name = format!("{draw} {routine_name}");
        } else if first_col == "TIME" {
            elements_start = (i + 1, 0u32);
        }

        if elements_start != (0u32, 0u32) && *first_col == Data::Empty
            || (i as usize) == sheet.rows().len() - 1
        {
            let elements_end = if (i as usize) == sheet.rows().len() - 1 {
                sheet.end().unwrap_or_default()
            } else {
                (i - 1, 10u32)
            };
            let er = sheet.range(elements_start, elements_end);
            // make up a theme since this report don't include them so
            // we don't warn about that for every single acro/combo
            let (elements, end_time) = parse_elements(er.rows(), category.event.is_team_event());
            cards.push((
                name,
                CoachCard { category, theme: "foo".into(), elements, end_time, iss_ver: None },
            ));
            elements_start = (0u32, 0u32);
            name = String::new();
        }
    }
    cards
}

fn parse_iss_card(name: &str, sheet: &Range<Data>) -> Vec<(String, CoachCard)> {
    let mut card = CoachCard::default();

    let mut cur_positon = 0_usize;
    for (i, row) in sheet.rows().enumerate() {
        cur_positon = i;
        let mut cols = row.iter().filter(|x| !x.is_empty());
        let row_name = match cols.next() {
            Some(Data::String(s)) => s,
            Some(Data::DateTime(dt)) => &dt.as_datetime().unwrap().format("%H:%M").to_string(),
            _ => continue,
        };

        if row_name.starts_with("Theme") {
            card.theme = cols.next().map_or_else(String::new, ToString::to_string);
        }
        if row_name.starts_with("Age Group") {
            card.category.ag =
                cols.next().map_or_else(Default::default, |c| AgeGroups::from_str(&c.to_string()));
        }
        if row_name.starts_with("Event") {
            card.category.event = cols.next().map_or_else(Default::default, |col| {
                let event_txt = col.to_string().to_uppercase();
                card.category.free = !event_txt.contains("TECH");
                let parsed_event = Events::from_str(event_txt.as_str());
                if parsed_event == Duet
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
                    Trio
                } else {
                    parsed_event
                }
            });
        }
        if row_name.contains("0:") {
            break;
        }
    }

    let mut elements_start = sheet.start().unwrap_or_default();
    let mut elements_end = sheet.end().unwrap_or_default();
    elements_start.0 += u32::try_from(cur_positon).unwrap_or_default();
    // remove ISS hidden checksum column
    elements_end.1 -= 1;
    let element_range = sheet.range(elements_start, elements_end);
    (card.elements, card.end_time) =
        parse_elements(element_range.rows(), card.category.event.is_team_event());

    for col in element_range
        .rows()
        .filter_map(|r| r.first().and_then(Data::as_string))
        .filter(|col| col.starts_with("ISS Coach Card Version: "))
    {
        let ver_str = col.strip_prefix("ISS Coach Card Version: ");
        card.iss_ver = match semver::Version::parse(ver_str.unwrap_or_default()) {
            Ok(ver) => Some(ver),
            Err(_) => {
                continue;
            }
        }
    }

    vec![(name.into(), card)]
}

fn parse_sheet(name: &str, sheet: &Range<Data>) -> Vec<(String, CoachCard)> {
    let row = sheet.rows().next();
    if row.and_then(|r| r.first()).map(ToString::to_string) == Some("JUDGE #".into()) {
        parse_report(sheet)
    } else {
        parse_iss_card(name, sheet)
    }
}

// TODO how to handle non-fatal, i.e. unknown event?
pub fn parse_excel<R: Read + Seek>(
    name: &str,
    reader: &mut R,
) -> Result<Vec<(String, CoachCard)>, String> {
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
