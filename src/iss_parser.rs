use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
use crate::Events::{Duet, Trio, Unknown};
use crate::{AgeGroups, Category, CoachCard, Element, Events, TeamAcrobatic};
use calamine::{Data, DataType, Range, Reader, Rows, Xls, Xlsx};
use chrono::NaiveTime;
use std::io::{Read, Seek};

fn parse_elements(sheet: Rows<Data>, team_event: bool) -> (Vec<Element>, NaiveTime) {
    let mut elements = vec![];
    let mut end_time = NaiveTime::default();

    for element_row in sheet {
        let mut row_cols = element_row.iter().filter(|x| !x.is_empty());

        let mut elem = Element {
            number: 0,
            start_time: NaiveTime::default(),
            stop_time: NaiveTime::default(),
            kind: ChoHy,
        };

        let start_end_time_str =
            row_cols.next().unwrap_or(&Data::String(String::new())).to_string();
        let mut parts = start_end_time_str.split('-');
        let val = "0:".to_owned() + parts.next().unwrap_or("0:00");
        elem.start_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();
        let val = "0:".to_owned() + parts.next().unwrap_or("0:00");
        elem.stop_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();

        if elem.stop_time > end_time {
            end_time = elem.stop_time;
        }

        let element_type_str =
            row_cols.next().unwrap_or(&Data::String(String::new())).to_string().to_uppercase();
        let (elem_num, element_type_str) = match element_type_str.parse() {
            Ok(num) => (
                num,
                row_cols.next().unwrap_or(&Data::String(String::new())).to_string().to_uppercase(),
            ),
            Err(_) => (
                row_cols
                    .next()
                    .unwrap_or(&Data::Int(0))
                    .as_i64()
                    .unwrap_or(0)
                    .try_into()
                    .unwrap_or(0),
                element_type_str,
            ),
        };
        elem.number = elem_num;

        match element_type_str.as_str() {
            "CHOHY" => elem.kind = ChoHy,
            "TRE" => {
                let code = row_cols.next().unwrap_or(&Data::String(String::new())).to_string();
                let code = code.split('-').next_back().unwrap_or("").to_owned();
                let dd_col = row_cols.next_back().unwrap_or(&Data::Float(0.0));
                let dd = if dd_col.is_float() { dd_col.to_string() } else { String::new() };
                elem.kind = TRE(code, dd);
            }
            "ACRO" | "ACROBATIC" | "ACRO-A" | "ACRO-B" | "ACRO-C" | "ACRO-P" => {
                if team_event {
                    let dd_col = row_cols.clone().next_back().unwrap_or(&Data::Float(0.0));
                    // turn DD into a string because it makes implementing Eq easier
                    let dd = if dd_col.is_float() { dd_col.to_string() } else { String::new() };

                    row_cols.next(); // base mark/acro type
                    let parts = row_cols.filter(|x| x.is_string()).map(ToString::to_string);
                    let code = parts.fold(String::new(), |acc, x| acc + "-" + &x);
                    let code = code.strip_prefix('-').unwrap_or(code.as_str());

                    if let Ok(acro) = TeamAcrobatic::from(code) {
                        elem.kind = TeamAcro(acro, dd.clone());
                    } else {
                        // TODO log
                    }
                } else {
                    // filter out ACRO, cards that were originally created
                    // older versions of ISS can have "ACRO" both in the
                    // part column and in the base mark column.
                    elem.kind = PairAcro(
                        row_cols
                            .find(|x| x.as_string() != Some("ACRO".into()))
                            .unwrap_or(&Data::String(String::new()))
                            .to_string(),
                    );
                }
            }

            element_type_str => {
                // ISS tags this as "TRANS-SUCONN", unknown what AQUA will do
                if element_type_str.contains("SUCONN") {
                    elem.kind = SuConn;
                } else if element_type_str.contains("HYBRID") || element_type_str.contains("REQHY")
                {
                    //row1_cols.next(); // base mark
                    // turn DD into a string because it makes implementing Eq easier
                    let dd_col = row_cols.clone().next_back().unwrap_or(&Data::Float(0.0));
                    let dd = if dd_col.is_float() { dd_col.to_string() } else { String::new() };
                    let decls = row_cols
                        .filter(|x| x.is_string() && *x != "Hybrid")
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .iter()
                        .flat_map(|x| x.split_whitespace())
                        .map(str::to_owned)
                        .collect::<Vec<_>>();
                    elem.kind = Hybrid(decls, dd);
                } else {
                    // TODO log
                    continue;
                }
            }
        }
        elements.push(elem);
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
        if first_col == "EVENT" && category.event == Unknown {
            let event_txt = cols.next().unwrap_or(&Data::Empty).to_string().to_uppercase();
            category.ag = AgeGroups::from_str(event_txt.as_str());
            category.free = !event_txt.contains("TECH");
            category.event = Events::from_str(event_txt.as_str());
        } else if first_col == "ROUTINE #" {
            let draw = cols.next().unwrap_or(&Data::Empty).to_string();
            let routine_name = cols.next().unwrap_or(&Data::Empty).to_string();
            name = format!("{draw} {routine_name}");
        } else if first_col == "TIME" {
            elements_start = (i + 1, 0u32);
        }

        if elements_start != (0u32, 0u32) && *first_col == Data::Empty
            || (i as usize) == sheet.rows().len() - 1
        {
            let elements_end = if (i as usize) == sheet.rows().len() - 1 {
                sheet.end().unwrap()
            } else {
                (i - 1, 10u32)
            };
            let element_range = sheet.range(elements_start, elements_end);
            // make up a theme since this report don't include them so
            // we don't warn about that for every single acro/combo
            let mut card = CoachCard { category, theme: String::from("foo"), ..Default::default() };
            (card.elements, card.end_time) =
                parse_elements(element_range.rows(), card.category.event.is_team_event());
            //println!("TEST {:?}", card);
            cards.push((name, card.clone()));
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
            Some(Data::String(s)) => s.clone(),
            Some(Data::DateTime(dt)) => dt.as_datetime().unwrap().format("%H:%M").to_string(),
            _ => continue,
        };

        if row_name.starts_with("Theme") {
            card.theme = cols.next().map_or_else(String::new, ToString::to_string);
        }
        if row_name.starts_with("Age Group") {
            card.category.ag = cols.next().map_or_else(AgeGroups::default, |col| {
                AgeGroups::from_str(col.to_string().as_str())
            });
        }
        if row_name.starts_with("Event") {
            card.category.event = match cols.next() {
                Some(col) => {
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
                }
                None => card.category.event,
            }
        }
        if row_name.contains("0:") {
            break;
        }
    }

    let mut elements_start = sheet.start().unwrap();
    let mut elements_end = sheet.end().unwrap();
    elements_start.0 += u32::try_from(cur_positon).unwrap_or(0);
    // remove ISS hidden checksum column
    elements_end.1 -= 1;
    let element_range = sheet.range(elements_start, elements_end);
    (card.elements, card.end_time) =
        parse_elements(element_range.rows(), card.category.event.is_team_event());

    for row in element_range.rows() {
        assert!(!row.is_empty());
        if row[0].is_string() && row[0].to_string().starts_with("ISS Coach Card Version: ") {
            let temp = row[0].to_string();
            let ver_str = temp.strip_prefix("ISS Coach Card Version: ");
            card.iss_ver = match semver::Version::parse(ver_str.unwrap_or("")) {
                Ok(ver) => Some(ver),
                Err(_) => {
                    continue;
                } // TODO log
            }
        }
    }

    vec![(name.to_string(), card)]
}

fn parse_sheet(name: &str, sheet: &Range<Data>) -> Vec<(String, CoachCard)> {
    if sheet.rows().next().unwrap()[0] == "JUDGE #" {
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
