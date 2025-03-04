use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
use crate::Events::{Duet, Trio, Unknown};
use crate::{AgeGroups, Category, CoachCard, Element, Events, TeamAcrobatic};
use calamine::{Data, DataType, ExcelDateTime, Range, Reader, Rows, Xls, Xlsx};
use chrono::{NaiveTime, Timelike};
use std::io::{Read, Seek};

fn parse_elements(
    mut sheet: Rows<Data>,
    aqua_card: bool,
    team_event: bool,
) -> (Vec<Element>, NaiveTime) {
    let mut elements = vec![];
    let mut end_time = NaiveTime::default();

    while let Some(element_row1) = sheet.next() {
        let element_row2 = if aqua_card {
            match sheet.next() {
                Some(row2) => row2,
                None => break,
            }
        } else {
            element_row1
        };

        let mut row1_cols = element_row1.iter().filter(|x| !x.is_empty());
        let mut row2_cols = element_row2.iter().filter(|x| !x.is_empty());

        let mut elem = Element {
            number: 0,
            start_time: NaiveTime::default(),
            stop_time: NaiveTime::default(),
            kind: ChoHy,
        };

        if aqua_card {
            let def_date_time = Data::DateTime(ExcelDateTime::default());
            let start_time =
                row1_cols.next().unwrap_or(&def_date_time).as_datetime().unwrap_or_default();
            let end_time =
                row2_cols.next().unwrap_or(&def_date_time).as_datetime().unwrap_or_default();
            elem.start_time = NaiveTime::from_hms_opt(0, start_time.hour(), start_time.minute())
                .unwrap_or_default();
            elem.stop_time =
                NaiveTime::from_hms_opt(0, end_time.hour(), end_time.minute()).unwrap_or_default();
        } else {
            let start_end_time_str =
                row1_cols.next().unwrap_or(&Data::String(String::new())).to_string();
            let mut parts = start_end_time_str.split('-');
            let val = "0:".to_owned() + parts.next().unwrap_or("0:00");
            elem.start_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();
            let val = "0:".to_owned() + parts.next().unwrap_or("0:00");
            elem.stop_time = NaiveTime::parse_from_str(val.as_str(), "%T").unwrap_or_default();
        }

        if elem.stop_time > end_time {
            end_time = elem.stop_time;
        }

        let elment_type_str =
            row1_cols.next().unwrap_or(&Data::String(String::new())).to_string().to_uppercase();
        let (elem_num, elment_type_str) = match elment_type_str.parse() {
            Ok(num) => (
                num,
                row1_cols.next().unwrap_or(&Data::String(String::new())).to_string().to_uppercase(),
            ),
            Err(_) => (
                row1_cols
                    .next()
                    .unwrap_or(&Data::Int(0))
                    .as_i64()
                    .unwrap_or(0)
                    .try_into()
                    .unwrap_or(0),
                elment_type_str,
            ),
        };
        elem.number = elem_num;

        match elment_type_str.as_str() {
            "CHOHY" => elem.kind = ChoHy,
            "TRE" => {
                let code = row1_cols.next().unwrap_or(&Data::String(String::new())).to_string();
                let code = code.split('-').last().unwrap_or("").to_owned();
                elem.kind = TRE(code);
            }
            "ACRO" | "ACROBATIC" | "ACRO-A" | "ACRO-B" | "ACRO-C" | "ACRO-P" => {
                if team_event {
                    row1_cols.next(); // base mark/acro type
                    let parts =
                        row1_cols.filter(|x| x.is_string()).map(std::string::ToString::to_string);
                    let code = parts.fold(String::new(), |acc, x| acc + "-" + &x);
                    let code = code.strip_prefix('-').unwrap_or(code.as_str());

                    // turn DD into a string because it makes implementing Eq easier
                    let dd_col = row2_cols.last().unwrap_or(&Data::Float(0.0));
                    let dd = if dd_col.is_float() { dd_col.to_string() } else { String::new() };
                    if let Ok(acro) = TeamAcrobatic::from(code) {
                        elem.kind = TeamAcro(acro, dd.to_string());
                    } else {
                        // TODO log
                    }
                } else {
                    // TODO next might not be correct for AQUA cards
                    elem.kind = PairAcro(
                        row1_cols.next().unwrap_or(&Data::String(String::new())).to_string(),
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
                    let dd_col = row2_cols.last().unwrap_or(&Data::Float(0.0));
                    let dd = if dd_col.is_float() { dd_col.to_string() } else { String::new() };
                    let decls = row1_cols
                        .filter(|x| x.is_string() && *x != "Hybrid")
                        .map(std::string::ToString::to_string)
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
        };
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
            let mut card = CoachCard { category, ..Default::default() };
            (card.elements, card.end_time) =
                parse_elements(element_range.rows(), false, card.category.event.is_team_event());
            //println!("TEST {:?}", card);
            cards.push((name, card.clone()));
            elements_start = (0u32, 0u32);
            name = String::new();
        }
    }
    cards
}

fn parse_iss_card(name: &str, sheet: &Range<Data>) -> Vec<(String, CoachCard)> {
    let mut aqua_card = true;

    let mut card = CoachCard::default();

    let mut cur_positon = 0_usize;
    for (i, row) in sheet.rows().enumerate() {
        cur_positon = i;
        let mut cols = row.iter().filter(|x| !x.is_empty());
        let row_name = match cols.next() {
            Some(Data::String(s)) => s.to_string(),
            Some(Data::DateTime(dt)) => dt.as_datetime().unwrap().format("%H:%M").to_string(),
            _ => continue,
        };
        if row_name.contains("COACH CARD") {
            // this isn't great but it works
            aqua_card = false;
        }
        if row_name.starts_with("Theme") {
            card.theme = cols.next().map_or_else(String::new, std::string::ToString::to_string);
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

    if aqua_card && card.category.ag == AgeGroups::Unknown {
        card.category.ag = AgeGroups::JRSR;
    }

    let mut elements_start = sheet.start().unwrap();
    let mut elements_end = sheet.end().unwrap();
    elements_start.0 += u32::try_from(cur_positon).unwrap_or(0);
    if !aqua_card {
        // remove ISS hidden checksum column
        elements_end.1 -= 1;
    }
    let element_range = sheet.range(elements_start, elements_end);
    (card.elements, card.end_time) =
        parse_elements(element_range.rows(), aqua_card, card.category.event.is_team_event());

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
