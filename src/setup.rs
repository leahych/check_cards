use crate::CardIssues;
use crate::ElementKind::TeamAcro;
use crate::card_checks::{check_one_element, run_checks};
use crate::card_checks_acros::{check_one_acro, run_acro_checks};
use crate::iss_parser::parse_excel;
use crate::text_parser::{ParseResult, parse_text};
use gloo::utils::document;
use std::io::Cursor;
use std::panic;
use wasm_bindgen::prelude::*;
use web_sys::{Document, Event, HtmlInputElement, HtmlTableElement, HtmlTableSectionElement};

const ACCEPT_LIST: [&str; 4] = [
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "application/vnd.ms-excel",
    ".coachCard",
    ".json",
];

fn is_supported_file(file: &gloo::file::File) -> bool {
    if ACCEPT_LIST.contains(&file.raw_mime_type().as_str()) {
        return true;
    }

    ACCEPT_LIST.into_iter().any(|accept| file.name().ends_with(accept))
}

#[wasm_bindgen]
pub fn on_file_input_changed(event: &Event) {
    process_files(&event.target().unwrap().dyn_into::<HtmlInputElement>().unwrap());
}

fn process_files(input_element: &HtmlInputElement) {
    let files = input_element.files();
    let files = match files {
        Some(files) => gloo::file::FileList::from(files),
        None => return,
    };
    for file in files.iter() {
        if !is_supported_file(file) {
            continue;
        }
        let doc = input_element.owner_document().unwrap();
        let file = file.clone();
        wasm_bindgen_futures::spawn_local(async move {
            let res = gloo::file::futures::read_as_bytes(&file).await;
            let ci = blob_to_issues(file.name().as_str(), res);
            show_issues(&doc, ci);
        });
    }
}

#[wasm_bindgen]
pub fn on_text_input_changed(ag: String, event: String, free: bool, input: String) {
    let doc = document();
    clear_issues(&doc);

    // note: I think this means if we don't finish processing
    // before input changes again we'll get interleaved results
    if !input.trim().is_empty() {
        wasm_bindgen_futures::spawn_local(async move {
            let ci = text_to_issues(ag.as_str(), free, event.as_str(), input.as_str());
            show_issues(&doc, ci);
        });
    }
}

#[wasm_bindgen]
pub fn version_text() -> JsValue {
    JsValue::from_str(&format!("Last updated on {}", env!("DATE")))
}

#[wasm_bindgen(start)]
fn main() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
}

fn add_card_issues(
    document: &Document,
    name: &str,
    issues: &CardIssues,
) -> Result<HtmlTableElement, JsValue> {
    let table = document.create_element("table").unwrap().dyn_into::<HtmlTableElement>()?;
    let thead = table.create_t_head().dyn_into::<HtmlTableSectionElement>()?;

    let th = document.create_element("th")?;
    let name = name.rsplit_once('.').unwrap_or((name, "")).0;
    th.append_child(&document.create_text_node(name))?;
    let row = thead.insert_row()?;
    row.append_child(&th)?;

    if issues.errors.is_empty() && issues.warnings.is_empty() {
        let row = table.insert_row()?;
        row.append_child(&document.create_text_node("\u{2705} No problems found!"))?;
    }

    for issue in &issues.errors {
        let row = table.insert_row()?;
        row.append_child(&document.create_text_node(&("\u{26D4} ".to_owned() + issue)))?;
    }

    for issue in &issues.warnings {
        let row = table.insert_row()?;
        row.append_child(&document.create_text_node(&("\u{26A0} ".to_owned() + issue)))?;
    }

    Ok(table)
}

fn blob_to_issues(
    name: &str,
    file: Result<Vec<u8>, gloo::file::FileReadError>,
) -> Vec<(String, CardIssues)> {
    let mut issues = Vec::new();
    match file {
        Ok(buf) => {
            let mut c = Cursor::new(buf);
            match parse_excel(name, &mut c) {
                Ok(cards) => {
                    for (card_name, card) in cards {
                        issues.push((card_name, run_checks(&card) + run_acro_checks(&card)));
                    }
                }
                Err(e) => {
                    let mut ci = CardIssues::default();
                    ci.errors.push(format!("could not parse file: {e}"));
                    issues.push((name.to_string(), ci));
                }
            }
        }
        Err(e) => {
            let mut ci = CardIssues::default();
            ci.errors.push(format!("could not read file: {e}"));
            issues.push((name.to_string(), ci));
        }
    }
    issues
}

fn text_to_issues(ag: &str, free: bool, event: &str, input: &str) -> Vec<(String, CardIssues)> {
    let mut issues = Vec::new();

    let result = parse_text(ag, free, event, input);
    match result {
        ParseResult::Element(category, element) => issues.push((
            String::new(),
            match element {
                TeamAcro(acro, dd) => check_one_acro(category, &acro, dd.as_str()),
                _ => check_one_element(category, &element),
            },
        )),
        ParseResult::Card(card) => {
            issues.push((String::new(), run_checks(&card) + run_acro_checks(&card)));
        }
        ParseResult::Err(e) => {
            let mut ci = CardIssues::default();
            ci.errors.push(format!("could not parse input: {e}"));
            issues.push((String::new(), ci));
        }
    }

    issues
}

fn clear_issues(document: &Document) {
    let results = document.get_element_by_id("results");
    if let Some(results) = results {
        let children = results.children();
        for i in 0..children.length() {
            if let Some(child) = children.get_with_index(i) {
                child.remove();
            }
        }
    }
}

fn show_issues(document: &Document, issues: Vec<(String, CardIssues)>) {
    let results = document.get_element_by_id("results");
    if let Some(results) = results {
        for (card_name, ci) in issues {
            let table = add_card_issues(document, &card_name, &ci);
            if let Ok(table) = table {
                let _unused = results.append_child(&table);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blob_to_issues() {
        let issues = blob_to_issues("", Err(gloo::file::FileReadError::NotReadable("".to_owned())));
        assert_eq!(issues.len(), 1);
        let ci = &issues[0].1;
        assert_eq!(ci.errors.len(), 1);
        assert_eq!(ci.warnings.len(), 0);

        let issues = blob_to_issues("", Ok(vec![]));
        let ci = &issues[0].1;
        assert_eq!(ci.errors.len(), 1);
        assert_eq!(ci.warnings.len(), 0);

        let data = std::fs::read("./tests/SENIOR-Team_Free-PRELIMS-OCC-.xlsx")
            .expect("Could not open file");
        let issues = blob_to_issues("", Ok(data));
        let ci = &issues[0].1;
        assert_eq!(ci.errors.len(), 0);
        assert_eq!(ci.warnings.len(), 0);
    }

    #[test]
    fn test_text_to_issues() {
        let res = text_to_issues("12-U", true, "Solo", "A4a 2R1 S1");
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].1, CardIssues::default());

        let res = text_to_issues("12-U", true, "Team", "B-St-FS-sd");
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].1, CardIssues::default());

        let res = text_to_issues("12-U", true, "Solo", "A4a 2R1 S1\nF1a F2a\nT6a\nF1b F6a");
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].1, CardIssues::default());
    }
}
