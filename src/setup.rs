use crate::CardIssues;
use crate::card_checks::run_checks;
use crate::card_checks_acros::run_acro_checks;
use crate::iss_parser::parse_iss_card;
use gloo::events::EventListener;
use gloo::utils::document;
use std::io::Cursor;
use std::panic;
use wasm_bindgen::prelude::*;
use web_sys::{Document, Event, HtmlInputElement, HtmlTableElement, HtmlTableSectionElement};

const ACCEPT_LIST: [&str; 3] =
    ["application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".coachCard", ".json"];

fn is_supported_file(file: &gloo::file::File) -> bool {
    if ACCEPT_LIST.contains(&file.raw_mime_type().as_str()) {
        return true;
    }

    ACCEPT_LIST.into_iter().any(|accept| file.name().ends_with(accept))
}

fn on_file_input_changed(event: &Event) {
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
            show_issues(&doc, &file.name(), &ci);
        });
    }
}

fn get_file_input(document: &Document) -> HtmlInputElement {
    document
        .get_element_by_id("card-file")
        .expect("should have #card-file on the page")
        .dyn_into::<HtmlInputElement>()
        .expect("#card-file should be an `HtmlElement`")
}

#[wasm_bindgen(start)]
fn main() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));

    // TODO should these be exported functions that JS calls?
    // moves knowledge of HTML into HTML, makes this more library-ish
    let card_file = get_file_input(&document());
    card_file.set_accept(&ACCEPT_LIST.join(","));
    let on_change = EventListener::new(&card_file, "change", on_file_input_changed);
    on_change.forget();

    let card_folder = document()
        .get_element_by_id("card-folder")
        .expect("should have #card-folder on the page")
        .dyn_into::<HtmlInputElement>()
        .expect("#card-folder should be an `HtmlElement`");
    let on_change = EventListener::new(&card_folder, "change", on_file_input_changed);
    on_change.forget();

    let version_footer = document().get_element_by_id("version-info");
    if let Some(version_footer) = version_footer {
        let txt = String::from("Last updated on ") + env!("DATE");
        version_footer.set_text_content(Some(&txt));
    }
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

fn blob_to_issues(name: &str, file: Result<Vec<u8>, gloo::file::FileReadError>) -> CardIssues {
    let mut issues = CardIssues::default();
    match file {
        Ok(buf) => {
            let mut c = Cursor::new(buf);
            match parse_iss_card(name, &mut c) {
                Ok(card) => issues += run_checks(&card) + run_acro_checks(&card),
                Err(e) => {
                    issues.errors.push(format!("could not parse file: {e}"));
                }
            }
        }
        Err(e) => {
            issues.errors.push(format!("could not read file: {e}"));
        }
    };
    issues
}

fn show_issues(document: &Document, name: &str, issues: &CardIssues) {
    let results = document.get_element_by_id("results");
    if let Some(results) = results {
        let table = add_card_issues(document, name, issues);
        if let Ok(table) = table {
            let _unused = results.append_child(&table);
        }
    } // TODO else log?
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blob_to_issues() {
        let ci = blob_to_issues("", Err(gloo::file::FileReadError::NotReadable("".to_owned())));
        assert_eq!(ci.errors.len(), 1);
        assert_eq!(ci.warnings.len(), 0);

        let ci = blob_to_issues("", Ok(vec![]));
        assert_eq!(ci.errors.len(), 1);
        assert_eq!(ci.warnings.len(), 0);

        let data = std::fs::read("./tests/SENIOR-Team_Free-PRELIMS-OCC-.xlsx")
            .expect("Could not open file");
        let ci = blob_to_issues("", Ok(data));
        assert_eq!(ci.errors.len(), 0);
        assert_eq!(ci.warnings.len(), 0);
    }
}
