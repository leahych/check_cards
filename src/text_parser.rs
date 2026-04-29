use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
use crate::{Category, CoachCard, Element, ElementKind, get_expected_routine_time};
use chrono::NaiveTime;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseResult {
    Card(CoachCard),
    Element(Category, ElementKind),
    Err(String),
}

fn parse_element(line: &str) -> Result<ElementKind, String> {
    if line == "ChoHy" || line == "ChoHY" {
        return Ok(ChoHy);
    }

    if line == "SuConn" {
        return Ok(SuConn);
    }

    if line.starts_with("TRE") {
        return Ok(TRE(line.into(), String::new()));
    }

    if ["A-", "B-", "C-", "P-"].iter().any(|s| line.starts_with(*s)) {
        return match line.parse() {
            Ok(acro) => Ok(TeamAcro(acro, "1.0".into())),
            Err(e) => Err(e),
        };
    }

    if line.starts_with(['L', 'J', 'W']) || line.starts_with("SL") {
        return Ok(PairAcro(line.into()));
    }

    Ok(Hybrid(line.split_whitespace().map(str::to_owned).collect(), "1.0".into()))
}

pub fn parse_text(ag: &str, free: bool, evt: &str, input: &str) -> ParseResult {
    let category = Category { ag: ag.parse().unwrap(), free, event: evt.parse().unwrap() };

    if !input.contains('\n') {
        return match parse_element(input) {
            Ok(element) => ParseResult::Element(category, element),
            Err(e) => ParseResult::Err(e),
        };
    }

    let mut card = CoachCard { category, theme: "foo".into(), ..Default::default() };
    if let Some(time) = get_expected_routine_time(&category) {
        card.end_time = *time;
    }
    for (i, line) in input.lines().map(str::trim).filter(|line| !line.is_empty()).enumerate() {
        match parse_element(line) {
            Ok(kind) => {
                let start_second = u32::try_from(i).unwrap_or_default();
                let element = Element {
                    number: i + 1,
                    start_time: NaiveTime::from_hms_opt(0, 0, start_second).unwrap_or_default(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, start_second).unwrap_or_default(),
                    kind,
                };
                card.elements.push(element);
            }
            Err(e) => return ParseResult::Err(e),
        }
    }
    ParseResult::Card(card)
}

#[cfg(test)]
mod tests {
    use crate::AcroDirection::Backwards;
    use crate::AcroGroup::Airborne;
    use crate::AgeGroups::JRSR;
    use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
    use crate::Events::Solo;
    use crate::text_parser::{ParseResult, parse_text};
    use crate::{Category, TeamAcrobatic};

    const EXPECTED_CATEGORY: Category = Category { ag: JRSR, free: true, event: Solo };

    #[test]
    fn test_parse_text_chohy() {
        assert_eq!(
            parse_text("SR", true, "Solo", "ChoHy"),
            ParseResult::Element(EXPECTED_CATEGORY, ChoHy)
        );
    }

    #[test]
    fn test_parse_text_suconn() {
        assert_eq!(
            parse_text("SR", true, "Solo", "SuConn"),
            ParseResult::Element(EXPECTED_CATEGORY, SuConn)
        );
    }

    #[test]
    fn test_parse_text_tre() {
        assert_eq!(
            parse_text("SR", true, "Solo", "TRE1a"),
            ParseResult::Element(EXPECTED_CATEGORY, TRE("TRE1a".into(), "".into()))
        );
    }

    #[test]
    fn test_parse_text_pair() {
        assert_eq!(
            parse_text("SR", true, "Solo", "L!fr1"),
            ParseResult::Element(EXPECTED_CATEGORY, PairAcro("L!fr1".into()))
        );
    }

    #[test]
    fn test_parse_text_team() {
        assert_eq!(
            parse_text("SR", true, "Solo", "A-Sq-Back-tk/2pk-s1"),
            ParseResult::Element(
                EXPECTED_CATEGORY,
                TeamAcro(
                    TeamAcrobatic {
                        group: Airborne,
                        construction: "Sq".to_string(),
                        direction: Some(Backwards),
                        connection_grip: "".to_string(),
                        positions: vec!["tk".into(), "2pk".into()],
                        rotations: vec!["s1".into()],
                        bonuses: vec![],
                    },
                    "1.0".into()
                )
            )
        );
    }

    #[test]
    fn test_parse_text_hybrid() {
        assert_eq!(
            parse_text("SR", true, "Solo", "A4b C4+  2R1"),
            ParseResult::Element(
                EXPECTED_CATEGORY,
                Hybrid(vec!["A4b".into(), "C4+".into(), "2R1".into()], "1.0".into())
            )
        );
    }
}
