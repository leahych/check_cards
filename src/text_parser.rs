use crate::element::{ElementKind, parse_elem_kind};
use crate::{CardIssue, Category, CoachCard, Element, IssueLevel, get_expected_routine_time};
use anyhow::Error;
use chrono::NaiveTime;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseResult {
    Card(CoachCard, Box<[CardIssue]>),
    Element(Category, ElementKind),
}

pub fn parse_text(ag: &str, free: bool, evt: &str, input: &str) -> Result<ParseResult, Error> {
    let mut ci = Vec::new();
    let category = Category { ag: ag.into(), free, event: evt.into() };

    if !input.contains('\n') {
        return Ok(ParseResult::Element(category, parse_elem_kind(evt.into(), free, input, None)?));
    }

    let mut card = CoachCard { category, theme: "foo".into(), ..Default::default() };
    if let Some(time) = get_expected_routine_time(&category) {
        card.end_time = time;
    }

    let mut elements = Vec::new();
    for (i, line) in input.lines().map(str::trim).filter(|line| !line.is_empty()).enumerate() {
        match parse_elem_kind(category.event, category.free, line, None) {
            Ok(kind) => {
                let start_second = u32::try_from(i).unwrap_or_default();
                elements.push(Element {
                    number: i + 1,
                    start_time: NaiveTime::from_hms_opt(0, 0, start_second).unwrap_or_default(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, start_second).unwrap_or_default(),
                    kind,
                });
            }
            Err(e) => {
                ci.push(CardIssue::new(IssueLevel::Error, e.to_string()));
            }
        }
    }
    card.elements = elements.into();
    Ok(ParseResult::Card(card, ci.into()))
}

#[cfg(test)]
mod tests {
    use crate::AgeGroups::{JRSR, Youth};
    use crate::Category;
    use crate::Events::{Combo, Duet, MixedDuet, Solo, Team};
    use crate::element::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
    use crate::hybrid::{AwLC, ConnLC, Decl, Factor, HybridDecl, LevelCode, TwistLC};
    use crate::pair_acro::PairAcroKind;
    use crate::team_acro::{AConst, ADir, APos, ARotation, AcroA, Positions, TeamAcroKind};
    use crate::text_parser::{ParseResult, parse_text};
    use crate::tre::{SoloTRE, TREKind};

    #[test]
    fn test_parse_text_chohy() {
        assert_eq!(
            parse_text("Youth", true, "Combo", "ChoHy").unwrap(),
            ParseResult::Element(Category { ag: Youth, free: true, event: Combo }, ChoHy(None))
        );
    }

    #[test]
    fn test_parse_text_suconn() {
        assert_eq!(
            parse_text("SR", true, "Mixed Duet", "SuConn").unwrap(),
            ParseResult::Element(Category { ag: JRSR, free: true, event: MixedDuet }, SuConn)
        );
    }

    #[test]
    fn test_parse_text_tre() {
        assert_eq!(
            parse_text("SR", false, "Solo", "TRE1a").unwrap(),
            ParseResult::Element(
                Category { ag: JRSR, free: false, event: Solo },
                TRE(TREKind::Solo(SoloTRE::_1a), None)
            )
        );
    }

    #[test]
    fn test_parse_text_pair() {
        assert_eq!(
            parse_text("SR", true, "Duet", "L!fr1").unwrap(),
            ParseResult::Element(
                Category { ag: JRSR, free: true, event: Duet },
                PairAcro(PairAcroKind::LHeadDownFlexr1, None)
            )
        );
    }

    #[test]
    fn test_parse_text_team() {
        assert_eq!(
            parse_text("SR", true, "Team", "A-Sq-Back-tk/2pk-s1").unwrap(),
            ParseResult::Element(
                Category { ag: JRSR, free: true, event: Team },
                TeamAcro(
                    TeamAcroKind::Airborne(AcroA {
                        construction: AConst::Sq,
                        dir: ADir::Back,
                        positions: Positions { first: APos::tk, second: Some(APos::pk) },
                        rotation: Some(ARotation::s1),
                        bonuses: [].into(),
                    }),
                    None
                )
            )
        )
    }

    #[test]
    fn test_parse_text_hybrid() {
        assert_eq!(
            parse_text("SR", true, "Solo", "A4b C4+ 2R1").unwrap(),
            ParseResult::Element(
                Category { ag: JRSR, free: true, event: Solo },
                Hybrid(
                    HybridDecl {
                        decls: Box::from([
                            Decl { lc: LevelCode::Aw(AwLC::A4b), f: Factor::No },
                            Decl { lc: LevelCode::Conn(ConnLC::C4, true), f: Factor::No },
                            Decl { lc: LevelCode::Twist(TwistLC::_2R1), f: Factor::No }
                        ]),
                        pc_bonus: None
                    },
                    None
                )
            )
        );
    }
}
