use check_cards::AcroDirection::{Backwards, Forwards};
use check_cards::AcroGroup::{Airborne, Combined, Platform};
use check_cards::AgeGroups::JRSR;
use check_cards::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
use check_cards::Events::Combo;
use check_cards::{AgeGroups, Category, CoachCard, Element, Events, TeamAcrobatic, parse_excel};
use chrono::NaiveTime;
use pretty_assertions::assert_eq;
use semver::Version;
use std::fs::File;
use std::io::BufReader;

#[test]
fn test_parse_aqua_team() {
    let f = File::open("./tests/Tech Team test.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let issues = parse_excel("", &mut file).expect("Could not parse card");
    assert_eq!(
        CoachCard {
            category: Category { ag: AgeGroups::JRSR, free: false, event: Events::Team },
            elements: vec![
                Element {
                    number: 1,
                    start_time: NaiveTime::from_hms_opt(0, 0, 2).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    kind: Hybrid(
                        vec!["T9a*0.5".to_string(), "F8a".to_string(), "2PC".to_string()],
                        "2.8".into()
                    ),
                },
                Element {
                    number: 2,
                    start_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 4).unwrap(),
                    kind: TRE("TRE2a".to_string()),
                },
                Element {
                    number: 3,
                    start_time: NaiveTime::from_hms_opt(0, 0, 4).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 5).unwrap(),
                    kind: TeamAcro(
                        TeamAcrobatic {
                            group: Airborne,
                            construction: "Sq".to_string(),
                            direction: Some(Forwards),
                            connection_grip: "".to_string(),
                            positions: vec!["tk".to_string(), "2pk".to_string()],
                            rotations: vec!["s1".into()],
                            bonuses: vec!["Dbl".to_string(), "Pos3".to_string()],
                        },
                        "2.45".into()
                    ),
                }
            ],
            theme: "Random".to_string(),
            iss_ver: None,
            end_time: NaiveTime::from_hms_opt(0, 0, 5).unwrap(),
        },
        issues.iter().next().unwrap().1
    );
}

fn vec_of_strings(input: &str) -> Vec<String> {
    input.split(' ').map(|s| s.to_string()).collect()
}

#[test]
fn test_parse_iss_team() {
    let f = File::open("./tests/SENIOR-Team_Free-PRELIMS-OCC-.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let issues = parse_excel("", &mut file).expect("Could not parse card");
    assert_eq!(
        CoachCard {
            category: Category { ag: AgeGroups::JRSR, free: true, event: Events::Team },
            elements: vec![
                Element {
                    number: 1,
                    start_time: NaiveTime::from_hms_opt(0, 0, 13).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 18).unwrap(),
                    kind: TeamAcro(
                        TeamAcrobatic {
                            group: Platform,
                            construction: "P".to_string(),
                            direction: None,
                            connection_grip: "HA".to_string(),
                            positions: vec!["bb".to_string(), "2wi".to_string()],
                            rotations: vec![],
                            bonuses: vec!["Porp".to_string(), "Trav".to_string()],
                        },
                        "2.238".into()
                    )
                },
                Element {
                    number: 2,
                    start_time: NaiveTime::from_hms_opt(0, 0, 21).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 24).unwrap(),
                    kind: TeamAcro(
                        TeamAcrobatic {
                            group: Combined,
                            construction: "Thr^2F".to_string(),
                            direction: Some(Forwards),
                            connection_grip: "".to_string(),
                            positions: vec!["ow".to_string(), "2ln".to_string()],
                            rotations: vec![],
                            bonuses: vec![],
                        },
                        "1.875".into()
                    )
                },
                Element {
                    number: 3,
                    start_time: NaiveTime::from_hms_opt(0, 0, 31).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 35).unwrap(),
                    kind: TeamAcro(
                        TeamAcrobatic {
                            group: Airborne,
                            construction: "Shou".to_string(),
                            direction: Some(Backwards),
                            connection_grip: "".to_string(),
                            positions: vec!["tk".to_string()],
                            rotations: vec!["s1".into()],
                            bonuses: vec![],
                        },
                        "1.9".into()
                    )
                },
                Element {
                    number: 4,
                    start_time: NaiveTime::from_hms_opt(0, 0, 46).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 1, 0).unwrap(),
                    kind: Hybrid(
                        vec_of_strings("FB F6a AB A6 S1 AB 2R1 F1a F2b F3a 1PC"),
                        "3.75".into()
                    ),
                },
                Element {
                    number: 5,
                    start_time: NaiveTime::from_hms_opt(0, 1, 24).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 1, 38).unwrap(),
                    kind: Hybrid(
                        vec_of_strings("A3a A6 A1c A6 F4f*0.3 F4e*0.5 F4f*0.5 F4f A1d 4PC"),
                        "4.92".into()
                    ),
                },
                Element {
                    number: 6,
                    start_time: NaiveTime::from_hms_opt(0, 1, 44).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 06).unwrap(),
                    kind: Hybrid(vec_of_strings("C4 C4 C4 RD2 S1 2PC"), "3.8".into())
                },
                Element {
                    number: 7,
                    start_time: NaiveTime::from_hms_opt(0, 2, 30).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 45).unwrap(),
                    kind: Hybrid(
                        vec_of_strings("CB+ A5 1RB A6 CB+ CB+ A1c 2RB 1PC"),
                        "3.35".into()
                    ),
                },
                Element {
                    number: 8,
                    start_time: NaiveTime::from_hms_opt(0, 2, 50).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 7).unwrap(),
                    kind: Hybrid(vec_of_strings("T3d A2b A2b A2b A3a S2"), "2.55".into()),
                },
                Element {
                    number: 9,
                    start_time: NaiveTime::from_hms_opt(0, 3, 15).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 26).unwrap(),
                    kind: Hybrid(vec_of_strings("AB A6 RB A1c RO1 F2b 3PC"), "3.25".into()),
                },
            ],
            theme: "Test".to_string(),
            iss_ver: Some(Version::new(3, 0, 5)),
            end_time: NaiveTime::from_hms_opt(0, 3, 29).unwrap(),
        },
        issues.iter().next().unwrap().1
    );
}

#[test]
fn test_parse_iss_combo() {
    let f = File::open("./tests/Combo.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let issues = parse_excel("", &mut file).expect("Could not parse card");
    assert_eq!(
        CoachCard {
            category: Category { ag: AgeGroups::Youth, free: true, event: Events::Combo },
            elements: vec![Element {
                number: 1,
                start_time: NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
                stop_time: NaiveTime::from_hms_opt(0, 0, 1).unwrap(),
                kind: ChoHy,
            }],
            theme: "".to_string(),
            end_time: NaiveTime::from_hms_opt(0, 0, 1).unwrap(),
            iss_ver: Some(Version::new(3, 0, 2)),
        },
        issues.iter().next().unwrap().1
    )
}

#[test]
fn test_parse_iss_mixed_duet() {
    let f = File::open("./tests/Mixed Duet.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let issues = parse_excel("", &mut file).expect("Could not parse card");
    assert_eq!(
        CoachCard {
            category: Category { ag: JRSR, free: true, event: Events::MixedDuet },
            elements: vec![
                Element {
                    number: 0,
                    start_time: NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 1).unwrap(),
                    kind: SuConn,
                },
                Element {
                    number: 1,
                    start_time: NaiveTime::from_hms_opt(0, 0, 2).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 2).unwrap(),
                    kind: Hybrid(vec_of_strings("T9a CB C3"), "3".to_string()),
                },
                Element {
                    number: 2,
                    start_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    kind: PairAcro("Js1B+pf".to_string()),
                },
            ],
            theme: "".to_string(),
            end_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
            iss_ver: Some(Version::new(3, 0, 2)),
        },
        issues.iter().next().unwrap().1
    )
}

#[test]
fn test_parse_iss_report() {
    let f = File::open("./tests/report.xls").expect("Could not open file");
    let mut file = BufReader::new(f);
    let cards = parse_excel("./tests/report.xls", &mut file).expect("Could not parse card");
    assert_eq!(2, cards.len());

    assert_eq!(
        (
            "1 MCA - MAD CITY AQUASTARS".to_string(),
            CoachCard {
                category: Category { ag: JRSR, free: true, event: Combo },
                elements: vec![
                    Element {
                        number: 1,
                        start_time: NaiveTime::from_hms_opt(0, 0, 15).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 17).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("A-Sq-Back-tk-t1").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 2,
                        start_time: NaiveTime::from_hms_opt(0, 0, 33).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 47).unwrap(),
                        kind: ChoHy,
                    },
                    Element {
                        number: 3,
                        start_time: NaiveTime::from_hms_opt(0, 0, 53).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 3).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("P-P-HA-wi/2ow-Pos3/Trav").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 4,
                        start_time: NaiveTime::from_hms_opt(0, 1, 8).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 28).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("F9 2R1 RC1 RC1 F6c 2RB RU1 A6 F3a F3b F2a"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 5,
                        start_time: NaiveTime::from_hms_opt(0, 1, 39).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 58).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("F7 2RB A3a F5a 1RB A3a F5a F1a F3b A1d A6 SC1 2PC"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 6,
                        start_time: NaiveTime::from_hms_opt(0, 2, 4).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 7).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("C-Thr>St-Forw-co-Jump").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 7,
                        start_time: NaiveTime::from_hms_opt(0, 2, 12).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 26).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("T6a A3b F5a F5a RU1 A8 F1a RC1 F8a"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 8,
                        start_time: NaiveTime::from_hms_opt(0, 2, 47).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 2).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("F9 A7 A4b F5a A4b F5a F1a RC1 RU1 RD1"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 9,
                        start_time: NaiveTime::from_hms_opt(0, 3, 15).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 28).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("T5c A3b 2R1 RC1 F5a F5a C4+ C4+ 1PC"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 10,
                        start_time: NaiveTime::from_hms_opt(0, 3, 32).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 34).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("B-StH-ShF-bb/2spl").unwrap(),
                            "".to_string()
                        ),
                    },
                ],
                theme: "foo".to_string(),
                end_time: NaiveTime::from_hms_opt(0, 3, 34).unwrap(),
                iss_ver: None,
            }
        ),
        cards[0]
    );

    assert_eq!(
        (
            "2 HEA - HEARTLAND".to_string(),
            CoachCard {
                category: Category { ag: JRSR, free: true, event: Combo },
                elements: vec![
                    Element {
                        number: 1,
                        start_time: NaiveTime::from_hms_opt(0, 0, 9).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 13).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("C-Thr>F-Forw-ln").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 2,
                        start_time: NaiveTime::from_hms_opt(0, 0, 21).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 40).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("A1c A6 A1c 2RB A1c 2RB 2R1 F1a F2b 2PC"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 3,
                        start_time: NaiveTime::from_hms_opt(0, 1, 24).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 40).unwrap(),
                        kind: Hybrid(vec_of_strings("A3b A6 1R4 S1 T7"), "".to_string()),
                    },
                    Element {
                        number: 4,
                        start_time: NaiveTime::from_hms_opt(0, 1, 41).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 55).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("P-2S-F2A-sd-Climb/Fall").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 5,
                        start_time: NaiveTime::from_hms_opt(0, 2, 3).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 27).unwrap(),
                        kind: Hybrid(
                            vec_of_strings("CB+*0.5 AB SC1 2RB A2b A3a S1 1PC"),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 6,
                        start_time: NaiveTime::from_hms_opt(0, 2, 30).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 34).unwrap(),
                        kind: TeamAcro(
                            TeamAcrobatic::from("A-Thr-Up-ln-t1").unwrap(),
                            "".to_string()
                        ),
                    },
                    Element {
                        number: 7,
                        start_time: NaiveTime::from_hms_opt(0, 2, 35).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 38).unwrap(),
                        kind: TeamAcro(TeamAcrobatic::from("B-St-FS-sd").unwrap(), "".to_string()),
                    },
                    Element {
                        number: 8,
                        start_time: NaiveTime::from_hms_opt(0, 2, 46).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 57).unwrap(),
                        kind: Hybrid(vec_of_strings("A3a S2 CB A1c"), "".to_string()),
                    },
                    Element {
                        number: 9,
                        start_time: NaiveTime::from_hms_opt(0, 2, 58).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 58).unwrap(),
                        kind: Hybrid(vec_of_strings("T6c SC1"), "".to_string()),
                    },
                    Element {
                        number: 10,
                        start_time: NaiveTime::from_hms_opt(0, 3, 6).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 22).unwrap(),
                        kind: ChoHy,
                    }
                ],
                theme: "foo".to_string(),
                end_time: NaiveTime::from_hms_opt(0, 3, 22).unwrap(),
                iss_ver: None,
            }
        ),
        cards[1]
    );
}
