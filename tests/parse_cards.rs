use check_cards::AcroDirection::{Backwards, Forwards};
use check_cards::AcroGroup::{Airborne, Combined, Platform};
use check_cards::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TeamAcro, TRE};
use check_cards::{parse_iss_card, AgeGroups, Category, CoachCard, Element, Events, TeamAcrobatic};
use chrono::NaiveTime;
use pretty_assertions::assert_eq;
use semver::Version;
use std::fs::File;
use std::io::BufReader;

#[test]
fn test_parse_aqua_team() {
    let f = File::open("./tests/Tech Team test.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let card = parse_iss_card(&mut file).expect("Could not parse card");
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
        card
    );
}

macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

#[test]
fn test_parse_iss_team() {
    let f = File::open("./tests/SENIOR-Team_Free-PRELIMS-OCC-.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let card = parse_iss_card(&mut file).expect("Could not parse card");
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
                            positions: vec!["bb".to_string()],
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
                        vec_of_strings![
                            "FB", "F6a", "AB", "A6", "S1", "AB", "2R1", "F1a", "F2b", "F3a", "1PC"
                        ],
                        "3.75".into()
                    ),
                },
                Element {
                    number: 5,
                    start_time: NaiveTime::from_hms_opt(0, 1, 24).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 1, 38).unwrap(),
                    kind: Hybrid(
                        vec_of_strings![
                            "A3a", "A6", "A1c", "A6", "F4f*0.3", "F4f*0.5", "F4f*0.5", "F4f",
                            "A1d", "4PC"
                        ],
                        "4.92".into()
                    ),
                },
                Element {
                    number: 6,
                    start_time: NaiveTime::from_hms_opt(0, 1, 44).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 06).unwrap(),
                    kind: Hybrid(
                        vec_of_strings!["C4", "C4", "C4", "RD2", "S1", "2PC"],
                        "3.8".into()
                    )
                },
                Element {
                    number: 7,
                    start_time: NaiveTime::from_hms_opt(0, 2, 30).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 45).unwrap(),
                    kind: Hybrid(
                        vec_of_strings![
                            "CB+", "A4b", "1RB", "A6", "CB+", "CB+", "A1c", "2RB", "1PC"
                        ],
                        "3.35".into()
                    ),
                },
                Element {
                    number: 8,
                    start_time: NaiveTime::from_hms_opt(0, 2, 50).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 7).unwrap(),
                    kind: Hybrid(
                        vec_of_strings!["T3d", "A2b", "A2b", "A2b", "A3a", "S2"],
                        "2.55".into()
                    ),
                },
                Element {
                    number: 9,
                    start_time: NaiveTime::from_hms_opt(0, 3, 15).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 26).unwrap(),
                    kind: Hybrid(
                        vec_of_strings!["AB", "A6", "RB", "A1c", "RO1", "F2b", "3PC"],
                        "3.25".into()
                    ),
                },
            ],
            theme: "Test".to_string(),
            iss_ver: Some(Version::new(3, 0, 3)),
            end_time: NaiveTime::from_hms_opt(0, 3, 29).unwrap()
        },
        card
    );
}

#[test]
fn test_parse_iss_combo() {
    let f = File::open("./tests/Combo.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let card = parse_iss_card(&mut file).expect("Could not parse card");
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
        card
    )
}

#[test]
fn test_parse_iss_mixed_duet() {
    let f = File::open("./tests/Mixed Duet.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let card = parse_iss_card(&mut file).expect("Could not parse card");
    assert_eq!(
        CoachCard {
            category: Category { ag: AgeGroups::JRSR, free: true, event: Events::MixedDuet },
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
                    kind: Hybrid(vec_of_strings!["T9a", "CB", "C3"], "3".to_string()),
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
        card
    )
}
