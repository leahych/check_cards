#![allow(clippy::too_many_lines)]

use check_cards::*;
use chrono::NaiveTime;
use pretty_assertions::assert_eq;
use semver::Version;
use std::fs::File;
use std::io::BufReader;

use AwLC::*;
use ConnLC::*;
use ElementKind::*;
use FlexLC::*;
use LevelCode::*;
use SpinLC::*;
use TeamAcroKind::*;
use ThrustLC::*;
use TwistLC::*;

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
                        Platform(AcroP {
                            construction: PConst::P,
                            conn: PConn::HA,
                            positions: Positions { first: BPos::bb, second: Some(BPos::wi) },
                            rotation: None,
                            bonuses: [PBonus::Porp, PBonus::Trav].into(),
                        }),
                        Some(MilliDD(2225))
                    )
                },
                Element {
                    number: 2,
                    start_time: NaiveTime::from_hms_opt(0, 0, 21).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 24).unwrap(),
                    kind: TeamAcro(
                        Combined(AcroC {
                            construction: CConst::ThrAbove2F,
                            dir: CDir::Base(ADir::Forw),
                            positions: Positions {
                                first: CPos::B(BPos::ow),
                                second: Some(CPos::A(APos::ln))
                            },
                            bonusrotation: AcroCBonusRotation {
                                base: None,
                                featured: None,
                                bonuses: [].into(),
                            }
                        },),
                        Some(MilliDD(2125))
                    )
                },
                Element {
                    number: 3,
                    start_time: NaiveTime::from_hms_opt(0, 0, 31).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 35).unwrap(),
                    kind: TeamAcro(
                        Airborne(AcroA {
                            construction: AConst::Shou,
                            dir: ADir::Back,
                            positions: Positions { first: APos::tk, second: None },
                            rotation: Some(ARotation::s1),
                            bonuses: [].into(),
                        },),
                        Some(MilliDD(1925))
                    ),
                },
                Element {
                    number: 4,
                    start_time: NaiveTime::from_hms_opt(0, 0, 46).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 1, 0).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Flex(FB), f: Factor::No },
                                Decl { lc: Flex(F6a), f: Factor::No },
                                Decl { lc: Aw(AB), f: Factor::No },
                                Decl { lc: Aw(A6), f: Factor::No },
                                Decl { lc: Spin(S1), f: Factor::No },
                                Decl { lc: Aw(AB), f: Factor::No },
                                Decl { lc: Twist(_2R1), f: Factor::No },
                                Decl { lc: Flex(F1a), f: Factor::No },
                                Decl { lc: Flex(F2b), f: Factor::No },
                                Decl { lc: Flex(F3b), f: Factor::No },
                            ]),
                            pc_bonus: Some(PatternChanges(1))
                        },
                        Some(MilliDD(4050))
                    ),
                },
                Element {
                    number: 5,
                    start_time: NaiveTime::from_hms_opt(0, 1, 24).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 1, 38).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Aw(A3a), f: Factor::No },
                                Decl { lc: Aw(A6), f: Factor::No },
                                Decl { lc: Aw(A1c), f: Factor::No },
                                Decl { lc: Aw(A6), f: Factor::No },
                                Decl { lc: Flex(F4f), f: Factor::_0_3 },
                                Decl { lc: Flex(F4e), f: Factor::_0_5 },
                                Decl { lc: Flex(F4f), f: Factor::_0_5 },
                                Decl { lc: Flex(F4f), f: Factor::No },
                                Decl { lc: Aw(A1d), f: Factor::No },
                            ]),
                            pc_bonus: Some(PatternChanges(4))
                        },
                        Some(MilliDD(4920))
                    ),
                },
                Element {
                    number: 6,
                    start_time: NaiveTime::from_hms_opt(0, 1, 44).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 6).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Conn(C4, false), f: Factor::No },
                                Decl { lc: Conn(C4, false), f: Factor::No },
                                Decl { lc: Conn(C4, false), f: Factor::No },
                                Decl { lc: Twist(RD2), f: Factor::No },
                                Decl { lc: Spin(S1), f: Factor::No },
                            ]),
                            pc_bonus: Some(PatternChanges(2))
                        },
                        Some(MilliDD(3800))
                    ),
                },
                Element {
                    number: 7,
                    start_time: NaiveTime::from_hms_opt(0, 2, 30).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 2, 45).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Conn(CB, true), f: Factor::No },
                                Decl { lc: Aw(A5), f: Factor::No },
                                Decl { lc: Twist(_1RB), f: Factor::No },
                                Decl { lc: Aw(A6), f: Factor::No },
                                Decl { lc: Conn(CB, true), f: Factor::No },
                                Decl { lc: Conn(CB, true), f: Factor::No },
                                Decl { lc: Aw(A1c), f: Factor::No },
                                Decl { lc: Twist(_2RB), f: Factor::No },
                            ]),
                            pc_bonus: Some(PatternChanges(1))
                        },
                        Some(MilliDD(3550))
                    ),
                },
                Element {
                    number: 8,
                    start_time: NaiveTime::from_hms_opt(0, 2, 50).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 7).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Thrust(T3d), f: Factor::No },
                                Decl { lc: Aw(A2b), f: Factor::No },
                                Decl { lc: Aw(A2b), f: Factor::No },
                                Decl { lc: Aw(A2b), f: Factor::No },
                                Decl { lc: Aw(A3a), f: Factor::No },
                                Decl { lc: Spin(S2), f: Factor::No },
                            ]),
                            pc_bonus: None
                        },
                        Some(MilliDD(2550))
                    ),
                },
                Element {
                    number: 9,
                    start_time: NaiveTime::from_hms_opt(0, 3, 15).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 3, 26).unwrap(),
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Aw(AB), f: Factor::No },
                                Decl { lc: Aw(A6), f: Factor::No },
                                Decl { lc: Twist(RB), f: Factor::No },
                                Decl { lc: Aw(A1c), f: Factor::No },
                                Decl { lc: Twist(RO1), f: Factor::No },
                                Decl { lc: Flex(F2b), f: Factor::No },
                            ]),
                            pc_bonus: Some(PatternChanges(3))
                        },
                        Some(MilliDD(3250))
                    ),
                },
            ]
            .into(),
            theme: "Test".to_string(),
            iss_ver: Some(Version::new(3, 0, 6)),
            end_time: NaiveTime::from_hms_opt(0, 3, 29).unwrap(),
        },
        issues.first().unwrap().1
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
                kind: ChoHy(Some(MilliDD(1000))),
            }]
            .into(),
            theme: String::new(),
            end_time: NaiveTime::from_hms_opt(0, 0, 1).unwrap(),
            iss_ver: Some(Version::new(3, 0, 2)),
        },
        issues.first().unwrap().1
    );
}

#[test]
fn test_parse_iss_mixed_duet() {
    let f = File::open("./tests/Mixed Duet.xlsx").expect("Could not open file");
    let mut file = BufReader::new(f);
    let issues = parse_excel("", &mut file).expect("Could not parse card");
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
                    kind: Hybrid(
                        HybridDecl {
                            decls: Box::from([
                                Decl { lc: Thrust(T9a), f: Factor::No },
                                Decl { lc: Conn(CB, false), f: Factor::No },
                                Decl { lc: Conn(C3, false), f: Factor::No }
                            ]),
                            pc_bonus: None
                        },
                        Some(MilliDD(3000))
                    ),
                },
                Element {
                    number: 2,
                    start_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    stop_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
                    kind: PairAcro(PairAcroKind::Js1BPluspFlex, Some(MilliDD(2250))),
                },
            ]
            .into(),
            theme: String::new(),
            end_time: NaiveTime::from_hms_opt(0, 0, 3).unwrap(),
            iss_ver: Some(Version::new(3, 0, 2)),
        },
        issues.first().unwrap().1
    );
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
                category: Category { ag: AgeGroups::JRSR, free: true, event: Events::Combo },
                elements: vec![
                    Element {
                        number: 1,
                        start_time: NaiveTime::from_hms_opt(0, 0, 15).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 17).unwrap(),
                        kind: TeamAcro(
                            Airborne(AcroA {
                                construction: AConst::Sq,
                                dir: ADir::Back,
                                positions: Positions { first: APos::tk, second: None },
                                bonuses: [].into(),
                                rotation: Some(ARotation::t1),
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 2,
                        start_time: NaiveTime::from_hms_opt(0, 0, 33).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 47).unwrap(),
                        kind: ChoHy(None),
                    },
                    Element {
                        number: 3,
                        start_time: NaiveTime::from_hms_opt(0, 0, 53).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 3).unwrap(),
                        kind: TeamAcro(
                            Platform(AcroP {
                                construction: PConst::P,
                                conn: PConn::HA,
                                positions: Positions { first: BPos::wi, second: Some(BPos::ow) },
                                bonuses: [PBonus::Pos3, PBonus::Trav].into(),
                                rotation: None,
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 4,
                        start_time: NaiveTime::from_hms_opt(0, 1, 8).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 28).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Flex(F9), f: Factor::No },
                                    Decl { lc: Twist(_2R1), f: Factor::No },
                                    Decl { lc: Twist(RC1), f: Factor::No },
                                    Decl { lc: Twist(RC1), f: Factor::No },
                                    Decl { lc: Flex(F6c), f: Factor::No },
                                    Decl { lc: Twist(_2RB), f: Factor::No },
                                    Decl { lc: Twist(RU1), f: Factor::No },
                                    Decl { lc: Aw(A6), f: Factor::No },
                                    Decl { lc: Flex(F3a), f: Factor::No },
                                    Decl { lc: Flex(F3b), f: Factor::No },
                                    Decl { lc: Flex(F2a), f: Factor::No },
                                ]),
                                pc_bonus: None
                            },
                            None
                        ),
                    },
                    Element {
                        number: 5,
                        start_time: NaiveTime::from_hms_opt(0, 1, 39).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 58).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Flex(F7), f: Factor::No },
                                    Decl { lc: Twist(_2RB), f: Factor::No },
                                    Decl { lc: Aw(A3a), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Twist(_1RB), f: Factor::No },
                                    Decl { lc: Aw(A3a), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Flex(F1a), f: Factor::No },
                                    Decl { lc: Flex(F3b), f: Factor::No },
                                    Decl { lc: Aw(A1d), f: Factor::No },
                                    Decl { lc: Aw(A6), f: Factor::No },
                                    Decl { lc: Spin(SC1), f: Factor::No },
                                ]),
                                pc_bonus: Some(PatternChanges(2)),
                            },
                            None
                        ),
                    },
                    Element {
                        number: 6,
                        start_time: NaiveTime::from_hms_opt(0, 2, 4).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 7).unwrap(),
                        kind: TeamAcro(
                            Combined(AcroC {
                                construction: CConst::ThrOntoSt,
                                dir: CDir::Base(ADir::Forw),
                                positions: Positions { first: CPos::B(BPos::co), second: None },
                                bonusrotation: AcroCBonusRotation {
                                    bonuses: [CBonus::Jump].into(),
                                    base: None,
                                    featured: None,
                                }
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 7,
                        start_time: NaiveTime::from_hms_opt(0, 2, 12).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 26).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Thrust(T6a), f: Factor::No },
                                    Decl { lc: Aw(A3b), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Twist(RU1), f: Factor::No },
                                    Decl { lc: Aw(A8), f: Factor::No },
                                    Decl { lc: Flex(F1a), f: Factor::No },
                                    Decl { lc: Twist(RC1), f: Factor::No },
                                    Decl { lc: Flex(F8a), f: Factor::No },
                                ]),
                                pc_bonus: None,
                            },
                            None
                        ),
                    },
                    Element {
                        number: 8,
                        start_time: NaiveTime::from_hms_opt(0, 2, 47).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 2).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Flex(F9), f: Factor::No },
                                    Decl { lc: Aw(A7), f: Factor::No },
                                    Decl { lc: Aw(A4b), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Aw(A4b), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Flex(F1a), f: Factor::No },
                                    Decl { lc: Twist(RC1), f: Factor::No },
                                    Decl { lc: Twist(RU1), f: Factor::No },
                                    Decl { lc: Twist(RD1), f: Factor::No },
                                ]),
                                pc_bonus: None,
                            },
                            None
                        ),
                    },
                    Element {
                        number: 9,
                        start_time: NaiveTime::from_hms_opt(0, 3, 15).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 28).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Thrust(T5c), f: Factor::No },
                                    Decl { lc: Aw(A3b), f: Factor::No },
                                    Decl { lc: Twist(_2R1), f: Factor::No },
                                    Decl { lc: Twist(RC1), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Flex(F5a), f: Factor::No },
                                    Decl { lc: Conn(C4, true), f: Factor::No },
                                    Decl { lc: Conn(C4, true), f: Factor::No },
                                ]),
                                pc_bonus: Some(PatternChanges(1)),
                            },
                            None
                        ),
                    },
                    Element {
                        number: 10,
                        start_time: NaiveTime::from_hms_opt(0, 3, 32).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 34).unwrap(),
                        kind: TeamAcro(
                            Balance(AcroB {
                                construction: BConst::StH,
                                conn: BConn::ShF,
                                positions: Positions { first: BPos::bb, second: Some(BPos::spl) },
                                bonuses: [].into(),
                                rotation: None,
                            }),
                            None
                        ),
                    },
                ]
                .into(),
                theme: "foo".to_string(),
                end_time: NaiveTime::from_hms_opt(0, 3, 34).unwrap(),
                iss_ver: None,
            },
            [].into()
        ),
        cards[0]
    );

    assert_eq!(
        (
            "2 HEA - HEARTLAND".to_string(),
            CoachCard {
                category: Category { ag: AgeGroups::JRSR, free: true, event: Events::Combo },
                elements: vec![
                    Element {
                        number: 1,
                        start_time: NaiveTime::from_hms_opt(0, 0, 9).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 13).unwrap(),
                        kind: TeamAcro(
                            Combined(AcroC {
                                construction: CConst::ThrOntoF,
                                dir: CDir::Base(ADir::Forw),
                                positions: Positions { first: CPos::A(APos::ln), second: None },
                                bonusrotation: AcroCBonusRotation {
                                    bonuses: [].into(),
                                    base: None,
                                    featured: None,
                                }
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 2,
                        start_time: NaiveTime::from_hms_opt(0, 0, 21).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 0, 40).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Aw(A1c), f: Factor::No },
                                    Decl { lc: Aw(A6), f: Factor::No },
                                    Decl { lc: Aw(A1c), f: Factor::No },
                                    Decl { lc: Twist(_2RB), f: Factor::No },
                                    Decl { lc: Aw(A1c), f: Factor::No },
                                    Decl { lc: Twist(_2RB), f: Factor::No },
                                    Decl { lc: Twist(_2R1), f: Factor::No },
                                    Decl { lc: Flex(F1a), f: Factor::No },
                                    Decl { lc: Flex(F2b), f: Factor::No },
                                ]),
                                pc_bonus: Some(PatternChanges(2))
                            },
                            None
                        ),
                    },
                    Element {
                        number: 3,
                        start_time: NaiveTime::from_hms_opt(0, 1, 24).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 40).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Aw(A3b), f: Factor::No },
                                    Decl { lc: Aw(A6), f: Factor::No },
                                    Decl { lc: Twist(_1R4), f: Factor::No },
                                    Decl { lc: Spin(S1), f: Factor::No },
                                    Decl { lc: Thrust(T7), f: Factor::No },
                                ]),
                                pc_bonus: None
                            },
                            None
                        ),
                    },
                    Element {
                        number: 4,
                        start_time: NaiveTime::from_hms_opt(0, 1, 41).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 1, 55).unwrap(),
                        kind: TeamAcro(
                            Platform(AcroP {
                                construction: PConst::_2S,
                                conn: PConn::F2A,
                                positions: Positions { first: BPos::sd, second: None },
                                bonuses: [PBonus::Climb, PBonus::Fall].into(),
                                rotation: None,
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 5,
                        start_time: NaiveTime::from_hms_opt(0, 2, 3).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 27).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Conn(CB, true), f: Factor::_0_5 },
                                    Decl { lc: Aw(AB), f: Factor::No },
                                    Decl { lc: Spin(SC1), f: Factor::No },
                                    Decl { lc: Twist(_2RB), f: Factor::No },
                                    Decl { lc: Aw(A2b), f: Factor::No },
                                    Decl { lc: Aw(A3a), f: Factor::No },
                                    Decl { lc: Spin(S1), f: Factor::No },
                                ]),
                                pc_bonus: Some(PatternChanges(1))
                            },
                            None
                        ),
                    },
                    Element {
                        number: 6,
                        start_time: NaiveTime::from_hms_opt(0, 2, 30).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 34).unwrap(),
                        kind: TeamAcro(
                            Airborne(AcroA {
                                construction: AConst::Thr,
                                dir: ADir::Up,
                                positions: Positions { first: APos::ln, second: None },
                                bonuses: [].into(),
                                rotation: Some(ARotation::t1),
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 7,
                        start_time: NaiveTime::from_hms_opt(0, 2, 35).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 38).unwrap(),
                        kind: TeamAcro(
                            Balance(AcroB {
                                construction: BConst::St,
                                conn: BConn::FS,
                                positions: Positions { first: BPos::sd, second: None },
                                bonuses: [].into(),
                                rotation: None,
                            }),
                            None
                        ),
                    },
                    Element {
                        number: 8,
                        start_time: NaiveTime::from_hms_opt(0, 2, 46).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 57).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Aw(A3a), f: Factor::No },
                                    Decl { lc: Spin(S2), f: Factor::No },
                                    Decl { lc: Conn(CB, false), f: Factor::No },
                                    Decl { lc: Aw(A1c), f: Factor::No },
                                ]),
                                pc_bonus: None
                            },
                            None
                        ),
                    },
                    Element {
                        number: 9,
                        start_time: NaiveTime::from_hms_opt(0, 2, 58).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 2, 58).unwrap(),
                        kind: Hybrid(
                            HybridDecl {
                                decls: Box::from([
                                    Decl { lc: Thrust(T6c), f: Factor::No },
                                    Decl { lc: Spin(SC1), f: Factor::No }
                                ]),
                                pc_bonus: None
                            },
                            None
                        ),
                    },
                    Element {
                        number: 10,
                        start_time: NaiveTime::from_hms_opt(0, 3, 6).unwrap(),
                        stop_time: NaiveTime::from_hms_opt(0, 3, 22).unwrap(),
                        kind: ChoHy(None),
                    }
                ]
                .into(),
                theme: "foo".to_string(),
                end_time: NaiveTime::from_hms_opt(0, 3, 22).unwrap(),
                iss_ver: None,
            },
            [].into()
        ),
        cards[1]
    );
}
