use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::{AgeGroups, CardIssues, Category, CoachCard, Element, Events};
use chrono::NaiveTime;
use regex_lite::Regex;
use std::collections::{HashMap, HashSet};
use std::time::Duration;

macro_rules! hybrids {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            Hybrid(decl, dd) => Some((e.number, decl, dd)),
            _ => None,
        })
    };
}

macro_rules! pair_acros {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            PairAcro(d) => Some((e.number, d)),
            _ => None,
        })
    };
}
pub(crate) use pair_acros;

macro_rules! team_acros {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            TeamAcro(d, dd) => Some((e.number, d, dd)),
            _ => None,
        })
    };
}
use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};
pub(crate) use team_acros;

macro_rules! tres {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            TRE(d) => Some((e.number, d)),
            _ => None,
        })
    };
}

const LATEST_ISS_VERSION: semver::Version = semver::Version::new(3, 0, 3);
fn check_iss_version(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if let Some(iss_ver) = card.iss_ver.as_ref() {
        if iss_ver < &LATEST_ISS_VERSION {
            ci.warnings.push(format!(
                "Card produced with version {iss_ver}, latest is {LATEST_ISS_VERSION}"
            ));
        }
    }
    ci
}

fn points_for_declaration<T: AsRef<str>>(declaration: T) -> usize {
    let re = Regex::new(r"0\.\d").unwrap();
    match re.captures(declaration.as_ref()) {
        // as far as points go, factoring by 0.3 is the same as factoring
        // by 0.5. Ex. if the limit is 3, you get x6 0.5s or x6 0.3s not
        // x9 0.3s.
        Some(_) => 5,
        None => 10,
    }
}

fn check_max_families<T: AsRef<str>>(decls: &[T], family_regex: &Regex) -> usize {
    let matching_families = decls.iter().filter(|value| family_regex.is_match(value.as_ref()));
    let points = matching_families.map(points_for_declaration);
    points.reduce(|total, item| total + item).unwrap_or(0)
}

fn check_element_declaration_maxes<T: AsRef<str>>(num: usize, decls: &[T]) -> CardIssues {
    let mut ci = CardIssues::default();

    let max_families: &[(&str, &str)] = &[
        ("Airborne Weight", "^A"),
        ("Connections", "^C"),
        ("Flexibility", "^F"),
        ("Twists", "^R|^1R|^2R"),
        ("Spins", "^S"),
        ("Thrusts", "^T"),
    ];

    for (name, rx_str) in max_families {
        let rx = Regex::new(rx_str).unwrap();
        if check_max_families(decls, &rx) > 50 {
            ci.errors.push(format!("Element {num}: {name} can only be declared 5 times"));
        }
    }

    let factor_regex = Regex::new(r"\*0\.\d").unwrap();
    let mut decl_points = HashMap::<String, usize>::new();
    for decl in decls {
        let base_name = factor_regex.replace_all(decl.as_ref(), "").replace('+', "");
        let points =
            decl_points.get(&base_name).copied().unwrap_or(0) + points_for_declaration(decl);
        decl_points.insert(base_name, points);
    }

    for (decl, points) in decl_points {
        if points > 30 {
            ci.errors.push(format!("Element {num}: {decl} is used more than 3 times"));
        }
    }
    ci
}

fn check_hybrid_declaration_maxes(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    for (num, hybrid, _) in hybrids!(card.elements) {
        ci += check_element_declaration_maxes(num, hybrid);
    }
    ci
}

struct ElementLimit {
    cho_hy: usize,
    su_conn: usize,
    tre: usize,
    acrobatic: usize,
    hybrid: usize,
}

#[allow(clippy::too_many_lines)]
fn element_maxes(category: &Category) -> Option<&ElementLimit> {
    static HM: std::sync::OnceLock<HashMap<Category, ElementLimit>> = std::sync::OnceLock::new();
    let map = HM.get_or_init(|| {
        HashMap::from([
            // 12-U
            (
                Category { ag: AG12U, event: Solo, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 0, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: Duet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 1, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: MixedDuet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 3, tre: 0, acrobatic: 2, hybrid: 3 },
            ),
            (
                Category { ag: AG12U, event: Team, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 3, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: Combo, free: true },
                ElementLimit { cho_hy: 1, su_conn: 0, tre: 0, acrobatic: 3, hybrid: 4 },
            ),
            // Youth free
            (
                Category { ag: Youth, event: Solo, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 0, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: Duet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 1, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: MixedDuet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 3, tre: 0, acrobatic: 2, hybrid: 3 },
            ),
            (
                Category { ag: Youth, event: Team, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 3, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: Combo, free: true },
                ElementLimit { cho_hy: 1, su_conn: 0, tre: 0, acrobatic: 4, hybrid: 4 },
            ),
            // Youth tech - USAAS experimental
            (
                Category { ag: Youth, event: Solo, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 0, hybrid: 1 },
            ),
            (
                Category { ag: Youth, event: Duet, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 1, hybrid: 1 },
            ),
            (
                Category { ag: Youth, event: MixedDuet, free: false },
                ElementLimit { cho_hy: 0, su_conn: 3, tre: 3, acrobatic: 2, hybrid: 2 },
            ),
            (
                Category { ag: Youth, event: Team, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 1, hybrid: 2 },
            ),
            // JR/SR free
            (
                Category { ag: JRSR, event: Solo, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 0, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: Duet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 2, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: MixedDuet, free: true },
                ElementLimit { cho_hy: 0, su_conn: 4, tre: 0, acrobatic: 3, hybrid: 4 },
            ),
            (
                Category { ag: JRSR, event: Trio, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 3, hybrid: 5 },
            ),
            (
                Category { ag: JRSR, event: Team, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 3, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: Acrobatic, free: true },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 7, hybrid: 0 },
            ),
            (
                Category { ag: JRSR, event: Combo, free: true },
                ElementLimit { cho_hy: 1, su_conn: 0, tre: 0, acrobatic: 4, hybrid: 5 },
            ),
            // JR/SR tech
            (
                Category { ag: JRSR, event: Solo, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 0, hybrid: 1 },
            ),
            (
                Category { ag: JRSR, event: Duet, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 1, hybrid: 1 },
            ),
            (
                Category { ag: JRSR, event: MixedDuet, free: false },
                ElementLimit { cho_hy: 0, su_conn: 3, tre: 3, acrobatic: 2, hybrid: 2 },
            ),
            (
                Category { ag: JRSR, event: Team, free: false },
                ElementLimit { cho_hy: 0, su_conn: 0, tre: 5, acrobatic: 1, hybrid: 2 },
            ),
        ])
    });
    map.get(category)
}

fn count_elements(elements: &[Element]) -> ElementLimit {
    let mut el = ElementLimit { cho_hy: 0, su_conn: 0, tre: 0, acrobatic: 0, hybrid: 0 };
    for element in elements {
        match element.kind {
            ChoHy => el.cho_hy += 1,
            SuConn => el.su_conn += 1,
            TRE(..) => el.tre += 1,
            PairAcro(..) | TeamAcro(..) => el.acrobatic += 1,
            Hybrid(..) => el.hybrid += 1,
        }
    }
    el
}

fn check_routine_element_maxes(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    element_maxes(&card.category).map_or_else(
        || {
            ci.warnings.push(format!("Could not determine element limits for {}", card.category));
        },
        |maxes| {
            let num_elements = count_elements(&card.elements);
            if num_elements.cho_hy != maxes.cho_hy {
                ci.errors.push(format!(
                    "{} Choreography Hybrids expected in {}, but {} found",
                    maxes.cho_hy, card.category, num_elements.cho_hy
                ));
            }
            if num_elements.su_conn != maxes.su_conn {
                ci.errors.push(format!(
                    "{} Surface Connections expected in {}, but {} found",
                    maxes.su_conn, card.category, num_elements.su_conn
                ));
            }
            if num_elements.tre != maxes.tre {
                ci.errors.push(format!(
                    "{} TREs expected in {}, but {} found",
                    maxes.tre, card.category, num_elements.tre
                ));
            }
            if num_elements.acrobatic != maxes.acrobatic {
                ci.errors.push(format!(
                    "{} Acrobatics expected in {}, but {} found",
                    maxes.acrobatic, card.category, num_elements.acrobatic
                ));
            }
            if num_elements.hybrid != maxes.hybrid {
                ci.errors.push(format!(
                    "{} Hybrids expected in {}, but {} found",
                    maxes.hybrid, card.category, num_elements.hybrid
                ));
            }
        },
    );
    ci
}

fn check_theme(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    let theme_error = match card.category.event {
        Acrobatic | Combo => card.theme.is_empty(),
        _ => false,
    };
    if theme_error {
        ci.errors.push("A theme is required for acrobatic and Combo routines".into());
    }
    ci
}

fn check_small_bonuses(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    match card.category.event {
        Combo | Team => {}
        _ => {
            let re = Regex::new(r"\dPC").unwrap();
            for (idx, declarations, _) in hybrids!(card.elements) {
                if declarations.iter().any(|decl| re.is_match(decl)) {
                    ci.errors.push(format!(
                        "Element {}: {} cannot have Pattern Change bonuses",
                        idx + 1,
                        card.category.event.as_str()
                    ));
                }
            }
        }
    }
    ci
}

fn check_acros_event(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    let pair_acros = pair_acros!(card.elements);
    let team_acros = team_acros!(card.elements);

    for (num, _, _) in team_acros {
        match card.category.event {
            Solo => ci
                .errors
                .push(format!("Element {num}: Acrobatic elements are not allowed in a solo")),
            Duet | MixedDuet | Trio => ci.errors.push(format!(
                "Element {num}: Invalid Acrobatic element, must be one of Jump, Lift or Throw"
            )),
            Acrobatic | Combo | Team | Events::Unknown => {}
        }
    }
    for (num, _) in pair_acros {
        match card.category.event {
            Solo => ci
                .errors
                .push(format!("Element {num}: Acrobatic elements are not allowed in a solo")),
            Duet | MixedDuet | Trio | Events::Unknown => {}
            Acrobatic | Combo | Team => {
                ci.errors.push(format!("Element {num}: Invalid Acrobatic element, must be one of Airborne, Balance, Platform or Combined"));
            }
        }
    }
    ci
}

fn check_valid_hybrid_declarations(card: &CoachCard) -> CardIssues {
    let valid_decl_regex = Regex::new(concat!(
    "^(",
    r"(S([B1-9]|10)|SCD?[B1-6])(\*0.[35])?",
    r"|(R[B1-4]|R[CO][B1]|(1R|RD)[B1-6]|(2R|RU)([B1-9]|10))(\*0.[35])?",
    r"|A(B|1[a-dA-D]|2[abAB]|3[abAB]|4[abAB]|[5-8])(\*0.[35])?",
    r"|F(B|1[abcABC]|2[abcABC]|3[abc|ABC]|4[a-fA-F]|5[abcABC]|6[a-dA-D]|7|8[abAB]|9|10)(\*0.[35])?",
    r"|C(B|1[abAB]|2[abcABC]|3|4|5|6[abAB]|7)\+?(\*0.[35])?",
    r"|T(B|1|2[abAB]|3[a-dA-D]|4[a-eA-E]|5[a-eA-E]|6[abcABC]|7|8|9[abAB])(\*0.[35])?",
    r"|\dPC",
    ")$"
    ))
        .unwrap();

    let mut ci = CardIssues::default();
    for (num, hybrid, _) in hybrids!(card.elements) {
        for decl in hybrid {
            if !valid_decl_regex.is_match(decl) {
                ci.errors
                    .push(format!("Element {num}: '{decl}' is not a valid difficulty declaration"));
            }
        }
    }
    ci
}

// TODO this might change to only one thrust/2 connection but other decls are ok
fn check_mixed_duet_elements(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    let expected_cat = Category { ag: JRSR, event: MixedDuet, free: false };
    if card.category != expected_cat {
        return ci;
    }

    let conn_regex = Regex::new(r"^C[\dB][A-z]?").unwrap();
    let thrust_regex = Regex::new(r"^T[\dB]").unwrap();
    let other_families_regex = Regex::new(r"^[AFRS]").unwrap();
    let hybrids = hybrids!(card.elements);
    let con_thrust_hybrids = hybrids
        .filter(|(_, declarations, _)| {
            declarations.iter().filter(|decl| conn_regex.is_match(decl)).count() == 2
        })
        .filter(|(_, declarations, _)| {
            declarations
                .iter()
                .filter(|decl| thrust_regex.is_match(decl) && !decl.contains('*'))
                .count()
                == 1
        })
        .filter(|(_, declarations, _)| {
            declarations.iter().filter(|decl| other_families_regex.is_match(decl)).count() == 0
        });

    for (_, declarations, _) in con_thrust_hybrids {
        let mut connections = declarations.iter().filter(|decl| conn_regex.is_match(decl));
        if connections.next() != connections.next() {
            return ci;
        }
    }
    ci.errors.push("Mixed Duet Tech routines must have one hybrid with only one Thrust and two different Connections".into());
    ci
}

fn check_factoring(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    for (num, hybrid, _) in hybrids!(card.elements) {
        let mut prev_decl = "";
        for decl in hybrid {
            if card.category.event == Solo && decl.contains("*0.") {
                ci.errors.push(format!("Element {num}: cannot factor in a Solo`"));
            }
            if card.category.event == Duet || card.category.event == MixedDuet {
                if decl.contains("0.3") {
                    ci.errors.push(format!("Element {num}: cannot factor by 0.3 in a Duet"));
                } else if Regex::new(r"^C.*0\.").unwrap().is_match(decl) {
                    ci.errors.push(format!(
                        "Element {num}: factoring connections in a Duet seems suspicious"
                    ));
                }
            }
            if !card.category.free && decl.contains("*0.") {
                ci.warnings.push(format!(
                    "Element {num}: factoring in a Tech {:?} seems suspicious",
                    card.category.event
                ));
            }

            let conn_factored_rx = Regex::new(r"^C[\dB][A-z]?\*0.5").unwrap();
            let conn_plus_factored_rx = Regex::new(r"^C[\dB][A-z]?\+\*0.5").unwrap();
            if (conn_factored_rx.is_match(decl) && conn_plus_factored_rx.is_match(prev_decl))
                || (conn_plus_factored_rx.is_match(decl) && conn_factored_rx.is_match(prev_decl))
            {
                ci.warnings.push(format!("Element {num}: if factoring a connection because 5-7 are swimming, one connection should be factored by 0.3"));
            }
            if (prev_decl.starts_with("C4*0.") && decl == "C4+*0.5")
                || (prev_decl == "C4+*0.5" && decl.starts_with("C4*0."))
            {
                ci.warnings.push(format!("Element {num}: if factoring a C4+ because 5-7 are swimming, is the smaller group still doing C4?"));
            }

            let decl_parts: Vec<&str> = decl.split('*').collect();
            let prev_decl_parts: Vec<&str> = prev_decl.split('*').collect();
            if decl_parts.len() == 2
                && prev_decl_parts.len() == 2
                && decl_parts[0] == prev_decl_parts[0]
            {
                ci.warnings.push(format!("Element {num}: if two groups are doing the same choreography in two groups, do not factor"));
            }

            let cplus_less_half_rx = Regex::new(r"^C[\dB][A-z]?\+\*0.3").unwrap();
            if cplus_less_half_rx.is_match(decl) {
                ci.warnings
                    .push(format!("Element {num}: factoring C+ by 0.3 requires 9-10 athletes"));
            }

            prev_decl = decl;
        }
    }
    ci
}

const fn routine_time(min: u32, secs: u32) -> NaiveTime {
    NaiveTime::from_hms_opt(0, min, secs).unwrap()
}

fn check_routine_times(card: &CoachCard) -> CardIssues {
    static HM: std::sync::OnceLock<HashMap<Category, NaiveTime>> = std::sync::OnceLock::new();
    let map = HM.get_or_init(|| {
        HashMap::from([
            // 12-U
            (Category { ag: AG12U, event: Solo, free: true }, routine_time(2, 0)),
            (Category { ag: AG12U, event: Duet, free: true }, routine_time(2, 30)),
            (Category { ag: AG12U, event: MixedDuet, free: true }, routine_time(2, 30)),
            (Category { ag: AG12U, event: Team, free: true }, routine_time(3, 0)),
            (Category { ag: AG12U, event: Combo, free: true }, routine_time(3, 0)),
            // Youth free
            (Category { ag: Youth, event: Solo, free: true }, routine_time(2, 0)),
            (Category { ag: Youth, event: Duet, free: true }, routine_time(2, 30)),
            (Category { ag: Youth, event: MixedDuet, free: true }, routine_time(2, 30)),
            (Category { ag: Youth, event: Team, free: true }, routine_time(3, 0)),
            (Category { ag: Youth, event: Combo, free: true }, routine_time(3, 0)),
            // Youth tech - USAAS experimental
            (Category { ag: Youth, event: Solo, free: false }, routine_time(2, 0)),
            (Category { ag: Youth, event: Duet, free: false }, routine_time(2, 20)),
            (Category { ag: Youth, event: MixedDuet, free: false }, routine_time(2, 20)),
            (Category { ag: Youth, event: Team, free: false }, routine_time(2, 50)),
            // JR/SR free
            (Category { ag: JRSR, event: Solo, free: true }, routine_time(2, 15)),
            (Category { ag: JRSR, event: Duet, free: true }, routine_time(2, 45)),
            (Category { ag: JRSR, event: MixedDuet, free: true }, routine_time(2, 45)),
            (Category { ag: JRSR, event: Trio, free: true }, routine_time(2, 45)),
            (Category { ag: JRSR, event: Team, free: true }, routine_time(3, 30)),
            (Category { ag: JRSR, event: Acrobatic, free: true }, routine_time(3, 0)),
            (Category { ag: JRSR, event: Combo, free: true }, routine_time(3, 30)),
            // JR/SR tech
            (Category { ag: JRSR, event: Solo, free: false }, routine_time(2, 0)),
            (Category { ag: JRSR, event: Duet, free: false }, routine_time(2, 20)),
            (Category { ag: JRSR, event: MixedDuet, free: false }, routine_time(2, 20)),
            (Category { ag: JRSR, event: Team, free: false }, routine_time(2, 50)),
        ])
    });

    let mut ci = CardIssues::default();
    let expected_time = map.get(&card.category);
    if let Some(expected_time) = expected_time {
        let min_time = *expected_time - Duration::new(5, 0);
        let max_time = *expected_time + Duration::new(5, 0);
        if card.end_time < min_time || card.end_time > max_time {
            ci.errors.push(format!(
                "The end time of the routine, {}, is not between {} and {} as expected for a {}",
                card.end_time.format("%M:%S"),
                min_time.format("%M:%S"),
                max_time.format("%M:%S"),
                card.category
            ));
        }
    } else {
        ci.warnings.push(format!("Could not determine routine time for {}", card.category));
    }
    ci
}

fn check_tres(card: &CoachCard) -> CardIssues {
    static HASHMAP: std::sync::OnceLock<HashMap<Events, HashSet<&str>>> =
        std::sync::OnceLock::new();
    let tre_map = HASHMAP.get_or_init(|| {
        HashMap::from([
            (
                Solo,
                vec![
                    "TRE1a", "TRE1b", "TRE2a", "TRE2b", "TRE3", "TRE4a", "TRE4b", "TRE5a", "TRE5b",
                ]
                .into_iter()
                .collect(),
            ),
            (
                Duet,
                vec![
                    "TRE1a", "TRE1b", "TRE2a", "TRE2b", "TRE3", "TRE4a", "TRE4b", "TRE5a", "TRE5b",
                ]
                .into_iter()
                .collect(),
            ),
            (MixedDuet, vec!["TRE1a", "TRE1b", "TRE2a", "TRE2b", "TRE3"].into_iter().collect()),
            (
                Team,
                vec![
                    "TRE1a", "TRE1b", "TRE2a", "TRE2b", "TRE3a", "TRE3b", "TRE4", "TRE5a", "TRE5b",
                ]
                .into_iter()
                .collect(),
            ),
        ])
    });

    let mut ci = CardIssues::default();
    for (num, tre) in tres!(card.elements) {
        if card.category.free {
            ci.errors.push(format!("Element {num}: TRE in free routine?"));
            continue;
        }
        let empty_set = HashSet::<&str>::new();
        let valid_tres = tre_map.get(&card.category.event).unwrap_or(&empty_set);
        if !valid_tres.contains(tre.as_str()) {
            ci.errors.push(format!(
                "Element {num}: {tre} is not a valid TRE for {:?}",
                card.category.event
            ));
        }
    }
    ci
}

fn check_connections_in_non_team(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.event == Combo || card.category.event == Team {
        return ci;
    }

    let c_regex = Regex::new(r"^C[\dB][A-z]?").unwrap();
    let cplus_regex = Regex::new(r"^C[\dB][A-z]?\+").unwrap();
    for (num, hybrid, _) in hybrids!(card.elements) {
        for decl in hybrid {
            if cplus_regex.is_match(decl) {
                ci.errors.push(format!(
                    "Element {num}: C+ connections can only be used in team routines"
                ));
            } else if card.category.event == Solo && c_regex.is_match(decl) {
                ci.errors.push(format!("Element {num}: connections can not be used in solos"));
            }
        }
    }
    ci
}

fn check_routine_has_all_families(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if !card.category.free || card.category.event == Acrobatic {
        return ci;
    }

    let mut regex_map = vec![
        ("Thrust", Regex::new(r"^T[\dB][A-z]?").unwrap()),
        ("Spin", Regex::new(r"^S(C|CD)?[\dB][A-z]?$").unwrap()),
        ("Twist", Regex::new(r"^[12]?R[DUCO]?[\dB][A-z]?$").unwrap()),
        ("Airborne Weight", Regex::new(r"^A[\dB][A-z]?$").unwrap()),
        ("Flexiblity", Regex::new(r"^F[\dB][A-z]?$").unwrap()),
    ];
    if card.category.event != Solo {
        regex_map.push(("Connection", Regex::new(r"^C[\dB][A-z]?\+?$").unwrap()));
    }

    let mut has_family_map = HashMap::new();
    for (family, _) in &regex_map {
        has_family_map.insert(family, false);
    }

    for (_, hybrid, _) in hybrids!(card.elements) {
        for decl in hybrid {
            for (family, rx) in &regex_map {
                if rx.is_match(decl) {
                    has_family_map.insert(family, true);
                    break;
                }
            }
        }
    }

    let missing_families = has_family_map.into_iter().filter(|(_, v)| !*v).map(|(k, _)| k);
    for family in missing_families {
        ci.errors.push(format!(
            "Routine needs at least one hybrid with at least one unfactored {family} declared"
        ));
    }
    ci
}

fn check_overlapping_elements(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.event != Combo {
        return ci;
    }

    let mut prev_elem: Option<&Element> = None;
    for elem in &card.elements {
        if let Some(prev_elem) = prev_elem {
            if elem.start_time < prev_elem.stop_time {
                ci.errors.push(format!(
                    "Element {}: starts before Element {} ends",
                    elem.number,
                    elem.number - 1
                ));
            }
        }
        prev_elem = Some(elem);
    }
    ci
}

fn check_dd_limits(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.ag != AG12U {
        return ci;
    }

    for (num, _, dd) in hybrids!(card.elements) {
        if dd.parse().unwrap_or(0.0) > 7.0 {
            ci.errors.push(format!(
                "Element {}: USAAS 12U routines may not have hybrid with a DD greater than 7",
                &num
            ));
        }
    }
    ci
}

fn check_category(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.ag == AgeGroups::Unknown {
        ci.errors.push("Could not determine Age Group for card".to_string());
    }
    if card.category.event == Events::Unknown {
        ci.errors.push("Could not determine Event for card".to_string());
    }

    if card.category.ag == Youth && !card.category.free {
        ci.warnings.push(
            "Youth Tech routines are an experimental event, was this routine category detected correctly?"
                .to_string(),
        );
    }

    ci
}

fn check_hybrid_common_base_marks(card: &CoachCard) -> CardIssues {
    const PROBLEM_CODES: &[&str] = &["A5", "F8"]; // F8 catches a&b
    const TECH_DUET_MIRROR_CODES: &[&str] = &["C1a", "C2a", "C4", "C6a", "C6b", "C7"];
    let mut ci = CardIssues::default();
    for (num, hybrid, _) in hybrids!(card.elements) {
        for decl in hybrid {
            for code in PROBLEM_CODES {
                if decl.starts_with(code) {
                    ci.warnings.push(format!("Element {num}: when {decl} is performed quickly, it has a very high risk of base marking"));
                }
            }

            if card.category.event == Trio && decl.starts_with("C4") {
                ci.warnings.push(format!("Element {num}: the two legs in a line variation of C4, requires C4+ and 4+ athletes"));
            }

            // Mixed Duet can have mirror action, so only check Duet
            // Tech Trios aren't an official event so ignore them
            if card.category.event == Duet
                && !card.category.free
                && TECH_DUET_MIRROR_CODES.contains(&decl.as_str())
            {
                ci.warnings
                    .push(format!("Element {num}: {decl} in Tech Duet, is this mirror action?"));
            }
        }
    }
    ci
}

fn check_hybrid_start_end(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    for (num, hybrid, _) in hybrids!(card.elements) {
        // get the last normal decl and ignore PC bonuses
        let end_pos = hybrid.len() - 1;
        let end_pos = if hybrid[end_pos].ends_with("PC") { end_pos - 1 } else { end_pos };

        for (i, decl) in hybrid.iter().enumerate() {
            if decl.starts_with("FB") && i != 0 {
                ci.errors.push(format!("Element {num}: {decl} must be at the start of a hybrid"));
            }
            if decl.starts_with("F2a") && i != end_pos {
                ci.errors.push(format!("Element {num}: {decl} must be at the end of a hybrid"));
            }
            if decl.starts_with("F4a") && i != 0 {
                ci.warnings
                    .push(format!("Element {num}: {decl} is not at the start, is this correct?"));
            }
        }
    }
    ci
}

pub fn run_checks(card: &CoachCard) -> CardIssues {
    static CHECKS: &[fn(&CoachCard) -> CardIssues] = &[
        check_iss_version,
        check_hybrid_declaration_maxes,
        check_routine_element_maxes,
        check_theme,
        check_small_bonuses,
        check_acros_event,
        check_mixed_duet_elements,
        check_valid_hybrid_declarations,
        check_routine_has_all_families,
        check_connections_in_non_team,
        check_tres,
        check_routine_times,
        check_factoring,
        check_overlapping_elements,
        check_dd_limits,
        check_category,
        check_hybrid_common_base_marks,
        check_hybrid_start_end,
    ];
    let mut ci = CardIssues::default();
    for check in CHECKS {
        ci += check(card);
    }
    ci
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Category, Element, Events, TeamAcrobatic};
    use chrono::NaiveTime;

    struct CardBuilder {
        card: CoachCard,
    }

    impl CardBuilder {
        fn new() -> CardBuilder {
            CardBuilder { card: CoachCard::default() }
        }

        fn hybrids(mut self, hybrids: &[&[&str]]) -> Self {
            for hybrid in hybrids.into_iter() {
                let decls: Vec<String> = hybrid.into_iter().map(|s| s.to_string()).collect();
                let kind = if decls[0].starts_with("TRE") {
                    TRE(decls[0].clone())
                } else if decls[0] == "ChoHy" {
                    ChoHy
                } else if decls[0] == "SuConn" {
                    SuConn
                } else {
                    Hybrid(decls, "1.0".into())
                };
                self.card.elements.push(Element {
                    number: self.card.elements.len() + 1,
                    start_time: NaiveTime::default(),
                    stop_time: NaiveTime::default(),
                    kind,
                })
            }
            self
        }

        fn pair_acros(mut self, acros: &[&str]) -> Self {
            for acro in acros.into_iter() {
                self.card.elements.push(Element {
                    number: self.card.elements.len() + 1,
                    start_time: NaiveTime::default(),
                    stop_time: NaiveTime::default(),
                    kind: PairAcro(acro.to_string()),
                });
            }
            self
        }

        fn team_acros(mut self, acros: &[&str]) -> Self {
            for acro in acros.into_iter() {
                self.card.elements.push(Element {
                    number: self.card.elements.len() + 1,
                    start_time: NaiveTime::default(),
                    stop_time: NaiveTime::default(),
                    kind: TeamAcro(TeamAcrobatic::from(acro).unwrap(), "1.0".into()),
                });
            }
            self
        }

        fn category(mut self, category: Category) -> Self {
            self.card.category = category;
            self
        }

        fn event_type(mut self, event_type: Events) -> Self {
            self.card.category.event = event_type;
            self
        }

        fn theme(mut self, theme: String) -> Self {
            self.card.theme = theme;
            self
        }

        fn iss_ver(mut self, iss_ver: Option<semver::Version>) -> Self {
            self.card.iss_ver = iss_ver;
            self
        }

        fn end_time(mut self, end_time: NaiveTime) -> Self {
            self.card.end_time = end_time;
            self
        }
    }

    fn run_test(name: &str, check_fn: fn(&CoachCard) -> CardIssues, card: &CoachCard) {
        // FUTURE update tests to use _err
        let err_expected = if name.ends_with("_ok") || name.ends_with("_warn") { 0 } else { 1 };
        let warn_expected = if name.ends_with("_warn") { 1 } else { 0 };
        let result = check_fn(card);
        //println!("TEST {:?}", result.errors);
        assert_eq!(result.errors.len(), err_expected);
        assert_eq!(result.warnings.len(), warn_expected);
    }

    #[test]
    fn test_check_iss_version() {
        let not_set = check_iss_version(&CardBuilder::new().card);
        assert_eq!(not_set.warnings.len(), 0);
        let old = check_iss_version(
            &CardBuilder::new().iss_ver(Some(semver::Version::new(0, 0, 1))).card,
        );
        assert_eq!(old.warnings.len(), 1);
        let current = check_iss_version(&CardBuilder::new().iss_ver(Some(LATEST_ISS_VERSION)).card);
        assert_eq!(current.warnings.len(), 0);
    }

    macro_rules! hybrid_tests {
        ($($name:ident: $fname:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), $fname, &CardBuilder::new().hybrids($value).card);
            }
        )*
        }
    }

    macro_rules! hybrid_cat_tests {
        ($($name:ident: $fname:expr, $category:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), $fname, &CardBuilder::new().category($category).hybrids($value).card);
            }
        )*
        }
    }

    hybrid_tests! {
        nm_err: check_valid_hybrid_declarations, &[&["NM2"]], // yes people are still doing this
        old_bonus_err: check_valid_hybrid_declarations, &[&["PL"]],
        old_bonus2_err: check_valid_hybrid_declarations, &[&["TR"]],
        just_pc_ok: check_valid_hybrid_declarations, &[&["2PC"]],
        factored_pc_err: check_valid_hybrid_declarations, &[&["2PC*0.3"]],
        spin_base_ok: check_valid_hybrid_declarations, &[&["SB"]],
        spin_ten_ok: check_valid_hybrid_declarations, &[&["S10*0.3"]],
        spin_five_ok: check_valid_hybrid_declarations, &[&["S5*0.5"]],
        spin_factor_err: check_valid_hybrid_declarations, &[&["S5*0.4"]],
        combined_spin_level_ok: check_valid_hybrid_declarations, &[&["SC4*0.5"]],
        combined_spin_invalid_level: check_valid_hybrid_declarations, &[&["SC8"]],
        two_dir_spin_level_ok: check_valid_hybrid_declarations, &[&["SCD2*0.3"]],
        two_dir_spin_level_err: check_valid_hybrid_declarations, &[&["SCD7"]],
        conn_bad_option_err: check_valid_hybrid_declarations, &[&["C4a"]],
        conn_factored_plus_ok: check_valid_hybrid_declarations, &[&["C2b+*0.5"]],
        flex_ok: check_valid_hybrid_declarations, &[&["F4e*0.3"]],
        flex_err: check_valid_hybrid_declarations, &[&["F4k"]],
        air_ok: check_valid_hybrid_declarations, &[&["A7*0.3"]],
        air_err: check_valid_hybrid_declarations, &[&["A7a"]],
        swirl_ok: check_valid_hybrid_declarations, &[&["RB*0.5"]],
        swirl_err: check_valid_hybrid_declarations, &[&["R5"]],
        twist_ok: check_valid_hybrid_declarations, &[&["2R10*0.3"]],
        twist_err: check_valid_hybrid_declarations, &[&["2R11"]],
        unbal_ok: check_valid_hybrid_declarations, &[&["RU10*0.3"]],
        unbal_err: check_valid_hybrid_declarations, &[&["RU11"]],
        open_ok: check_valid_hybrid_declarations, &[&["ROB"]],
        close_err: check_valid_hybrid_declarations, &[&["RC2"]],
        one_leg_ok: check_valid_hybrid_declarations, &[&["1R6"]],
        one_leg_err: check_valid_hybrid_declarations, &[&["1R7"]],
        six_factored_ok: check_hybrid_declaration_maxes, &[&["TB*0.5", "T1*0.5", "T2a", "T2b", "T3a", "T3b"]],
        six_factored_thrusts_ok: check_hybrid_declaration_maxes,&[&["TB*0.5", "T1*0.5", "T2a", "T2b", "T3a", "T3b"]],
        too_many_factored_thrusts_err: check_hybrid_declaration_maxes,&[&["TB*0.3", "T1*0.3", "T2a", "T2b", "T3a", "T3b", "T3c*0.3"]],
        five_spins_ok: check_hybrid_declaration_maxes,&[&["SC1", "S2", "SCD3", "SC4", "S5"]],
        six_spins_err: check_hybrid_declaration_maxes,&[&["SCDB", "SC1", "S2", "SCD3", "SC4", "S5"]],
        five_twists_ok: check_hybrid_declaration_maxes,&[&["RB", "R1", "1R1", "RU1", "RO1"]],
        six_twists_err: check_hybrid_declaration_maxes,&[&["RB", "R1", "1R1", "RU1", "RO1", "RC1"]],
        five_flex_ok: check_hybrid_declaration_maxes,&[&["FB", "F1a", "F1b", "F1c", "F2a"]],
        six_flex_err: check_hybrid_declaration_maxes,&[&["FB", "F1a", "F1b", "F1c", "F2a", "F2b"]],
        five_connections_ok: check_hybrid_declaration_maxes,&[&["CB*0.5", "C1", "C2", "C3", "C4+", "C6*0.5"]],
        six_connections_err: check_hybrid_declaration_maxes,&[&["CB", "C1", "C2", "C3", "C4+", "C6"]],
        five_spin_variations_ok: check_hybrid_declaration_maxes,&[&["SB", "SCB", "SCDB", "S1", "SCD1"]],
        five_rb_variations_ok: check_hybrid_declaration_maxes,&[&["RB", "1RB", "2RB", "ROB", "RCB"]],
        five_r1_variations_ok: check_hybrid_declaration_maxes,&[&["R1", "1R1", "2R1", "RO1", "RC1"]],
        four_a1s_ok: check_hybrid_declaration_maxes,&[&["A1a", "A1b", "A1c", "A1d"]],
        four_c4s_err: check_hybrid_declaration_maxes,&[&["C4", "C4", "C4", "C4+"]],
        four_factored_c1s_err: check_hybrid_declaration_maxes,&[&["C4", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3"]],
        walkout_in_middle_err: check_hybrid_start_end, &[&["F1a", "F2a", "R1"]],
        walkout_at_end_ok: check_hybrid_start_end, &[&["R1", "F1a", "F2a"]],
        walkout_at_end_with_pc_ok: check_hybrid_start_end, &[&["R1", "F1a", "F2a", "2PC"]],
        back_layout_in_middle_err: check_hybrid_start_end, &[&["R1", "FB", "T4e"]],
        back_layout_at_start_ok: check_hybrid_start_end, &[&["FB", "R1", "T4e"]],
        front_layout_in_middle_warn: check_hybrid_start_end, &[&["R1", "F4a", "T4e"]],
        front_layout_at_start_ok: check_hybrid_start_end, &[&["F4a", "R1", "T4e"]],
    }

    const TECH_MIXED: Category = Category { ag: JRSR, event: MixedDuet, free: false };

    hybrid_cat_tests! {
        too_many_hybrids: check_routine_element_maxes, Category{ag: AG12U, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        too_few_hybrids: check_routine_element_maxes, Category{ag: JRSR, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        correct_hybrids_ok: check_routine_element_maxes, Category{ag: Youth, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        duet_with_pc: check_small_bonuses, Category{ag: JRSR, event: Duet, free: true}, &[&["2PC"]],
        team_with_pc_ok: check_small_bonuses, Category{ag: JRSR, event: Team, free: true}, &[&["2PC"]],
        missing_thrust: check_mixed_duet_elements, TECH_MIXED, &[&["CB", "C4"]],
        missing_thrust_in_free_ok: check_mixed_duet_elements, Category{ag: JRSR, event: MixedDuet, free: true}, &[&["CB", "C4"]],
        missing_thrust_in_duet_ok: check_mixed_duet_elements, Category{ag: JRSR, event: Duet, free: false}, &[&["CB", "C4"]],
        one_conn_one_thrust: check_mixed_duet_elements, TECH_MIXED, &[&["C4", "T9a"]],
        two_conn_two_thrust: check_mixed_duet_elements, TECH_MIXED, &[&["T4", "T9a", "CB", "C4"]],
        three_conn_one_thrust: check_mixed_duet_elements, TECH_MIXED, &[&["C1", "T9a", "CB", "C4"]],
        extra_decls: check_mixed_duet_elements, TECH_MIXED, &[&["C1", "T9a", "R2", "CB"]],
        same_conn: check_mixed_duet_elements, TECH_MIXED, &[&["C1", "T9a", "C1"]],
        two_conn_one_factored_thrust: check_mixed_duet_elements, TECH_MIXED, &[&["CB", "T9a*0.5", "C1"]],
        two_conn_one_thrust_ok: check_mixed_duet_elements, TECH_MIXED, &[&["CB", "T9a", "C1"]],
        solo_missing_flex: check_routine_has_all_families, Category{ag: AG12U, event: Solo, free: true}, &[&["TB", "SCD1", "2R2"], &["A3b"]],
        solo_familes_ok: check_routine_has_all_families, Category{ag: AG12U, event: Solo, free: true}, &[&["TB", "SCD1", "2R2"], &["A3b", "F4f"]],
        duet_missing_conn: check_routine_has_all_families, Category{ag: AG12U, event: Duet, free: true}, &[&["TB", "SC1", "2R2"], &["A3b", "F4f"]],
        tech_duet_families_ok: check_routine_has_all_families, Category{ag: AG12U, event: Duet, free: false}, &[&["TB", "SC1", "2R2"], &["A3b", "F4f"]],
        duet_factored_flex: check_routine_has_all_families, Category{ag: AG12U, event: Duet, free: true}, &[&["TB", "SC1", "2R2"], &["A3b", "F4f*0.5", "CB"]],
        duet_c4plus: check_connections_in_non_team, Category{ag: AG12U, event: Duet, free: true}, &[&["C4+"]],
        combo_c4plus_ok: check_connections_in_non_team, Category{ag: AG12U, event: Combo, free: true}, &[&["C4+"]],
        duet_c4_ok: check_connections_in_non_team, Category{ag: AG12U, event: Duet, free: true}, &[&["C4"]],
        solo_cb: check_connections_in_non_team, Category{ag: AG12U, event: Solo, free: true}, &[&["CB"]],
        solo_factored: check_factoring, Category{ag: AG12U, event: Solo, free: true}, &[&["R1*0.5"]],
        duet_factored_too_small: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &[&["R1*0.3"]],
        duet_factored_ok: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &[&["R1*0.5"]],
        team_factored_ok: check_factoring, Category{ag: AG12U, event: Team, free: true}, &[&["R1*0.3"]],
        duet_factored_conn: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &[&["C4*0.5"]],
        mix_duet_factored_conn: check_factoring, Category{ag: AG12U, event: MixedDuet, free: true}, &[&["CB*0.5"]],
        team_factored_conn_ok: check_factoring, Category{ag: AG12U, event: Team, free: true}, &[&["CB*0.5"]],
        solo_invalid_tre: check_tres, Category{ag: JRSR, event: Solo, free: false}, &[&["TRE5m"]],
        team_tre4a_err: check_tres, Category{ag: JRSR, event: Team, free: false}, &[&["TRE4a"]],
        solo_tre4a_ok: check_tres, Category{ag: JRSR, event: Solo, free: false}, &[&["TRE4a"]],
        free_solo_tre4a_err: check_tres, Category{ag: JRSR, event: Solo, free: true}, &[&["TRE4a"]],
        unknown_ag_err: check_category, Category{ag: AgeGroups::Unknown, event: Solo, free: true}, &[],
        unknown_event_err: check_category, Category{ag: JRSR, event: Events::Unknown, free: true}, &[],
        youth_tech_warn: check_category, Category{ag: Youth, event: Team, free: false}, &[],
    }

    #[test]
    fn test_check_factoring_warnings() {
        // FUTURE turn these into hybrid_cat_tests
        let tech_team_factored_conn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Team, free: false })
                .hybrids(&[&["CB*0.5"]])
                .card,
        );
        assert_eq!(tech_team_factored_conn.warnings.len(), 1);
        let tech_duet_factored_conn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: false })
                .hybrids(&[&["R1*0.5"]])
                .card,
        );
        assert_eq!(tech_duet_factored_conn.warnings.len(), 1);

        let c_c_plus_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C3*0.5", &"C3+*0.5"]])
                .card,
        );
        assert_eq!(c_c_plus_warn.warnings.len(), 1);

        let c_c_plus_ok1 = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C3*0.3", &"C3+*0.5"]])
                .card,
        );
        assert_eq!(c_c_plus_ok1.warnings.len(), 0);

        let c_plus_c_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C3+*0.5", &"C3*0.5"]])
                .card,
        );
        assert_eq!(c_plus_c_warn.warnings.len(), 1);

        let c_plus_c_ok = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C3+*0.5", &"C3*0.3"]])
                .card,
        );
        assert_eq!(c_plus_c_ok.warnings.len(), 0);

        let c4_c4plus_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C4*0.3", &"C4+*0.5"]])
                .card,
        );
        assert_eq!(c4_c4plus_warn.warnings.len(), 1);

        let c4_c4plus_ok = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C2b*0.3", &"C4+*0.5"]])
                .card,
        );
        assert_eq!(c4_c4plus_ok.warnings.len(), 0);

        let c4plus_c4_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C4+*0.5", &"C4*0.3"]])
                .card,
        );
        assert_eq!(c4plus_c4_warn.warnings.len(), 1);

        let c4plus_c4_ok = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C4+*0.5", &"C2b*0.3"]])
                .card,
        );
        assert_eq!(c4plus_c4_ok.warnings.len(), 0);

        let c4plus_c4_ok = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C4+*0.5", &"C2b*0.3"]])
                .card,
        );
        assert_eq!(c4plus_c4_ok.warnings.len(), 0);

        let repeat_decl_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["2R1*0.5", &"2R1*0.3"]])
                .card,
        );
        assert_eq!(repeat_decl_warn.warnings.len(), 1);

        let non_repeat_decl_ok = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["2R1*0.5", &"1R1*0.5"]])
                .card,
        );
        assert_eq!(non_repeat_decl_ok.warnings.len(), 0);

        let cplus_less_half_warn = check_factoring(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .hybrids(&[&["C1a+*0.3"]])
                .card,
        );
        assert_eq!(cplus_less_half_warn.warnings.len(), 1);
    }

    #[test]
    fn test_check_theme() {
        let combo_missing_err = check_theme(&CardBuilder::new().event_type(Combo).card);
        assert_eq!(combo_missing_err.errors.len(), 1);
        let combo_ok =
            check_theme(&CardBuilder::new().event_type(Combo).theme("foo".to_string()).card);
        assert_eq!(combo_ok.errors.len(), 0);
        let solo_missing_ok = check_theme(&CardBuilder::new().event_type(Solo).card);
        assert_eq!(solo_missing_ok.errors.len(), 0);
    }

    #[test]
    fn test_check_routine_element_maxes() {
        let unknown_event = check_routine_element_maxes(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Acrobatic, free: false })
                .card,
        );
        assert_eq!(unknown_event.warnings.len(), 1);

        let too_many_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["TRE1a"], &["TRE2a"], &["TRE3"], &["TRE4a"], &["TRE5a"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(too_many_pair_acros.errors.len(), 1);
        let too_few_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(Category { ag: JRSR, event: Duet, free: true })
                .card,
        );
        assert_eq!(too_few_pair_acros.errors.len(), 1);
        let ok_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(Category { ag: JRSR, event: Duet, free: true })
                .card,
        );
        assert_eq!(ok_pair_acros.errors.len(), 0);

        let too_many_chohy = check_routine_element_maxes(
            &CardBuilder::new()
                .team_acros(&["A-Sq-Back-tk", "A-Sq-Back-tk", "A-Sq-Back-tk"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["ChoHy"], &["ChoHy"]])
                .category(Category { ag: AG12U, event: Combo, free: true })
                .card,
        );
        assert_eq!(too_many_chohy.errors.len(), 1);

        let too_many_suconn = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[
                    &["R1"],
                    &["R1"],
                    &["TRE1"],
                    &["TRE2"],
                    &["TRE3"],
                    &["SuConn"],
                    &["SuConn"],
                    &["SuConn"],
                    &["SuConn"],
                ])
                .category(Category { ag: JRSR, event: MixedDuet, free: false })
                .card,
        );
        assert_eq!(too_many_suconn.errors.len(), 1);

        let too_many_tres = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[
                    &["R1"],
                    &["R1"],
                    &["TRE1"],
                    &["TRE2"],
                    &["TRE3"],
                    &["TRE4"],
                    &["SuConn"],
                    &["SuConn"],
                    &["SuConn"],
                ])
                .category(Category { ag: JRSR, event: MixedDuet, free: false })
                .card,
        );
        assert_eq!(too_many_tres.errors.len(), 1);
        let too_few_tres = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(too_few_tres.errors.len(), 1);
        let ok_tres = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"], &["TRE5"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(ok_tres.errors.len(), 0);

        let too_many_team_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .team_acros(&[
                    "A-Shou-Back-tk-s1",
                    "P-P-HA-bb/2wi-Porp/Trav",
                    "C-Thr^2F-Forw-bb",
                    "A-Sq-Back-pk/2ln-s1",
                    "B-St-1P1P-bb/2ow",
                    "C-Thr>St-Bln-tk-Cs1",
                    "P-Knees-3pA-ne",
                    "P-2S-FA+PF-ne/2ey",
                ])
                .category(Category { ag: JRSR, event: Acrobatic, free: true })
                .card,
        );
        assert_eq!(too_many_team_acros.errors.len(), 1);
        let too_few_team_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .team_acros(&[
                    "A-Shou-Back-tk-s1",
                    "P-P-HA-bb/2wi-Porp/Trav",
                    "C-Thr^2F-Forw-bb",
                    "A-Sq-Back-pk/2ln-s1",
                    "B-St-1P1P-bb/2ow",
                    "C-Thr>St-Bln-tk-Cs1",
                ])
                .category(Category { ag: JRSR, event: Acrobatic, free: true })
                .card,
        );
        assert_eq!(too_few_team_acros.errors.len(), 1);
        let ok_team_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .team_acros(&[
                    "A-Shou-Back-tk-s1",
                    "P-P-HA-bb/2wi-Porp/Trav",
                    "C-Thr^2F-Forw-bb",
                    "A-Sq-Back-pk/2ln-s1",
                    "B-St-1P1P-bb/2ow",
                    "C-Thr>St-Bln-tk-Cs1",
                    "P-Knees-3pA-ne",
                ])
                .category(Category { ag: JRSR, event: Acrobatic, free: true })
                .card,
        );
        assert_eq!(ok_team_acros.errors.len(), 0);
    }

    #[test]
    fn test_check_acros_event() {
        let pair_acro_in_solo =
            check_acros_event(&CardBuilder::new().pair_acros(&["Js1B"]).event_type(Solo).card);
        assert_eq!(pair_acro_in_solo.errors.len(), 1);
        let team_acro_in_solo = check_acros_event(
            &CardBuilder::new().team_acros(&["A-Shou-Back-tk-s1"]).event_type(Solo).card,
        );
        assert_eq!(team_acro_in_solo.errors.len(), 1);
        let duet_acro_in_solo =
            check_acros_event(&CardBuilder::new().pair_acros(&["Js1B"]).event_type(Solo).card);
        assert_eq!(duet_acro_in_solo.errors.len(), 1);

        let pair_acro_in_duet =
            check_acros_event(&CardBuilder::new().pair_acros(&["Js1B"]).event_type(Duet).card);
        assert_eq!(pair_acro_in_duet.errors.len(), 0);
        let team_acro_in_duet = check_acros_event(
            &CardBuilder::new().team_acros(&["A-Shou-Back-tk-s1"]).event_type(Duet).card,
        );
        assert_eq!(team_acro_in_duet.errors.len(), 1);

        let pair_acro_in_team =
            check_acros_event(&CardBuilder::new().pair_acros(&["Js1B"]).event_type(Team).card);
        assert_eq!(pair_acro_in_team.errors.len(), 1);
        let team_acro_in_team = check_acros_event(
            &CardBuilder::new().team_acros(&["A-Shou-Back-tk-s1"]).event_type(Team).card,
        );
        assert_eq!(team_acro_in_team.errors.len(), 0);
    }

    #[test]
    fn test_check_routine_times() {
        let unknown_event = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Acrobatic, free: false })
                .card,
        );
        assert_eq!(unknown_event.warnings.len(), 1);

        let under = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 1, 15).unwrap())
                .card,
        );
        assert_eq!(under.errors.len(), 1);

        let over = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 2, 15).unwrap())
                .card,
        );
        assert_eq!(over.errors.len(), 1);

        let within_time = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 2, 03).unwrap())
                .card,
        );
        assert_eq!(within_time.errors.len(), 0);
    }

    #[test]
    fn test_check_overlapping_elements() {
        let mut base_card = CardBuilder::new()
            .category(Category { ag: AG12U, event: Combo, free: true })
            .hybrids(&[&["C4"], &["F8a"]])
            .team_acros(&["A-Sq-Back-ln"])
            .card;
        base_card.elements[0].start_time = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
        base_card.elements[0].stop_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[1].start_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].stop_time = NaiveTime::from_hms_opt(0, 0, 3).unwrap();
        base_card.elements[2].start_time = NaiveTime::from_hms_opt(0, 0, 3).unwrap();
        base_card.elements[2].stop_time = NaiveTime::from_hms_opt(0, 0, 4).unwrap();
        let overlapping_hybrids = check_overlapping_elements(&base_card);
        assert_eq!(overlapping_hybrids.errors.len(), 1);

        base_card.elements[0].start_time = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
        base_card.elements[0].stop_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].start_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].stop_time = NaiveTime::from_hms_opt(0, 0, 3).unwrap();
        base_card.elements[2].start_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].stop_time = NaiveTime::from_hms_opt(0, 0, 4).unwrap();
        let overlapping_hybrid_acro = check_overlapping_elements(&base_card);
        assert_eq!(overlapping_hybrid_acro.errors.len(), 1);

        base_card.category.event = Team;
        let overlapping_in_team_ok = check_overlapping_elements(&base_card);
        assert_eq!(overlapping_in_team_ok.errors.len(), 0);
        base_card.category.event = Combo;

        base_card.elements[0].start_time = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
        base_card.elements[0].stop_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].start_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].stop_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].start_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].stop_time = NaiveTime::from_hms_opt(0, 0, 4).unwrap();
        let no_overlap = check_overlapping_elements(&base_card);
        assert_eq!(no_overlap.errors.len(), 0);
    }

    #[test]
    fn test_check_dd_limits() {
        let above_limit_hybrid = Element {
            number: 1,
            start_time: Default::default(),
            stop_time: Default::default(),
            kind: Hybrid(vec!["C4".into()], "7.1".into()),
        };
        let below_limit_hybrid = Element {
            number: 1,
            start_time: Default::default(),
            stop_time: Default::default(),
            kind: Hybrid(vec!["C4".into()], "6.9".into()),
        };
        let mut base_card = CardBuilder::new()
            .category(Category { ag: AG12U, event: Combo, free: true })
            .hybrids(&[&["C4"]])
            .card;

        base_card.elements[0] = above_limit_hybrid.clone();
        let above_limit_err = check_dd_limits(&base_card);
        assert_eq!(above_limit_err.errors.len(), 1);

        base_card.elements[0] = below_limit_hybrid;
        let below_limit_ok = check_dd_limits(&base_card);
        assert_eq!(below_limit_ok.errors.len(), 0);

        base_card.elements[0] = above_limit_hybrid;
        base_card.category.ag = Youth;
        let above_limit_youth_ok = check_dd_limits(&base_card);
        assert_eq!(above_limit_youth_ok.errors.len(), 0);
    }

    #[test]
    fn test_check_hybrid_common_base_marks() {
        let card = CardBuilder::new().hybrids(&[&["A5", "F8a*0.5", "F8b"]]).card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 3);

        let card = CardBuilder::new().hybrids(&[&["A4b", "F6a*0.5", "F6c"]]).card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 0);

        let card = CardBuilder::new().hybrids(&[&["C4"]]).event_type(Trio).card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 1);

        let card = CardBuilder::new().hybrids(&[&["C4"]]).event_type(MixedDuet).card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 0);

        let card = CardBuilder::new().hybrids(&[&["C4"]]).event_type(Duet).card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 1);

        let card = CardBuilder::new()
            .hybrids(&[&["C4"]])
            .category(Category { ag: JRSR, free: false, event: Duet })
            .card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 1);

        let card = CardBuilder::new()
            .hybrids(&[&["C4"]])
            .category(Category { ag: JRSR, free: false, event: MixedDuet })
            .card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 0);

        let card = CardBuilder::new()
            .hybrids(&[&["C2b"]])
            .category(Category { ag: JRSR, free: false, event: Duet })
            .card;
        let ci = check_hybrid_common_base_marks(&card);
        assert_eq!(ci.warnings.len(), 0);
    }
}
