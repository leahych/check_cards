use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::{
    AgeGroups, CardIssue, Category, CoachCard, Element, ElementKind, Events, TeamAcrobatic, ci_err,
    ci_errs, ci_warn, ci_warns, get_expected_routine_time,
};
use regex_lite::Regex;
use std::collections::HashMap;
use std::time::Duration;

fn hybrids(v: &[Element]) -> impl Iterator<Item = (usize, &Vec<String>, &String)> {
    v.iter().filter_map(|e| match &e.kind {
        Hybrid(decl, dd) => Some((e.number, decl, dd)),
        _ => None,
    })
}

use crate::ElementKind::{ChoHy, Hybrid, PairAcro, SuConn, TRE, TeamAcro};

const LATEST_ISS_VERSION: semver::Version = semver::Version::new(3, 0, 5);
fn check_iss_version(card: &CoachCard) -> Vec<CardIssue> {
    if let Some(ver) = card.iss_ver.as_ref()
        && ver < &LATEST_ISS_VERSION
    {
        return ci_warns(format!(
            "Card created with version {ver}, latest is {LATEST_ISS_VERSION}"
        ));
    }
    Vec::new()
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
    decls
        .iter()
        .filter(|value| family_regex.is_match(value.as_ref()))
        .map(points_for_declaration)
        .reduce(|total, item| total + item)
        .unwrap_or_default()
}

fn check_hybrid_declaration_maxes(category: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();

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
            ci_err(&mut ci, format!("{name} can only be declared 5 times"));
        }
    }

    let factor_regex = Regex::new(r"\*0\.\d").unwrap();
    let mut decl_points = HashMap::<String, usize>::new();
    for decl in decls {
        let base_name = factor_regex.replace_all(decl.as_ref(), "").replace('+', "");
        let points =
            decl_points.get(&base_name).copied().unwrap_or_default() + points_for_declaration(decl);
        decl_points.insert(base_name, points);
    }

    for (decl, points) in decl_points {
        if points > 30 {
            ci_err(&mut ci, format!("{decl} is used more than 3 times"));
        } else if points > 20
            && decl.starts_with('C')
            && (category.event == Duet || category.event == MixedDuet)
        {
            ci_err(
                &mut ci,
                format!("Max of 2 connections ({decl}) with the same technique in Duets"),
            );
        }
    }
    ci
}

struct ElementLimit {
    chohy: usize,
    suconn: usize,
    tre: usize,
    acro: usize,
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
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: Duet, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 1, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: MixedDuet, free: true },
                ElementLimit { chohy: 0, suconn: 3, tre: 0, acro: 2, hybrid: 3 },
            ),
            (
                Category { ag: AG12U, event: Team, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 4 },
            ),
            (
                Category { ag: AG12U, event: Combo, free: true },
                ElementLimit { chohy: 1, suconn: 0, tre: 0, acro: 3, hybrid: 4 },
            ),
            // Youth free
            (
                Category { ag: Youth, event: Solo, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: Duet, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 1, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: MixedDuet, free: true },
                ElementLimit { chohy: 0, suconn: 3, tre: 0, acro: 2, hybrid: 3 },
            ),
            (
                Category { ag: Youth, event: Team, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 5 },
            ),
            (
                Category { ag: Youth, event: Combo, free: true },
                ElementLimit { chohy: 1, suconn: 0, tre: 0, acro: 4, hybrid: 4 },
            ),
            // Youth tech - USAAS experimental
            (
                Category { ag: Youth, event: Solo, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 0, hybrid: 1 },
            ),
            (
                Category { ag: Youth, event: Duet, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 1 },
            ),
            (
                Category { ag: Youth, event: MixedDuet, free: false },
                ElementLimit { chohy: 0, suconn: 3, tre: 3, acro: 2, hybrid: 2 },
            ),
            (
                Category { ag: Youth, event: Team, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 2 },
            ),
            // JR/SR free
            (
                Category { ag: JRSR, event: Solo, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: Duet, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 2, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: MixedDuet, free: true },
                ElementLimit { chohy: 0, suconn: 4, tre: 0, acro: 3, hybrid: 4 },
            ),
            (
                Category { ag: JRSR, event: Trio, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 5 },
            ),
            (
                Category { ag: JRSR, event: Team, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 6 },
            ),
            (
                Category { ag: JRSR, event: Acrobatic, free: true },
                ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 7, hybrid: 0 },
            ),
            (
                Category { ag: JRSR, event: Combo, free: true },
                ElementLimit { chohy: 1, suconn: 0, tre: 0, acro: 4, hybrid: 5 },
            ),
            // JR/SR tech
            (
                Category { ag: JRSR, event: Solo, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 0, hybrid: 1 },
            ),
            (
                Category { ag: JRSR, event: Duet, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 1 },
            ),
            (
                Category { ag: JRSR, event: MixedDuet, free: false },
                ElementLimit { chohy: 0, suconn: 3, tre: 3, acro: 2, hybrid: 2 },
            ),
            (
                Category { ag: JRSR, event: Team, free: false },
                ElementLimit { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 2 },
            ),
        ])
    });
    map.get(category)
}

fn count_elements(elements: &[Element]) -> ElementLimit {
    let mut el = ElementLimit { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 0 };
    for element in elements {
        match element.kind {
            ChoHy => el.chohy += 1,
            SuConn => el.suconn += 1,
            TRE(..) => el.tre += 1,
            PairAcro(..) | TeamAcro(..) => el.acro += 1,
            Hybrid(..) => el.hybrid += 1,
        }
    }
    el
}

fn check_routine_element_maxes(card: &CoachCard) -> Vec<CardIssue> {
    fn check_max(num: usize, max: usize, name: &str, ci: &mut Vec<CardIssue>) {
        if num != max {
            ci_err(ci, format!("{max} {name} expected, but {num} found"));
        }
    }

    element_maxes(&card.category).map_or_else(
        || ci_warns(format!("Could not determine element limits for {}", card.category)),
        |max| {
            let mut ci = Vec::new();
            let num = count_elements(&card.elements);
            check_max(num.chohy, max.chohy, "Choreography Hybrids", &mut ci);
            check_max(num.suconn, max.suconn, "Surface Connections", &mut ci);
            check_max(num.tre, max.tre, "TREs", &mut ci);
            check_max(num.acro, max.acro, "Acrobatics", &mut ci);
            check_max(num.hybrid, max.hybrid, "Hybrids", &mut ci);
            ci
        },
    )
}

fn check_theme(card: &CoachCard) -> Vec<CardIssue> {
    match card.category.event {
        Acrobatic | Combo if card.theme.is_empty() => {
            ci_errs("Theme is required for Acrobatic and Combo routines")
        }
        _ => Vec::new(),
    }
}

fn check_small_bonuses(category: Category, decls: &[String]) -> Vec<CardIssue> {
    let is_small = category.event != Combo && category.event != Team;
    let re = Regex::new(r"\dPC").unwrap();
    if is_small && decls.iter().any(|decl| re.is_match(decl)) {
        return ci_errs(format!("{} cannot have Pattern Change bonuses", category.event.as_str()));
    }
    Vec::new()
}

fn check_team_acro(category: Category, _: &TeamAcrobatic, _: &String) -> Vec<CardIssue> {
    match category.event {
        Solo => ci_errs("Acrobatic elements are not allowed in a solo"),
        Duet | MixedDuet | Trio => ci_errs(format!("Invalid Acrobatic for {category}")),
        Acrobatic | Combo | Team | Events::Unknown => Vec::new(),
    }
}

fn check_pair_acro(category: Category, acro: &String) -> Vec<CardIssue> {
    match category.event {
        Solo => ci_errs("Acrobatic elements are not allowed in a solo"),
        Duet | MixedDuet | Trio | Events::Unknown => Vec::new(),
        Acrobatic | Combo | Team => ci_errs(format!("Invalid Acrobatic '{acro}'")),
    }
}

fn check_valid_hybrid_declarations(_: Category, decls: &[String]) -> Vec<CardIssue> {
    let valid_decl_regex = Regex::new(concat!(
        "^(",
        r"(S([B1-9]|10)|SCD?[B1-6])(\*0.[35])?",
        r"|(R[B1-4]|R[CO][B1]|(1R|RD)[B1-6]|(2R|RU)([B1-9]|10))(\*0.[35])?",
        r"|A(B|1[a-d]|2[ab]|3[ab]|4[ab]|[5-8])(\*0.[35])?",
        r"|F(B|1[abc]|2[abc]|3[abc]|4[a-f]|5[abc]|6[a-d]|7|8[ab]|9|10)(\*0.[35])?",
        r"|C(B|1[ab]|2[abc]|3|4|5|6[ab]|7)\+?(\*0.[35])?",
        r"|T(B|1|2[ab]|3[a-d]|4[a-e]|5[a-e]|6[abc]|7|8|9[ab])(\*0.[35])?",
        r"|[1-6]PC",
        ")$"
    ))
    .unwrap();

    let mut ci = Vec::new();
    for decl in decls {
        if !valid_decl_regex.is_match(decl) {
            ci_err(&mut ci, format!("'{decl}' is not a valid difficulty declaration"));
        }
    }
    ci
}

fn check_mixed_duet_elements(card: &CoachCard) -> Vec<CardIssue> {
    let expected_cat = Category { ag: JRSR, event: MixedDuet, free: false };
    if card.category != expected_cat {
        return Vec::new();
    }

    let conn_regex = Regex::new(r"^C[\dB][A-z]?").unwrap();
    let thrust_regex = Regex::new(r"^T[\dB]").unwrap();
    let other_families_regex = Regex::new(r"^[AFRS]").unwrap();
    let hybrids = hybrids(&card.elements);
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
            return Vec::new();
        }
    }
    ci_errs(
        "Mixed Duet Tech routines must have one hybrid with only one Thrust and two different Connections",
    )
}

fn check_factoring(category: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    let mut prev_decl = "";
    for decl in decls {
        if category.event == Solo && decl.contains("*0.") {
            ci_err(&mut ci, "cannot factor in a Solo`");
        }
        if category.event == Duet || category.event == MixedDuet {
            if decl.contains("0.3") {
                ci_err(&mut ci, " cannot factor by 0.3 in Duets");
            } else if Regex::new(r"^C.*0\.").unwrap().is_match(decl) {
                ci_err(&mut ci, "factoring connections in a Duet seems suspicious");
            }
        }
        // don't warn about C4+*0.5 C2*0.3 since that is valid for a
        // 7-person tech team doing a line of 4 and a line of 3.
        if !category.free && decl.contains("*0.") && decl != "C4+*0.5" && decl != "C2b*0.3" {
            ci_warn(&mut ci, format!("factoring in a Tech {:?} seems suspicious", category.event));
        }

        let conn_factored_rx = Regex::new(r"^C[\dB][A-z]?\*0.5").unwrap();
        let conn_plus_factored_rx = Regex::new(r"^C[\dB][A-z]?\+\*0.5").unwrap();
        if (conn_factored_rx.is_match(decl) && conn_plus_factored_rx.is_match(prev_decl))
            || (conn_plus_factored_rx.is_match(decl) && conn_factored_rx.is_match(prev_decl))
        {
            ci_warn(
                &mut ci,
                "if factoring a connection because 5-7 are swimming, one connection should be factored by 0.3",
            );
        }
        if (prev_decl.starts_with("C4*0.") && decl == "C4+*0.5")
            || (prev_decl == "C4+*0.5" && decl.starts_with("C4*0."))
        {
            ci_warn(
                &mut ci,
                "if factoring C4+ because 5-7 are swimming, is the smaller group still doing C4?",
            );
        }

        let decl_parts: Vec<&str> = decl.split('*').collect();
        let prev_decl_parts: Vec<&str> = prev_decl.split('*').collect();
        if decl_parts.len() == 2
            && prev_decl_parts.len() == 2
            && decl_parts[0] == prev_decl_parts[0]
        {
            ci_warn(&mut ci, "if performing same choreography in two groups, do not factor");
        }

        let cplus_less_half_rx = Regex::new(r"^C[\dB][A-z]?\+\*0.3").unwrap();
        if cplus_less_half_rx.is_match(decl) {
            ci_warn(&mut ci, "factoring C+ by 0.3 requires 9-10 athletes");
        }

        prev_decl = decl;
    }
    ci
}

fn check_routine_times(card: &CoachCard) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    let expected_time = get_expected_routine_time(&card.category);
    if let Some(expected_time) = expected_time {
        let min_time = *expected_time - Duration::new(5, 0);
        let max_time = *expected_time + Duration::new(5, 0);
        if card.end_time < min_time || card.end_time > max_time {
            ci_err(
                &mut ci,
                format!(
                    "The end time of the routine, {}, is not between {} and {} as expected for a {}",
                    card.end_time.format("%M:%S"),
                    min_time.format("%M:%S"),
                    max_time.format("%M:%S"),
                    card.category
                ),
            );
        }
    } else {
        ci_warn(&mut ci, format!("Could not determine routine time for {}", card.category));
    }
    ci
}

fn check_tres(category: Category, tre: &str, dd: &str) -> Vec<CardIssue> {
    if category.free {
        return ci_errs("TRE in free routine?");
    }

    let mut ci = Vec::new();
    let valid_tres = match category.event {
        Solo => HashMap::from([
            ("TRE1a", "2.7"),
            ("TRE1b", "2.1"),
            ("TRE2a", "3"),
            ("TRE2b", "2.7"),
            ("TRE3", "3.2"),
            ("TRE4a", "2.9"),
            ("TRE4b", "2.6"),
            ("TRE5a", "2.4"),
            ("TRE5b", "2.1"),
        ]),
        Duet => HashMap::from([
            ("TRE1a", "3"),
            ("TRE1b", "2.5"),
            ("TRE2a", "2.8"),
            ("TRE2b", "2.4"),
            ("TRE3", "3.1"),
            ("TRE4a", "3.2"),
            ("TRE4b", "2.7"),
            ("TRE5a", "2.3"),
            ("TRE5b", "2.1"),
        ]),
        MixedDuet => HashMap::from([
            ("TRE1a", "2.7"),
            ("TRE1b", "2.5"),
            ("TRE2a", "2.4"),
            ("TRE2b", "2.2"),
            ("TRE3", "3"),
        ]),
        Team => HashMap::from([
            ("TRE1a", "2.5"),
            ("TRE1b", "2.3"),
            ("TRE2a", "2.6"),
            ("TRE2b", "2.3"),
            ("TRE3a", "2.6"),
            ("TRE3b", "2.3"),
            ("TRE4", "2.9"),
            ("TRE5a", "2.4"),
            ("TRE5b", "2.1"),
        ]),
        Acrobatic | Combo | Trio | Events::Unknown => HashMap::new(),
    };
    match valid_tres.get(&tre) {
        Some(expected_dd) => {
            // when someone enters a card into a textbox, we leave the DD as blank
            // in that case we don't want to warn about that since there is no way
            // for the user to fix that.
            if !dd.is_empty() && dd != *expected_dd {
                ci_err(
                    &mut ci,
                    format!("expected {tre} to have a DD of {expected_dd} but DD is {dd}"),
                );
            }
        }
        None => {
            ci_err(&mut ci, format!("{tre} is not a valid TRE for {:?}", category.event));
        }
    }
    ci
}

fn check_connections_in_non_team(category: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    if category.event == Combo || category.event == Team {
        return ci;
    }

    let c_regex = Regex::new(r"^C[\dB][A-z]?").unwrap();
    let cplus_regex = Regex::new(r"^C[\dB][A-z]?\+").unwrap();
    for decl in decls {
        if cplus_regex.is_match(decl) {
            ci_err(&mut ci, "C+ connections can only be used in team routines");
        } else if category.event == Solo && c_regex.is_match(decl) {
            ci_err(&mut ci, "connections can not be used in solos");
        }
    }
    ci
}

fn check_routine_has_all_families(card: &CoachCard) -> Vec<CardIssue> {
    let mut ci = Vec::new();
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

    for (_, hybrid, _) in hybrids(&card.elements) {
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
        ci_err(&mut ci, format!("missing one hybrid with one unfactored {family} declared"));
    }
    ci
}

fn check_overlapping_elements(card: &CoachCard) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    if card.category.event != Combo {
        return ci;
    }

    let mut prev_elem: Option<&Element> = None;
    for elem in &card.elements {
        if let Some(prev_elem) = prev_elem
            && elem.start_time < prev_elem.stop_time
        {
            ci_err(
                &mut ci,
                format!("Element {}: starts before Element {} ends", elem.number, elem.number - 1),
            );
        }
        prev_elem = Some(elem);
    }
    ci
}

fn check_dd_limits(category: Category, dd: &str) -> Vec<CardIssue> {
    if category.ag == AG12U && dd.parse().unwrap_or(0.0) > 7.0 {
        return ci_warns("USAAS 12U routines may not have hybrid with a DD greater than 7");
    }
    Vec::new()
}

fn check_category(card: &CoachCard) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    if card.category.ag == AgeGroups::Unknown {
        ci_err(&mut ci, "Could not determine Age Group for card");
    }
    if card.category.event == Events::Unknown {
        ci_err(&mut ci, "Could not determine Event for card");
    }
    ci
}

fn check_hybrid_common_base_marks(category: Category, decls: &[String]) -> Vec<CardIssue> {
    const PROBLEM_JOIN_CODES: &[&str] = &["A4b", "F10"]; // FUTURE add F9?
    const TECH_DUET_MIRROR_CODES: &[&str] = &["C1a", "C2a", "C4", "C6a", "C6b", "C7"];
    const KNIGHT_CODES: &[&str] = &["F3c", "F5a", "F5c", "F6b", "F6c", "F8a"];
    let mut ci = Vec::new();
    let mut prev_decl = "";
    for decl in decls {
        for code in PROBLEM_JOIN_CODES {
            if decl.starts_with(code) {
                ci_warn(
                    &mut ci,
                    format!(
                        "{decl} has a very high risk of base marking, athletes must not be vertical at ¾ point"
                    ),
                );
            }
        }
        if decl.starts_with("T9b") {
            ci_warn(
                &mut ci,
                "T9b has a very high risk of base marking, it needs 8.5 height and a 1 second hold",
            );
        }

        if category.event == Trio && decl.starts_with("C4") {
            ci_warn(
                &mut ci,
                "the two legs in a line variation of C4, requires C4+ and 4+ athletes",
            );
        }

        // Mixed Duet can have mirror action, so only check Duet
        // Tech Trios aren't an official event so ignore them
        if (category.event == Duet || category.event == Team)
            && !category.free
            && TECH_DUET_MIRROR_CODES.contains(&decl.as_str())
            && !decl.contains('+')
        {
            ci_warn(&mut ci, format!("{decl} in Tech Duet, is this mirror action?"));
        }

        // these next two checks aren't "common" errors, but this was a
        // convenient place to check for something that is probably a
        // mistake if we see the two decls back-to-back
        if decl.starts_with("A6") && prev_decl.starts_with("A1d") {
            ci_warn(&mut ci, "A1d before A6, should this be A1a or A1c?");
        }

        for code in KNIGHT_CODES {
            if decl.starts_with(code) && prev_decl.starts_with("F1a") {
                ci_warn(&mut ci, format!("F1a before {decl}, should this be F1b?"));
            }
        }

        prev_decl = decl;
    }
    ci
}

fn check_hybrid_start_end(_: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();

    let decls: &[String] = if decls.last().is_some_and(|x| x.ends_with("PC")) {
        &decls[..decls.len() - 1]
    } else {
        decls
    };

    let end_pos = decls.len().wrapping_sub(1);
    for (i, decl) in decls.iter().enumerate() {
        if decl.starts_with("FB") && i != 0 {
            ci_err(&mut ci, format!("{decl} must be at the start of a hybrid"));
        }
        if decl.starts_with("F2a") && i != end_pos {
            ci_err(&mut ci, format!("{decl} must be at the end of a hybrid"));
        }
        if decl.starts_with("F4a") && i != 0 {
            ci_warn(&mut ci, format!("{decl} is not at the start, is this correct?"));
        }
    }
    ci
}

fn check_ascent_connection(_: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    let mut prev_decl = "";
    for decl in decls {
        // TODO is A1c in writing? Manual only mentions A3a/A3b
        // A1c/C4 is for duet lift back-to-back
        // A3a is for from open pike to VP
        // A3b is for vert rise while connected
        if (prev_decl == "A1c" && decl == "C4")
            || (prev_decl == "A3a" && ["C3", "C3+", "C4+"].contains(&decl.as_str()))
            || (prev_decl == "A3b" && ["C3", "C3+", "C4", "C4+"].contains(&decl.as_str()))
        {
            ci_warn(
                &mut ci,
                format!(
                    "Ascents and Lifts cannot be declared simultaneously with a connection. If legs are connected during the {prev_decl}, there must be a disconnect or another action before the {decl}"
                ),
            );
        }
        prev_decl = decl;
    }
    ci
}

fn check_flexibility_combinations(_: Category, decls: &[String]) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    let mut prev_decl = "";
    for decl in decls {
        if (decl.starts_with("F2c") && prev_decl.starts_with("F1a"))
            || (decl.starts_with("F3c") && prev_decl.starts_with("F1b"))
            || (decl.starts_with("F1a") && prev_decl.starts_with("RO"))
        {
            ci_warn(
                &mut ci,
                format!(
                    "An additional action (of any sort) must be performed between {prev_decl} and {decl}"
                ),
            );
        }
        if decl.starts_with("F3a") {
            ci_warn(
                &mut ci,
                "F3a means Right/Left split to opposite split, back to the initial split, 3 total splits",
            );
        }

        // check for declaring a knight, but then doing a flex move
        // that starts with a fishtail
        //
        // don't use .starts_with because this is something that is more
        // likely to be correct when there are multiple groups. If it
        // becomes an issue, I could make the check more complicated.
        if prev_decl == "F1b" && (decl == "F4e" || decl == "F4f") {
            ci_warn(
                &mut ci,
                format!(
                    "Claiming {prev_decl} {decl} involves going to a knight, and then back to a fishtail is this correct?"
                ),
            );
        }
        prev_decl = decl;
    }
    ci
}

pub fn check_one_element(category: Category, element: &ElementKind) -> Vec<CardIssue> {
    match &element {
        TeamAcro(ta, dd) => check_team_acro(category, ta, dd).into_iter().collect(),
        PairAcro(decl) => check_pair_acro(category, decl).into_iter().collect(),
        Hybrid(decls, dd) => {
            let mut ci: Vec<_> = [
                check_hybrid_declaration_maxes,
                check_small_bonuses,
                check_valid_hybrid_declarations,
                check_factoring,
                check_connections_in_non_team,
                check_hybrid_common_base_marks,
                check_hybrid_start_end,
                check_ascent_connection,
                check_flexibility_combinations,
            ]
            .iter()
            .flat_map(|check| check(category, decls))
            .collect();
            ci.extend(check_dd_limits(category, dd));
            ci
        }
        TRE(decl, dd) => check_tres(category, decl, dd),
        ChoHy | SuConn => Vec::new(),
    }
}

fn check_elements(card: &CoachCard) -> Vec<CardIssue> {
    let mut ci = Vec::new();
    for elem in &card.elements {
        let _ = check_one_element(card.category, &elem.kind).iter().map(|i| {
            ci.push(CardIssue::new(i.level, format!("Element {}: {}", elem.number, i.text)));
        });
    }
    ci
}

pub fn run_checks(card: &CoachCard) -> Vec<CardIssue> {
    [
        check_iss_version,
        check_routine_element_maxes,
        check_theme,
        check_mixed_duet_elements,
        check_routine_has_all_families,
        check_routine_times,
        check_overlapping_elements,
        check_category,
        check_elements,
    ]
    .iter()
    .flat_map(|check| check(card))
    .collect()
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
                    TRE(decls[0].clone(), "".into())
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

    fn run_test(name: &str, check_fn: fn(&CoachCard) -> Vec<CardIssue>, card: &CoachCard) {
        // FUTURE update tests to use _err
        let expected = if name.ends_with("_ok") { 0 } else { 1 };
        let result = check_fn(card);
        //println!("TEST {:?}", result);
        assert_eq!(result.len(), expected);
    }

    fn run_hybrid_test(
        name: &str,
        check_fn: fn(Category, &[String]) -> Vec<CardIssue>,
        cat: Category,
        hybrid: &[&str],
    ) {
        // FUTURE update tests to use _err
        let expected = if name.ends_with("_ok") { 0 } else { 1 };
        let decls: Vec<String> = hybrid.into_iter().map(|s| s.to_string()).collect();
        let result = check_fn(cat, &decls);
        // println!("TEST {:?}", result.errors);
        assert_eq!(result.len(), expected);
    }

    #[test]
    fn test_check_iss_version() {
        let not_set = check_iss_version(&CardBuilder::new().card);
        assert_eq!(not_set.len(), 0);
        let old = check_iss_version(
            &CardBuilder::new().iss_ver(Some(semver::Version::new(0, 0, 1))).card,
        );
        assert_eq!(old.len(), 1);
        let current = check_iss_version(&CardBuilder::new().iss_ver(Some(LATEST_ISS_VERSION)).card);
        assert_eq!(current.len(), 0);
    }

    macro_rules! routine_tests {
        ($($name:ident: $fname:expr, $category:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), $fname, &CardBuilder::new().category($category).hybrids($value).card);
            }
        )*
        }
    }

    macro_rules! hybrid_tests {
        ($($name:ident: $fname:expr, $category:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_hybrid_test(stringify!($name), $fname, $category, $value);
            }
        )*
        }
    }

    const TECH_MIXED: Category = Category { ag: JRSR, event: MixedDuet, free: false };

    hybrid_tests! {
        six_factored_ok: check_hybrid_declaration_maxes, TECH_MIXED, &["TB*0.5", "T1*0.5", "T2a", "T2b", "T3a", "T3b"],
        six_factored_thrusts_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["TB*0.5", "T1*0.5", "T2a", "T2b", "T3a", "T3b"],
        too_many_factored_thrusts_err: check_hybrid_declaration_maxes,TECH_MIXED, &["TB*0.3", "T1*0.3", "T2a", "T2b", "T3a", "T3b", "T3c*0.3"],
        five_spins_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["SC1", "S2", "SCD3", "SC4", "S5"],
        six_spins_err: check_hybrid_declaration_maxes,TECH_MIXED, &["SCDB", "SC1", "S2", "SCD3", "SC4", "S5"],
        five_twists_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["RB", "R1", "1R1", "RU1", "RO1"],
        six_twists_err: check_hybrid_declaration_maxes,TECH_MIXED, &["RB", "R1", "1R1", "RU1", "RO1", "RC1"],
        five_flex_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["FB", "F1a", "F1b", "F1c", "F2a"],
        six_flex_err: check_hybrid_declaration_maxes,TECH_MIXED, &["FB", "F1a", "F1b", "F1c", "F2a", "F2b"],
        five_connections_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["CB*0.5", "C1", "C2", "C3", "C4+", "C6*0.5"],
        six_connections_err: check_hybrid_declaration_maxes,TECH_MIXED, &["CB", "C1", "C2", "C3", "C4+", "C6"],
        five_spin_variations_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["SB", "SCB", "SCDB", "S1", "SCD1"],
        five_rb_variations_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["RB", "1RB", "2RB", "ROB", "RCB"],
        five_r1_variations_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["R1", "1R1", "2R1", "RO1", "RC1"],
        four_a1s_ok: check_hybrid_declaration_maxes,TECH_MIXED, &["A1a", "A1b", "A1c", "A1d"],
        four_c4s_err: check_hybrid_declaration_maxes,TECH_MIXED, &["C4", "C4", "C4", "C4+"],
        four_factored_c1s_err: check_hybrid_declaration_maxes,TECH_MIXED, &["C4", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3"],
        three_c4s_duet_err: check_hybrid_declaration_maxes,TECH_MIXED, &["C4", "C4", "C4"],
        three_c4s_team_ok: check_hybrid_declaration_maxes, Category{ag: JRSR, event: Team, free: true}, &["C4", "C4", "C4"],
        nm_err: check_valid_hybrid_declarations, TECH_MIXED, &["NM2"], // yes people are still doing this
        old_bonus_err: check_valid_hybrid_declarations, TECH_MIXED, &["PL"],
        old_bonus2_err: check_valid_hybrid_declarations, TECH_MIXED,  &["TR"],
        just_pc_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["2PC"],
        factored_pc_err: check_valid_hybrid_declarations,  TECH_MIXED, &["2PC*0.3"],
        too_many_pc_err: check_valid_hybrid_declarations,  TECH_MIXED, &["7PC"],
        spin_base_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["SB"],
        spin_ten_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["S10*0.3"],
        spin_five_ok: check_valid_hybrid_declarations, TECH_MIXED,  &["S5*0.5"],
        spin_factor_err: check_valid_hybrid_declarations, TECH_MIXED,  &["S5*0.4"],
        combined_spin_level_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["SC4*0.5"],
        combined_spin_invalid_level: check_valid_hybrid_declarations, TECH_MIXED,  &["SC8"],
        two_dir_spin_level_ok: check_valid_hybrid_declarations, TECH_MIXED,  &["SCD2*0.3"],
        two_dir_spin_level_err: check_valid_hybrid_declarations,  TECH_MIXED, &["SCD7"],
        conn_bad_option_err: check_valid_hybrid_declarations,  TECH_MIXED, &["C4a"],
        conn_factored_plus_ok: check_valid_hybrid_declarations, TECH_MIXED,  &["C2b+*0.5"],
        flex_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["F4e*0.3"],
        flex_err: check_valid_hybrid_declarations, TECH_MIXED,  &["F4k"],
        air_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["A7*0.3"],
        air_err: check_valid_hybrid_declarations, TECH_MIXED,  &["A7a"],
        swirl_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["RB*0.5"],
        swirl_err: check_valid_hybrid_declarations,  TECH_MIXED, &["R5"],
        twist_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["2R10*0.3"],
        twist_err: check_valid_hybrid_declarations,  TECH_MIXED, &["2R11"],
        unbal_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["RU10*0.3"],
        unbal_err: check_valid_hybrid_declarations, TECH_MIXED,  &["RU11"],
        open_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["ROB"],
        close_err: check_valid_hybrid_declarations,  TECH_MIXED, &["RC2"],
        one_leg_ok: check_valid_hybrid_declarations,  TECH_MIXED, &["1R6"],
        one_leg_err: check_valid_hybrid_declarations,  TECH_MIXED, &["1R7"],
        capital_letter_err: check_valid_hybrid_declarations,  TECH_MIXED, &["A3B"],
        duet_with_pc_err: check_small_bonuses, Category{ag: JRSR, event: Duet, free: true}, &["2PC"],
        team_with_pc_ok: check_small_bonuses, Category{ag: JRSR, event: Team, free: true}, &["2PC"],
        solo_factored_err: check_factoring, Category{ag: AG12U, event: Solo, free: true}, &["R1*0.5"],
        duet_factored_too_small_err: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &["R1*0.3"],
        duet_factored_ok: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &["R1*0.5"],
        team_factored_ok: check_factoring, Category{ag: AG12U, event: Team, free: true}, &["R1*0.3"],
        duet_factored_conn_err: check_factoring, Category{ag: AG12U, event: Duet, free: true}, &["C4*0.5"],
        mix_duet_factored_conn_err: check_factoring, Category{ag: AG12U, event: MixedDuet, free: true}, &["CB*0.5"],
        free_team_factored_conn_ok: check_factoring, Category{ag: AG12U, event: Team, free: true}, &["CB*0.5"],
        tech_team_factored_conn_ok: check_factoring, Category{ag: JRSR, event: Team, free: false}, &["C4+*0.5", "C2b*0.3"],
        tech_team_factored_conn_warn: check_factoring, Category { ag: AG12U, event: Team, free: false }, &["CB*0.5"],
        tech_duet_factored_decl_warn: check_factoring, Category { ag: AG12U, event: Duet, free: false }, &["R1*0.5"],
        c_c_plus_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C3*0.5", "C3+*0.5"],
        c_c_plus_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C3*0.3", "C3+*0.5"],
        c_plus_c_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C3+*0.5", "C3*0.5"],
        c_plus_c_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C3+*0.5", "C3*0.3"],
        c4_c4plus_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C4*0.3", "C4+*0.5"],
        c4_c4plus_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C2b*0.3", "C4+*0.5"],
        c4plus_c4_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C4+*0.5", "C4*0.3"],
        c4plus_c4_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C4+*0.5", "C2b*0.3"],
        c4plus_c4_2_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C4+*0.5", "C2b*0.3"],
        repeat_decl_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["2R1*0.5", "2R1*0.3"],
        non_repeat_decl_ok: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["2R1*0.5", "1R1*0.5"],
        cplus_less_half_warn: check_factoring, Category { ag: AG12U, event: Team, free: true }, &["C1a+*0.3"],
        walkout_in_middle_err: check_hybrid_start_end, TECH_MIXED, &["F1a", "F2a", "R1"],
        walkout_at_end_ok: check_hybrid_start_end, TECH_MIXED, &["R1", "F1a", "F2a"],
        walkout_at_end_with_pc_ok: check_hybrid_start_end, TECH_MIXED, &["R1", "F1a", "F2a", "2PC"],
        back_layout_in_middle_err: check_hybrid_start_end, TECH_MIXED, &["R1", "FB", "T4e"],
        back_layout_at_start_ok: check_hybrid_start_end, TECH_MIXED, &["FB", "R1", "T4e"],
        front_layout_in_middle_warn: check_hybrid_start_end, TECH_MIXED, &["R1", "F4a", "T4e"],
        front_layout_at_start_ok: check_hybrid_start_end, TECH_MIXED, &["F4a", "R1", "T4e"],
        no_decls_ok: check_hybrid_start_end, TECH_MIXED, &[],
        just_pc2_ok: check_hybrid_start_end, TECH_MIXED, &["4PC"],
        duet_c4plus: check_connections_in_non_team, Category{ag: AG12U, event: Duet, free: true}, &["C4+"],
        combo_c4plus_ok: check_connections_in_non_team, Category{ag: AG12U, event: Combo, free: true}, &["C4+"],
        duet_c4_ok: check_connections_in_non_team, Category{ag: AG12U, event: Duet, free: true}, &["C4"],
        solo_cb: check_connections_in_non_team, Category{ag: AG12U, event: Solo, free: true}, &["CB"],
        a5_warn: check_hybrid_common_base_marks, TECH_MIXED, &["A4b"],
        f10_warn: check_hybrid_common_base_marks, TECH_MIXED, &["F10*0.5"],
        other_decls_ok: check_hybrid_common_base_marks, TECH_MIXED, &["A5", "F6a*0.5", "F6c"],
        c4_trio_warn: check_hybrid_common_base_marks, Category{ag: AG12U, event: Trio, free: true}, &["C4"],
        c4_duet_ok: check_hybrid_common_base_marks, Category{ag: AG12U, event: Duet, free: true}, &["C4"],
        c4_tech_duet_warn: check_hybrid_common_base_marks, Category { ag: JRSR, free: false, event: Duet }, &["C4"],
        c4_tech_mixed_ok: check_hybrid_common_base_marks, TECH_MIXED, &["C4"],
        c4_tech_team_warn: check_hybrid_common_base_marks, Category { ag: JRSR, free: false, event: Team }, &["C4"],
        c4_plus_tech_team_ok: check_hybrid_common_base_marks, Category { ag: JRSR, free: false, event: Team }, &["C4+"],
        c2b_tech_duet_ok: check_hybrid_common_base_marks, Category{ag: AG12U, event: Duet, free: true}, &["C2b"],
        join_before_a6_warn: check_hybrid_common_base_marks, TECH_MIXED, &["A1d*0.3", "A6*0.5"],
        split_before_knight_warn: check_hybrid_common_base_marks, TECH_MIXED, &["F1a*0.5", "F6c*0.3"],
        a1c_c4_warn: check_ascent_connection, TECH_MIXED, &["A1c", "C4"],
        a1c_c4_plus_ok: check_ascent_connection, TECH_MIXED, &["A1c", "C4+"],
        pike_to_side_conn_warn: check_ascent_connection, TECH_MIXED, &["A3a", "C3"],
        pike_to_back_conn_ok: check_ascent_connection, TECH_MIXED, &["A3a", "C4"],
        rise_to_conn_warn: check_ascent_connection, TECH_MIXED, &["A3b", "C4+"],
        rise_to_rotate_conn_ok: check_ascent_connection, TECH_MIXED, &["A3b", "C5"],
    }

    #[test]
    fn test_check_tres() {
        let conditions = [
            ("solo_invalid_tre", Category { ag: JRSR, event: Solo, free: false }, "TRE5m", "", 1),
            ("team_tre4a", Category { ag: JRSR, event: Team, free: false }, "TRE4a", "", 1),
            ("team_wrong_dd", Category { ag: JRSR, event: Team, free: false }, "TRE4", "1.4", 1),
            ("team_right_dd", Category { ag: JRSR, event: Team, free: false }, "TRE4", "2.9", 0),
            ("solo_tre4a", Category { ag: JRSR, event: Solo, free: false }, "TRE4a", "", 0),
            ("free_solo_tre4a", Category { ag: JRSR, event: Solo, free: true }, "TRE4a", "", 1),
            ("md_tre4a", Category { ag: JRSR, event: MixedDuet, free: false }, "TRE4a", "", 1),
            ("md_tre3", Category { ag: JRSR, event: MixedDuet, free: false }, "TRE3", "", 0),
            ("tre_in_combo_err", Category { ag: Youth, event: Combo, free: false }, "TRE4a", "", 1),
        ];
        for (case, cat, tre, dd, errs) in conditions {
            assert_eq!(check_tres(cat, tre, dd).len(), errs, "{}", case);
        }
    }

    routine_tests! {
        too_many_hybrids_err: check_routine_element_maxes, Category{ag: AG12U, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        too_few_hybrids_err: check_routine_element_maxes, Category{ag: JRSR, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        correct_hybrids_ok: check_routine_element_maxes, Category{ag: Youth, event: Solo, free: true}, &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]],
        unknown_event_warn: check_routine_element_maxes, Category{ag: AG12U, event: Acrobatic, free: false}, &[],
        missing_thrust_err: check_mixed_duet_elements, TECH_MIXED, &[&["CB", "C4"]],
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
        unknown_ag_err: check_category, Category{ag: AgeGroups::Unknown, event: Solo, free: true}, &[],
        unknown_event_err: check_category, Category{ag: JRSR, event: Events::Unknown, free: true}, &[],
    }

    #[test]
    fn test_check_theme() {
        let combo_missing_err = check_theme(&CardBuilder::new().event_type(Combo).card);
        assert_eq!(combo_missing_err.len(), 1);
        let combo_ok =
            check_theme(&CardBuilder::new().event_type(Combo).theme("foo".to_string()).card);
        assert_eq!(combo_ok.len(), 0);
        let solo_missing_ok = check_theme(&CardBuilder::new().event_type(Solo).card);
        assert_eq!(solo_missing_ok.len(), 0);
    }

    #[test]
    fn test_check_routine_element_maxes() {
        let too_many_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["TRE1a"], &["TRE2a"], &["TRE3"], &["TRE4a"], &["TRE5a"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(too_many_pair_acros.len(), 1);
        let too_few_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(Category { ag: JRSR, event: Duet, free: true })
                .card,
        );
        assert_eq!(too_few_pair_acros.len(), 1);
        let ok_pair_acros = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(Category { ag: JRSR, event: Duet, free: true })
                .card,
        );
        assert_eq!(ok_pair_acros.len(), 0);

        let too_many_chohy = check_routine_element_maxes(
            &CardBuilder::new()
                .team_acros(&["A-Sq-Back-tk", "A-Sq-Back-tk", "A-Sq-Back-tk"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["ChoHy"], &["ChoHy"]])
                .category(Category { ag: AG12U, event: Combo, free: true })
                .card,
        );
        assert_eq!(too_many_chohy.len(), 1);

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
        assert_eq!(too_many_suconn.len(), 1);

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
        assert_eq!(too_many_tres.len(), 1);
        let too_few_tres = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(too_few_tres.len(), 1);
        let ok_tres = check_routine_element_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"], &["TRE5"]])
                .category(Category { ag: JRSR, event: Duet, free: false })
                .card,
        );
        assert_eq!(ok_tres.len(), 0);

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
        assert_eq!(too_many_team_acros.len(), 1);
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
        assert_eq!(too_few_team_acros.len(), 1);
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
        assert_eq!(ok_team_acros.len(), 0);
    }

    #[test]
    fn test_check_acros_event() {
        let pair_acro_in_solo =
            check_pair_acro(Category { ag: AG12U, event: Solo, free: true }, &"Js1B".into());
        assert_eq!(pair_acro_in_solo.len(), 1);
        let team_acro_in_solo = check_team_acro(
            Category { ag: AG12U, event: Solo, free: true },
            &TeamAcrobatic::from("A-Shou-Back-tk-s1").unwrap(),
            &"".into(),
        );
        assert_eq!(team_acro_in_solo.len(), 1);

        let pair_acro_in_duet =
            check_pair_acro(Category { ag: AG12U, event: Duet, free: true }, &"Js1B".into());
        assert_eq!(pair_acro_in_duet.len(), 0);
        let team_acro_in_duet = check_team_acro(
            Category { ag: AG12U, event: Duet, free: true },
            &TeamAcrobatic::from("A-Shou-Back-tk-s1").unwrap(),
            &"".into(),
        );
        assert_eq!(team_acro_in_duet.len(), 1);

        let pair_acro_in_team =
            check_pair_acro(Category { ag: AG12U, event: Team, free: true }, &"Js1B".into());
        assert_eq!(pair_acro_in_team.len(), 1);
        let team_acro_in_team = check_team_acro(
            Category { ag: AG12U, event: Team, free: true },
            &TeamAcrobatic::from("A-Shou-Back-tk-s1").unwrap(),
            &"".into(),
        );
        assert_eq!(team_acro_in_team.len(), 0);
    }

    #[test]
    fn test_check_routine_times() {
        let unknown_event = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Acrobatic, free: false })
                .card,
        );
        assert_eq!(unknown_event.len(), 1);

        let under = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 1, 15).unwrap())
                .card,
        );
        assert_eq!(under.len(), 1);

        let over = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 2, 15).unwrap())
                .card,
        );
        assert_eq!(over.len(), 1);

        let within_time = check_routine_times(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Solo, free: true })
                .end_time(NaiveTime::from_hms_opt(0, 2, 03).unwrap())
                .card,
        );
        assert_eq!(within_time.len(), 0);
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
        assert_eq!(overlapping_hybrids.len(), 1);

        base_card.elements[0].start_time = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
        base_card.elements[0].stop_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].start_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].stop_time = NaiveTime::from_hms_opt(0, 0, 3).unwrap();
        base_card.elements[2].start_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].stop_time = NaiveTime::from_hms_opt(0, 0, 4).unwrap();
        let overlapping_hybrid_acro = check_overlapping_elements(&base_card);
        assert_eq!(overlapping_hybrid_acro.len(), 1);

        base_card.category.event = Team;
        let overlapping_in_team_ok = check_overlapping_elements(&base_card);
        assert_eq!(overlapping_in_team_ok.len(), 0);
        base_card.category.event = Combo;

        base_card.elements[0].start_time = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
        base_card.elements[0].stop_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].start_time = NaiveTime::from_hms_opt(0, 0, 1).unwrap();
        base_card.elements[1].stop_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].start_time = NaiveTime::from_hms_opt(0, 0, 2).unwrap();
        base_card.elements[2].stop_time = NaiveTime::from_hms_opt(0, 0, 4).unwrap();
        let no_overlap = check_overlapping_elements(&base_card);
        assert_eq!(no_overlap.len(), 0);
    }

    #[test]
    fn test_check_dd_limits() {
        let above_limit_err =
            check_dd_limits(Category { ag: AG12U, event: Combo, free: true }, &"7.1");
        assert_eq!(above_limit_err.len(), 1);

        let below_limit_ok =
            check_dd_limits(Category { ag: AG12U, event: Combo, free: true }, &"6.9");
        assert_eq!(below_limit_ok.len(), 0);

        let above_limit_youth_ok =
            check_dd_limits(Category { ag: Youth, event: Combo, free: true }, &"7.1");
        assert_eq!(above_limit_youth_ok.len(), 0);
    }

    #[test]
    fn test_check_flexibility_combinations() {
        let hybrids = [
            (&["F1a".to_string(), "F2a".to_string()], 0),
            (&["F1a".to_string(), "F2b".to_string()], 0),
            (&["F1a".to_string(), "F3a".to_string()], 1),
            (&["F1a".to_string(), "F3b".to_string()], 0),
            (&["F1a".to_string(), "F6d".to_string()], 0),
            (&["F1a".to_string(), "F2c".to_string()], 1),
            (&["F1b".to_string(), "F3c".to_string()], 1),
            (&["ROB".to_string(), "F1a".to_string()], 1),
            (&["RO1".to_string(), "F1a".to_string()], 1),
            (&["F3a".to_string(), "F3a".to_string()], 2),
            (&["F1b".to_string(), "F4e".to_string()], 1),
            (&["F1b".to_string(), "F4f".to_string()], 1),
        ];
        for (decls, warns) in hybrids {
            let ci = check_flexibility_combinations(TECH_MIXED, decls);
            assert_eq!(warns, ci.len(), "hybrid {:?}: {:?}", decls, ci);
        }
    }
}
