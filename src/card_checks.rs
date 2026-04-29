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

fn check_hybrid_maxes(category: Category, decls: &[String]) -> Vec<CardIssue> {
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

fn check_routine_maxes(card: &CoachCard) -> Vec<CardIssue> {
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

fn check_mixduet_elems(card: &CoachCard) -> Vec<CardIssue> {
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

fn check_families(card: &CoachCard) -> Vec<CardIssue> {
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
                check_hybrid_maxes,
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
        check_routine_maxes,
        check_theme,
        check_mixduet_elems,
        check_families,
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
    use crate::{Category, Element};
    use chrono::NaiveTime;

    struct CardBuilder {
        card: CoachCard,
    }

    impl CardBuilder {
        fn new() -> CardBuilder {
            CardBuilder { card: Default::default() }
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
                    start_time: Default::default(),
                    stop_time: Default::default(),
                    kind,
                })
            }
            self
        }

        fn pair_acros(mut self, acros: &[&str]) -> Self {
            for acro in acros.into_iter() {
                self.card.elements.push(Element {
                    number: self.card.elements.len() + 1,
                    start_time: Default::default(),
                    stop_time: Default::default(),
                    kind: PairAcro(acro.to_string()),
                });
            }
            self
        }

        fn team_acros(mut self, acros: &[&str]) -> Self {
            for acro in acros.into_iter() {
                self.card.elements.push(Element {
                    number: self.card.elements.len() + 1,
                    start_time: Default::default(),
                    stop_time: Default::default(),
                    kind: TeamAcro(acro.parse().unwrap(), "1.0".into()),
                });
            }
            self
        }

        fn category(mut self, category: Category) -> Self {
            self.card.category = category;
            self
        }

        fn end_time(mut self, end_time: NaiveTime) -> Self {
            self.card.end_time = end_time;
            self
        }
    }

    #[test]
    fn test_check_iss_version() {
        let old_ver = semver::Version::new(0, 0, 1);
        let tests = [(None, 0), (Some(old_ver), 1), (Some(LATEST_ISS_VERSION), 0)];
        for (ver, expected) in tests {
            let card = CoachCard { iss_ver: ver, ..Default::default() };
            assert_eq!(check_iss_version(&card).len(), expected);
        }
    }

    const TMIXED: Category = Category { ag: JRSR, event: MixedDuet, free: false };
    const FSOLO: Category = Category { ag: JRSR, event: Solo, free: true };
    const FDUET: Category = Category { ag: JRSR, event: Duet, free: true };
    const FTEAM: Category = Category { ag: JRSR, event: Team, free: true };
    const TDUET: Category = Category { ag: JRSR, event: Duet, free: false };
    const TTEAM: Category = Category { ag: JRSR, event: Team, free: false };
    const FMDUET: Category = Category { ag: JRSR, event: MixedDuet, free: true };
    const TRIO: Category = Category { ag: JRSR, event: Trio, free: true };
    const COMBO: Category = Category { ag: JRSR, event: Combo, free: true };

    #[test]
    fn test_hybrid_issues() {
        let def = Category { ..Default::default() };

        type CheckFn = fn(Category, &[String]) -> Vec<CardIssue>;
        let tests: &[(&str, CheckFn, Category, &[&str], usize)] = &[
            (
                "6_factored_ok",
                check_hybrid_maxes,
                def,
                &["TB*0.5", "T1*0.5", "T2a", "T2b", "T3a", "T3b"],
                0,
            ),
            (
                "too_many_Ts",
                check_hybrid_maxes,
                def,
                &["TB*0.3", "T1*0.3", "T2a", "T2b", "T3a", "T3b", "T3c*0.3"],
                1,
            ),
            ("five_spins_ok", check_hybrid_maxes, def, &["SC1", "S2", "SCD3", "SC4", "S5"], 0),
            ("six_s_err", check_hybrid_maxes, def, &["SCDB", "SC1", "S2", "SCD3", "SC4", "S5"], 1),
            ("five_twists_ok", check_hybrid_maxes, def, &["RB", "R1", "1R1", "RU1", "RO1"], 0),
            ("six_t_err", check_hybrid_maxes, def, &["RB", "R1", "1R1", "RU1", "RO1", "RC1"], 1),
            ("five_flex_ok", check_hybrid_maxes, def, &["FB", "F1a", "F1b", "F1c", "F2a"], 0),
            ("six_f_err", check_hybrid_maxes, def, &["FB", "F1a", "F1b", "F1c", "F2a", "F2b"], 1),
            ("5_c_ok", check_hybrid_maxes, def, &["CB*0.5", "C1", "C2", "C3", "C4+", "C6*0.5"], 0),
            ("six_c_err", check_hybrid_maxes, def, &["CB", "C1", "C2", "C3", "C4+", "C6"], 1),
            ("five_s_var_ok", check_hybrid_maxes, def, &["SB", "SCB", "SCDB", "S1", "SCD1"], 0),
            ("five_rb_var_ok", check_hybrid_maxes, def, &["RB", "1RB", "2RB", "ROB", "RCB"], 0),
            ("five_r1_var_ok", check_hybrid_maxes, def, &["R1", "1R1", "2R1", "RO1", "RC1"], 0),
            ("four_a1s_ok", check_hybrid_maxes, def, &["A1a", "A1b", "A1c", "A1d"], 0),
            ("four_c4s_err", check_hybrid_maxes, def, &["C4", "C4", "C4", "C4+"], 1),
            (
                "too_many_Cs",
                check_hybrid_maxes,
                def,
                &["C4", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3", "C4*0.3"],
                1,
            ),
            ("three_c4s_duet_err", check_hybrid_maxes, TMIXED, &["C4", "C4", "C4"], 1),
            ("three_c4s_team_ok", check_hybrid_maxes, FTEAM, &["C4", "C4", "C4"], 0),
            ("just_pc_ok", check_valid_hybrid_declarations, def, &["2PC"], 0),
            ("factored_pc_err", check_valid_hybrid_declarations, def, &["2PC*0.3"], 1),
            ("too_many_pc_err", check_valid_hybrid_declarations, def, &["7PC"], 1),
            ("spin_base_ok", check_valid_hybrid_declarations, def, &["SB"], 0),
            ("spin_ten_ok", check_valid_hybrid_declarations, def, &["S10*0.3"], 0),
            ("spin_five_ok", check_valid_hybrid_declarations, def, &["S5*0.5"], 0),
            ("spin_factor_err", check_valid_hybrid_declarations, def, &["S5*0.4"], 1),
            ("combined_spin_level_ok", check_valid_hybrid_declarations, def, &["SC4*0.5"], 0),
            ("combined_spin_invalid_level", check_valid_hybrid_declarations, def, &["SC8"], 1),
            ("two_dir_spin_level_ok", check_valid_hybrid_declarations, def, &["SCD2*0.3"], 0),
            ("two_dir_spin_level_err", check_valid_hybrid_declarations, def, &["SCD7"], 1),
            ("conn_bad_option_err", check_valid_hybrid_declarations, def, &["C4a"], 1),
            ("conn_factored_plus_ok", check_valid_hybrid_declarations, def, &["C2b+*0.5"], 0),
            ("flex_ok", check_valid_hybrid_declarations, def, &["F4e*0.3"], 0),
            ("flex_err", check_valid_hybrid_declarations, def, &["F4k"], 1),
            ("air_ok", check_valid_hybrid_declarations, def, &["A7*0.3"], 0),
            ("air_err", check_valid_hybrid_declarations, def, &["A7a"], 1),
            ("swirl_ok", check_valid_hybrid_declarations, def, &["RB*0.5"], 0),
            ("swirl_err", check_valid_hybrid_declarations, def, &["R5"], 1),
            ("twist_ok", check_valid_hybrid_declarations, def, &["2R10*0.3"], 0),
            ("twist_err", check_valid_hybrid_declarations, def, &["2R11"], 1),
            ("unbal_ok", check_valid_hybrid_declarations, def, &["RU10*0.3"], 0),
            ("unbal_err", check_valid_hybrid_declarations, def, &["RU11"], 1),
            ("open_ok", check_valid_hybrid_declarations, def, &["ROB"], 0),
            ("close_err", check_valid_hybrid_declarations, def, &["RC2"], 1),
            ("one_leg_ok", check_valid_hybrid_declarations, def, &["1R6"], 0),
            ("one_leg_err", check_valid_hybrid_declarations, def, &["1R7"], 1),
            ("capital_letter_err", check_valid_hybrid_declarations, def, &["A3B"], 1),
            ("duet_with_pc", check_small_bonuses, FDUET, &["2PC"], 1),
            ("team_with_pc_ok", check_small_bonuses, FTEAM, &["2PC"], 0),
            ("solo_factored_err", check_factoring, FSOLO, &["R1*0.5"], 1),
            ("duet_factored_too_small_err", check_factoring, FDUET, &["R1*0.3"], 1),
            ("duet_factored_ok", check_factoring, FDUET, &["R1*0.5"], 0),
            ("team_factored_ok", check_factoring, FTEAM, &["R1*0.3"], 0),
            ("duet_factored_conn_err", check_factoring, FDUET, &["C4*0.5"], 1),
            ("mix_duet_factored_conn_err", check_factoring, FMDUET, &["CB*0.5"], 1),
            ("free_team_factored_conn_ok", check_factoring, FTEAM, &["CB*0.5"], 0),
            ("tech_team_factored_conn_ok", check_factoring, TTEAM, &["C4+*0.5", "C2b*0.3"], 0),
            ("tech_team_factored_conn_warn", check_factoring, TTEAM, &["CB*0.5"], 1),
            ("tech_duet_factored_decl_warn", check_factoring, TDUET, &["R1*0.5"], 1),
            ("c_c_plus_warn", check_factoring, FTEAM, &["C3*0.5", "C3+*0.5"], 1),
            ("c_c_plus_ok", check_factoring, FTEAM, &["C3*0.3", "C3+*0.5"], 0),
            ("c_plus_c_warn", check_factoring, FTEAM, &["C3+*0.5", "C3*0.5"], 1),
            ("c_plus_c_ok", check_factoring, FTEAM, &["C3+*0.5", "C3*0.3"], 0),
            ("c4_c4plus_warn", check_factoring, FTEAM, &["C4*0.3", "C4+*0.5"], 1),
            ("c4_c4plus_ok", check_factoring, FTEAM, &["C2b*0.3", "C4+*0.5"], 0),
            ("c4plus_c4_warn", check_factoring, FTEAM, &["C4+*0.5", "C4*0.3"], 1),
            ("c4plus_c4_ok", check_factoring, FTEAM, &["C4+*0.5", "C2b*0.3"], 0),
            ("c4plus_c4_2_ok", check_factoring, FTEAM, &["C4+*0.5", "C2b*0.3"], 0),
            ("repeat_decl_warn", check_factoring, FTEAM, &["2R1*0.5", "2R1*0.3"], 1),
            ("non_repeat_decl_ok", check_factoring, FTEAM, &["2R1*0.5", "1R1*0.5"], 0),
            ("cplus_less_half_warn", check_factoring, FTEAM, &["C1a+*0.3"], 1),
            ("walkout_in_middle_err", check_hybrid_start_end, def, &["F1a", "F2a", "R1"], 1),
            ("walkout_at_end_ok", check_hybrid_start_end, def, &["R1", "F1a", "F2a"], 0),
            ("walkout_at_end_pc", check_hybrid_start_end, def, &["R1", "F1a", "F2a", "2PC"], 0),
            ("back_layout_in_middle_err", check_hybrid_start_end, def, &["R1", "FB", "T4e"], 1),
            ("back_layout_at_start_ok", check_hybrid_start_end, def, &["FB", "R1", "T4e"], 0),
            ("front_layout_in_middle_warn", check_hybrid_start_end, def, &["R1", "F4a", "T4e"], 1),
            ("front_layout_at_start_ok", check_hybrid_start_end, def, &["F4a", "R1", "T4e"], 0),
            ("no_decls_ok", check_hybrid_start_end, def, &[], 0),
            ("just_pc2_ok", check_hybrid_start_end, def, &["4PC"], 0),
            ("duet_c4plus", check_connections_in_non_team, FDUET, &["C4+"], 1),
            ("combo_c4plus_ok", check_connections_in_non_team, COMBO, &["C4+"], 0),
            ("duet_c4_ok", check_connections_in_non_team, FDUET, &["C4"], 0),
            ("solo_cb", check_connections_in_non_team, FSOLO, &["CB"], 1),
            ("a5_warn", check_hybrid_common_base_marks, def, &["A4b"], 1),
            ("f10_warn", check_hybrid_common_base_marks, def, &["F10*0.5"], 1),
            ("other_decls_ok", check_hybrid_common_base_marks, def, &["A5", "F6a*0.5", "F6c"], 0),
            ("c4_trio_warn", check_hybrid_common_base_marks, TRIO, &["C4"], 1),
            ("c4_duet_ok", check_hybrid_common_base_marks, FDUET, &["C4"], 0),
            ("c4_tech_duet_warn", check_hybrid_common_base_marks, TDUET, &["C4"], 1),
            ("c4_tech_mixed_ok", check_hybrid_common_base_marks, TMIXED, &["C4"], 0),
            ("c4_tech_team_warn", check_hybrid_common_base_marks, TTEAM, &["C4"], 1),
            ("c4_plus_tech_team_ok", check_hybrid_common_base_marks, TTEAM, &["C4+"], 0),
            ("c2b_tech_duet_ok", check_hybrid_common_base_marks, TDUET, &["C2b"], 0),
            ("join_before_a6_warn", check_hybrid_common_base_marks, def, &["A1d*0.3", "A6*0.5"], 1),
            ("split_then_knight", check_hybrid_common_base_marks, def, &["F1a*0.5", "F6c*0.3"], 1),
            ("a1c_c4_warn", check_ascent_connection, def, &["A1c", "C4"], 1),
            ("a1c_c4_plus_ok", check_ascent_connection, def, &["A1c", "C4+"], 0),
            ("pike_to_side_conn_warn", check_ascent_connection, def, &["A3a", "C3"], 1),
            ("pike_to_back_conn_ok", check_ascent_connection, def, &["A3a", "C4"], 0),
            ("rise_to_conn_warn", check_ascent_connection, def, &["A3b", "C4+"], 1),
            ("rise_to_rotate_conn_ok", check_ascent_connection, def, &["A3b", "C5"], 0),
        ];
        for (name, check, cat, hybrid, expected) in tests {
            let decls: Vec<String> = hybrid.iter().map(|s| s.to_string()).collect();
            assert_eq!(check(*cat, &decls).len(), *expected, "{name}");
        }
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

    #[test]
    fn test_routine_issue() {
        let five_hybrids: &[&[&str]] = &[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"]];

        let ag12solo = Category { ag: AG12U, event: Solo, free: true };
        let ysolo = Category { ag: Youth, event: Solo, free: true };

        let tests: &[(&str, fn(&CoachCard) -> Vec<CardIssue>, Category, &[&[&str]], usize)] = &[
            ("too_many_hybrids", check_routine_maxes, ag12solo, five_hybrids, 1),
            ("too_few_hybrids", check_routine_maxes, FSOLO, five_hybrids, 1),
            ("ok_hybrids", check_routine_maxes, ysolo, five_hybrids, 0),
            ("unk_evt", check_routine_maxes, Category { ag: AG12U, ..Default::default() }, &[], 1),
            ("no_thrust_err", check_mixduet_elems, TMIXED, &[&["CB", "C4"]], 1),
            ("no_thrust_in_free_ok", check_mixduet_elems, FMDUET, &[&["CB", "C4"]], 0),
            ("no_thrust_in_duet_ok", check_mixduet_elems, TDUET, &[&["CB", "C4"]], 0),
            ("1_C_1_T", check_mixduet_elems, TMIXED, &[&["C4", "T9a"]], 1),
            ("2_C_2_T", check_mixduet_elems, TMIXED, &[&["T4", "T9a", "CB", "C4"]], 1),
            ("3_C_1_T", check_mixduet_elems, TMIXED, &[&["C1", "T9a", "CB", "C4"]], 1),
            ("extra_decls", check_mixduet_elems, TMIXED, &[&["C1", "T9a", "R2", "CB"]], 1),
            ("same_conn", check_mixduet_elems, TMIXED, &[&["C1", "T9a", "C1"]], 1),
            ("2_C_1_factored_T", check_mixduet_elems, TMIXED, &[&["CB", "T9a*0.5", "C1"]], 1),
            ("2_C_1_T_ok", check_mixduet_elems, TMIXED, &[&["CB", "T9a", "C1"]], 0),
            ("solo_no_f", check_families, ysolo, &[&["TB", "SCD1", "2R2"], &["A3b"]], 1),
            ("solo_all", check_families, ysolo, &[&["TB", "SCD1", "2R2"], &["A3b", "F4f"]], 0),
            ("duet_no_C", check_families, FDUET, &[&["TB", "SC1", "2R2"], &["A3b", "F4f"]], 1),
            ("tduet_no_C_ok", check_families, TDUET, &[&["TB", "SC1", "2R2"], &["A3b", "F4f"]], 0),
            ("duet_no_F", check_families, FDUET, &[&["TB", "S1", "R2", "A5", "F9*0.5", "CB"]], 1),
            ("unk_ag", check_category, Category { event: Solo, ..Default::default() }, &[], 1),
            ("unk_evt", check_category, Category { ag: JRSR, ..Default::default() }, &[], 1),
        ];
        for (name, check, cat, hybrids, expected) in tests {
            let card = &CardBuilder::new().category(*cat).hybrids(hybrids).card;
            assert_eq!(check(card).len(), *expected, "{name}");
        }
    }

    #[test]
    fn test_check_theme() {
        assert_eq!(check_theme(&CoachCard { category: COMBO, ..Default::default() }).len(), 1);
        let card = CoachCard { category: COMBO, theme: "foo".into(), ..Default::default() };
        assert!(check_theme(&card).is_empty());
        assert!(check_theme(&CoachCard { category: FSOLO, ..Default::default() }).is_empty());
    }

    #[test]
    fn test_check_routine_element_maxes() {
        let too_many_pair_acros = check_routine_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["TRE1a"], &["TRE2a"], &["TRE3"], &["TRE4a"], &["TRE5a"]])
                .category(TDUET)
                .card,
        );
        assert_eq!(too_many_pair_acros.len(), 1);
        let too_few_pair_acros = check_routine_maxes(
            &CardBuilder::new()
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(FDUET)
                .card,
        );
        assert_eq!(too_few_pair_acros.len(), 1);
        let ok_pair_acros = check_routine_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B", "L!fr1"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["R1"], &["R1"]])
                .category(FDUET)
                .card,
        );
        assert_eq!(ok_pair_acros.len(), 0);

        let too_many_chohy = check_routine_maxes(
            &CardBuilder::new()
                .team_acros(&["A-Sq-Back-tk", "A-Sq-Back-tk", "A-Sq-Back-tk"])
                .hybrids(&[&["R1"], &["R1"], &["R1"], &["R1"], &["ChoHy"], &["ChoHy"]])
                .category(Category { ag: AG12U, event: Combo, free: true })
                .card,
        );
        assert_eq!(too_many_chohy.len(), 1);

        let too_many_suconn = check_routine_maxes(
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
                .category(TMIXED)
                .card,
        );
        assert_eq!(too_many_suconn.len(), 1);

        let too_many_tres = check_routine_maxes(
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
                .category(TMIXED)
                .card,
        );
        assert_eq!(too_many_tres.len(), 1);
        let too_few_tres = check_routine_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"]])
                .category(TDUET)
                .card,
        );
        assert_eq!(too_few_tres.len(), 1);
        let ok_tres = check_routine_maxes(
            &CardBuilder::new()
                .pair_acros(&["Js1B"])
                .hybrids(&[&["R1"], &["TRE1"], &["TRE2"], &["TRE3"], &["TRE4"], &["TRE5"]])
                .category(TDUET)
                .card,
        );
        assert_eq!(ok_tres.len(), 0);

        let too_many_team_acros = check_routine_maxes(
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
        let too_few_team_acros = check_routine_maxes(
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
        let ok_team_acros = check_routine_maxes(
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
        let team_acro = &"A-Shou-Back-tk-s1".parse().unwrap();

        assert_eq!(check_pair_acro(FSOLO, &"Js1B".into()).len(), 1);
        assert_eq!(check_team_acro(FSOLO, team_acro, &"".into()).len(), 1);
        assert_eq!(check_pair_acro(FDUET, &"Js1B".into()).len(), 0);
        assert_eq!(check_team_acro(FDUET, team_acro, &"".into()).len(), 1);
        assert_eq!(check_pair_acro(FTEAM, &"Js1B".into()).len(), 1);
        assert_eq!(check_team_acro(FTEAM, team_acro, &"".into()).len(), 0);
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
            .category(COMBO)
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
        let above = check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, "7.1");
        assert_eq!(above.len(), 1);
        assert!(check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, "6.9").is_empty());
        assert!(check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, "7.0").is_empty());
        assert!(check_dd_limits(Category { ag: Youth, event: Solo, free: true }, "7.1").is_empty());
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
            let ci = check_flexibility_combinations(TMIXED, decls);
            assert_eq!(warns, ci.len(), "hybrid {:?}: {:?}", decls, ci);
        }
    }
}
