use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::IssueLevel::Error;
use crate::hybrid::AwLC::*;
use crate::hybrid::ConnLC::*;
use crate::hybrid::FlexLC::*;
use crate::hybrid::LevelCode::*;
use crate::hybrid::ThrustLC::*;
use crate::hybrid::TwistLC::*;
use crate::hybrid::{Decl, Factor, HybridDecl, LevelCode};
use crate::{
    AgeGroups, CardIssue, Category, CoachCard, DD, Element, ElementKind, Events, MilliDD, ci_err,
    ci_errs, ci_warn, ci_warns, get_expected_routine_time,
};
use std::collections::HashMap;
use std::time::Duration;

type FamilyMatcherFn = fn(d: &Decl) -> bool;
type NamedFamilyMatcher = (&'static str, FamilyMatcherFn);

const fn thrust_matcher(d: &Decl) -> bool {
    matches!(d.lc, Thrust(_))
}

const fn spin_matcher(d: &Decl) -> bool {
    matches!(d.lc, Spin(_))
}

const fn twist_matcher(d: &Decl) -> bool {
    matches!(d.lc, Twist(_))
}

const fn airborne_matcher(d: &Decl) -> bool {
    matches!(d.lc, Aw(_))
}

const fn flex_matcher(d: &Decl) -> bool {
    matches!(d.lc, Flex(_))
}

const fn connection_matcher(d: &Decl) -> bool {
    matches!(d.lc, Conn(_, _))
}

const LATEST_ISS_VERSION: semver::Version = semver::Version::new(3, 0, 6);
fn check_iss_version(card: &CoachCard) -> Box<[CardIssue]> {
    if let Some(ver) = card.iss_ver.as_ref()
        && ver < &LATEST_ISS_VERSION
    {
        return ci_warns(format!(
            "Card created with version {ver}, latest is {LATEST_ISS_VERSION}"
        ));
    }
    [].into()
}

const fn points_for_declaration(declaration: &Decl) -> usize {
    match declaration.f {
        // as far as points go, factoring by 0.3 is the same as factoring
        // by 0.5. Ex. if the limit is 3, you get x6 0.5s or x6 0.3s not
        // x9 0.3s.
        Factor::_0_3 | Factor::_0_5 => 5,
        Factor::No => 10,
    }
}

fn check_max_families(decls: &[Decl], matcher: fn(&Decl) -> bool) -> usize {
    decls
        .iter()
        .filter(|p| matcher(p))
        .map(points_for_declaration)
        .reduce(|total, item| total + item)
        .unwrap_or_default()
}

fn check_hybrid_maxes(category: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    const MAX_FAMILIES: &[NamedFamilyMatcher] = &[
        ("Airborne Weight", airborne_matcher),
        ("Connection", connection_matcher),
        ("Flexibility", flex_matcher),
        ("Twist", twist_matcher),
        ("Spin", spin_matcher),
        ("Thrust", thrust_matcher),
    ];

    let mut ci = Vec::new();

    for (name, matcher) in MAX_FAMILIES {
        if check_max_families(decls, *matcher) > 50 {
            ci_err(&mut ci, format!("{name} can only be declared 5 times"));
        }
    }

    let mut decl_points = HashMap::<LevelCode, usize>::new();
    for decl in decls {
        // drop the plus information from the connection code since C4
        // and C4+ are considered the same code.
        let lc = match decl.lc {
            Conn(c, _) => Conn(c, false),
            _ => decl.lc,
        };

        let points =
            decl_points.get(&lc).copied().unwrap_or_default() + points_for_declaration(decl);
        decl_points.insert(lc, points);
    }

    for (decl, points) in decl_points {
        if points > 30 {
            ci_err(&mut ci, format!("{decl} is used more than 3 times"));
        } else if points > 20
            && matches!(decl, Conn(_, _))
            && matches!(category.event, Duet | MixedDuet)
        {
            ci_err(
                &mut ci,
                format!("Max of 2 connections with the same technique ({decl}) in Duets"),
            );
        }
    }
    ci.into()
}

struct EMax {
    chohy: usize,
    suconn: usize,
    tre: usize,
    acro: usize,
    hybrid: usize,
}

#[rustfmt::skip]
const fn element_maxes(category: &Category) -> Option<EMax> {
    #[allow(clippy::match_same_arms)]
    match (category.ag, category.event, category.free) {
        (AG12U, Solo, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 4 }),
        (AG12U, Duet, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 1, hybrid: 4 }),
        (AG12U, MixedDuet, true) => Some(EMax { chohy: 0, suconn: 3, tre: 0, acro: 2, hybrid: 3 }),
        (AG12U, Team, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 4 }),
        (AG12U, Combo, true) =>     Some(EMax { chohy: 1, suconn: 0, tre: 0, acro: 3, hybrid: 4 }),
        (Youth, Solo, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 5 }),
        (Youth, Duet, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 1, hybrid: 5 }),
        (Youth, MixedDuet, true) => Some(EMax { chohy: 0, suconn: 3, tre: 0, acro: 2, hybrid: 3 }),
        (Youth, Team, true) =>      Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 5 }),
        (Youth, Combo, true) =>     Some(EMax { chohy: 1, suconn: 0, tre: 0, acro: 4, hybrid: 4 }),
        (JRSR, Solo, true) =>       Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 6 }),
        (JRSR, Duet, true) =>       Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 2, hybrid: 6 }),
        (JRSR, MixedDuet, true) =>  Some(EMax { chohy: 0, suconn: 4, tre: 0, acro: 3, hybrid: 4 }),
        (JRSR, Trio, true) =>       Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 5 }),
        (JRSR, Team, true) =>       Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 3, hybrid: 6 }),
        (JRSR, Acrobatic, true) =>  Some(EMax { chohy: 0, suconn: 0, tre: 0, acro: 7, hybrid: 0 }),
        (JRSR, Combo, true) =>      Some(EMax { chohy: 1, suconn: 0, tre: 0, acro: 4, hybrid: 5 }),
        (JRSR, Solo, false) =>      Some(EMax { chohy: 0, suconn: 0, tre: 5, acro: 0, hybrid: 1 }),
        (JRSR, Duet, false) =>      Some(EMax { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 1 }),
        (JRSR, MixedDuet, false) => Some(EMax { chohy: 0, suconn: 3, tre: 3, acro: 2, hybrid: 2 }),
        (JRSR, Team, false) =>      Some(EMax { chohy: 0, suconn: 0, tre: 5, acro: 1, hybrid: 2 }),
        _ => None,
    }
}

fn check_routine_maxes(card: &CoachCard) -> Box<[CardIssue]> {
    fn check_max(ci: &mut Vec<CardIssue>, num: usize, max: usize, name: &str) {
        if num != max {
            ci_err(ci, format!("{max} {name} expected, but {num} found"));
        }
    }

    fn count_elements(elements: &[Element]) -> EMax {
        let mut el = EMax { chohy: 0, suconn: 0, tre: 0, acro: 0, hybrid: 0 };
        for element in elements {
            match element.kind {
                ElementKind::ChoHy(_) => el.chohy += 1,
                ElementKind::SuConn => el.suconn += 1,
                ElementKind::TRE(..) => el.tre += 1,
                ElementKind::PairAcro(..) | ElementKind::TeamAcro(..) => el.acro += 1,
                ElementKind::Hybrid(..) => el.hybrid += 1,
            }
        }
        el
    }

    element_maxes(&card.category).map_or_else(
        || ci_warns(format!("Could not determine element limits for {}", card.category)),
        |max| {
            let mut ci = Vec::new();
            let num = count_elements(&card.elements);
            check_max(&mut ci, num.chohy, max.chohy, "Choreography Hybrids");
            check_max(&mut ci, num.suconn, max.suconn, "Surface Connections");
            check_max(&mut ci, num.tre, max.tre, "TREs");
            check_max(&mut ci, num.acro, max.acro, "Acrobatics");
            check_max(&mut ci, num.hybrid, max.hybrid, "Hybrids");
            ci.into()
        },
    )
}

fn check_theme(card: &CoachCard) -> Box<[CardIssue]> {
    if matches!(card.category.event, Acrobatic | Combo if card.theme.is_empty()) {
        return ci_errs("Theme is required for Acrobatic and Combo routines");
    }
    [].into()
}

fn check_small_bonuses(category: Category, hybrid: &HybridDecl) -> Box<[CardIssue]> {
    if !matches!(category.event, Combo | Team) && hybrid.pc_bonus.is_some() {
        return ci_errs(format!("{} cannot have Pattern Change bonuses", category.event));
    }
    [].into()
}

fn check_mixduet_elems(card: &CoachCard) -> Box<[CardIssue]> {
    fn is_valid_req_hq(decls: &[Decl]) -> bool {
        // could match as Decl{} but then the cases don't fit on one line
        let lcs = decls.iter().map(|d| d.lc).collect::<Vec<_>>();
        matches!(lcs.as_slice(),
            [Conn(c1, false), Conn(c2, false), Thrust(_)] |
            [Conn(c1, false), Thrust(_), Conn(c2, false)] |
            [Thrust(_), Conn(c1, false), Conn(c2, false)] if c1 != c2
        ) && decls.iter().all(|d| d.f == Factor::No)
    }

    const EXPECTED_CAT: Category = Category { ag: JRSR, event: MixedDuet, free: false };
    if card.category == EXPECTED_CAT && !card.hybrids().any(|h| is_valid_req_hq(&h.decls)) {
        ci_errs(
            "Mixed Duet Tech routines must have one hybrid with only one Thrust and two different Connections",
        )
    } else {
        [].into()
    }
}

fn check_factoring(category: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    use Factor::*;
    const C2B_0_3: Decl = Decl { lc: Conn(C2b, false), f: _0_3 };
    const C4PLUS_0_5: Decl = Decl { lc: Conn(C4, true), f: _0_5 };

    let mut ci = Vec::new();

    if category.event == Solo && decls.iter().any(|d| d.f != No) {
        ci_err(&mut ci, "cannot factor in a Solo`");
    }

    if matches!(category.event, Duet | MixedDuet) {
        if decls.iter().any(|d| d.f == _0_3) {
            ci_err(&mut ci, "cannot factor by 0.3 in Duets");
        }

        if decls.iter().any(|d| d.f != No && matches!(d.lc, Conn(_, _))) {
            ci_err(&mut ci, "factoring connections in a Duet seems suspicious");
        }
    }

    // don't warn about C4+*0.5 C2*0.3 since that is valid for a
    // 7-person tech team doing a line of 4 and a line of 3.
    if !category.free && decls.iter().any(|d| d.f != No && !matches!(d, &C4PLUS_0_5 | &C2B_0_3)) {
        ci_warn(&mut ci, format!("factoring in a Tech {} seems suspicious", category.event));
    }

    if decls.iter().any(|d| d.f == _0_3 && matches!(d.lc, Conn(_, true))) {
        ci_warn(&mut ci, "factoring C+ by 0.3 requires 9-10 athletes");
    }

    for [prev_decl, decl] in decls.array_windows() {
        let is_c_0_5 = |d: &Decl| d.f == _0_5 && matches!(d.lc, Conn(_, false));
        let is_c_plus_0_5 = |d: &Decl| d.f == _0_5 && matches!(d.lc, Conn(_, true));
        if (is_c_0_5(decl) && is_c_plus_0_5(prev_decl))
            || (is_c_plus_0_5(decl) && is_c_0_5(prev_decl))
        {
            ci_warn(
                &mut ci,
                "if factoring a connection because 5-7 are swimming, one connection should be factored by 0.3",
            );
        }

        let is_c4_factored = |d: &Decl| d.lc == Conn(C4, false) && d.f != No;
        if (is_c4_factored(prev_decl) && decl == &C4PLUS_0_5)
            || (prev_decl == &C4PLUS_0_5 && is_c4_factored(decl))
        {
            ci_warn(
                &mut ci,
                "if factoring C4+ because 5-7 are swimming, is the smaller group still doing C4?",
            );
        }

        if prev_decl.f != No && decl.f != No && prev_decl.lc == decl.lc {
            ci_warn(&mut ci, "if performing same choreography in two groups, do not factor");
        }
    }

    ci.into()
}

fn check_routine_times(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    let expected_time = get_expected_routine_time(&card.category);
    if let Some(expected_time) = expected_time {
        let min_time = expected_time - Duration::new(5, 0);
        let max_time = expected_time + Duration::new(5, 0);
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
    ci.into()
}

fn check_connections_in_non_team(c: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    if matches!(c.event, Duet | MixedDuet | Trio)
        && decls.iter().any(|d| matches!(d.lc, Conn(_, true)))
    {
        ci_errs("C+ connections can only be used in team routines")
    } else if c.event == Solo && decls.iter().any(|d| matches!(d.lc, Conn(_, _))) {
        ci_errs("connections can not be used in solos")
    } else {
        [].into()
    }
}

fn check_families(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    if !card.category.free || card.category.event == Acrobatic {
        return ci.into();
    }

    let mut families: &[NamedFamilyMatcher] = &[
        ("Thrust", thrust_matcher),
        ("Spin", spin_matcher),
        ("Twist", twist_matcher),
        ("Airborne Weight", airborne_matcher),
        ("Flexibility", flex_matcher),
        ("Connection", connection_matcher),
    ];
    if card.category.event == Solo {
        families = &families[..families.len() - 1];
    }

    for (name, matcher) in families {
        if !card.hybrids().any(|h| h.decls.iter().any(|d| d.f == Factor::No && matcher(d))) {
            ci_err(&mut ci, format!("need at least one hybrid with one unfactored {name}"));
        }
    }

    ci.into()
}

fn check_overlapping_elements(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    if card.category.event != Combo {
        return ci.into();
    }

    for [prev_elem, elem] in card.elements.array_windows() {
        if elem.start_time < prev_elem.stop_time {
            ci_err(
                &mut ci,
                format!("Element {}: starts before previous element ends", elem.number),
            );
        }
    }
    ci.into()
}

fn check_dd_limits(category: Category, elem: &ElementKind) -> Box<[CardIssue]> {
    if let ElementKind::Hybrid(h, _) = elem
        && category.ag == AG12U
        && h.dd() > MilliDD(7000)
    {
        return ci_warns("USAAS 12U routines may not have hybrids with a DD greater than 7.0");
    }
    [].into()
}

fn check_category(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    if card.category.ag == AgeGroups::Unknown {
        ci_err(&mut ci, "Could not determine Age Group for card");
    }
    if card.category.event == Events::Unknown {
        ci_err(&mut ci, "Could not determine Event for card");
    }
    ci.into()
}

fn check_hybrid_common_base_marks(category: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    const fn is_knight(lc: LevelCode) -> bool {
        matches!(lc, Flex(F3c | F5a | F5c | F6b | F6c | F8a))
    }

    const fn is_mirror_conn(lc: LevelCode) -> bool {
        matches!(lc, Conn(C1a | C2a | C4 | C6a | C6b | C7, false))
    }

    let mut ci = Vec::new();

    for decl in decls {
        let lc = decl.lc;
        // FUTURE add F9?
        if matches!(lc, Aw(A4b) | Flex(F10)) {
            ci_warn(
                &mut ci,
                format!(
                    "{lc} has a high risk of base marking, athletes must not be vertical at ¾ point"
                ),
            );
        }

        if lc == Thrust(T9b) {
            ci_warn(
                &mut ci,
                "T9b has a high risk of base marking, it needs 8.5 height and a 1 second hold",
            );
        }

        if category.event == Trio && lc == Conn(C4, false) {
            ci_warn(&mut ci, "the two legs in a line variation of C4 requires C4+ and 4+ athletes");
        }

        // Mixed Duet can have mirror action, so only check Duet/Team
        // Tech Trios aren't an official event so ignore them
        if matches!(category.event, Duet | Team) && !category.free && is_mirror_conn(lc) {
            ci_warn(&mut ci, format!("{decl} in Tech {}, is this mirror action?", category.event));
        }
    }

    for [prev_lc, lc] in decls.array_windows().map(|[pd, d]| [pd.lc, d.lc]) {
        // these next two checks aren't "common" errors, but this was a
        // convenient place to check for something that is probably a
        // mistake if we see the two decls back-to-back
        if prev_lc == Aw(A1d) && lc == Aw(A6) {
            ci_warn(&mut ci, "A1d before A6, should this be A1a or A1c?");
        }

        if prev_lc == Flex(F1a) && is_knight(lc) {
            ci_warn(&mut ci, format!("F1a before {lc}, should this be F1b?"));
        }
    }
    ci.into()
}

fn check_hybrid_start_end(_: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    let end_pos = decls.len().wrapping_sub(1);
    for (i, decl) in decls.iter().enumerate() {
        if decl.lc == Flex(FB) && i != 0 {
            ci_err(&mut ci, format!("{decl} must be at the start of a hybrid"));
        }
        if decl.lc == Flex(F2a) && i != end_pos {
            ci_err(&mut ci, format!("{decl} must be at the end of a hybrid"));
        }
        if decl.lc == Flex(F4a) && i != 0 {
            ci_warn(&mut ci, format!("{decl} is not at the start, is this correct?"));
        }
    }
    ci.into()
}

fn check_ascent_connection(_: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    for [prev_lc, lc] in decls.array_windows().map(|[pd, d]| [pd.lc, d.lc]) {
        // TODO is A1c in writing? Manual only mentions A3a/A3b
        // A1c/C4 is for duet lift back-to-back
        // A3a is for from open pike to VP
        // A3b is for vert rise while connected
        if (prev_lc == Aw(A1c) && lc == Conn(C4, false))
            || (prev_lc == Aw(A3a) && matches!(lc, Conn(C3, _) | Conn(C4, true)))
            || (prev_lc == Aw(A3b) && matches!(lc, Conn(C3 | C4, _)))
        {
            ci_warn(
                &mut ci,
                format!(
                    "Ascents and Lifts cannot be declared simultaneously with a connection. If legs are connected during the {prev_lc}, there must be a disconnect or another action before the {lc}"
                ),
            );
        }
    }
    ci.into()
}

fn check_flexibility_combinations(_: Category, decls: &[Decl]) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    if decls.iter().any(|d| d.lc == Flex(F3a)) {
        ci_warn(
            &mut ci,
            "F3a means Right/Left split to opposite split, back to the initial split, 3 total splits",
        );
    }

    for [prev_lc, lc] in decls.array_windows().map(|[pd, d]| [pd.lc, d.lc]) {
        if (lc == Flex(F2c) && prev_lc == Flex(F1a))
            || (lc == Flex(F3c) && prev_lc == Flex(F1b))
            || (lc == Flex(F1a) && matches!(prev_lc, Twist(ROB | RO1)))
        {
            ci_warn(
                &mut ci,
                format!(
                    "an additional action (of any sort) must be performed between {prev_lc} and {lc}"
                ),
            );
        }

        // check for declaring a knight, but then doing a flex move
        // that starts with a fishtail
        //
        // don't use .starts_with because this is something that is more
        // likely to be correct when there are multiple groups. If it
        // becomes an issue, I could make the check more complicated.
        if prev_lc == Flex(F1b) && matches!(lc, Flex(F4e | F4f)) {
            ci_warn(
                &mut ci,
                format!(
                    "claiming {prev_lc} {lc} involves going to a knight, and then back to a fishtail is this correct?"
                ),
            );
        }
    }
    ci.into()
}

fn check_expected_dd(elem: &ElementKind) -> Option<CardIssue> {
    // if we are using text entry there is no reported DD to check
    // against, so we shouldn't report an error in that case.
    let dd = elem.dd();
    if let Some(r) = elem.reported_dd()
        && &dd != r
    {
        Some(CardIssue::new(Error, format!("card reports a DD of {r}, calculated a DD of {dd}")))
    } else {
        None
    }
}

pub fn check_one_element(category: Category, element: &ElementKind) -> Box<[CardIssue]> {
    use ElementKind::*;
    let mut ci = vec![];
    ci.extend(check_expected_dd(element));

    match &element {
        Hybrid(hybrid, _) => {
            ci.extend(
                [
                    check_hybrid_maxes,
                    check_factoring,
                    check_connections_in_non_team,
                    check_hybrid_common_base_marks,
                    check_hybrid_start_end,
                    check_ascent_connection,
                    check_flexibility_combinations,
                ]
                .iter()
                .flat_map(|check| check(category, &hybrid.decls)),
            );
            ci.extend(check_dd_limits(category, element));
            ci.extend(check_small_bonuses(category, hybrid));
        }
        ChoHy(_) | PairAcro(_, _) | TeamAcro(_, _) | SuConn | TRE(_, _) => {}
    }
    ci.into()
}

fn check_elements(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    for elem in &card.elements {
        for i in check_one_element(card.category, &elem.kind) {
            ci.push(CardIssue::new(i.level, format!("Element {}: {}", elem.number, i.text)));
        }
    }
    ci.into()
}

pub fn run_checks(card: &CoachCard) -> Box<[CardIssue]> {
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
#[cfg_attr(test, allow(clippy::too_many_lines))]
mod tests {
    use super::*;
    use crate::element::parse_elem_kind;
    use chrono::NaiveTime;

    fn new_card(category: Category, elems: &[&str]) -> CoachCard {
        let mut elements = Vec::new();
        for hybrid in elems {
            let kind = parse_elem_kind(category.event, category.free, hybrid, None).unwrap();
            elements.push(Element {
                number: elements.len() + 1,
                start_time: Default::default(),
                stop_time: Default::default(),
                kind,
            });
        }

        CoachCard { category, elements: elements.into(), ..Default::default() }
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
        type CheckFn = fn(Category, &[Decl]) -> Box<[CardIssue]>;
        let def = Default::default();

        let tests: &[(&str, CheckFn, Category, &str, usize)] = &[
            ("6_factored_ok", check_hybrid_maxes, def, "TB*0.5 T1*0.5 T2a T2b T3a T3b", 0),
            ("too_many_Ts", check_hybrid_maxes, def, "TB*0.3 T1*0.3 T2a T2b T3a T3b T3c*0.3", 1),
            ("five_spins_ok", check_hybrid_maxes, def, "SC1 S2 SCD3 SC4 S5", 0),
            ("six_s_err", check_hybrid_maxes, def, "SCDB SC1 S2 SCD3 SC4 S5", 1),
            ("five_twists_ok", check_hybrid_maxes, def, "RB R1 1R1 RU1 RO1", 0),
            ("six_t_err", check_hybrid_maxes, def, "RB R1 1R1 RU1 RO1 RC1", 1),
            ("five_flex_ok", check_hybrid_maxes, def, "FB F1a F1b F1c F2a", 0),
            ("six_f_err", check_hybrid_maxes, def, "FB F1a F1b F1c F2a F2b", 1),
            ("5_c_ok", check_hybrid_maxes, def, "CB*0.5 C7 C5 C3 C4+ C6a*0.5", 0),
            ("six_c_err", check_hybrid_maxes, def, "CB C1a C2a C3 C4+ C6a", 1),
            ("five_s_var_ok", check_hybrid_maxes, def, "SB SCB SCDB S1 SCD1", 0),
            ("five_rb_var_ok", check_hybrid_maxes, def, "RB 1RB 2RB ROB RCB", 0),
            ("five_r1_var_ok", check_hybrid_maxes, def, "R1 1R1 2R1 RO1 RC1", 0),
            ("four_a1s_ok", check_hybrid_maxes, def, "A1a A1b A1c A1d", 0),
            ("four_c4s_err", check_hybrid_maxes, def, "C4 C4 C4 C4+", 1),
            ("too_many_Cs", check_hybrid_maxes, def, "C4 C4*0.3 C4*0.3 C4*0.3 C4*0.3 C4*0.3", 1),
            ("three_c4s_duet_err", check_hybrid_maxes, TMIXED, "C4 C4 C4", 1),
            ("three_c4s_team_ok", check_hybrid_maxes, FTEAM, "C4 C4 C4", 0),
            ("solo_factored_err", check_factoring, FSOLO, "R1*0.5", 1),
            ("duet_factored_too_small_err", check_factoring, FDUET, "R1*0.3", 1),
            ("duet_factored_ok", check_factoring, FDUET, "R1*0.5", 0),
            ("team_factored_ok", check_factoring, FTEAM, "R1*0.3", 0),
            ("duet_factored_conn_err", check_factoring, FDUET, "C4*0.5", 1),
            ("mix_duet_factored_conn_err", check_factoring, FMDUET, "CB*0.5", 1),
            ("free_team_factored_conn_ok", check_factoring, FTEAM, "CB*0.5", 0),
            ("tech_team_factored_conn_ok", check_factoring, TTEAM, "C4+*0.5 C2b*0.3", 0),
            ("tech_team_factored_conn_warn", check_factoring, TTEAM, "CB*0.5", 1),
            ("tech_duet_factored_decl_warn", check_factoring, TDUET, "R1*0.5", 1),
            ("c_c_plus_warn", check_factoring, FTEAM, "C3*0.5 C3+*0.5", 1),
            ("c_c_plus_ok", check_factoring, FTEAM, "C3*0.3 C3+*0.5", 0),
            ("c_plus_c_warn", check_factoring, FTEAM, "C3+*0.5 C3*0.5", 1),
            ("c_plus_c_ok", check_factoring, FTEAM, "C3+*0.5 C3*0.3", 0),
            ("c4_c4plus_warn", check_factoring, FTEAM, "C4*0.3 C4+*0.5", 1),
            ("c4_c4plus_ok", check_factoring, FTEAM, "C2b*0.3 C4+*0.5", 0),
            ("c4plus_c4_warn", check_factoring, FTEAM, "C4+*0.5 C4*0.3", 1),
            ("c4plus_c4_ok", check_factoring, FTEAM, "C4+*0.5 C2b*0.3", 0),
            ("c4plus_c4_2_ok", check_factoring, FTEAM, "C4+*0.5 C2b*0.3", 0),
            ("repeat_decl_warn", check_factoring, FTEAM, "2R1*0.5 2R1*0.3", 1),
            ("non_repeat_decl_ok", check_factoring, FTEAM, "2R1*0.5 1R1*0.5", 0),
            ("cplus_less_half_warn", check_factoring, FTEAM, "C1a+*0.3", 1),
            ("walkout_in_middle_err", check_hybrid_start_end, def, "F1a F2a R1", 1),
            ("walkout_at_end_ok", check_hybrid_start_end, def, "R1 F1a F2a", 0),
            ("walkout_at_end_pc", check_hybrid_start_end, def, "R1 F1a F2a 2PC", 0),
            ("back_layout_in_middle_err", check_hybrid_start_end, def, "R1 FB T4e", 1),
            ("back_layout_at_start_ok", check_hybrid_start_end, def, "FB R1 T4e", 0),
            ("front_layout_in_middle_warn", check_hybrid_start_end, def, "R1 F4a T4e", 1),
            ("front_layout_at_start_ok", check_hybrid_start_end, def, "F4a R1 T4e", 0),
            ("no_decls_ok", check_hybrid_start_end, def, "", 0),
            ("just_pc2_ok", check_hybrid_start_end, def, "4PC", 0),
            ("duet_c4plus", check_connections_in_non_team, FDUET, "C4+", 1),
            ("combo_c4plus_ok", check_connections_in_non_team, COMBO, "C4+", 0),
            ("duet_c4_ok", check_connections_in_non_team, FDUET, "C4", 0),
            ("solo_cb", check_connections_in_non_team, FSOLO, "CB", 1),
            ("a5_warn", check_hybrid_common_base_marks, def, "A4b", 1),
            ("f10_warn", check_hybrid_common_base_marks, def, "F10*0.5", 1),
            ("other_decls_ok", check_hybrid_common_base_marks, def, "A5 F6a*0.5 F6c", 0),
            ("c4_trio_warn", check_hybrid_common_base_marks, TRIO, "C4", 1),
            ("c4_duet_ok", check_hybrid_common_base_marks, FDUET, "C4", 0),
            ("c4_tech_duet_warn", check_hybrid_common_base_marks, TDUET, "C4", 1),
            ("c4_tech_mixed_ok", check_hybrid_common_base_marks, TMIXED, "C4", 0),
            ("c4_tech_team_warn", check_hybrid_common_base_marks, TTEAM, "C4", 1),
            ("c4_plus_tech_team_ok", check_hybrid_common_base_marks, TTEAM, "C4+", 0),
            ("c2b_tech_duet_ok", check_hybrid_common_base_marks, TDUET, "C2b", 0),
            ("join_before_a6_warn", check_hybrid_common_base_marks, def, "A1d*0.3 A6*0.5", 1),
            ("split_then_knight", check_hybrid_common_base_marks, def, "F1a*0.5 F6c*0.3", 1),
            ("a1c_c4_warn", check_ascent_connection, def, "A1c C4", 1),
            ("a1c_c4_plus_ok", check_ascent_connection, def, "A1c C4+", 0),
            ("pike_to_side_conn_warn", check_ascent_connection, def, "A3a C3", 1),
            ("pike_to_back_conn_ok", check_ascent_connection, def, "A3a C4", 0),
            ("rise_to_conn_warn", check_ascent_connection, def, "A3b C4+", 1),
            ("rise_to_rotate_conn_ok", check_ascent_connection, def, "A3b C5", 0),
        ];
        for (name, check, cat, hybrid, expected) in tests {
            assert_eq!(
                check(*cat, &hybrid.parse::<HybridDecl>().unwrap().decls).len(),
                *expected,
                "{name}"
            );
        }
    }

    #[test]
    fn test_routine_issue() {
        let five_hybrids: &[&str] = &["R1", "R1", "R1", "R1", "R1"];

        let ag12solo = Category { ag: AG12U, event: Solo, free: true };
        let ysolo = Category { ag: Youth, event: Solo, free: true };

        let tests: &[(&str, fn(&CoachCard) -> Box<[CardIssue]>, Category, &[&str], usize)] = &[
            ("too_many_hybrids", check_routine_maxes, ag12solo, five_hybrids, 1),
            ("too_few_hybrids", check_routine_maxes, FSOLO, five_hybrids, 1),
            ("ok_hybrids", check_routine_maxes, ysolo, five_hybrids, 0),
            ("unk_evt", check_routine_maxes, Category { ag: AG12U, ..Default::default() }, &[], 1),
            ("no_thrust_err", check_mixduet_elems, TMIXED, &["CB  C4"], 1),
            ("no_thrust_in_free_ok", check_mixduet_elems, FMDUET, &["CB  C4"], 0),
            ("no_thrust_in_duet_ok", check_mixduet_elems, TDUET, &["CB  C4"], 0),
            ("1_C_1_T", check_mixduet_elems, TMIXED, &["C4  T9a"], 1),
            ("2_C_2_T", check_mixduet_elems, TMIXED, &["T4a  T9a  CB  C4"], 1),
            ("3_C_1_T", check_mixduet_elems, TMIXED, &["C1a  T9a  CB C4"], 1),
            ("extra_decls", check_mixduet_elems, TMIXED, &["C1a T9a R2 CB"], 1),
            ("same_conn", check_mixduet_elems, TMIXED, &["C1a T9a C1a"], 1),
            ("2_C_1_factored_T", check_mixduet_elems, TMIXED, &["CB T9a*0.5 C1a"], 1),
            ("2_C_1_T_ok", check_mixduet_elems, TMIXED, &["CB T9a C1a"], 0),
            ("solo_no_f", check_families, ysolo, &["TB SCD1 2R2", "A3b"], 1),
            ("solo_no_r", check_families, ysolo, &["FB SCD1 TB", "A3b"], 1),
            ("solo_no_s", check_families, ysolo, &["FB TB 2R2", "A3b"], 1),
            ("solo_no_t", check_families, ysolo, &["FB SCD1 2R2", "A3b"], 1),
            ("solo_all", check_families, ysolo, &["TB SCD1 2R2", "A3b F4f"], 0),
            ("duet_no_C", check_families, FDUET, &["TB SC1 2R2", "A3b F4f"], 1),
            ("tduet_no_C_ok", check_families, TDUET, &["TB SC1 2R2", "A3b F4f"], 0),
            ("duet_no_F", check_families, FDUET, &["TB S1 R2 A5 F9*0.5 CB"], 1),
            ("unk_ag", check_category, Category { event: Solo, ..Default::default() }, &[], 1),
            ("unk_evt", check_category, Category { ag: JRSR, ..Default::default() }, &[], 1),
        ];
        for (name, check, cat, hybrids, expected) in tests {
            assert_eq!(check(&new_card(*cat, hybrids)).len(), *expected, "{name}");
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
        let too_many_pair_acros = check_routine_maxes(&new_card(
            TDUET,
            &["Js1B", "L!fr1", "R1", "TRE1a", "TRE2a", "TRE3", "TRE4a", "TRE5a"],
        ));
        assert_eq!(too_many_pair_acros.len(), 1);
        let too_few_pair_acros =
            check_routine_maxes(&new_card(FDUET, &["R1", "R1", "R1", "R1", "R1", "R1"]));
        assert_eq!(too_few_pair_acros.len(), 1);
        let ok_pair_acros = check_routine_maxes(&new_card(
            FDUET,
            &["Js1B", "L!fr1", "R1", "R1", "R1", "R1", "R1", "R1"],
        ));
        assert_eq!(ok_pair_acros.len(), 0);

        let too_many_chohy = check_routine_maxes(&new_card(
            Category { ag: AG12U, event: Combo, free: true },
            &[
                "A-Sq-Back-tk",
                "A-Sq-Back-tk",
                "A-Sq-Back-tk",
                "R1",
                "R1",
                "R1",
                "R1",
                "ChoHy",
                "ChoHy",
            ],
        ));
        assert_eq!(too_many_chohy.len(), 1);

        let too_many_suconn = check_routine_maxes(&new_card(
            TMIXED,
            &[
                "Js1B", "L!fr1", "R1", "R1", "TRE1a", "TRE2a", "TRE3", "SuConn", "SuConn",
                "SuConn", "SuConn",
            ],
        ));
        assert_eq!(too_many_suconn.len(), 1);

        let too_many_tres = check_routine_maxes(&new_card(
            TMIXED,
            &[
                "Js1B", "L!fr1", "R1", "R1", "TRE1a", "TRE2a", "TRE3", "TRE1b", "SuConn", "SuConn",
                "SuConn",
            ],
        ));
        assert_eq!(too_many_tres.len(), 1);
        let too_few_tres = check_routine_maxes(&new_card(
            TDUET,
            &["Js1B", "R1", "TRE1a", "TRE2a", "TRE3", "TRE4a"],
        ));
        assert_eq!(too_few_tres.len(), 1);
        let ok_tres = check_routine_maxes(&new_card(
            TDUET,
            &["Js1B", "R1", "TRE1a", "TRE2a", "TRE3", "TRE4a", "TRE5a"],
        ));
        assert_eq!(ok_tres.len(), 0);

        let too_many_team_acros = check_routine_maxes(&new_card(
            Category { ag: JRSR, event: Acrobatic, free: true },
            &[
                "A-Shou-Back-tk-s1",
                "P-P-HA-bb/2wi-Porp/Trav",
                "C-Thr^2F-Forw-bb",
                "A-Sq-Back-pk/2ln-s1",
                "B-St-1P1P-bb/2ow",
                "C-Thr>St-Bln-tk-Cs1",
                "P-Knees-3pA-ne",
                "P-2S-FA+PF-ne/2ey",
            ],
        ));
        assert_eq!(too_many_team_acros.len(), 1);
        let too_few_team_acros = check_routine_maxes(&new_card(
            Category { ag: JRSR, event: Acrobatic, free: true },
            &[
                "A-Shou-Back-tk-s1",
                "P-P-HA-bb/2wi-Porp/Trav",
                "C-Thr^2F-Forw-bb",
                "A-Sq-Back-pk/2ln-s1",
                "B-St-1P1P-bb/2ow",
                "C-Thr>St-Bln-tk-Cs1",
            ],
        ));
        assert_eq!(too_few_team_acros.len(), 1);
        let ok_team_acros = check_routine_maxes(&new_card(
            Category { ag: JRSR, event: Acrobatic, free: true },
            &[
                "A-Shou-Back-tk-s1",
                "P-P-HA-bb/2wi-Porp/Trav",
                "C-Thr^2F-Forw-bb",
                "A-Sq-Back-pk/2ln-s1",
                "B-St-1P1P-bb/2ow",
                "C-Thr>St-Bln-tk-Cs1",
                "P-Knees-3pA-ne",
            ],
        ));
        assert_eq!(ok_team_acros.len(), 0);
    }

    #[test]
    fn test_check_routine_times() {
        let unknown_event = check_routine_times(&new_card(
            Category { ag: AG12U, event: Acrobatic, free: false },
            &[],
        ));
        assert_eq!(unknown_event.len(), 1);

        let under = check_routine_times(&CoachCard {
            category: Category { ag: AG12U, event: Solo, free: true },
            end_time: NaiveTime::from_hms_opt(0, 1, 15).unwrap(),
            ..Default::default()
        });
        assert_eq!(under.len(), 1);

        let over = check_routine_times(&CoachCard {
            category: Category { ag: AG12U, event: Solo, free: true },
            end_time: NaiveTime::from_hms_opt(0, 2, 15).unwrap(),
            ..Default::default()
        });
        assert_eq!(over.len(), 1);

        let within_time = check_routine_times(&CoachCard {
            category: Category { ag: AG12U, event: Solo, free: true },
            end_time: NaiveTime::from_hms_opt(0, 2, 3).unwrap(),
            ..Default::default()
        });
        assert_eq!(within_time.len(), 0);
    }

    #[test]
    fn test_check_overlapping_elements() {
        let mut base_card = new_card(COMBO, &["C4", "F8a", "A-Sq-Back-ln"]);
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
        let bl = ElementKind::Hybrid("T9b A7 A7 A7 A1c".parse().unwrap(), Some(MilliDD(6950)));
        let at = ElementKind::Hybrid("T9b A7 A7 A7 A1c AB".parse().unwrap(), Some(MilliDD(7000)));
        let ab = ElementKind::Hybrid("T9b A7 A7 A7 A1c RB".parse().unwrap(), Some(MilliDD(7050)));

        assert_eq!(check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, &ab).len(), 1);
        assert!(check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, &bl).is_empty());
        assert!(check_dd_limits(Category { ag: AG12U, event: Solo, free: true }, &at).is_empty());
        assert!(check_dd_limits(Category { ag: Youth, event: Solo, free: true }, &ab).is_empty());
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
            (&["F3a".to_string(), "F3a".to_string()], 1),
            (&["F1b".to_string(), "F4e".to_string()], 1),
            (&["F1b".to_string(), "F4f".to_string()], 1),
        ];
        for (decls, warns) in hybrids {
            let ci = check_flexibility_combinations(
                TMIXED,
                &decls.join(" ").parse::<HybridDecl>().unwrap().decls,
            );
            assert_eq!(warns, ci.len(), "hybrid {decls:?}: {ci:?}");
        }
    }

    #[test]
    fn test_check_expected_dd() {
        const ELEMS: &[(Events, bool, &str, u32)] = &[
            (Team, true, "P-P-F2A-sd", 1750),
            (Duet, true, "L!fr1", 1300),
            (Combo, true, "ChoHy", 1000),
            (Combo, true, "A5 A8", 2800),
            (Solo, false, "TRE1a", 2700),
        ];
        for (evt, free, s, dd) in ELEMS {
            let elem = parse_elem_kind(*evt, *free, s, Some(MilliDD(*dd))).unwrap();
            assert!(check_expected_dd(&elem).is_none(), "elem {} calc dd {}", s, elem.dd());
            let elem = parse_elem_kind(*evt, *free, s, Some(MilliDD(1234))).unwrap();
            assert!(
                check_expected_dd(&elem).is_some(),
                "elem {} dd is {} not {}",
                s,
                elem.dd(),
                "1.234"
            );
        }
    }
}
