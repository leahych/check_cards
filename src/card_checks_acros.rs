use crate::card_checks::{pair_acros, team_acros};
use crate::AcroDirection::{Backwards, Forwards, Sideways};
use crate::AcroGroup::{Airborne, Balance, Combined, Platform};
use crate::AgeGroups::{Youth, AG12U, JRSR};
use crate::ElementKind::{PairAcro, TeamAcro};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::{AcroGroup, CardIssues, Category, CoachCard, Events, TeamAcrobatic};
use regex_lite::Regex;
use std::collections::{HashMap, HashSet};

fn check_dd_limits(card: &CoachCard) -> CardIssues {
    #[derive(Eq, Hash, PartialEq)]
    struct Cg {
        c: Category,
        g: AcroGroup,
    }
    let map = HashMap::from([
        (Cg { c: Category { ag: AG12U, event: Team, free: true }, g: Airborne }, 2.5),
        (Cg { c: Category { ag: AG12U, event: Team, free: true }, g: Balance }, 2.6),
        (Cg { c: Category { ag: AG12U, event: Team, free: true }, g: Combined }, 2.6),
        (Cg { c: Category { ag: AG12U, event: Team, free: true }, g: Platform }, 2.8),
        (Cg { c: Category { ag: AG12U, event: Combo, free: true }, g: Airborne }, 2.5),
        (Cg { c: Category { ag: AG12U, event: Combo, free: true }, g: Balance }, 2.6),
        (Cg { c: Category { ag: AG12U, event: Combo, free: true }, g: Combined }, 2.6),
        (Cg { c: Category { ag: AG12U, event: Combo, free: true }, g: Platform }, 2.8),
        (Cg { c: Category { ag: Youth, event: Team, free: true }, g: Airborne }, 2.7),
        (Cg { c: Category { ag: Youth, event: Team, free: true }, g: Balance }, 2.8),
        (Cg { c: Category { ag: Youth, event: Team, free: true }, g: Combined }, 2.8),
        (Cg { c: Category { ag: Youth, event: Team, free: true }, g: Platform }, 3.0),
        (Cg { c: Category { ag: Youth, event: Combo, free: true }, g: Airborne }, 2.7),
        (Cg { c: Category { ag: Youth, event: Combo, free: true }, g: Balance }, 2.8),
        (Cg { c: Category { ag: Youth, event: Combo, free: true }, g: Combined }, 2.8),
        (Cg { c: Category { ag: Youth, event: Combo, free: true }, g: Platform }, 3.0),
        (Cg { c: Category { ag: JRSR, event: Team, free: false }, g: Airborne }, 3.0),
        (Cg { c: Category { ag: JRSR, event: Team, free: false }, g: Balance }, 3.0),
        (Cg { c: Category { ag: JRSR, event: Team, free: false }, g: Combined }, 3.0),
        (Cg { c: Category { ag: JRSR, event: Team, free: false }, g: Platform }, 3.0),
    ]);

    let mut ci = CardIssues::default();
    for (num, acro, dd) in team_acros!(card.elements) {
        if let Some(max_dd) = map.get(&Cg { c: card.category, g: acro.group }) {
            if dd.parse().unwrap_or(0.0) > *max_dd {
                ci.errors.push(format!(
                    "Element {num}: {} may not have an acrobatic that has a DD > {max_dd}",
                    card.category
                ));
            }
        }
    }
    ci
}

fn check_groups_for_acro_routine(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.event != Acrobatic {
        return ci;
    }

    let mut group_counts = HashMap::<AcroGroup, usize>::new();
    for (num, acro, _) in team_acros!(card.elements) {
        let mut count = *group_counts.get(&acro.group).unwrap_or(&0usize);
        count += 1;
        group_counts.insert(acro.group, count);
        if count > 2 {
            ci.errors.push(format!(
                "Element {num}: may not have more than 2 {:?} acrobatics",
                acro.group
            ));
        }
    }

    for group in AcroGroup::iter() {
        if !group_counts.contains_key(&group) {
            ci.errors.push(format!("Missing {group:?} acrobatic"));
        }
    }
    ci
}

fn check_duplicate_pair_acros(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    let mut prev_acros = HashSet::new();
    for (num, acro) in pair_acros!(card.elements) {
        if prev_acros.contains(&acro) {
            ci.errors
                .push(format!("Element {num}: cannot repeat acrobatics in {:?}", card.category));
        } else {
            prev_acros.insert(acro);
        }
    }
    ci
}

fn check_duplicate_elements<'a, F: Fn(&TeamAcrobatic) -> Vec<String>>(
    acros: impl Iterator<Item = &'a (usize, &'a TeamAcrobatic, &'a String)>,
    map_function: F,
) -> CardIssues {
    let mut ci = CardIssues::default();

    let mut routine_elements = HashSet::new();
    for (num, acro, _) in acros {
        let acro_elements = map_function(acro);
        for element in acro_elements {
            if routine_elements.contains(&element) {
                ci.errors.push(format!(
                    "Element {num}: {:?} can only be used once in a {:?} acrobatic",
                    element, acro.group
                ));
            }
            routine_elements.insert(element);
        }
    }
    ci
}

fn check_team_duplicate_acros(card: &CoachCard) -> CardIssues {
    let group_a: Vec<(usize, &TeamAcrobatic, &String)> =
        team_acros!(card.elements).filter(|(_, acro, _)| acro.group == Airborne).collect();
    let group_b: Vec<(usize, &TeamAcrobatic, &String)> =
        team_acros!(card.elements).filter(|(_, acro, _)| acro.group == Balance).collect();
    let group_c: Vec<(usize, &TeamAcrobatic, &String)> =
        team_acros!(card.elements).filter(|(_, acro, _)| acro.group == Combined).collect();
    let group_p: Vec<(usize, &TeamAcrobatic, &String)> =
        team_acros!(card.elements).filter(|(_, acro, _)| acro.group == Platform).collect();

    let positions = |a: &TeamAcrobatic| a.positions.iter().map(|p| p.replace('2', "")).collect();
    let constructions = |acro: &TeamAcrobatic| vec![acro.construction.clone()];
    let connections = |acro: &TeamAcrobatic| vec![acro.connection_grip.clone()];

    let mut ci = check_duplicate_elements(group_a.iter(), positions);
    ci += check_duplicate_elements(group_b.iter(), constructions);
    ci += check_duplicate_elements(group_b.iter(), connections);
    ci += check_duplicate_elements(group_c.iter(), constructions);
    ci += check_duplicate_elements(group_p.iter(), constructions);
    ci += check_duplicate_elements(group_p.iter(), connections);
    ci += check_duplicate_elements(group_p.iter(), positions);
    ci
}

fn check_num_athletes(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    if acro.bonuses.contains(&"Dbl".to_string()) {
        ci.warnings.push(format!("{err_prefix}: requires 8 or more athletes"));
    }
    ci
}

fn check_team_acro_validity(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    if acro.construction.is_empty()
        || (acro.connection_grip.is_empty() && acro.direction.is_none())
        || acro.positions.is_empty()
        || acro.positions.len() > 2
    {
        ci.errors.push(format!("{err_prefix}: is missing one or more parts"));
    } else if !acro.positions.is_empty() && acro.positions[0].starts_with('2') {
        ci.errors.push(format!(
            "{err_prefix}: has second position, {}, but missing first position",
            acro.positions[0]
        ));
    } else if acro.positions.len() == 2 && (acro.positions[0] == acro.positions[1].replace('2', ""))
    {
        ci.errors.push(format!(
            "{err_prefix}: first position, {}, and second position, {} are the same",
            acro.positions[0], acro.positions[1]
        ));
    }
    if acro.bonuses.contains(&"Pos3".into()) && acro.positions.len() < 2 {
        ci.errors.push(format!(
            "{err_prefix}: 3rd position bonus declared, but only {} position(s)",
            acro.positions.len()
        ));
    }
    ci
}

fn check_direction(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    for rotation in &acro.rotations {
        if rotation.starts_with('c') && acro.direction != Some(Sideways) {
            ci.errors
                .push(format!("{err_prefix}: Direction should always be Sideways for cartwheels"));
        }
        if rotation.starts_with('h')
            && (acro.direction != Some(Forwards) && acro.direction != Some(Backwards))
        {
            ci.errors.push(format!(
                "{err_prefix}: Direction should always be Forwards or Backwards for handsprings"
            ));
        }
    }
    ci
}

fn check_rotations(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    if acro.rotations.iter().any(|rotation| rotation.contains('o'))
        && !acro.positions.contains(&"2ln".into())
        && !acro.bonuses.contains(&"Pos3".into())
    {
        ci.warnings.push(format!("{err_prefix}: Somersault with open declared, but 2ln or Pos3 not declared, is this right?"));
    }

    if acro.rotations.iter().any(|rotation| rotation.contains("ss"))
        && !acro.positions.contains(&"ln".into())
        && !acro.positions.contains(&"2ln".into())
    {
        ci.warnings.push(format!("{err_prefix}: Straight somersault declared, but no line position declared, is this right?"));
    }

    let rotation_map: &[(Regex, &[&str])] = &[
        (
            Regex::new(r"^r(0.5|1|1.5)$").unwrap(),
            &["FPx", "FP", "SiSb", "Bp", "E", "AP", "SiS", "F1S", "Tw", "S+", "1F1P", "1F1F"],
        ),
        (Regex::new(r"^r(0.5|1|1.5)/$").unwrap(), &["FS"]),
        (Regex::new(r"^r(0.5|1|1.5|2)\+$").unwrap(), &["F1S", "FPx", "FP", "1F1P", "1F1F"]),
        (
            Regex::new(r"^r(0.5|1|1.5|2)!$").unwrap(),
            &["1P1P", "1P1F", "Px1P", "PP", "PF", "PH/", "FF", "FF/", "ShF", "LayF", "SiF", "S+"],
        ),
        (Regex::new(r"^r(/|0.5|1)L$").unwrap(), &["LiH", "Li"]),
        (Regex::new(r"^Cr(0.5|1|1.5)$").unwrap(), &["Thr>St", "Thr>L"]),
        (Regex::new(r"^Cr(0.5|1|1.5)!$").unwrap(), &["Thr>StH", "Thr>L"]),
        (Regex::new(r"^Cr0.5L$").unwrap(), &["Thr^Lh"]),
        (Regex::new(r"^CP0.5$").unwrap(), &["Thr>FF", "Thr>F"]),
        (Regex::new(r"^2F(0.5|1)$").unwrap(), &["Thr^2F"]),
        (
            Regex::new(r"^P(r|r0.5|r1)$").unwrap(),
            &[
                "F2A", "FAb", "3pA", "1FA", "HA", "3pK", "3pb", "FA+PF", "SP+L", "ShF+P", "4p",
                "2pA", "4pAb", "Bb", "2pK", ">F1P", "3pBb", "3pB+b", ">1P1P/", "1Fxs/",
            ],
        ),
        (Regex::new(r"^P(r|r0.5|r1)/$").unwrap(), &["SiA", "SP+K", "SiF+Pb", "L/SiF+P"]),
    ];

    let construction_or_connection = match acro.group {
        Airborne | Combined => &acro.construction,
        Balance | Platform => &acro.connection_grip,
    };

    for rotation in &acro.rotations {
        for (rx, connections) in rotation_map {
            if rx.is_match(rotation) {
                if !connections.contains(&construction_or_connection.as_str()) {
                    ci.errors.push(format!(
                        "{err_prefix}: {rotation} can only be used with {}",
                        connections.join(", ")
                    ));
                }
                break;
            }
        }

        // let's be consistent about what is allowed for different rotations
        // except for Platform!!
        if acro.group == Platform {
            if Regex::new(r"^P(0.5|1)h$").unwrap().is_match(rotation) && acro.construction != "Hand"
            {
                ci.errors.push(format!("{err_prefix}: {rotation} can only be used with Hand"));
            }
            if Regex::new(r"^P(r|r0.5|r1)//$").unwrap().is_match(rotation)
                && acro.construction != "2S"
                && acro.construction != "Flower"
            {
                ci.errors
                    .push(format!("{err_prefix}: {rotation} can only be used with 2S, Flower"));
            }
        }
    }
    ci
}

fn check_bonuses(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    let first_pos = acro.positions.first().map_or("", |v| v);
    if acro.bonuses.contains(&"Split".into()) && first_pos == "sp" {
        ci.errors.push(format!(
            "{err_prefix}: for Split bonus, sp is considered a take-off and should not be declared first"
        ));
    }

    if acro.bonuses.contains(&"Porp".into()) && (first_pos == "Bo" || first_pos == "bo") {
        ci.errors
            .push(format!("{err_prefix}: Box cannot be claimed as Position 1 with Porp Bonus"));
    }

    if acro.bonuses.len() < 2 {
        for rotation in &acro.rotations {
            // in here assuming that if they have 2 they don't want to claim Conn
            if (rotation.starts_with('c') || rotation.starts_with('h'))
                && !acro.bonuses.contains(&"Conn".into())
                && !acro.bonuses.contains(&"Grip".into())
                && !acro.bonuses.contains(&"Catch".into())
            {
                ci.warnings.push(format!("{err_prefix}: can claim 'Conn' with {rotation}"));
            }
        }
        return ci;
    }
    if acro.bonuses.len() > 2 {
        ci.errors.push(format!(
            "{err_prefix}: {} bonuses declared, but only 2 bonuses allowed",
            acro.bonuses.len()
        ));
        return ci;
    }

    let bonus1 = &acro.bonuses[0];
    let bonus2 = &acro.bonuses[1];
    if bonus1 == bonus2 && bonus1 != "CRoll" {
        ci.errors.push(format!("{err_prefix}: cannot declare the same bonus twice"));
        return ci;
    }

    let exclusive_bonuses: &[&[&str]] = &[
        &["Grip", "Conn", "Catch"],
        &["Hula", "RetSq", "RetPa"],
        &["Twirl", "RotF"],
        &["Jump", "Jump>", "On1Foot", "1F>1F"],
        &["Run", "BRun"],
        &["Porp", "Spich"],
        &["Dive", "Ps1", "Ps1t0.5", "Ps1op", "Ps1t0,5o", "Ps1t1", "CH+"],
        &["Spider", "Climb"],
        &["Fall", "FTurn"],
    ];
    for exclusive in exclusive_bonuses {
        if exclusive.contains(&bonus1.as_str()) && exclusive.contains(&bonus2.as_str()) {
            ci.errors.push(format!(
                "{err_prefix}: cannot declare {bonus1} and {bonus2} in the same acrobatic"
            ));
            return ci;
        }
    }

    ci
}

fn check_age_restrictions(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    for (num, acro, _) in team_acros!(card.elements) {
        // TODO need to handle JR vs SR
        // add raw name to card and attempt to parse JR vs SR here?
        let is_not_sr = card.category.ag == AG12U || card.category.ag == Youth;
        if acro.bonuses.contains(&"1F>1F".into()) && is_not_sr {
            ci.errors.push(format!("Element {num}: 1F>1F is only allowed in Senior routines"));
        }
    }
    ci
}

fn check_construction(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    const ALLOWED_GRIPS: &[&str] = &["PP", "PF", "Bp", "ShF", "E", "F1S", "LayF"];

    let mut ci = CardIssues::default();
    if acro.construction == "St>" && !ALLOWED_GRIPS.contains(&acro.connection_grip.as_str()) {
        ci.warnings.push(format!(
                "{err_prefix}: St> cannot be used with {}, must be a connection marked with ∞, such as {}",
                acro.connection_grip, ALLOWED_GRIPS.join(", ")
        ));
    }
    ci
}

const B_ONE_LEG_POSITIONS: &[&str] = &["he", "vs", "gl", "ba", "sa", "ne", "ey"];
const B_TWO_LEG_POSITIONS: &[&str] = &["sd"];
const B_SIT_STAND_LAY_POSITONS: &[&str] =
    &["mo", "sh", "spl", "hp", "sc", "co", "fl", "so", "tu", "pi"];
const B_HEAD_DOWN_POSITIONS: &[&str] = &["bb", "bo", "wi", "ow"];
const B_EXTREME_FLEX_POSITIONS: &[&str] = &["dr", "qu"];

fn check_connection(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    // not a category but positions that can be done by standing w/two feet
    const TWO_FOOT_POSITIONS: &[&str] = &["sd", "mo", "sh", "dr"];
    // ShF and E aren't handstand, but have the same sort of movement
    const HANDSTAND_CONNECTIONS: &[&str] = &["1P1P", "1P1F", "Px1P", "PP", "PF", "PH/", "ShF", "E"];

    // similar, anything that could be considered head up
    let head_up_positions = [B_ONE_LEG_POSITIONS, B_SIT_STAND_LAY_POSITONS, &["sd", "dr"]].concat();

    let mut ci = CardIssues::default();
    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    match acro.connection_grip.as_str() {
        "FS" => {
            if !TWO_FOOT_POSITIONS.contains(&first_pos) {
                ci.warnings.push(format!(
                    "{err_prefix}: expected two foot position with FS, but found {first_pos}"
                ));
            }
        }
        "1P1P" | "1P1F" | "Px1P" | "PP" | "PF" | "Bp" | "ShF" | "E" | "PH/" | "Tw" => {
            if !B_HEAD_DOWN_POSITIONS.contains(&first_pos) {
                ci.warnings.push(format!(
                    "{err_prefix}: expected head-down position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "FPx" | "FP" | "FF" | "FF/" | "F1S" | "SiF" | "1FH+1FP" | "1F1P" | "1F1F" => {
            if !head_up_positions.contains(&first_pos) {
                ci.warnings.push(format!(
                    "{err_prefix}: expected head-up position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "LayF" | "S+" => {
            if !B_SIT_STAND_LAY_POSITONS.contains(&first_pos) {
                ci.warnings.push(format!(
                    "{err_prefix}: expected sit, stand, or lay position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        _ => {}
    }

    if HANDSTAND_CONNECTIONS.contains(&acro.connection_grip.as_str()) && first_pos != "bb" {
        ci.warnings.push(format!(
            "{err_prefix}: in handstand positions, the first position should be bb unless the featured swimmer goes directly to Position 1 from underwater"
        ));
    }
    ci
}

fn check_positions(err_prefix: &str, acro: &TeamAcrobatic) -> CardIssues {
    const ONE_LEG_CONNECTIONS: &[&str] =
        &["FPx", "F1S", "1F1P", "1F1F", "FAb", "3pA", "3pb", ">F1P", "3pBb", "3pB+b", "1Fxs/"];
    let mut ci = CardIssues::default();
    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    match first_pos {
        "he" | "vs" | "gl" | "ba" | "sa" | "ne" | "ey" | "qu" => {
            if !ONE_LEG_CONNECTIONS.contains(&acro.connection_grip.as_str()) {
                ci.warnings.push(format!("{err_prefix}: one leg position, {first_pos}, declared, but {} is not a one leg connection",
                                         acro.connection_grip));
            }
        }
        _ => {}
    }

    let all_b_positions = [
        B_ONE_LEG_POSITIONS,
        B_TWO_LEG_POSITIONS,
        B_SIT_STAND_LAY_POSITONS,
        B_HEAD_DOWN_POSITIONS,
        B_EXTREME_FLEX_POSITIONS,
    ]
    .concat();

    if acro.group == Combined
        && acro.construction.contains('^')
        && !all_b_positions.contains(&first_pos)
    {
        ci.warnings.push(format!("{err_prefix}: In fly-above acrobatics, the first featured swimmer is probably the balance, but position 1 is {first_pos}"));
    }

    ci
}

pub fn run_acro_checks(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();

    let checks: &[fn(&CoachCard) -> CardIssues] = match card.category.event {
        Solo | Events::Unknown => &[],
        Duet | MixedDuet | Trio => &[check_duplicate_pair_acros],
        Acrobatic | Combo | Team => &[
            check_dd_limits,
            check_groups_for_acro_routine,
            check_team_duplicate_acros,
            check_age_restrictions,
        ],
    };
    for check in checks {
        ci += check(card);
    }
    if card.category.event.is_team_event() {
        let acros_checks = &[
            check_num_athletes,
            check_team_acro_validity,
            check_direction,
            check_rotations,
            check_bonuses,
            check_construction,
            check_connection,
            check_positions,
        ];
        for (num, acro, _) in team_acros!(card.elements) {
            for check in acros_checks {
                ci += check(format!("Element {num}").as_str(), acro);
            }
        }
    }
    ci
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Element;
    use chrono::NaiveTime;

    // TODO share this as test-only code
    struct CardBuilder {
        card: CoachCard,
    }

    impl CardBuilder {
        fn new() -> CardBuilder {
            CardBuilder { card: CoachCard::default() }
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

        fn team_acro_with_dd(mut self, acro: &str, dd: f32) -> Self {
            self.card.elements.push(Element {
                number: self.card.elements.len() + 1,
                start_time: NaiveTime::default(),
                stop_time: NaiveTime::default(),
                kind: TeamAcro(TeamAcrobatic::from(acro).unwrap(), dd.to_string()),
            });
            self
        }

        fn category(mut self, category: Category) -> Self {
            self.card.category = category;
            self
        }
    }

    fn run_test(name: &str, check_fn: fn(&CoachCard) -> CardIssues, card: &CoachCard) {
        let expected = if name.ends_with("_ok") { 0 } else { 1 };
        let result = check_fn(card);
        //println!("TEST {:?}", result.errors);
        assert_eq!(result.errors.len(), expected);
    }

    macro_rules! acro_tests {
        ($($name:ident: $fname:expr, $code:expr, $errs:literal, $warns:literal,)*) => {
        $(
            #[test]
            fn $name () {
                let acro = TeamAcrobatic::from($code).unwrap();
                let result = $fname("", &acro);
                //println!("TEST {:?}", result.errors);
                assert_eq!(result.errors.len(), $errs);
                assert_eq!(result.warnings.len(), $warns);
            }
        )*
        }
    }

    macro_rules! card_acro_tests {
        ($($name:ident: $fname:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), $fname, &CardBuilder::new().team_acros($value).card);
            }
        )*
        }
    }

    macro_rules! card_acro_cat_tests {
        ($($name:ident: $fname:expr, $category:expr, $value:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), $fname, &CardBuilder::new().category($category).team_acros($value).card);
            }
        )*
        }
    }

    card_acro_tests! {
        repeat_pos_group_a: check_team_duplicate_acros,  &["A-Sq-Back-pk/2tk", "A-Sq-Back-tk/2ja"],
        group_a_no_dups_ok: check_team_duplicate_acros,  &["A-Sq-Back-pk/2ln", "A-Sq-Back-tk/2spl"],
        repeat_construction_group_b: check_team_duplicate_acros, &["B-St-1P1P-bb", "B-St-PP-ow"],
        repeat_connection_group_b: check_team_duplicate_acros, &["B-St-PP-bb", "B-StH-PP-ow"],
        group_b_no_dups_ok: check_team_duplicate_acros, &["B-St-1P1P-bb", "B-StH-PPOw"],
        repeat_pos_group_c:  check_team_duplicate_acros, &["C-Thr>St-Bln-tk-Cs1", "C-Thr>St-Forw-sd/2tk-Cd-Jump"],
        group_c_no_dups_ok:  check_team_duplicate_acros, &["C-Thr>St-Bln-tk-Cs1", "C-Thr>F-Forw-sd/2tk-Cd-Jump"],
        repeat_construction_group_p: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow", "P-Knees-3pA-ne"],
        repeat_connection_group_p: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow", "P-2S-SP+K-ne/2ey"],
        repeat_pos_group_p: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow", "P-2S-FA+PF-ow/2ey"],
        group_p_no_dups_ok: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow", "P-2S-FA+PF-ne/2ey"],
    }
    card_acro_cat_tests! {
        all_groups_ok: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["A-Shou-Back-tk-s1", "B-St-FS-ln", "C-Thr^2F-Forw-bb", "P-P-HA-bb/2wi-Porp/Trav"],
        missing_a: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["B-St-FS-ln", "C-Thr^2F-Forw-bb", "P-P-HA-bb/2wi-Porp/Trav"],
        missing_a_team_ok: check_groups_for_acro_routine, Category{ag: JRSR, event: Team, free: true}, &["B-St-FS-ln", "C-Thr^2F-Forw-bb", "P-P-HA-bb/2wi-Porp/Trav"],
        missing_b: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["A-Shou-Back-tk-s1", "C-Thr^2F-Forw-bb", "P-P-HA-bb/2wi-Porp/Trav"],
        missing_c: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["A-Shou-Back-tk-s1", "B-St-FS-ln", "P-P-HA-bb/2wi-Porp/Trav"],
        missing_p: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["A-Shou-Back-tk-s1", "B-St-FS-ln", "C-Thr^2F-Forw-bb"],
        too_many_a: check_groups_for_acro_routine, Category{ag: JRSR, event: Acrobatic, free: true}, &["A-Shou-Back-tk-s1", "A-Shou-Back-tk-s1", "A-Shou-Back-tk-s1", "B-St-FS-ln", "C-Thr^2F-Forw-bb", "P-P-HA-bb/2wi-Porp/Trav"],
        ag12u_1f1f_err: check_age_restrictions, Category{ag: AG12U, event: Team, free: true}, &["C-Thr>StH-Forw-ln-1F>1F"],
        youth_1f1f_err: check_age_restrictions, Category{ag: Youth, event: Team, free: true}, &["C-Thr>StH-Forw-ln-1F>1F"],
        sr_1f1f_ok: check_age_restrictions, Category{ag: JRSR, event: Team, free: true}, &["C-Thr>StH-Forw-ln-1F>1F"],
    }

    macro_rules! dd_tests {
        ($($name:ident: $category:expr, $acro:expr, $dd:expr,)*) => {
        $(
            #[test]
            fn $name () {
                run_test(stringify!($name), check_dd_limits, &CardBuilder::new().category($category).team_acro_with_dd($acro, $dd).card);
            }
        )*
        }
    }

    dd_tests! {
        tech_limit: Category { ag: JRSR, event: Team, free: false }, "A-Shou-Back-tk-s1", 3.05,
        free_no_limit_ok: Category { ag: JRSR, event: Team, free: true }, "A-Shou-Back-tk-s1", 3.05,
        ag12u_ok: Category{ag: AG12U, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", 2.75,
        too_high_12u: Category{ag: AG12U, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", 2.85,
        youth_ok: Category{ag: Youth, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", 2.85,
        youth_too_high: Category{ag: Youth, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", 3.05,
    }

    acro_tests! {
        dbl_warns: check_num_athletes, "A-Sq-Back-pk/2rg-s1-Dbl", 0, 1,
        no_dbl_ok: check_num_athletes, "A-Sq-Back-pk/2rg-s1", 0, 0,
        //missing_direction: check_team_acro_validity, "A-Sq", 1, 0,
        missing_positions: check_team_acro_validity, "A-Sq-Back", 1, 0,
        missing_first_pos: check_team_acro_validity, "A-Sq-Back-2ln", 1, 0,
        duplicate_positions: check_team_acro_validity, "A-Sq-Back-ln/2ln", 1, 0,
        missing_2nd_pos: check_team_acro_validity, "A-Sq-Back-pk-Pos3", 1, 0,
        pos3_bonus_ok: check_team_acro_validity, "A-Sq-Back-pk/2rg-Pos3", 0, 0,
        back_with_cart_err: check_direction, "A-Sq-Back-ln-ct0.5", 1, 0,
        side_with_cart_ok: check_direction, "A-Sq-Side-ln-ct0.5", 0, 0,
        side_with_hand_err: check_direction, "A-Sq-Side-ln-hd", 1, 0,
        forw_with_hand_ok: check_direction, "A-Sq-Forw-ln-hd", 0, 0,
        airborne_rotation_ok: check_rotations, "A-Sq-Side-ln-ct0.5", 0, 0,
        fs_with_r_err: check_rotations, "B-St-FS-sd-r0.5", 1, 0,
        fpx_with_r_ok: check_rotations, "B-St-FPx-sd-r0.5", 0, 0,
        fpx_with_r_slash_err: check_rotations, "B-St-FPx-sd-r0.5/", 1, 0,
        fs_with_r_slash_ok: check_rotations, "B-St-FS-sd-r0.5/", 0, 0,
        fs_with_r_plus_err: check_rotations, "B-St-FS-sd-r0.5+", 1, 0,
        fpx_with_r_plus_ok: check_rotations, "B-St-FPx-sd-r0.5+", 0, 0,
        fpx_with_rl_err: check_rotations, "B-St-FPx-sd-r0.5L", 1, 0,
        fpx_with_rl_err2: check_rotations, "B-St-FPx-sd-r/L", 1, 0,
        lih_with_rl_ok: check_rotations, "B-St-LiH-sd-r/L", 0, 0,
        sth_with_cr_err: check_rotations, "C-Thr>StH-Forw-ln-Cr0.5", 1, 0,
        st_with_cr_ok: check_rotations, "C-Thr>St-Forw-ln-Cr0.5", 0, 0,
        st_with_cr_bang_err: check_rotations, "C-Thr>St-Forw-ln-Cr0.5!", 1, 0,
        sth_with_cr_bang_ok: check_rotations, "C-Thr>StH-Forw-ln-Cr0.5!", 0, 0,
        two_f_with_crl_err: check_rotations, "C-Thr^2F-Back-tk-Cr0.5L-Cs1", 1, 0,
        lh_with_crl_ok: check_rotations, "C-Thr^Lh-Back-tk-Cr0.5L-Cs1", 0, 0,
        pair_with_cp_err: check_rotations, "C-Thr>Pair-Forw-ln-CP0.5", 1, 0,
        ff_with_cp_ok: check_rotations, "C-Thr>FF-Forw-ln-CP0.5", 0, 0,
        lh_with_2f_err: check_rotations, "C-Thr^Lh-Back-tk-2F0.5-Cs1", 1, 0,
        two_f_with_2f_ok: check_rotations, "C-Thr^2F-Back-tk-2F0.5-Cs1", 0, 0,
        sia_with_pr_err: check_rotations, "P-P-SiA-mo-Pr0.5", 1, 0,
        f2a_with_pr_ok: check_rotations, "P-P-F2A-ln-Pr", 0, 0,
        f2a_with_pr_slash_err: check_rotations,"P-P-F2A-ln-Pr0.5/", 1, 0,
        sia_with_pr_slash_ok: check_rotations, "P-P-SiA-mo-Pr0.5/", 0, 0,
        p_with_ph_err: check_rotations, "P-P-F2A-ln-P0.5h", 1, 0,
        hand_with_ph_ok: check_rotations, "P-Hand-F2A-ln-P0.5h", 0, 0,
        p_with_pr_slash_slash_err: check_rotations, "P-P-SiA-mo-Pr//", 1, 0,
        flower_with_pr_slash_slash_ok: check_rotations, "P-Flower-SiA-mo-Pr//", 0, 0,
        only_ln_with_open_warn: check_rotations, "A-Sq-Back-ln-s1.5t0.5o", 0, 1,
        ln_pk_with_open_ok: check_rotations, "A-Sq-Back-pk/2ln-s1.5t0.5o", 0, 0,
        ss_without_ln_warn: check_rotations, "A-Sq-Back-pk-ss1", 0, 1,
        ss_with_ln_ok: check_rotations, "A-Sq-Back-pk/2ln-ss1", 0, 0,
        sp_with_split_err: check_bonuses, "A-2Sup-Up-sp-Split", 1, 0,
        box_with_porp_err: check_bonuses, "P-Knees-4p-Bo-Porp", 1, 0,
        no_conn_with_c: check_bonuses, "A-Thr-Side-ln-c", 0, 1,
        no_conn_with_c_ok: check_bonuses, "A-Thr-Side-ln-c-Catch", 0, 0,
        no_conn_with_c_ok2: check_bonuses, "A-Thr-Side-ln-c-Dbl/Pos3", 0, 0,
        three_bonuses: check_bonuses, "B-St-FS-ln/2he-Hold/Mov/Dbl", 1, 0,
        dup_bonuses: check_bonuses, "B-LH-Le-mo-Mov/Mov", 1, 0,
        dup_bonuses_ok: check_bonuses, "C-Thr>FF-Forw-ln-CRoll/CRoll", 0, 0,
        mut_excl_bonuses: check_bonuses, "P-Hand-3pA-ne-Spider/Climb", 1, 0,
        non_mut_excl_bonuses_ok: check_bonuses, "P-Hand-3pA-ne-Climb/Fall", 0, 0,
        st_bad_connection: check_construction, "B-St>-FS-sd", 0, 1,
        st_good_connection: check_construction, "B-St>-F1S-he", 0, 0,
        non_st_bad_connection: check_construction, "B-St-FS-sd", 0, 0,
        one_leg_conn_2_leg_pos: check_connection, "B-St-FS-he", 0, 1,
        two_leg_conn_2_leg_pos: check_connection, "B-St-FS-sd", 0, 0,
        head_down_conn_head_up_pos: check_connection, "B-St-Bp-sd", 0, 1,
        head_down_conn_head_down_pos: check_connection, "B-St-PP-bb", 0, 0,
        head_up_conn_head_down_pos: check_connection, "B-St-FF-bb", 0, 1,
        head_up_conn_head_up_pos: check_connection, "B-St-FF-sd", 0, 0,
        sit_conn_head_up_pos: check_connection, "B-St-S+-sd", 0, 1,
        sit_conn_head_sit_pos: check_connection, "B-St-S+-mo", 0, 0,
        handstand_conn_without_bb: check_connection, "B-St-PP-ow", 0, 1,
        handstand_conn_with_bb: check_connection, "B-St-PP-bb/2ow", 0, 0,
        one_leg_pos_two_leg_conn: check_positions, "B-St-FS-he", 0, 1,
        one_leg_pos_one_leg_conn: check_positions, "B-St-F1S-he", 0, 0,
        fly_above_airborne_first: check_positions, "C-Thr^2F-Back-tk/2ow", 0, 1,
        fly_above_balance_first: check_positions, "C-Thr^2F-Back-ow/2tk", 0, 0,
    }

    #[test]
    fn test_check_duplicate_pair_acros() {
        let same_acros = check_duplicate_pair_acros(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .pair_acros(&[&"W!fr1", &"W!fr1"])
                .card,
        );
        assert_eq!(same_acros.errors.len(), 1);
        let different_acros = check_duplicate_pair_acros(
            &CardBuilder::new()
                .category(Category { ag: AG12U, event: Duet, free: true })
                .pair_acros(&[&"W!fr0.5", &"W!fr1"])
                .card,
        );
        assert_eq!(different_acros.errors.len(), 0);
    }

    #[test]
    fn test_run_checks() {
        let duet_card = CardBuilder::new()
            .category(Category { ag: AG12U, event: Duet, free: true })
            .pair_acros(&["W!fr1", "J"])
            .card;
        let ret = run_acro_checks(&duet_card);
        assert_eq!(ret.errors.len(), 0);
        assert_eq!(ret.warnings.len(), 0);

        let solo_card =
            CardBuilder::new().category(Category { ag: AG12U, event: Solo, free: true }).card;
        let ret = run_acro_checks(&solo_card);
        assert_eq!(ret.errors.len(), 0);
        assert_eq!(ret.warnings.len(), 0);
    }
}
