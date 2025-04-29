use crate::AcroDirection::{Backwards, Forwards, Sideways, Upwards};
use crate::AcroGroup::{Airborne, Balance, Combined, Platform};
use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::ElementKind::{PairAcro, TeamAcro};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::{AcroGroup, AgeGroups, CardIssues, Category, CoachCard, Events, TeamAcrobatic};
use regex_lite::Regex;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

macro_rules! pair_acros {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            PairAcro(d) => Some((e.number, d)),
            _ => None,
        })
    };
}

macro_rules! team_acros {
    ($elements:expr) => {
        $elements.iter().filter_map(|e| match &e.kind {
            TeamAcro(d, dd) => Some((e.number, d, dd)),
            _ => None,
        })
    };
}

fn check_dd_limits(category: Category, group: AcroGroup, dd: &str) -> CardIssues {
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
    if let Some(max_dd) = map.get(&Cg { c: category, g: group }) {
        if dd.parse().unwrap_or(0.0) > *max_dd {
            ci.errors
                .push(format!("{category} may not have an acrobatic that has a DD > {max_dd}"));
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
            ci.errors.push(format!(
                "Element {num}: cannot repeat acrobatics in {:?}",
                card.category.event
            ));
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

    let positions = |a: &TeamAcrobatic| {
        a.positions.iter().map(|p| p.strip_prefix('2').unwrap_or(p).to_string()).collect()
    };
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

fn check_num_athletes(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    if acro.bonuses.contains(&"Dbl".to_string()) {
        ci.warnings.push("requires 8 or more athletes".into());
    }
    ci
}

fn check_team_acro_validity(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    if acro.construction.is_empty()
        || (acro.connection_grip.is_empty() && acro.direction.is_none())
        || acro.positions.is_empty()
        || acro.positions.len() > 2
    {
        ci.errors.push("is missing one or more parts".into());
    } else if !acro.positions.is_empty() && acro.positions[0].starts_with('2') {
        ci.errors.push(format!(
            "has second position, {}, but missing first position",
            acro.positions[0]
        ));
    } else if acro.positions.len() == 2 {
        if acro.positions[0] == acro.positions[1].strip_prefix('2').unwrap_or(&acro.positions[1]) {
            ci.errors.push(format!(
                "first position, {}, and second position, {} are the same",
                acro.positions[0], acro.positions[1]
            ));
        } else if !acro.positions[1].starts_with('2') {
            ci.errors
                .push(format!("second position, {}, does not start with '2'", acro.positions[1]));
        }
    }
    if acro.bonuses.contains(&"Pos3".into()) && acro.positions.len() < 2 {
        ci.errors.push(format!(
            "3rd position bonus declared, but only {} position(s)",
            acro.positions.len()
        ));
    }

    ci
}

fn check_direction(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    for rotation in &acro.rotations {
        if rotation.starts_with('c') && acro.direction != Some(Sideways) {
            ci.errors.push("Direction should always be Sideways for cartwheels".into());
        }
        if rotation.starts_with('h')
            && (acro.direction != Some(Forwards) && acro.direction != Some(Backwards))
        {
            ci.errors
                .push("Direction should always be Forwards or Backwards for handsprings".into());
        }
    }

    let somersaults_only = Regex::new(r"^(d|s1|s1.5|s2|s2.5|s3)$").unwrap();
    if acro.direction == Some(Upwards)
        && somersaults_only.is_match(acro.rotations.first().unwrap_or(&String::new()))
    {
        ci.warnings
            .push("Up declared with somersault, should this be Forward or Backwards?".into());
    }

    ci
}

fn check_rotations(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    if acro.rotations.iter().any(|rotation| rotation.contains('o'))
        && !acro.positions.contains(&"2ln".into())
        && !acro.bonuses.contains(&"Pos3".into())
    {
        ci.warnings.push(
            "Somersault with open declared, but 2ln or Pos3 not declared, is this right?".into(),
        );
    }

    if acro.rotations.iter().any(|rotation| rotation.contains("ss"))
        && !acro.positions.contains(&"ln".into())
        && !acro.positions.contains(&"2ln".into())
    {
        ci.warnings.push(
            "Straight somersault declared, but no line position declared, is this right?".into(),
        );
    }

    let two_sup_constructions: &[&str] = &["2SupU", "2SupD", "2SupM", "2SupD2F"];
    let reg_r_rotations = Regex::new(r"^r(0.5|1|1.5)$").unwrap();
    let rotation_map: &[(Regex, &[&str])] = &[
        (
            reg_r_rotations.clone(),
            &["FPx", "FP", "SiSb", "Bp", "E", "AP", "SiS", "F1S", "Tw", "S+", "1F1P", "1F1F"],
        ),
        (Regex::new(r"^r(0.5|1|1.5)/$").unwrap(), &["FS"]),
        (Regex::new(r"^r(0.5|1|1.5|2)\+$").unwrap(), &["F1S", "FPx", "FP", "1F1P", "1F1F"]),
        (
            // E isn't listed, but meets the requirements in on page
            //451, who knows if that is intended or not
            Regex::new(r"^r(0.5|1|1.5|2)!$").unwrap(),
            &[
                "1P1P", "1P1F", "Px1P", "PP", "PF", "PH/", "FF", "FF/", "ShF", "LayF", "SiF", "S+",
                "1F1F", "E",
            ],
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

    let unlikely_twist_pos =
        &[vec!["tk".to_string()], vec!["pk".to_string()], vec!["rg".to_string()]];
    let just_twist_regex = Regex::new(r"^t(0.5|1|1.5|2|2.5|3)$").unwrap();

    let construction_or_connection = match acro.group {
        Airborne | Combined => &acro.construction,
        Balance | Platform => &acro.connection_grip,
    };

    for rotation in &acro.rotations {
        for (rx, connections) in rotation_map {
            // page 450 mentions 2Sup constructions can be used with
            // r rotations just to add an exception about group b
            // rotations only needing to match against connections
            // so we have to special case this check
            //
            // I don't have a good way to compare regex objects so
            // we'll just run the match again
            if reg_r_rotations.is_match(rotation)
                && two_sup_constructions.contains(&acro.construction.as_str())
            {
                continue;
            }

            if rx.is_match(rotation) {
                if !connections.contains(&construction_or_connection.as_str()) {
                    ci.errors.push(format!(
                        "{rotation} can only be used with {}",
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
                ci.errors.push(format!("{rotation} can only be used with Hand"));
            }
            if Regex::new(r"^P(r|r0.5|r1)//$").unwrap().is_match(rotation)
                && acro.construction != "2S"
                && acro.construction != "Flower"
            {
                ci.errors.push(format!("{rotation} can only be used with 2S, Flower"));
            }
        }

        if just_twist_regex.is_match(rotation) && unlikely_twist_pos.contains(&acro.positions) {
            ci.warnings.push(format!(
                "twist declared, but {} is usually performed as a somersault",
                acro.positions[0]
            ));
        }
    }
    ci
}

fn check_bonuses(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    let first_pos = acro.positions.first().map_or("", |v| v);
    let second_pos = acro.positions.get(1).map_or("", |v| v.strip_prefix('2').unwrap_or(v));
    if acro.bonuses.contains(&"Split".into()) && first_pos == "sp" {
        ci.errors.push(
            "for Split bonus, sp is considered a take-off and should not be declared first".into(),
        );
    }

    if acro.bonuses.contains(&"Porp".into()) && (first_pos == "Bo" || first_pos == "bo") {
        ci.errors.push("Box cannot be claimed as Position 1 with Porp Bonus".into());
    }

    match acro.bonuses.len().cmp(&2) {
        Ordering::Less => {
            for rotation in &acro.rotations {
                // in here assuming that if they have 2 they don't want to claim Conn
                if (rotation.starts_with('c') || rotation.starts_with('h'))
                    && !acro.bonuses.contains(&"Conn".into())
                    && !acro.bonuses.contains(&"Grip".into())
                    && !acro.bonuses.contains(&"Catch".into())
                {
                    ci.warnings.push(format!("can claim 'Conn' with {rotation}"));
                }
            }
        }
        Ordering::Equal => {
            let bonus1 = &acro.bonuses[0];
            let bonus2 = &acro.bonuses[1];
            if bonus1 == bonus2 && bonus1 != "CRoll" {
                ci.errors.push("cannot declare the same bonus twice".into());
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
                        "cannot declare {bonus1} and {bonus2} in the same acrobatic"
                    ));
                }
            }
        }
        Ordering::Greater => {
            ci.errors.push(format!(
                "{} bonuses declared, but only 2 bonuses allowed",
                acro.bonuses.len()
            ));
        }
    }

    if acro.bonuses.contains(&"Spider".to_string()) && acro.construction != "2S" {
        ci.errors.push("Spider bonus can only be used with 2S construction".into());
    }

    if (acro.bonuses.contains(&"Jump".into()) || acro.bonuses.contains(&"Jump>".into()))
        && !acro.construction.starts_with("Thr>")
    {
        ci.errors.push("Jump and Jump> can only be used with Thr> constructions".into());
    }

    if acro.bonuses.contains(&"Jump".into())
        && (!acro.rotations.is_empty()
            || (acro.positions.len() == 2 && A_POSITIONS.contains(&second_pos)))
    {
        ci.warnings.push(
            "Jump should only be used when the featured athlete remains on the construction".into(),
        );
    }

    if acro.bonuses.contains(&"SdUp".into())
        && !B_TORSO_DOWN_POSITIONS.contains(&first_pos)
        && !B_TORSO_DOWN_POSITIONS.contains(&second_pos)
    {
        ci.warnings.push("SdUp claimed, but no head/torso down position claimed".into());
    }

    ci
}

fn check_age_restrictions(ag: AgeGroups, acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    // TODO need to handle JR vs SR
    // add raw name to card and attempt to parse JR vs SR here?
    let is_not_sr = ag == AG12U || ag == Youth;
    if acro.bonuses.contains(&"1F>1F".into()) && is_not_sr {
        ci.errors.push("1F>1F is only allowed in Senior routines".into());
    }
    ci
}

fn check_construction(acro: &TeamAcrobatic) -> CardIssues {
    let allowed_grip_map = HashMap::<&str, &[&str]>::from([
        (
            "St",
            &[
                "1P1P", "Px1P", "PP", "FPx", "FP", "SiSb", "Bp", "E", "PH/", "AP", "SiS", "FS",
                "F1S", "Tw", "S+", "1F1P", "1F1F",
            ] as &[&str],
        ),
        ("StH", &["1P1F", "FF", "FF/", "PF", "ShF", "LayF", "SiF", "S+", "1F1F"]),
        ("2SupU", &["Le", "1FH+1FP"]),
        ("2SupD", &["Tow"]),
        ("2SupM", &["Le", "Ch"]),
        ("2SupDF", &["Tow"]),
        ("St>", &["PP", "PF", "Bp", "ShF", "E", "F1S", "LayF"]),
        ("L", &["Li"]),
        ("L2F+", &["Li"]),
        ("LH", &["LiH"]),
        ("Lh2F", &["LiH"]),
    ]);

    let mut ci = CardIssues::default();
    if let Some(allowed_grips) = allowed_grip_map.get(acro.construction.as_str()) {
        if !allowed_grips.contains(&acro.connection_grip.as_str()) {
            ci.warnings.push(format!(
                "{} cannot be used with {}, must be a connection such as {}",
                acro.construction,
                acro.connection_grip,
                allowed_grips.join(", ")
            ));
        }
    }

    if acro.construction == "Sq" && acro.bonuses.contains(&"Dbl".to_string()) {
        ci.warnings.push("Sq with Dbl requires 10 athletes!".into());
    }
    ci
}

const A_POSITIONS: &[&str] = &["tk", "pk", "kt", "ln", "sp", "ja", "rg"];
const B_ONE_LEG_POSITIONS: &[&str] = &["he", "vs", "gl", "ba", "sa", "ne", "ey"];
const B_TWO_LEG_POSITIONS: &[&str] = &["sd"];
const B_SIT_STAND_LAY_POSITONS: &[&str] =
    &["mo", "sh", "spl", "hp", "sc", "co", "fl", "so", "tu", "pi"];
const B_HEAD_DOWN_POSITIONS: &[&str] = &["bb", "bo", "wi", "ow"];
const B_EXTREME_FLEX_POSITIONS: &[&str] = &["dr", "qu"];

const B_TORSO_DOWN_POSITIONS: &[&str] = &["bb", "bo", "wi", "ow", "dr", "qu", "sh", "mo", "ne"];

fn check_connection(acro: &TeamAcrobatic) -> CardIssues {
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
                ci.warnings
                    .push(format!("expected two foot position with FS, but found {first_pos}"));
            }
        }
        "1P1P" | "1P1F" | "Px1P" | "PP" | "PF" | "Bp" | "ShF" | "E" | "PH/" | "Tw" => {
            // monkey is a free position so it can be head down
            if !B_HEAD_DOWN_POSITIONS.contains(&first_pos) && first_pos != "mo" {
                ci.warnings.push(format!(
                    "expected head-down position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "FPx" | "FP" | "FF" | "FF/" | "F1S" | "SiF" | "1FH+1FP" | "1F1P" | "1F1F" => {
            if !head_up_positions.contains(&first_pos) {
                ci.warnings.push(format!(
                    "expected head-up position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "LayF" | "S+" => {
            // it is ambiguous if owl should be used or split should be
            // used if the athlete is in splits head down, so allow it
            // end though LayF is Sit/Stand/Lay
            if !B_SIT_STAND_LAY_POSITONS.contains(&first_pos) && first_pos != "ow" {
                ci.warnings.push(format!(
                    "expected sit, stand, or lay position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        _ => {}
    }

    if HANDSTAND_CONNECTIONS.contains(&acro.connection_grip.as_str()) && first_pos != "bb" {
        ci.warnings.push("in handstand positions, the first position should be bb unless the featured swimmer goes directly to Position 1 from underwater".into());
    }
    ci
}

fn check_positions(acro: &TeamAcrobatic) -> CardIssues {
    // FP isn't listed, but nothing says your hands can't be close
    // together for FP, and the DD is the same, so while they probably
    // should use FPx, they probably aren't required to
    const ONE_LEG_CONNECTIONS: &[&str] = &[
        "FPx", "F1S", "1F1P", "1F1F", "FAb", "3pA", "1FA", "3pb", "FA+PF", "SP+L", ">F1P", "3pBb",
        "3pB+b", "1Fxs/", "FA+PF", "FP",
    ];
    let mut ci = CardIssues::default();

    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    match first_pos {
        "he" | "vs" | "gl" | "ba" | "sa" | "ne" | "ey" | "qu" => {
            if !acro.connection_grip.is_empty()
                && !ONE_LEG_CONNECTIONS.contains(&acro.connection_grip.as_str())
            {
                ci.warnings.push(format!(
                    "one leg position, {first_pos}, declared, but {} is not a one leg connection",
                    acro.connection_grip
                ));
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
        ci.warnings.push(format!("In fly-above acrobatics, the first featured swimmer is probably the balance, but position 1 is {first_pos}"));
    }
    if acro.construction == "Thr^2F" {
        if first_pos == "spl" {
            ci.warnings.push(
                "split position in fly-above construction, should that be owl position?".into(),
            );
        }
        if acro.positions.len() == 1 {
            ci.warnings.push(
                "flyover should have a balance position followed by an airborne position".into(),
            );
        }
    }

    let valid_positions = match acro.group {
        Airborne => A_POSITIONS,
        Balance | Platform => &all_b_positions,
        Combined => &[A_POSITIONS, &all_b_positions].concat(),
    };
    for position in &acro.positions {
        if !valid_positions.contains(&position.strip_prefix('2').unwrap_or(position)) {
            ci.errors.push(format!(
                "{position} is not a valid position for {:?} acrobatics",
                acro.group
            ));
        }
    }

    if (acro.group == Balance || acro.group == Combined) && acro.positions.len() == 2 {
        let pos2 = acro.positions[1].strip_prefix('2').unwrap_or("");
        let pos1_head_up =
            B_ONE_LEG_POSITIONS.contains(&first_pos) || B_TWO_LEG_POSITIONS.contains(&first_pos);
        let pos1_head_down = B_HEAD_DOWN_POSITIONS.contains(&first_pos);
        // sit, stand, lay has some things that could be "head down" but
        // I'm not sure any that could be combined with head up positions
        // we'll see if anyone complains
        let pos2_head_up = B_ONE_LEG_POSITIONS.contains(&pos2)
            || B_TWO_LEG_POSITIONS.contains(&pos2)
            || B_SIT_STAND_LAY_POSITONS.contains(&pos2);
        let pos2_head_down = B_HEAD_DOWN_POSITIONS.contains(&pos2);
        if pos1_head_up && pos2_head_down {
            ci.warnings
                .push(format!("{first_pos} is heads-up and {pos2} is heads-down, is this right?"));
        }
        if pos1_head_down && pos2_head_up && !acro.bonuses.contains(&"SdUp".into()) {
            ci.warnings
                .push(format!("{first_pos} is heads-down and {pos2} is heads-up, is this right?"));
        }
    }

    ci
}

fn check_pair_acro_common_base_marks(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();
    if card.category.event != Duet && card.category.event != MixedDuet {
        return ci;
    }

    for (num, acro) in pair_acros!(card.elements) {
        if acro == "J" || acro == "Jf" {
            // these can crash on the surface even though they don't end
            // '»', so we won't warn on them
            continue;
        }
        if (acro.starts_with('J') || acro.starts_with('W'))
            && !acro.contains("s0.5")
            && !acro.contains("s1")
            && !acro.ends_with('d')
            && !acro.ends_with('»')
        {
            ci.warnings.push(format!(
                "Element {num}: {acro} requires the featured-swimmer must be completely in the AIR (top of the head and toes must be above the surface at the same time)"
            ));
        }
    }
    ci
}

pub fn check_one_acro(category: Category, acro: &TeamAcrobatic, dd: &str) -> CardIssues {
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

    let mut element_ci = CardIssues::default();
    element_ci += check_age_restrictions(category.ag, acro);
    element_ci += check_dd_limits(category, acro.group, dd);
    for check in acros_checks {
        element_ci += check(acro);
    }
    element_ci
}

pub fn run_acro_checks(card: &CoachCard) -> CardIssues {
    let mut ci = CardIssues::default();

    let checks: &[fn(&CoachCard) -> CardIssues] = match card.category.event {
        Solo | Events::Unknown => &[],
        Duet | MixedDuet | Trio => &[check_duplicate_pair_acros, check_pair_acro_common_base_marks],
        Acrobatic | Combo | Team => &[check_groups_for_acro_routine, check_team_duplicate_acros],
    };
    for check in checks {
        ci += check(card);
    }
    if card.category.event.is_team_event() {
        for (num, acro, dd) in team_acros!(card.elements) {
            let mut element_ci = check_one_acro(card.category, acro, dd);
            let prefix = format!("Element {num}: ");
            for err in &mut element_ci.errors {
                err.insert_str(0, &prefix);
            }
            for warn in &mut element_ci.warnings {
                warn.insert_str(0, &prefix);
            }
            ci += element_ci;
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
                let result = $fname(&acro);
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
    }

    macro_rules! dd_tests {
        ($($name:ident: $category:expr, $acro:expr, $dd:expr,)*) => {
        $(
            #[test]
            fn $name () {
                let expected = if stringify!($name).ends_with("_ok") { 0 } else { 1 };
                let result = check_dd_limits($category, TeamAcrobatic::from($acro).unwrap().group, $dd);
                assert_eq!(result.errors.len(), expected);
            }
        )*
        }
    }

    dd_tests! {
        tech_limit: Category { ag: JRSR, event: Team, free: false }, "A-Shou-Back-tk-s1", "3.05",
        free_no_limit_ok: Category { ag: JRSR, event: Team, free: true }, "A-Shou-Back-tk-s1", "3.05",
        ag12u_ok: Category{ag: AG12U, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", "2.75",
        too_high_12u: Category{ag: AG12U, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", "2.85",
        youth_ok: Category{ag: Youth, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", "2.85",
        youth_too_high: Category{ag: Youth, event: Team, free: true}, "P-P-HA-bb/2wi-Porp/Trav", "3.05",
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
        bad_2nd_pos: check_team_acro_validity, "A-Sq-Back-pk/3rg", 1, 0,
        back_with_cart_err: check_direction, "A-Sq-Back-ln-ct0.5", 1, 0,
        side_with_cart_ok: check_direction, "A-Sq-Side-ln-ct0.5", 0, 0,
        side_with_hand_err: check_direction, "A-Sq-Side-ln-hd", 1, 0,
        forw_with_hand_ok: check_direction, "A-Sq-Forw-ln-hd", 0, 0,
        up_with_dive_warn: check_direction, "A-Sq-Up-ln-d", 0, 1,
        up_with_dive_twist_ok: check_direction, "A-Sq-Up-ln-dt1", 0, 0,
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
        bad_c_mutliple_rotations: check_rotations, "C-Thr^2F-Bln-ow/2tk-Cr1!+Cs1-Pos3", 1, 0,
        good_c_mutliple_rotations: check_rotations, "C-Thr^2F-Bln-ow/2tk-2F1+Cs1-Pos3", 0, 0,
        tuck_just_twist_warn: check_rotations, "A-Sq-Back-tk-t1", 0, 1,
        tuck_with_jay_twist_ok: check_rotations, "A-Sq-Back-tk/2ja-t1", 0, 0,
        line_with_twist_ok: check_rotations, "A-Sq-Up-ln-t1", 0, 0,
        st_trans_e_r_bang_ok: check_rotations, "B-St>-E-bo/2ow-r0.5!-Pos3", 0, 0,
        supu_le_r_ok: check_rotations, "B-2SupU-Le-bb/2ow-r0.5", 0, 0,
        fp_one_leg_r_ok: check_rotations, "B-St-FP-he/2ba-r0.5", 0, 0,
        sp_with_split_err: check_bonuses, "A-2Sup-Up-sp-Split", 1, 0,
        box_with_porp_err: check_bonuses, "P-Knees-4p-Bo-Porp", 1, 0,
        no_conn_with_c: check_bonuses, "A-Thr-Side-ln-c", 0, 1,
        no_conn_with_c_ok: check_bonuses, "A-Thr-Side-ln-c-Catch", 0, 0,
        no_conn_with_c_ok2: check_bonuses, "A-Thr-Side-ln-c-Dbl/Pos3", 0, 0,
        three_bonuses: check_bonuses, "B-St-FS-ln/2he-Hold/Mov/Dbl", 1, 0,
        dup_bonuses: check_bonuses, "B-LH-Le-mo-Mov/Mov", 1, 0,
        dup_bonuses_ok: check_bonuses, "C-Thr>FF-Forw-ln-CRoll/CRoll", 0, 0,
        mut_excl_bonuses: check_bonuses, "P-2S-3pA-ne-Spider/Climb", 1, 0,
        non_mut_excl_bonuses_ok: check_bonuses, "P-Hand-3pA-ne-Climb/Fall", 0, 0,
        spider_with_flower_err: check_bonuses, "P-Flower-4pAb-ne-Spider", 1, 0,
        spider_with_2s_ok: check_bonuses, "P-2S-4pAb-ne-Spider", 0, 0,
        flyover_with_jump_transit_err: check_bonuses, "C-Thr^St-Forw-ow/2ln-Jump>", 1, 0,
        jump_with_2nd_pos_airborne: check_bonuses, "C-Thr>St-Forw-ow/2ln-Jump", 0, 1,
        jump_with_rotation: check_bonuses, "C-Thr>St-Forw-sd-Cd-Jump", 0, 1,
        jump_transit_with_2nd_pos_airborne_ok: check_bonuses, "C-Thr>St-Forw-ow/2ln-Jump>", 0, 0,
        sdup_with_no_head_down_pos: check_bonuses, "B-St-F1S-he/2sa-SdUp", 0, 1,
        sdup_with_head_down_pos: check_bonuses, "B-St-F1S-he/2ne-SdUp", 0, 0,
        st_bad_connection: check_construction, "B-St>-FS-sd", 0, 1,
        st_good_connection: check_construction, "B-St>-F1S-he", 0, 0,
        sq_with_dbl: check_construction, "A-Sq-Back-tk-Dbl", 0, 1,
        thr_with_dbl_ok: check_construction, "A-Thr-Back-tk-Dbl", 0, 0,
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
        invalid_airborne_position: check_positions, "A-Sq-Forw-ar", 1, 0,
        one_leg_pos_two_leg_conn: check_positions, "B-St-FS-he", 0, 1,
        one_leg_pos_one_leg_conn: check_positions, "B-St-F1S-he", 0, 0,
        fly_above_airborne_first: check_positions, "C-Thr^2F-Back-tk/2ow", 0, 1,
        fly_above_balance_first: check_positions, "C-Thr^2F-Back-ow/2tk", 0, 0,
        fly_above_with_spl: check_positions, "C-Thr^2F-Back-spl/2tk", 0, 1,
        fly_above_just_balance: check_positions, "C-Thr^2F-Back-ow", 0, 1,
        head_up_with_head_down: check_positions, "B-St-FS-sd/2bb", 0, 1,
        head_up_with_head_up: check_positions, "C-Thr>StH-Forw-sd/2he", 0, 0,
        head_down_with_head_up: check_positions, "C-Thr>StH-Forw-bb/2spl", 0, 1,
        head_down_with_head_down: check_positions, "C-Thr>StH-Forw-bb/2ow", 0, 0,
        c_does_not_warn: check_positions, "C-Thr>StH-Forw-vs/2gl-1F>1F", 0, 0,
    }

    #[test]
    fn test_check_age_restrictions() {
        let acro = TeamAcrobatic::from("C-Thr>StH-Forw-ln-1F>1F").unwrap();
        assert_eq!(check_age_restrictions(AG12U, &acro).errors.len(), 1);
        assert_eq!(check_age_restrictions(Youth, &acro).errors.len(), 1);
        assert_eq!(check_age_restrictions(JRSR, &acro).errors.len(), 0);
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

    fn pair_acro_helper(event: Events, acro: &str) -> Vec<String> {
        let card = CardBuilder::new()
            .category(Category { ag: AG12U, event, free: true })
            .pair_acros(&[acro])
            .card;
        check_pair_acro_common_base_marks(&card).warnings
    }
    #[test]
    fn test_check_pair_acro_common_base_marks() {
        assert_eq!(pair_acro_helper(Duet, "Jr0.5").len(), 1);
        assert_eq!(pair_acro_helper(Trio, "Jr0.5").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "W!fr1").len(), 1);
        assert_eq!(pair_acro_helper(Trio, "W!fr1").len(), 0);

        assert_eq!(pair_acro_helper(Duet, "W!s0.5").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "J").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "Jd").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "Jf").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "W!»").len(), 0);
        assert_eq!(pair_acro_helper(Duet, "Jfs1B").len(), 0);
    }

    #[test]
    fn test_run_checks() {
        let duet_card = CardBuilder::new()
            .category(Category { ag: AG12U, event: Duet, free: true })
            .pair_acros(&["W!s1F", "J"])
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
