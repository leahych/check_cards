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
    let bonuses = |acro: &TeamAcrobatic| acro.bonuses.clone();

    let mut ci = check_duplicate_elements(group_a.iter(), positions);
    ci += check_duplicate_elements(group_b.iter(), constructions);
    ci += check_duplicate_elements(group_b.iter(), connections);
    ci += check_duplicate_elements(group_c.iter(), constructions);
    ci += check_duplicate_elements(group_p.iter(), constructions);
    ci += check_duplicate_elements(group_p.iter(), connections);
    ci += check_duplicate_elements(group_p.iter(), positions);
    ci += check_duplicate_elements(group_p.iter(), bonuses);
    ci
}

fn check_num_athletes(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();
    if !acro.bonuses.contains(&"Dbl".to_string()) {
        return ci;
    }

    // LH probably needs/usually is done with 5+, and 6+ for Lh2F, but not required
    // it isn't clear that 2Sup/2SupH requires 5
    let req5 = ["Sq", "2SupU", "2SupD", "2SupM", "St>", "2S", "Flower", "Thr>Pair>", "Thr^LH"];
    let req6 = ["2SuD2F", "2Sup+", "Thr>St2"];
    if req6.contains(&acro.construction.as_str()) {
        ci.errors.push(format!("{} with Dbl requires 12 athletes!", acro.construction));
    } else if req5.contains(&acro.construction.as_str()) {
        ci.warnings.push(format!("{} with Dbl requires 10 athletes!", acro.construction));
    } else {
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

    if acro.direction != Some(Upwards) {
        if acro.construction == "2Sup+" {
            ci.errors.push("Direction should be Up for 2Sup+".into());
        }
        if acro.bonuses.contains(&"Hula".into()) {
            ci.errors.push("Direction should be Up for Hula".into());
        }
        if acro.bonuses.contains(&"Turn".into()) {
            ci.errors.push("Direction should be Up for Turn".into());
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

fn check_rotation_construction(acro: &TeamAcrobatic) -> CardIssues {
    const R_SLASH: &[&str] = &["r0.5/", "r1/", "r1.5/"];
    const R_PLAIN: &[&str] = &["r0.5", "r1", "r1.5"];
    const R_PLUS: &[&str] = &["r0.5+", "r1+", "r1.5+", "r2+"];
    const R_BANG: &[&str] = &["r0.5!", "r1!", "r1.5!", "r2!"];
    const R_PLAIN_AND_PLUS: &[&str] = &["r0.5", "r1", "r1.5", "r0.5+", "r1+", "r1.5+", "r2+"];
    const R_PLAIN_AND_BANG: &[&str] = &["r0.5", "r1", "r1.5", "r0.5!", "r1!", "r1.5!", "r2!"];
    const R_L: &[&str] = &["r/L", "r0.5L", "r1L"];
    const P_R: &[&str] = &["Pr", "Pr0.5", "Pr1", "Pr1.5"];
    const P_H: &[&str] = &["Ph", "P0.5h", "P1h", "P1.5h"];
    const P_2S: &[&str] = &["P2S", "P2Sr0.5", "P2Sr1"];
    const P_DB: &[&str] = &["PDB", "PDB0.5", "PDB1"];
    const C_BANG: &[&str] = &["Cr0.5!", "Cr1!", "Cr1.5!"];
    const C_R_AND_BANG: &[&str] = &["Cr0.5", "Cr1", "Cr1.5", "Cr0.5!", "Cr1!", "Cr1.5!"];
    const C_L: &[&str] = &["Cr0.5L"];
    const C_P: &[&str] = &["CP0.5"];
    const C_2F: &[&str] = &["2F0.5", "2F1"];
    const C_FEATURED_ROTATIONS: &[&str] = &[
        "Ct0.5", "Ct1", "Ct1.5", "Ct2", "Ct2.5", "Ct3", "Cd", "Cdt0.5", "Cdt1", "Cdt1.5", "Cs1",
        "Css1", "Cs1.5", "Cs1.5o", "Cf1", "Cf1.5", "Cc", "Cct0.5", "Cct1", "Ch", "Cht0.5", "Cht1",
        "Cs1t0.5", "Cs1t1", "Cs1t1.5", "Cs1t2", "Css1t0.5", "Css1t1", "Css1t1.5", "Css1t2",
        "Cs1t1o", "Cs1t1.5o", "Cs1t2o", "Chs0.5",
    ];
    static HM: std::sync::OnceLock<HashMap<&str, &[&str]>> = std::sync::OnceLock::new();

    let mut ci = CardIssues::default();
    let con = match acro.group {
        Airborne => return ci,
        Combined | Platform => &acro.construction,
        Balance => &acro.connection_grip,
    };

    let rotation_map = HM.get_or_init(|| {
        HashMap::from([
            ("FS", R_SLASH),
            ("FP", R_PLAIN_AND_PLUS),
            ("SiSb", R_PLAIN),
            ("Bp", R_PLAIN),
            ("E", R_PLAIN),
            ("AP", R_PLAIN),
            ("SiS", R_PLAIN),
            ("F1S", R_PLAIN_AND_PLUS),
            ("Tw", R_PLAIN),
            ("1F1P", R_PLAIN_AND_PLUS),
            ("S+", R_PLAIN_AND_BANG),
            ("PP", R_PLAIN_AND_BANG),
            ("1F1F", R_PLAIN_AND_BANG),
            ("F1F", R_PLUS),
            ("1P1P", R_BANG),
            ("1P1F", R_BANG),
            ("1PPx", R_BANG),
            ("PF", R_BANG),
            ("PH/", R_BANG),
            ("PP2", R_BANG),
            ("2PH", R_BANG),
            ("1PH", R_BANG),
            ("FF", R_BANG),
            ("FF/", R_BANG),
            ("ShF", R_BANG),
            ("LayF", R_BANG),
            ("SiF", R_BANG),
            ("H1F/", R_BANG),
            ("HT+", R_BANG),
            ("LiH", R_L),
            ("Li", R_L),
            ("P", P_R),
            ("Box", P_R),
            ("Knees", P_R),
            ("B", P_R),
            ("Chariot", P_R),
            ("Hand", P_H),
            ("2S", P_2S),
            ("Flower", P_2S),
            ("DB", P_DB),
            ("Thr>St", C_R_AND_BANG),
            ("Thr>St2", C_R_AND_BANG),
            ("Thr>StH", C_BANG),
            ("Thr>StH>1F", C_BANG),
            ("Thr^Lh", C_L),
            ("Thr>F", C_P),
            ("Thr>FF", C_P),
            ("Thr>hand", C_P),
            ("Thr^2F", C_2F),
        ])
    });

    let allowed_rotations = rotation_map.get(con.as_str());
    for rotation in &acro.rotations {
        // Group C limits rotations of the base, but rotation of the
        // featured swimmer does not depend on the base.
        if acro.group == Combined && C_FEATURED_ROTATIONS.contains(&rotation.as_str()) {
            continue;
        }

        // need to check here because we don't want to return an error
        // when a group C acro doesn't allow rotation of the base but
        // does allow for rotation of the featured athlete.
        // TODO this check is more complicated since technically for
        // Group C we should only allow them to claim one rotation of
        // the base and one of the feature swimmer. ISS does not let
        // you mis-code this, so this is a lower priority.
        if let Some(allowed_rotations) = allowed_rotations {
            if !allowed_rotations.contains(&rotation.as_str()) {
                ci.errors.push(format!(
                    "for {con} the rotation must be one {}",
                    allowed_rotations.join(", ")
                ));
            }
        } else {
            ci.errors.push(format!("no rotations of the construction allowed for {con}"));
        }
    }

    ci
}

fn check_rotations(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    match acro.rotations.len().cmp(&2) {
        Ordering::Less => {
            if acro.rotations.is_empty() {
                return ci;
            }
        }
        Ordering::Equal => {
            if acro.group != Combined {
                ci.errors.push("Only one rotation allowed, but two declared".into());
            }
        }
        Ordering::Greater => {
            ci.errors.push(format!(
                "{} rotations declared, but only two rotations allowed",
                acro.rotations.len()
            ));
            return ci;
        }
    }

    if acro.rotations.iter().any(|rotation| rotation.contains('o'))
        && !acro.positions.contains(&"2ln".into())
        && !acro.bonuses.contains(&"Pos3".into())
    {
        ci.warnings.push(
            "Somersault with open declared, but 2ln or Pos3 not declared, is this right?".into(),
        );
    }

    if acro.rotations.iter().any(|rotation| rotation.contains("ss"))
        && (acro.positions.len() > 1 || !acro.positions.contains(&"ln".into()))
    {
        ci.errors.push(
            "Straight somersault can only be declared with one position and that must be ln".into(),
        );
    }

    ci += check_rotation_construction(acro);

    // FUTURE look at cleaning this up
    let unlikely_twist_pos =
        &[vec!["tk".to_string()], vec!["pk".to_string()], vec!["rg".to_string()]];
    let just_twist_regex = Regex::new(r"^t(0.5|1|1.5|2|2.5|3)$").unwrap();

    for rotation in &acro.rotations {
        if just_twist_regex.is_match(rotation) && unlikely_twist_pos.contains(&acro.positions) {
            ci.warnings.push(format!(
                "twist declared, but {} is usually performed as a somersault",
                acro.positions[0]
            ));
        }
    }
    ci
}

fn are_bonuses_exclusive(bonus1: &str, bonus2: &str) -> bool {
    let exclusive_bonuses: &[&[&str]] = &[
        &["Grip", "Conn", "Catch"],
        &["Hula", "RetSq", "RetPa"],
        &["Twirl", "RotF"],
        &["Moon", "Mov", "Hold"],
        &["Jump", "Jump>", "On1Foot", "1F>1F"],
        &["Porp", "Spich"],
        &["Stand", "Diva"],
        &["Spider", "Climb"],
        &[
            "Dive", "CH", "Ps1", "Ps1t0.5", "Ps1op", "Ps1t0,5o", "Ps1t1", "Pf1", "Pf1o", "Mov",
            "Mov1", "Mov1+t", "Fall", "FTurn",
        ],
        &["Ju", "1P>H", "H>1P", "Jump", "Jump>", "On1Foot", "1F>1F", "1F>1F+", "2F>2F"],
    ];
    for exclusive in exclusive_bonuses {
        if exclusive.contains(&bonus1) && exclusive.contains(&bonus2) {
            return true;
        }
    }
    false
}

fn check_bonuses_allowed_constructions(construction: &str, bonuses: &Vec<String>) -> CardIssues {
    const P_ALLOWED: &[&str] = &["2S", "Flower", "Hand"];
    static HM: std::sync::OnceLock<HashMap<&str, &[&str]>> = std::sync::OnceLock::new();

    let mut ci = CardIssues::default();
    let map = HM.get_or_init(|| {
        HashMap::from([
            ("Turn", &["2Sup+"] as &[&str]),
            ("Run", &["Thr>FF", "Thr>F"]),
            ("Ju", &["Thr>FF", "Thr>F", "Thr>hand", "Thr>Sq"]),
            ("Jump", &["Thr>St", "Thr>StH", "Thr>St2"]),
            ("Jump>", &["Thr>St", "Thr>StH", "Thr>FF", "Thr>F", "Thr>hand", "Thr>Sq", "Thr>St2"]),
            ("1F>1F+", &["Thr>StH>1F"]),
            ("2F>2F", &["Thr>StH"]),
            ("Spider", P_ALLOWED),
            ("Climb", P_ALLOWED),
            ("Fall", P_ALLOWED),
            ("FTurn", P_ALLOWED),
        ])
    });
    for bonus in bonuses {
        if let Some(allowed_constructions) = map.get(bonus.as_str())
            && !allowed_constructions.contains(&construction)
        {
            ci.errors.push(format!(
                "{bonus} can only be used with {} constructions",
                allowed_constructions.join(", ")
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
        ci.errors.push("for Split bonus, sp is a take-off and should not be declared first".into());
    }

    if acro.bonuses.contains(&"Porp".into()) && first_pos != "bb" {
        ci.errors.push("For Porp bonus, Position 1 must be bamboo".into());
    }

    if acro.bonuses.contains(&"Spich".into())
        && !((first_pos == "bb" && second_pos == "sh") || (first_pos == "sh" && second_pos == "bb"))
    {
        // warn because technically you could do something before Spich
        // and then you'd start with that position, but that's unlikely
        ci.warnings.push("Spich requires going from bamboo to shrimp or shrimp to bamboo".into());
    }

    if acro.bonuses.contains(&"RetPa".into()) || acro.bonuses.contains(&"RetSq".into()) {
        if acro.bonuses.contains(&"RetPa".into()) && acro.construction != "Shou" {
            ci.errors.push("RetPa probably needs to use Shou construction".into());
        }

        if acro.bonuses.contains(&"RetSq".into()) && acro.construction != "Sq" {
            ci.errors.push("RetSq must use Sq construction".into());
        }

        // this isn't in the wording, but doing anything else seems fishy
        if acro.direction != Some(Upwards) {
            ci.warnings.push("RetPa and RetSq should probably have Up as the direction".into());
        }

        // they don't say Cartwheel/Handspring, but I think that's not
        // going to be a winning argument, so let's ban them as well.
        // That leaves twists as the only option, and since it's group A
        //  there is only one rotation to check
        if !acro.rotations.is_empty() && !acro.rotations[0].starts_with('t') {
            ci.errors.push("Somersaults cannot be used with RetPa or RetSq".into());
        }
    }

    if acro.bonuses.contains(&"Catch".into()) && !acro.bonuses.contains(&"Dbl".into()) {
        ci.warnings
            .push("Catch requires two simultaneous acrobatics, so Dbl should be claimed".into());
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
            } else if are_bonuses_exclusive(bonus1, bonus2) {
                ci.errors
                    .push(format!("cannot declare {bonus1} and {bonus2} in the same acrobatic"));
            }
        }
        Ordering::Greater => {
            ci.errors.push(format!("only 2 bonuses allowed but {} declared", acro.bonuses.len()));
        }
    }

    ci += check_bonuses_allowed_constructions(acro.construction.as_str(), &acro.bonuses);

    if acro.bonuses.contains(&"Jump".into())
        && (!acro.rotations.is_empty()
            || (acro.positions.len() == 2 && A_POSITIONS.contains(&second_pos)))
    {
        ci.warnings.push(
            "Jump should only be used when the featured athlete remains on the construction".into(),
        );
    }

    if acro.bonuses.contains(&"Hold".into()) && !acro.rotations.is_empty() {
        ci.warnings.push("Hold and Rotation must not be simultaneous".into());
    }

    if acro.bonuses.contains(&"Hula".into())
        && !acro.positions.contains(&"rg".into())
        && !acro.positions.contains(&"ja".into())
    {
        ci.errors.push("Hula requires that the featured athlete be in Ring or Jay position".into());
    }

    let torso_down_positions =
        [B_FREE_POSITIONS, B_HORIZONTAL_POSITIONS, B_HEAD_DOWN_POSITIONS, B_EXTREME_FLEX_POSITIONS]
            .concat();
    if acro.bonuses.contains(&"SdUp".into())
        && !torso_down_positions.contains(&first_pos)
        && !torso_down_positions.contains(&second_pos)
    {
        ci.warnings.push("SdUp claimed, but no head/torso down position claimed".into());
    }

    ci
}

fn check_age_restrictions(ag: AgeGroups, acro: &TeamAcrobatic) -> CardIssues {
    const JRSR_ONLY_BONUSES: &[&str] = &["RetSq", "RetPa", "1F>1F", "1F>1F+", "1P>H", "H>1P"];
    let mut ci = CardIssues::default();
    if ag == JRSR {
        return ci;
    }
    for bonus in &acro.bonuses {
        if JRSR_ONLY_BONUSES.contains(&bonus.as_str()) {
            ci.errors.push(format!("{bonus} is only allowed in JR/SR routines"));
        }
    }
    ci
}

fn check_construction(acro: &TeamAcrobatic) -> CardIssues {
    let allowed_grip_map = HashMap::<&str, &[&str]>::from([
        (
            "St",
            &[
                "1P1P", "1PPx", "PP", "FP", "SiSb", "Bp", "E", "PH/", "AP", "SiS", "FS", "F1S",
                "Tw", "S+", "1PH", "1F1P", "1F1F",
            ] as &[&str],
        ),
        ("StH", &["1P1F", "FF", "FF/", "PF", "ShF", "LayF", "SiF", "S+", "1F1F", "H1F/", "HT+"]),
        ("2SupU", &["Le", "1FH+1FP", "PP2"]),
        ("2SupD", &["Tow"]),
        ("2SupM", &["Le", "Ch"]),
        ("2SupD2F", &["Tow"]),
        ("L", &["Li"]),
        ("L2F+", &["Li"]),
        ("St>", &["PP", "PF", "Bp", "ShF", "E", "F1S", "LayF", "1P1F", "2pH", "PH/"]),
        ("LH", &["LiH"]),
        ("Lh2F", &["LiH"]),
        ("P", &["F2A", "SiA", "1FA", "3pA", "HA", ">F1P", "2pA/", "4p", "3pbA", "BA", ">2P2P"]),
        ("Box", &["4p", "3pA", "SiA", "F2A", "HA"]),
        (
            "Knees",
            &["2pK", "3pbA", "3pK", "SP+K", "F2A", "SiA", "3pA", ">F1P", ">2P2P", "BA", "1FA"],
        ),
        (
            "B",
            &[
                "2pBb", ">F1P", "L/Sif+P", "SiF+Pb", "SP+L", "FA+PF", "F2A", "SiA", "HP+L", "FAb",
                "3pA", "1FA", "HA", "3pS",
            ],
        ),
        (
            "DB",
            &[
                "L/Sif+P", "ShF+P", "SiF+Pb", "SP+L", "FA+PF", "3pK", "F2A", "SiA", ">F1P",
                "ShiShi+", "SF+TP", "HP+L", "3pA", "1FA", "4p", "DBB",
            ],
        ),
        ("Chariot", &["2pA/", "4p", "3pbA", "3pA", "FAb", "F2A", "SiA", "1FA", "BA"]),
        (
            "2S",
            &[
                "2b/", "2pBb", "FA+PF", "3pbA", "HA", "3pA", "F2A", "SiA", "1FA", "SP+K", "3pS",
                "ShF+P", "L/Sif+P", "2pA/", "BA",
            ],
        ),
        ("Flower", &["2pA/", "3pbA", "HA", "3pA", "F2A", "SiA", "1FA", "BA"]),
        ("Hand", &["2pA/", "3pbA", "HA", "1FA", "3pA", "F2A", "SiA", "3pbA", "BA"]),
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

    ci
}

const A_POSITIONS: &[&str] = &["tk", "pk", "kt", "ln", "sp", "ja", "rg"];
const B_ONE_LEG_POSITIONS: &[&str] = &["he", "vs", "gl", "ba", "sa", "ne", "ey"];
const B_TWO_LEG_POSITIONS: &[&str] = &["sd"];
// TODO will ISS use PP or pp or should I check for both?
const B_FREE_POSITIONS: &[&str] = &["mo", "PP", "ct", "sh", "hp", "fl", "tu"];
const B_HORIZONTAL_POSITIONS: &[&str] = &["co", "spl", "so", "pi"];
const B_HEAD_DOWN_POSITIONS: &[&str] = &["bb", "bo", "ff", "wi", "br", "ow", "ma"];
const B_EXTREME_FLEX_POSITIONS: &[&str] = &["dr", "qu", "sn"];

fn check_connection(acro: &TeamAcrobatic) -> CardIssues {
    // not a category but positions that can be done by standing w/two feet
    const TWO_FOOT_POSITIONS: &[&str] = &["sd", "mo", "sh", "dr"];
    // a few of these aren't handstands, but have the same sort of movement
    // so they should also probably start in bamboo
    const HANDSTAND_CONNECTIONS: &[&str] = &[
        "1P1P", "1P1F", "1PPx", "PP", "PF", "PH/", "PP2", "1pH", "1PH", "2pH", "2PH", "H1F/",
        "HT+", "ShF", "E",
    ];
    const TWO_SUP_UP_CONSTRUCTIONS: &[&str] = &["2SupU", "2SupM"];

    // similar, anything that could be considered head up
    let head_up_positions =
        [B_ONE_LEG_POSITIONS, B_TWO_LEG_POSITIONS, B_FREE_POSITIONS, B_HORIZONTAL_POSITIONS]
            .concat();

    let laying_positions = [B_FREE_POSITIONS, B_HORIZONTAL_POSITIONS].concat();

    let mut ci = CardIssues::default();
    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    match acro.connection_grip.as_str() {
        "FS" => {
            if !TWO_FOOT_POSITIONS.contains(&first_pos) {
                ci.warnings
                    .push(format!("expected two foot position with FS, but found {first_pos}"));
            }
        }
        "1P1P" | "1P1F" | "1PPx" | "PP" | "PF" | "Bp" | "ShF" | "E" | "PH/" | "Tw" | "1pH"
        | "1PH" | "2pH" | "2PH" | "H1F/" | "HT+" => {
            if !B_HEAD_DOWN_POSITIONS.contains(&first_pos) && !B_FREE_POSITIONS.contains(&first_pos)
            {
                ci.warnings.push(format!(
                    "expected head-down position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "FP" | "FF" | "FF/" | "SiSb" | "F1S" | "SiF" | "1FH+1FP" | "1F1P" | "1F1F" => {
            if !head_up_positions.contains(&first_pos) {
                ci.warnings.push(format!(
                    "expected head-up position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        "LayF" | "S+" => {
            if !laying_positions.contains(&first_pos) {
                ci.warnings.push(format!(
                    "expected sit, stand, or lay position with {}, but found {first_pos}",
                    acro.connection_grip
                ));
            }
        }
        // LiH | AP | SiS | Le | Tow | Li | Ch
        _ => {}
    }

    if HANDSTAND_CONNECTIONS.contains(&acro.connection_grip.as_str()) && first_pos != "bb" {
        ci.warnings.push("in handstand positions, the first position should be bb unless the featured swimmer goes directly to Position 1 from underwater".into());
    }

    if acro.connection_grip == "Le"
        && !TWO_SUP_UP_CONSTRUCTIONS.contains(&acro.construction.as_str())
    {
        ci.warnings.push(format!("Lemur connections require 2 support athletes with at least 1 head-up. A construction of 2SupU or 2SupM should be used instead of {}", acro.construction));
    }

    ci
}

fn check_group_c_positions(acro: &TeamAcrobatic) -> CardIssues {
    let mut ci = CardIssues::default();

    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    let pos2 = acro.positions.get(1).map_or("", |s| s.strip_prefix('2').unwrap_or(""));

    let all_b_positions = [
        B_ONE_LEG_POSITIONS,
        B_TWO_LEG_POSITIONS,
        B_FREE_POSITIONS,
        B_HORIZONTAL_POSITIONS,
        B_HEAD_DOWN_POSITIONS,
        B_EXTREME_FLEX_POSITIONS,
    ]
    .concat();

    if acro.construction.contains('^')
        && (!all_b_positions.contains(&first_pos) || !A_POSITIONS.contains(&pos2))
    {
        ci.errors
            .push("fly-above must have a balance position followed by an airborne position".into());
    }

    if acro.construction == "Thr^Lh" && !(first_pos == "br" || first_pos == "ct") {
        ci.errors.push("For Thr^Lh balance position must be Bridge or Cat".into());
    }

    if acro.construction == "Thr^2F" && first_pos == "spl" {
        ci.warnings
            .push("split position in fly-above construction, should that be owl position?".into());
    }

    for position in &acro.positions {
        const AIRBORNE_CONSTRUCTIONS: &[&str] = &["2Sup+", "Thr+Thr", "Sn"];
        const AIRBORNE_BONUSES: &[&str] = &["Jump>", "Turn"];
        const BALANCE_CONSTRUCTIONS: &[&str] = &["Thr>FF", "Thr>F"];
        const BALANCE_BONUSES: &[&str] = &["Ju", "Jump", "1F>1F", "1F>1F+", "2F>2F"];
        if (AIRBORNE_CONSTRUCTIONS.contains(&acro.construction.as_str())
            || acro.construction.ends_with('>'))
            && all_b_positions.contains(&position.as_str())
        {
            ci.errors.push(format!(
                "{position} declared but {} requires airborne positions",
                acro.construction
            ));
        }

        if BALANCE_CONSTRUCTIONS.contains(&acro.construction.as_str())
            && A_POSITIONS.contains(&position.as_str())
        {
            ci.errors.push(format!(
                "{position} declared but {} requires balance positions",
                acro.construction
            ));
        }

        for bonus in &acro.bonuses {
            if AIRBORNE_BONUSES.contains(&bonus.as_str())
                && all_b_positions.contains(&position.as_str())
            {
                ci.errors
                    .push(format!("{position} declared but {bonus} requires airborne positions"));
            }
            if BALANCE_BONUSES.contains(&bonus.as_str()) && A_POSITIONS.contains(&position.as_str())
            {
                ci.errors
                    .push(format!("{position} declared but {bonus} requires balance positions"));
            }
        }
    }

    ci
}

fn check_positions(acro: &TeamAcrobatic) -> CardIssues {
    const ONE_LEG_CONNECTIONS: &[&str] = &[
        "F1S", "1F1P", "1F1F", "FAb", "3pA", "1FA", "3pb", "FA+PF", "SP+L", ">F1P", "3pBb",
        "3pB+b", "1Fxs/", "FA+PF", "FP",
    ];
    let mut ci = CardIssues::default();

    let first_pos = acro.positions.first().map_or("", |v| v.as_str());
    let pos2 = acro.positions.get(1).map_or("", |s| s.strip_prefix('2').unwrap_or(""));

    if (B_ONE_LEG_POSITIONS.contains(&first_pos) || first_pos == "qu" || first_pos == "sn")
        && !acro.connection_grip.is_empty()
        && !ONE_LEG_CONNECTIONS.contains(&acro.connection_grip.as_str())
    {
        ci.warnings.push(format!(
            "one leg position, {first_pos}, declared but {} is not a one leg connection",
            acro.connection_grip
        ));
    }

    let all_b_positions = [
        B_ONE_LEG_POSITIONS,
        B_TWO_LEG_POSITIONS,
        B_FREE_POSITIONS,
        B_HORIZONTAL_POSITIONS,
        B_HEAD_DOWN_POSITIONS,
        B_EXTREME_FLEX_POSITIONS,
    ]
    .concat();

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

    // above is just checking that the claimed position is one that is
    // allowed at all, now for Group C, we'll do a more complicated
    // check depending on construction, bonuses, etc.
    if acro.group == Combined {
        ci += check_group_c_positions(acro);
    }

    if acro.group == Balance && acro.positions.len() == 2 {
        let pos1_head_up =
            B_ONE_LEG_POSITIONS.contains(&first_pos) || B_TWO_LEG_POSITIONS.contains(&first_pos);
        let pos1_head_down = B_HEAD_DOWN_POSITIONS.contains(&first_pos);
        // sit, stand, lay has some things that could be "head down" but
        // I'm not sure any that could be combined with head up positions
        // we'll see if anyone complains
        // TODO see if this check is still correct
        let pos2_head_up = B_ONE_LEG_POSITIONS.contains(&pos2)
            || B_TWO_LEG_POSITIONS.contains(&pos2)
            || B_FREE_POSITIONS.contains(&pos2)
            || B_HORIZONTAL_POSITIONS.contains(&pos2);
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
    let acro_checks = &[
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
    for check in acro_checks {
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
        repeat_bonus_group_p: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow-Pos3", "P-2S-FA+PF-ne/2ey-Pos3"],
        group_p_no_dups_ok: check_team_duplicate_acros, &["P-Knees-SP+K-bb/2ow-Pos3", "P-2S-FA+PF-ne/2ey-Trav"],
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
        reqs_6_dbl_err: check_num_athletes, "B-2SuD2F-Le-co-Dbl", 1, 0,
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
        two_sup_wrong_dir_err: check_direction, "C-2Sup+-Side-sp", 1, 0,
        turn_wrong_dir_err: check_direction, "C-Thr>Pair>-Side-sp-Turn", 1, 0,
        two_sup_correct_decl_ok: check_direction, "C-2Sup+-Up-sp-Turn", 0, 0,
        hula_with_side_err: check_direction, "A-Shou-Side-rg-Hula", 1, 0,
        hula_with_up_ok: check_direction, "A-Shou-Up-rg-Hula", 0, 0,
        airborne_two_rotations_err: check_rotations, "A-Sq-Side-ln-ct0.5+s1", 1, 0,
        airborne_rotation_ok: check_rotations, "A-Sq-Side-ln-ct0.5", 0, 0,
        combined_three_rotations_err: check_rotations, "C-Thr^2F-Back-ow/2tk-2F0.5+Cs1+Ct1", 1, 0,
        combined_two_rotations_ok: check_rotations, "C-Thr^2F-Back-ow/2tk-2F0.5+Cs1", 0, 0,
        fs_with_r_err: check_rotations, "B-St-FS-sd-r0.5", 1, 0,
        fp_with_r_ok: check_rotations, "B-St-FP-sd-r0.5", 0, 0,
        fp_with_r_slash_err: check_rotations, "B-St-FP-sd-r0.5/", 1, 0,
        fs_with_r_slash_ok: check_rotations, "B-St-FS-sd-r0.5/", 0, 0,
        fs_with_r_plus_err: check_rotations, "B-St-FS-sd-r0.5+", 1, 0,
        fp_with_r_plus_ok: check_rotations, "B-St-FP-sd-r0.5+", 0, 0,
        fp_with_rl_err: check_rotations, "B-St-FP-sd-r0.5L", 1, 0,
        fp_with_rl_err2: check_rotations, "B-St-FP-sd-r/L", 1, 0,
        lih_with_rl_ok: check_rotations, "B-St-LiH-sd-r/L", 0, 0,
        sth_with_cr_err: check_rotations, "C-Thr>StH-Forw-ln-Cr0.5", 1, 0,
        st_with_cr_ok: check_rotations, "C-Thr>St-Forw-ln-Cr0.5", 0, 0,
        st_with_cr_bang_ok: check_rotations, "C-Thr>St-Forw-ln-Cr0.5!", 0, 0,
        sth_with_cr_bang_ok: check_rotations, "C-Thr>StH-Forw-ln-Cr0.5!", 0, 0,
        two_f_with_crl_err: check_rotations, "C-Thr^2F-Back-tk-Cr0.5L+Cs1", 1, 0,
        lh_with_crl_ok: check_rotations, "C-Thr^Lh-Back-tk-Cr0.5L+Cs1", 0, 0,
        pair_with_cp_err: check_rotations, "C-Thr>Pair>-Forw-ln-CP0.5", 1, 0,
        ff_with_cp_ok: check_rotations, "C-Thr>FF-Forw-ln-CP0.5", 0, 0,
        lh_with_2f_err: check_rotations, "C-Thr^Lh-Back-tk-2F0.5+Cs1", 1, 0,
        two_f_with_2f_ok: check_rotations, "C-Thr^2F-Back-tk-2F0.5+Cs1", 0, 0,
        p_with_ph_err: check_rotations, "P-P-F2A-ln-P0.5h", 1, 0,
        p_with_pr_ok: check_rotations, "P-P-F2A-ln-Pr", 0, 0,
        hand_with_pr_err: check_rotations, "P-Hand-F2A-ln-Pr0.5", 1, 0,
        hand_with_ph_ok: check_rotations, "P-Hand-F2A-ln-P0.5h", 0, 0,
        p_with_p2s_err: check_rotations, "P-P-SiA-mo-P2S", 1, 0,
        flower_with_p2s_ok: check_rotations, "P-Flower-SiA-mo-P2S", 0, 0,
        b_with_db_err: check_rotations, "P-B-F2A-ln-PDB1", 1, 0,
        db_with_db_ok: check_rotations, "P-DB-F2A-ln-PDB1", 0, 0,
        only_ln_with_open_warn: check_rotations, "A-Sq-Back-ln-s1.5t0.5o", 0, 1,
        ln_pk_with_open_ok: check_rotations, "A-Sq-Back-pk/2ln-s1.5t0.5o", 0, 0,
        ss_without_ln_err: check_rotations, "A-Sq-Back-pk-ss1", 1, 0,
        ss_with_two_positions_err: check_rotations, "A-Sq-Back-pk/2ln-ss1", 1, 0,
        ss_with_ln_ok: check_rotations, "A-Sq-Back-ln-ss1", 0, 0,
        bad_c_mutliple_rotations: check_rotations, "C-Thr^2F-Bln-ow/2tk-Cr1!+Cs1-Pos3", 1, 0,
        good_c_mutliple_rotations: check_rotations, "C-Thr^2F-Bln-ow/2tk-2F1+Cs1-Pos3", 0, 0,
        tuck_just_twist_warn: check_rotations, "A-Sq-Back-tk-t1", 0, 1,
        tuck_with_jay_twist_ok: check_rotations, "A-Sq-Back-tk/2ja-t1", 0, 0,
        line_with_twist_ok: check_rotations, "A-Sq-Up-ln-t1", 0, 0,
        st_trans_e_r_bang_ok: check_rotations, "B-St>-E-bo/2ow-r0.5-Pos3", 0, 0,
        supu_le_r_err: check_rotations, "B-2SupU-Le-bb/2ow-r0.5", 1, 0,
        fp_one_leg_r_ok: check_rotations, "B-St-FP-he/2ba-r0.5", 0, 0,
        sp_with_split_err: check_bonuses, "A-2Sup-Up-sp-Split", 1, 0,
        box_with_porp_err: check_bonuses, "P-Knees-4p-Bo-Porp", 1, 0,
        no_conn_with_c: check_bonuses, "A-Thr-Side-ln-c", 0, 1,
        no_conn_with_c_ok: check_bonuses, "A-Thr-Side-ln-c-Catch/Dbl", 0, 0,
        no_conn_with_c_ok2: check_bonuses, "A-Thr-Side-ln-c-Dbl/Pos3", 0, 0,
        conn_with_c_ok: check_bonuses, "A-Thr-Side-ln-c-Conn", 0, 0,
        three_bonuses: check_bonuses, "B-St-FS-ln/2he-Hold/Mov/Dbl", 1, 0,
        dup_bonuses: check_bonuses, "B-LH-Le-mo-Mov/Mov", 1, 0,
        dup_bonuses_ok: check_bonuses, "C-Thr>FF-Forw-ln-CRoll/CRoll", 0, 0,
        mut_excl_bonuses: check_bonuses, "P-2S-3pA-ne-Spider/Climb", 1, 0,
        non_mut_excl_bonuses_ok: check_bonuses, "P-Hand-3pA-ne-Climb/Fall", 0, 0,
        spider_with_p_err: check_bonuses, "P-P-4pAb-ne-Spider", 1, 0,
        spider_with_2s_ok: check_bonuses, "P-2S-4pAb-ne-Spider", 0, 0,
        flyover_with_jump_transit_err: check_bonuses, "C-Thr^St-Forw-ow/2ln-Jump>", 1, 0,
        jump_with_2nd_pos_airborne: check_bonuses, "C-Thr>St-Forw-ow/2ln-Jump", 0, 1,
        jump_with_rotation: check_bonuses, "C-Thr>St-Forw-sd-Cd-Jump", 0, 1,
        jump_transit_with_2nd_pos_airborne_ok: check_bonuses, "C-Thr>St-Forw-ow/2ln-Jump>", 0, 0,
        sdup_with_no_head_down_pos: check_bonuses, "B-St-F1S-he/2sa-SdUp", 0, 1,
        sdup_with_head_down_pos: check_bonuses, "B-St-F1S-he/2ow-SdUp", 0, 0,
        feet_with_retpa_err: check_bonuses, "A-Feet-Up-sp-RetPa", 1, 0,
        somersault_with_retpa_err: check_bonuses, "A-Shou-Up-tk-s1-RetPa", 1, 0,
        back_with_retpa_warn: check_bonuses, "A-Shou-Back-ln-RetPa", 0, 1,
        twist_with_retpa_ok: check_bonuses, "A-Shou-Up-ln-t1-RetPa", 0, 0,
        thr_with_retsq_err: check_bonuses, "A-Thr-Up-sp-RetSq", 1, 0,
        somersault_with_retsq_err: check_bonuses, "A-Sq-Up-tk-s1-RetSq", 1, 0,
        back_with_retsq_warn: check_bonuses, "A-Sq-Back-sp-RetSq", 0, 1,
        twist_with_retsq_ok: check_bonuses, "A-Sq-Up-sp-t1-RetSq", 0, 0,
        catch_without_dbl_warn: check_bonuses, "A-Thr-Forw-ln-Catch", 0, 1,
        catch_with_dbl_ok: check_bonuses, "A-Thr-Forw-ln-Catch/Dbl", 0, 0,
        hold_with_rotation_warn: check_bonuses, "B-St-FS-ln-r0.5/-Hold", 0, 1,
        hold_with_no_rotation_ok: check_bonuses, "B-St-FS-ln-Hold", 0, 0,
        rotation_with_no_hold_ok: check_bonuses, "B-St-FS-ln-r0.5", 0, 0,
        hula_with_pike_err: check_bonuses, "A-Shou-Up-pk-Hula", 1, 0,
        hula_with_ja_ok: check_bonuses, "A-Shou-Up-ja-Hula", 0, 0,
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
        le_with_ff_warn: check_connection, "B-FF-Le-so", 0, 1,
        le_with_2sup_d_warn: check_connection, "B-2SupD-Le-so", 0, 1,
        le_with_2sup_u_ok: check_connection, "B-2SupU-Le-so", 0, 0,
        invalid_airborne_position: check_positions, "A-Sq-Forw-ar", 1, 0,
        one_leg_pos_two_leg_conn: check_positions, "B-St-FS-he", 0, 1,
        one_leg_pos_one_leg_conn: check_positions, "B-St-F1S-he", 0, 0,
        fly_above_airborne_first: check_positions, "C-Thr^2F-Back-tk/2ow", 1, 0,
        fly_above_balance_first: check_positions, "C-Thr^2F-Back-ow/2tk", 0, 0,
        fly_above_with_spl: check_positions, "C-Thr^2F-Back-spl/2tk", 0, 1,
        fly_above_just_balance: check_positions, "C-Thr^2F-Back-ow", 1, 0,
        head_up_with_head_down: check_positions, "B-St-FS-sd/2bb", 0, 1,
        fly_above_lh_wrong_pos: check_positions, "C-Thr^Lh-Forw-so/2tk", 1, 0,
        fly_above_lh_right_pos: check_positions, "C-Thr^Lh-Forw-br/2tk", 0, 0,
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

    #[test]
    fn test_combined_positions() {
        let acros = [
            ("C-Thr>St-Forw-co", 0, 0),
            ("C-Thr>St-Forw-ln-Jump>", 0, 0),
            ("C-Thr>St-Forw-ln/2co-Jump>", 0, 0),
            ("C-Thr>St-Forw-sd/2co-Jump", 0, 0),
            ("C-Thr>StH-Forw-sd/2bb-Jump", 0, 0),
            ("C-Thr>St-Forw-co-Jump>", 1, 0),
            ("C-Thr>St-Forw-co-Jump", 0, 0),
            ("C-Thr>StH-Forw-bb-Jump>", 1, 0),
            ("C-Thr>StH-Forw-bb-Jump", 0, 0),
            ("C-Thr>St-Forw-ln", 0, 0),
            ("C-Thr>StH-Forw-ln", 0, 0),
            ("C-Thr>St-Forw-ln-Jump", 1, 0),
            ("C-Thr>St-Forw-sd/2ja-Jump", 0, 1),
            ("C-Thr>St-Forw-ln/2ja-Jump>", 0, 0),
        ];
        let cat = Category::default();
        for (s, errs, warns) in acros.into_iter() {
            let ci = check_one_acro(cat, &TeamAcrobatic::from(s).unwrap(), "1.0");
            assert_eq!(errs, ci.errors.len(), "acro {}: {:?}", s, ci.errors);
            assert_eq!(warns, ci.warnings.len(), "acro {}: {:?}", s, ci.warnings);
        }
    }
}
