use crate::AgeGroups::JRSR;
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::team_acro::TeamAcroKind::{Airborne, Balance, Combined, Platform};
use crate::team_acro::{Positions, TeamAcroKind};
use crate::utils::oxford_join;
use crate::{
    ABonus, AConst, ADir, APos, ARotationGroup, AcroA, AcroB, AcroC, AcroP, AgeGroups, BBonus,
    BConn, BConst, BPos, BRotationGroup, CBonus, CConst, CDir, CPos, CardIssue, Category,
    CoachCard, DD, Events, Family, FeaturedRotation, MilliDD, PBonus, PConn, PConst, ci_err,
    ci_errs, ci_warn, ci_warns,
};
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;

fn check_dd_limits(category: Category, acro: &TeamAcroKind) -> Box<[CardIssue]> {
    use AgeGroups::*;

    const COMBO_12U: Category = Category { ag: AG12U, event: Combo, free: true };
    const COMBO_YOUTH: Category = Category { ag: Youth, event: Combo, free: true };
    const TEAM_12U: Category = Category { ag: AG12U, event: Team, free: true };
    const TEAM_YOUTH: Category = Category { ag: Youth, event: Team, free: true };
    const TEAM_JRSR_TECH: Category = Category { ag: JRSR, event: Team, free: false };

    #[rustfmt::skip]
    const fn get_max_dd(category: Category, acro: &TeamAcroKind) -> Option<MilliDD> {
        #[allow(clippy::match_same_arms)]
        match (category, acro) {
            (TEAM_12U,       Airborne(_)) => Some(MilliDD(2500)),
            (COMBO_12U,      Airborne(_)) => Some(MilliDD(2500)),
            (TEAM_YOUTH,     Airborne(_)) => Some(MilliDD(2700)),
            (COMBO_YOUTH,    Airborne(_)) => Some(MilliDD(2700)),
            (TEAM_JRSR_TECH, Airborne(_)) => Some(MilliDD(3000)),
            (TEAM_12U,       Balance(_))  => Some(MilliDD(2600)),
            (COMBO_12U,      Balance(_))  => Some(MilliDD(2600)),
            (TEAM_YOUTH,     Balance(_))  => Some(MilliDD(2800)),
            (COMBO_YOUTH,    Balance(_))  => Some(MilliDD(2800)),
            (TEAM_JRSR_TECH, Balance(_))  => Some(MilliDD(3000)),
            (TEAM_12U,       Combined(_)) => Some(MilliDD(2600)),
            (COMBO_12U,      Combined(_)) => Some(MilliDD(2600)),
            (TEAM_YOUTH,     Combined(_)) => Some(MilliDD(2800)),
            (COMBO_YOUTH,    Combined(_)) => Some(MilliDD(2800)),
            (TEAM_JRSR_TECH, Combined(_)) => Some(MilliDD(3000)),
            (TEAM_12U,       Platform(_)) => Some(MilliDD(2800)),
            (COMBO_12U,      Platform(_)) => Some(MilliDD(2800)),
            (TEAM_YOUTH,     Platform(_)) => Some(MilliDD(3000)),
            (COMBO_YOUTH,    Platform(_)) => Some(MilliDD(3000)),
            (TEAM_JRSR_TECH, Platform(_)) => Some(MilliDD(3000)),
            _ => None,
        }
    }

    let mut ci = Vec::new();
    // Use the calculated DD for this check. There is a check for if the
    // calculated DD does not match the reported so there will be some
    // error even if the calculation is wrong. Using the calculated DD
    // makes this work from the text entry mode where there is no
    // reported DD to check against.
    if let Some(max) = get_max_dd(category, acro)
        && acro.dd() > max
    {
        ci_err(
            &mut ci,
            format!("{category} {} acrobatics may not have a DD > {max}", acro.family()),
        );
    }
    ci.into()
}

fn check_groups_for_acro_routine(card: &CoachCard) -> Box<[CardIssue]> {
    type NamedFamilyMatcher = (&'static str, fn(a: &TeamAcroKind) -> bool);
    const MATCHERS: &[NamedFamilyMatcher] = &[
        ("Airborne", |a: &TeamAcroKind| matches!(a, Airborne(_))),
        ("Balance", |a: &TeamAcroKind| matches!(a, Balance(_))),
        ("Combined", |a: &TeamAcroKind| matches!(a, Combined(_))),
        ("Platform", |a: &TeamAcroKind| matches!(a, Platform(_))),
    ];

    let mut ci = Vec::new();
    if card.category.event != Acrobatic {
        return ci.into();
    }

    for (name, matcher) in MATCHERS {
        let num = card.team_acros().filter(|(_, a)| matcher(a)).count();
        if num == 0 {
            ci_err(&mut ci, format!("Missing {name} acrobatic"));
        } else if num > 2 {
            ci_err(&mut ci, format!("may not have more than 2 {name} acrobatics"));
        }
    }

    ci.into()
}

fn check_duplicate_pair_acros(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    let mut prev_acros = HashSet::new();
    for (num, acro) in card.pair_acros() {
        if prev_acros.contains(&acro) {
            ci_err(&mut ci, format!("Element {num}: repeated acrobatic"));
        }
        prev_acros.insert(acro);
    }
    ci.into()
}

fn check_duplicate_elements<A, F, I, Part>(
    acros: impl IntoIterator<Item = (usize, A)>,
    map_fn: F,
) -> Box<[CardIssue]>
where
    A: Family,
    F: Fn(A) -> I,
    I: IntoIterator<Item = Part>,
    Part: Eq + Hash + Display,
{
    let mut ci = Vec::new();

    let mut prev_parts = HashSet::new();
    for (num, acro) in acros {
        for part in map_fn(acro) {
            if prev_parts.contains(&part) {
                let msg = format!(
                    "Element {num}: {part} can only be used once in a {} acrobatic",
                    A::family()
                );
                ci_err(&mut ci, msg);
            } else {
                prev_parts.insert(part);
            }
        }
    }

    ci.into()
}

fn check_team_duplicate_acros(card: &CoachCard) -> Box<[CardIssue]> {
    fn pos_to_slice<T: DD + FromStr>(p: &Positions<T>) -> Box<[&T]> {
        p.second.as_ref().map_or_else(|| [&p.first].into(), |pos2| [&p.first, pos2].into())
    }

    macro_rules! family {
        ($c: ident, $f:ident) => {
            card.team_acros().filter_map(|(num, a)| match a {
                $f(a) => Some((num, a)),
                _ => None,
            })
        };
    }

    let mut ci: Vec<CardIssue> = Vec::new();
    ci.extend(check_duplicate_elements(family!(card, Airborne), |a| pos_to_slice(&a.positions)));
    ci.extend(check_duplicate_elements(family!(card, Balance), |a| [&a.construction]));
    ci.extend(check_duplicate_elements(family!(card, Balance), |a| [&a.conn]));
    ci.extend(check_duplicate_elements(family!(card, Combined), |a| [&a.construction]));
    ci.extend(check_duplicate_elements(family!(card, Platform), |a| [&a.construction]));
    ci.extend(check_duplicate_elements(family!(card, Platform), |a| [&a.conn]));
    ci.extend(check_duplicate_elements(family!(card, Platform), |a| pos_to_slice(&a.positions)));
    ci.extend(check_duplicate_elements(family!(card, Platform), |a| a.bonuses.iter()));
    ci.into()
}

fn check_num_athletes(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    // LH probably needs/usually is done with 5+, but not required
    // it isn't clear that 2Sup/2SupH requires 5
    const A_REQ5: &[AConst] = &[AConst::Sq];
    const B_REQ5: &[BConst] =
        &[BConst::_2SupU, BConst::_2SupD, BConst::_2SupM, BConst::StTransitional];
    const C_REQ5: &[CConst] = &[CConst::ThrOntoPair, CConst::ThrOntoFF, CConst::ThrAboveLh];
    const P_REQ5: &[PConst] = &[PConst::_2S, PConst::Flower];

    // Lh2F might not really require 6+, but realistically it isn't happening with 4-5.
    const B_REQ6: &[BConst] = &[BConst::_2SupD2F, BConst::Lh2F];
    const C_REQ6: &[CConst] = &[CConst::_2SupPlus, CConst::ThrOntoSt2, CConst::ThrOntoStHOnto1F];

    fn check_nums<T: Display + PartialEq>(c: &T, req5: &[T], req6: &[T]) -> Box<[CardIssue]> {
        if req5.contains(c) {
            ci_warns(format!("{c} with Dbl requires 10 athletes!"))
        } else if req6.contains(c) {
            ci_errs(format!("{c} with Dbl requires 12 athletes!"))
        } else {
            ci_warns("Dbl requires 8 or more athletes")
        }
    }

    if !match acro {
        Airborne(a) => a.bonuses.contains(&ABonus::Dbl),
        Balance(a) => a.bonuses.contains(&BBonus::Dbl),
        Combined(a) => a.bonuses.contains(&CBonus::Dbl),
        Platform(a) => a.bonuses.contains(&PBonus::Dbl),
    } {
        return [].into();
    }

    match acro {
        Airborne(a) => check_nums(&a.construction, A_REQ5, &[]),
        Balance(a) => check_nums(&a.construction, B_REQ5, B_REQ6),
        Combined(a) => check_nums(&a.construction, C_REQ5, C_REQ6),
        Platform(a) => check_nums(&a.construction, P_REQ5, &[]),
    }
}

fn check_team_acro_validity(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    if match acro {
        Airborne(a) => a.bonuses.contains(&ABonus::Pos3) && a.positions.second.is_none(),
        Balance(a) => a.bonuses.contains(&BBonus::Pos3) && a.positions.second.is_none(),
        Combined(a) => a.bonuses.contains(&CBonus::Pos3) && a.positions.second.is_none(),
        Platform(a) => a.bonuses.contains(&PBonus::Pos3) && a.positions.second.is_none(),
    } {
        ci_err(&mut ci, "3rd position bonus declared, but only one position declared");
    }

    if match acro {
        Airborne(a) => matches!(&a.positions.second, Some(pos2) if pos2 == &a.positions.first),
        Balance(a) => matches!(&a.positions.second, Some(pos2) if pos2 == &a.positions.first),
        Combined(a) => matches!(&a.positions.second, Some(pos2) if pos2 == &a.positions.first),
        Platform(a) => matches!(&a.positions.second, Some(pos2) if pos2 == &a.positions.first),
    } {
        ci_err(&mut ci, "first and second positions are the same");
    }

    if match acro {
        Airborne(a) => a.bonuses.len() == 2 && a.bonuses[0] == a.bonuses[1],
        Balance(a) => a.bonuses.len() == 2 && a.bonuses[0] == a.bonuses[1],
        Combined(a) => {
            a.bonuses.len() == 2 && a.bonuses[0] == a.bonuses[1] && a.bonuses[0] != CBonus::CRoll
        }
        Platform(a) => a.bonuses.len() == 2 && a.bonuses[0] == a.bonuses[1],
    } {
        ci_err(&mut ci, "cannot declare the same bonus twice");
    }

    ci.into()
}

macro_rules! validate_exclusive_bonuses {
    ($a: ident, $excl: ident) => {{
        // FUTURE use into_array?
        if let [b1, b2] = &*($a.bonuses) {
            if $excl.iter().any(|set| set.contains(&b1) && set.contains(&b2)) {
                return ci_errs(format!("cannot declare {b1} and {b2} in the same acrobatic"));
            }
        }
        [].into()
    }};
}

fn check_exclusive_bonuses(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    use ABonus::*;
    use BBonus::*;
    use CBonus::*;
    use PBonus::*;

    const A_EXCL: &[&[ABonus]] = &[&[Grip, Conn, Catch], &[Hula, RetSq, RetPa]];
    const B_EXCL: &[&[BBonus]] = &[&[BBonus::Twirl, RotF], &[Moon, BBonus::Mov, Hold]];
    const C_EXCL: &[&[CBonus]] =
        &[&[Ju, _1POntoH, HOnto1P, Jump, JumpPass, On1Foot, _1FOnto1F, _1FOnto1FPlus, _2FOnto2F]];
    const P_EXCL: &[&[PBonus]] = &[
        &[Porp, Spich],
        &[Stand, Diva],
        &[Spider, Climb],
        &[
            Dive,
            CH,
            Ps1,
            Ps1t0_5,
            Ps1o,
            Ps1t0_5o,
            Ps1t1,
            Pf1,
            Pf1o,
            PBonus::Mov,
            Mov1,
            Mov1PlusT,
            Fall,
            FTurn,
        ],
    ];

    match acro {
        Airborne(a) => validate_exclusive_bonuses!(a, A_EXCL),
        Balance(a) => validate_exclusive_bonuses!(a, B_EXCL),
        Combined(a) => validate_exclusive_bonuses!(a, C_EXCL),
        Platform(a) => validate_exclusive_bonuses!(a, P_EXCL),
    }
}

fn check_age_restrictions(ag: AgeGroups, acro: &TeamAcroKind) -> Box<[CardIssue]> {
    use ABonus::*;
    use CBonus::*;

    fn a_bonuses(b: &ABonus) -> Option<String> {
        if let RetSq | RetPa = b { Some(b.to_string()) } else { None }
    }

    fn c_bonuses(b: &CBonus) -> Option<String> {
        if let _1FOnto1F | _1FOnto1FPlus | _1POntoH | HOnto1P = b {
            Some(b.to_string())
        } else {
            None
        }
    }

    if ag == JRSR {
        return [].into();
    }

    let mut ci = Vec::new();
    for bonus in match acro {
        Airborne(a) => a.bonuses.iter().filter_map(a_bonuses).collect(),
        Combined(a) => a.bonuses.iter().filter_map(c_bonuses).collect(),
        _ => Vec::new(), // B and P don't have any limited bonuses
    } {
        ci_err(&mut ci, format!("{bonus} is only allowed in JR/SR routines"));
    }
    ci.into()
}

fn check_group_c_positions(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    const AIRBORNE_CONSTRUCTIONS: &[CConst] = &[
        CConst::_2SupPlus,
        CConst::ThrPlusThr,
        CConst::Sn,
        CConst::ThrOntohead,
        CConst::ThrOntoPair,
    ];
    const AIRBORNE_BONUSES: &[CBonus] = &[CBonus::JumpPass, CBonus::Turn];
    const BALANCE_CONSTRUCTIONS: &[CConst] = &[CConst::ThrOntoFF, CConst::ThrOntoF];
    const BALANCE_BONUSES: &[CBonus] = &[
        CBonus::Ju,
        CBonus::_1POntoH,
        CBonus::HOnto1P,
        CBonus::Jump,
        CBonus::On1Foot,
        CBonus::_1FOnto1F,
        CBonus::_1FOnto1FPlus,
        CBonus::_2FOnto2F,
    ];

    let mut ci = Vec::new();

    let Combined(acro) = acro else {
        return ci.into();
    };

    if acro.construction == CConst::ThrAbove2F
        && let CPos::B(pos1) = &acro.positions.first
        && pos1.is_head_up()
    {
        ci_warn(
            &mut ci,
            "head-up position in fly-above construction, should that be a head-down position?",
        );
    }

    if AIRBORNE_CONSTRUCTIONS.contains(&acro.construction)
        && (matches!(acro.positions.first, CPos::B(_))
            || matches!(acro.positions.second, Some(CPos::B(_))))
    {
        ci_err(&mut ci, format!("{} requires airborne positions", acro.construction));
    }

    if BALANCE_CONSTRUCTIONS.contains(&acro.construction)
        && (matches!(acro.positions.first, CPos::A(_))
            || matches!(acro.positions.second, Some(CPos::A(_))))
    {
        ci_err(&mut ci, format!("{} requires balance positions", acro.construction));
    }

    for bonus in &acro.bonuses {
        if AIRBORNE_BONUSES.contains(bonus)
            && (matches!(acro.positions.first, CPos::B(_))
                || matches!(acro.positions.second, Some(CPos::B(_))))
        {
            ci_err(&mut ci, format!("{bonus} requires airborne positions"));
        }

        if BALANCE_BONUSES.contains(bonus)
            && (matches!(acro.positions.first, CPos::A(_))
                || matches!(acro.positions.second, Some(CPos::A(_))))
        {
            ci_err(&mut ci, format!("{bonus} requires balance positions"));
        }
    }

    ci.into()
}

fn check_positions(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    // FUTURE Group C also has Airborne take-off positions, but that
    // check is harder. For now, leave this as just for Airborne and
    // see if this become an issue for Combined.
    if let Airborne(acro) = acro
        && acro.positions.second.is_some()
        && acro.positions.first == APos::ln
    {
        ci_warn(&mut ci, "line claimed as 1st position, are you sure line isn't take-off");
    }

    let (positions, allow_stand_up) = if let Balance(acro) = acro {
        (&acro.positions, acro.bonuses.contains(&BBonus::SdUp))
    } else if let Platform(acro) = acro {
        // this assumes that when platforms stand they go to a two leg
        // stand. This will warn more than is needed, but not claiming
        // stand isn't common, and this will allow us to catch things
        // like 2spl being used instead of owl which is more common.
        (&acro.positions, acro.positions.second == Some(BPos::sd))
    } else {
        return ci.into();
    };

    if let Some(pos2) = &positions.second {
        if positions.first.is_head_up() && pos2.definitely_head_down() {
            ci_warn(
                &mut ci,
                format!("{} is heads-up and {pos2} is heads-down, is this right?", positions.first),
            );
        }

        // If SdUp bonus is there, they are purposefully standing up,
        // so a head up position is expected. For platforms, standing
        // up and then dismounting is common. For now, just check for
        // "sd" since that is the most common way of getting out of a
        // head down position in a platform.
        if positions.first.definitely_head_down() && pos2.is_head_up() && !allow_stand_up {
            ci_warn(
                &mut ci,
                format!("{} is heads-down and {pos2} is heads-up, is this right?", positions.first),
            );
        }
    }

    ci.into()
}

fn check_pair_acro_common_base_marks(card: &CoachCard) -> Box<[CardIssue]> {
    let mut ci = Vec::new();
    if card.category.event != Duet && card.category.event != MixedDuet {
        return ci.into();
    }

    for (num, acro) in card.pair_acros().filter(|(_, a)| a.is_airborne() && !a.is_crash()) {
        ci_warn(
            &mut ci,
            format!(
                "Element {num}: {acro} requires the featured-swimmer must be completely in the AIR (top of the head and toes must be above the surface at the same time)"
            ),
        );
    }

    ci.into()
}

macro_rules! acro_req {
    ($ci: ident, $req: expr, $with: expr, $found: expr) => {
        if let Some(r) = $req
            && &$found != r
        {
            ci_err(&mut $ci, format!("{r} is expected with {} but found {}", $with, $found));
        }
    };
}

macro_rules! acro_reqs {
    ($ci: ident, $reqs: expr, $with: expr, $found: expr) => {
        if !$reqs.is_empty() && !$reqs.contains(&$found) {
            ci_err(
                &mut $ci,
                format!("{} is expected with {} but found {}", oxford_join($reqs), $with, $found),
            )
        }
    };
}

#[derive(Clone, Copy)]
enum SecondAirborne<'a> {
    NotPossible,
    PossibleButNone,
    Has(&'a APos),
}

fn check_featured_rotation(
    r: &impl FeaturedRotation,
    dir: &ADir,
    pos1: &APos,
    pos2: SecondAirborne,
) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    if r.is_open() {
        if !matches!(pos1, APos::tk | APos::pk) {
            ci_warn(&mut ci, "Somersault with open, but first position is not tuck or pike");
        }

        // why on earth are we doing Option<Option<>>? Because for
        // Group C flyabove there is no declared 2nd airport position.
        // the other option would be to only do this check if there is
        // a declared 2nd position, but then we wouldn't handle the
        // type case where ss is select for GroupA when s is meant.
        // There isn't anything we can do to detect that typo for
        // Group C flyabove so this is probably as good as we can get.
        match pos2 {
            SecondAirborne::PossibleButNone => {
                ci_warn(&mut ci, "Somersault with open, but 2ln not declared");
            }
            SecondAirborne::Has(pos2) if pos2 != &APos::ln => {
                ci_err(&mut ci, format!("Somersault with open, but {pos2} declared not 2ln"));
            }
            _ => {}
        }
    }

    let group = r.group();
    if matches!(group, ARotationGroup::Somersault | ARotationGroup::StraightSomersault)
        && dir == &ADir::Up
    {
        ci_warn(&mut ci, "Up declared with somersault, should this be Forward or Backwards?");
    }

    if group == ARotationGroup::StraightSomersault && pos1 != &APos::ln {
        ci_warn(&mut ci, "Straight somersault can only be declared with ln");
    }

    if group == ARotationGroup::Cartwheel && dir != &ADir::Side {
        ci_err(&mut ci, "Direction should always be Sideways for cartwheels");
    }

    if group == ARotationGroup::Handspring && !matches!(dir, ADir::Back | ADir::Forw) {
        ci_err(&mut ci, "Direction should always be Forwards or Backwards for handsprings");
    }

    ci.into()
}

fn check_reqs_a(acro: &AcroA) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    if let Some(r) = &acro.rotation {
        let pos2 = acro
            .positions
            .second
            .as_ref()
            .map_or(SecondAirborne::PossibleButNone, SecondAirborne::Has);
        ci.extend(check_featured_rotation(r, &acro.dir, &acro.positions.first, pos2));

        let group = r.group();

        // they don't say Cartwheel/Handspring, but I think that's not
        // going to be a winning argument, so let's ban them as well.
        // That leaves twists as the only option, and since it's group A
        //  there is only one rotation to check
        if group != ARotationGroup::Twist
            && (acro.bonuses.contains(&ABonus::RetPa) || acro.bonuses.contains(&ABonus::RetSq))
        {
            ci_err(&mut ci, "Somersaults cannot be used with RetPa or RetSq");
        }

        if group == ARotationGroup::Twist
            && matches!(acro.positions.first, APos::tk | APos::pk | APos::rg)
            && acro.positions.second.is_none()
        {
            ci_warn(
                &mut ci,
                format!(
                    "twist declared, but {} is usually performed as a somersault",
                    acro.positions.first
                ),
            );
        }
    }

    for bonus in &acro.bonuses {
        acro_reqs!(ci, bonus.required_consts(), bonus, acro.construction);
        acro_req!(ci, &bonus.required_dir(), bonus, acro.dir);
        acro_reqs!(ci, bonus.required_positions(), bonus, acro.positions.first);
    }

    // This is an underdeclaration, the check is only here to because
    // the manual says Dbl is required. If they have a bonus that's
    // worth more than Dbl, then they know enough to ignore this
    // message. This might flag the case where a coach claimed
    // "Catch" but misunderstood the requirements.
    if acro.bonuses.contains(&ABonus::Catch) && !acro.bonuses.contains(&ABonus::Dbl) {
        ci_warn(&mut ci, "Catch requires two simultaneous acrobatics, so Dbl should be claimed");
    }

    ci.into()
}

fn check_reqs_b(acro: &AcroB) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    acro_reqs!(ci, acro.construction.required_conns(), acro.construction, acro.conn);
    acro_reqs!(ci, acro.conn.required_consts(), acro.conn, acro.construction);
    acro_reqs!(ci, acro.conn.required_positions(), acro.conn, acro.positions.first);

    let pos1 = &acro.positions.first;

    if acro.conn.expects_head_down_pos() && !pos1.could_be_head_down() {
        ci_warn(
            &mut ci,
            format!("expected head-down position with {}, but found {pos1}", acro.conn),
        );
    }

    // TODO is this over aggressive? Ex. can owl be done with some of these?
    if acro.conn.expects_head_up_pos() && !pos1.is_head_up() && !pos1.is_free() {
        ci_warn(&mut ci, format!("expected head-up position with {}, but found {pos1}", acro.conn));
    }

    let pos1 = &acro.positions.first;
    if matches!(acro.conn, BConn::LayF | BConn::SPlus) && !pos1.is_laying() {
        ci_warn(
            &mut ci,
            format!("expected sit, stand, or lay position with {}, but found {pos1}", acro.conn),
        );
    }

    if acro.conn.is_handstand() && acro.positions.first != BPos::bb {
        ci_warn(
            &mut ci,
            "in handstand positions, the first position should be bb unless the featured swimmer goes directly to Position 1 from underwater",
        );
    }

    if acro.conn == BConn::Le && !matches!(acro.construction, BConst::_2SupU | BConst::_2SupM) {
        ci_warn(
            &mut ci,
            format!(
                "Lemur connections require 2 support athletes with at least 1 head-up. A construction of 2SupU or 2SupM should be used instead of {}",
                acro.construction
            ),
        );
    }

    let req_r_for_conn = acro.conn.required_rotations();
    if let Some(r) = &acro.rotation
        && !req_r_for_conn.contains(&r.group())
    {
        if req_r_for_conn.is_empty() {
            ci_err(&mut ci, format!("rotations are not allowed for {}", acro.conn));
        } else {
            ci_err(
                &mut ci,
                format!(
                    "{} is expected with {} but found {}",
                    oxford_join(req_r_for_conn),
                    acro.conn,
                    r,
                ),
            );
        }
    }

    if let Some(r) = &acro.rotation {
        use BPos::*;
        const STANDING_SPLIT: &[BPos] = &[ey, ne, vs, gl, sa, ow, qu];
        acro_reqs!(ci, r.group().required_conns(), r.group(), acro.conn);

        let has_non_standing = !STANDING_SPLIT.contains(pos1)
            || acro.positions.second.as_ref().is_some_and(|pos2| !STANDING_SPLIT.contains(pos2));
        if r.group() == BRotationGroup::Plus && has_non_standing {
            ci_err(
                &mut ci,
                format!(
                    "r+ requires a standing split of 135+, expected {}",
                    oxford_join(STANDING_SPLIT)
                ),
            );
        }
    }

    // this could be a required check, but this might not always be
    // correct, so have a custom check here instead
    if pos1.is_one_foot() && !acro.conn.is_one_leg() {
        ci_warn(
            &mut ci,
            format!("one leg position, {pos1}, but {} is not a one leg connection", acro.conn),
        );
    }

    if acro.conn.is_only_one_leg() && pos1.is_two_foot() {
        ci_warn(
            &mut ci,
            format!("one leg connection, {}, but {pos1} is a two leg position", acro.conn),
        );
    }

    if acro.bonuses.contains(&BBonus::Hold) && acro.rotation.is_some() {
        ci_warn(&mut ci, "Hold and Rotation must not be simultaneous");
    }

    if acro.bonuses.contains(&BBonus::RotF) {
        // this is based on "rotates on feet of support"
        const ROTF_REQ_CONNS: &[BConn] = &[BConn::Li, BConn::Ch, BConn::LayF, BConn::SPlus];
        acro_reqs!(ci, ROTF_REQ_CONNS, &BBonus::RotF, acro.conn);

        if !pos1.is_horizontal()
            && !matches!(&acro.positions.second, Some(pos2) if pos2.is_horizontal())
        {
            ci_err(&mut ci, "RotF claimed, but no horizontal position claimed");
        }
    }

    if acro.bonuses.contains(&BBonus::SdUp) && pos1.is_head_up() {
        ci_warn(&mut ci, "SdUp claimed, but {pos} is head up");
    }

    ci.into()
}

fn check_reqs_c(acro: &AcroC) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    acro_req!(ci, &acro.construction.required_dir(), acro.construction, acro.dir);

    let req_r_for_const = acro.construction.required_rotations();
    if let Some(r) = &acro.base
        && !req_r_for_const.contains(&r.group())
    {
        if req_r_for_const.is_empty() {
            ci_err(&mut ci, format!("base rotations are not allowed for {}", acro.construction));
        } else {
            ci_err(
                &mut ci,
                format!(
                    "{} is expected with {} but found {}",
                    oxford_join(req_r_for_const),
                    acro.construction,
                    r,
                ),
            );
        }
    }

    if let Some(r) = &acro.base {
        acro_reqs!(ci, r.required_consts(), r, acro.construction);
    }

    if let Some(r) = &acro.featured {
        let dir = match &acro.dir {
            CDir::Base(b) => b,
            CDir::Bln => &ADir::Back,
        };

        // if flyabove doesn't have an airborne in the right place a
        // different check will handle that
        let res = if acro.construction.is_flyabove()
            && let Some(CPos::A(pos1)) = &acro.positions.second
        {
            Some((pos1, SecondAirborne::NotPossible))
        } else if let CPos::A(pos1) = &acro.positions.first {
            let pos2 = match &acro.positions.second {
                Some(CPos::A(pos2)) => SecondAirborne::Has(pos2),
                _ => SecondAirborne::PossibleButNone,
            };
            Some((pos1, pos2))
        } else {
            None
        };

        if let Some((pos1, pos2)) = res {
            ci.extend(check_featured_rotation(r, dir, pos1, pos2));
        }
    }

    if acro.construction.is_flyabove() {
        if !matches!(acro.positions.first, CPos::B(_))
            || !matches!(acro.positions.second, Some(CPos::A(_)))
        {
            ci_err(
                &mut ci,
                "fly-above must have a balance position followed by an airborne position",
            );
        }
    } else if (matches!(acro.positions.first, CPos::A(_))
        && matches!(acro.positions.second, Some(CPos::B(_))))
        || (matches!(acro.positions.first, CPos::B(_))
            && matches!(acro.positions.second, Some(CPos::A(_))))
    {
        ci_err(&mut ci, "Cannot have both airborne and balance positions in except for fly-above");
    }

    if acro.construction == CConst::ThrAboveLh
        && !matches!(acro.positions.first, CPos::B(BPos::br | BPos::ct))
    {
        ci_err(&mut ci, "For Thr^Lh balance position must be Bridge or Cat");
    }

    if acro.construction == CConst::ThrAbove2F
        && let CPos::B(pos1) = &acro.positions.first
        && pos1.is_head_up()
    {
        ci_warn(
            &mut ci,
            "head-up position in fly-above construction, should that be a head-down position?",
        );
    }

    for bonus in &acro.bonuses {
        acro_reqs!(ci, bonus.required_consts(), bonus, acro.construction);
        acro_req!(ci, &bonus.required_dir(), bonus, acro.dir);
    }

    ci.into()
}

fn check_reqs_p(acro: &AcroP) -> Box<[CardIssue]> {
    let mut ci = Vec::new();

    acro_reqs!(ci, acro.construction.required_conns(), &acro.construction, acro.conn);
    let req_r_for_const = acro.construction.required_rotation();
    if let Some(r) = &acro.rotation
        && req_r_for_const != r.group()
    {
        ci_err(
            &mut ci,
            format!("{} is expected with {} but found {}", req_r_for_const, acro.construction, r),
        );
    }

    acro_reqs!(ci, acro.conn.required_consts(), acro.conn, acro.construction);
    acro_reqs!(ci, acro.conn.required_positions(), acro.conn, acro.positions.first);

    let pos1 = &acro.positions.first;

    if matches!(
        acro.conn,
        PConn::SPPlusL
            | PConn::HA
            | PConn::ShFPlusP
            | PConn::_2pASlash
            | PConn::_2P2P
            | PConn::HPPlusL
    ) && !pos1.could_be_head_down()
    {
        ci_warn(
            &mut ci,
            format!("expected head-down position with {}, but found {pos1}", acro.conn),
        );
    }

    if matches!(acro.conn, PConn::FAb | PConn::SiFPlusPb) && !pos1.is_head_up() && !pos1.is_free() {
        ci_warn(&mut ci, format!("expected head-up position with {}, but found {pos1}", acro.conn));
    }

    if matches!(acro.conn, PConn::SiA | PConn::LSlashSiFPlusP | PConn::SFPlusTP | PConn::ShiShiPlus)
        && !pos1.is_laying()
    {
        ci_warn(
            &mut ci,
            format!("expected sit, stand, or lay position with {}, but found {pos1}", acro.conn),
        );
    }

    if let Some(r) = &acro.rotation {
        acro_reqs!(ci, r.group().required_consts(), r.group(), acro.construction);
    }

    if pos1.is_one_foot() && !acro.conn.is_one_leg() {
        ci_warn(
            &mut ci,
            format!(
                "one leg position, {pos1}, declared but {} is not a one leg connection",
                acro.conn
            ),
        );
    }

    if acro.conn.is_one_leg() && pos1.is_two_foot() {
        ci_warn(
            &mut ci,
            format!("one leg connection, {}, declared but {pos1} is a two leg position", acro.conn),
        );
    }

    for bonus in &acro.bonuses {
        acro_reqs!(ci, bonus.required_consts(), bonus, acro.construction);
        acro_req!(ci, &bonus.required_conns(), bonus, acro.conn);
        acro_req!(ci, &bonus.required_position(), bonus, acro.positions.first);
    }

    if acro.bonuses.contains(&PBonus::Spich)
        && !((pos1 == &BPos::bb && acro.positions.second == Some(BPos::sh))
            || (pos1 == &BPos::sh && acro.positions.second == Some(BPos::bb)))
    {
        // warn because technically you could do something before Spich,
        // and then you'd start with that position, but that's unlikely
        ci_err(&mut ci, "Spich requires going from bamboo to shrimp or shrimp to bamboo");
    }

    ci.into()
}

fn check_reqs(acro: &TeamAcroKind) -> Box<[CardIssue]> {
    match acro {
        Airborne(a) => check_reqs_a(a),
        Balance(a) => check_reqs_b(a),
        Combined(a) => check_reqs_c(a),
        Platform(a) => check_reqs_p(a),
    }
}

pub fn check_one_acro(category: Category, acro: &TeamAcroKind) -> Box<[CardIssue]> {
    let mut element_ci = Vec::new();
    element_ci.extend(check_age_restrictions(category.ag, acro));
    element_ci.extend(check_dd_limits(category, acro));
    element_ci.extend(
        [
            check_num_athletes,
            check_team_acro_validity,
            check_reqs,
            check_exclusive_bonuses,
            check_positions,
            check_group_c_positions,
        ]
        .iter()
        .flat_map(|check| check(acro)),
    );
    element_ci.into()
}

type CardCheckFn = fn(&CoachCard) -> Box<[CardIssue]>;

pub fn run_acro_checks(card: &CoachCard) -> Box<[CardIssue]> {
    let checks: &[CardCheckFn] = match card.category.event {
        Solo | Events::Unknown => &[],
        Duet | MixedDuet | Trio => &[check_duplicate_pair_acros, check_pair_acro_common_base_marks],
        Acrobatic | Combo | Team => &[check_groups_for_acro_routine, check_team_duplicate_acros],
    };

    let mut ci = checks.iter().flat_map(|check| check(card)).collect::<Vec<_>>();
    for (num, acro) in card.team_acros() {
        for i in check_one_acro(card.category, acro) {
            ci.push(CardIssue::new(i.level, format!("Element {num}: {}", i.text)));
        }
    }
    ci.into()
}

#[cfg(test)]
#[cfg_attr(test, allow(clippy::too_many_lines))]
mod tests {
    use super::*;
    use crate::AgeGroups::{AG12U, Youth};
    use crate::{Element, ElementKind};

    fn elements(acros: &[&str], to_element: fn(&str) -> ElementKind) -> Box<[Element]> {
        let mut ret = Vec::new();
        for (i, acro) in acros.iter().enumerate() {
            ret.push(Element {
                number: i + 1,
                start_time: Default::default(),
                stop_time: Default::default(),
                kind: to_element(acro),
            });
        }
        ret.into()
    }

    fn pair_acros(acros: &[&str]) -> Box<[Element]> {
        elements(acros, |acro| ElementKind::PairAcro(acro.parse().unwrap(), None))
    }

    fn team_acros(acros: &[&str]) -> Box<[Element]> {
        elements(acros, |acro| ElementKind::TeamAcro(acro.parse().unwrap(), None))
    }

    #[test]
    fn test_check_team_duplicate_acros() {
        let tests: &[(&str, &[&str], usize)] = &[
            ("repeat_pos_group_a", &["A-Sq-Back-pk/2tk", "A-Sq-Back-tk/2ja"], 1),
            ("group_a_no_dups", &["A-Sq-Back-pk/2ln", "A-Sq-Back-tk/2sp"], 0),
            ("repeat_construction_group_b", &["B-St-1P1P-bb", "B-St-PP-ow"], 1),
            ("repeat_connection_group_b", &["B-St-PP-bb", "B-StH-PP-ow"], 1),
            ("group_b_no_dups_ok", &["B-St-1P1P-bb", "B-StH-PP-ow"], 0),
            ("repeat_pos_group_c", &["C-Thr>St-Bln-tk-Cs1", "C-Thr>St-Forw-sd/2tk-Cd-Jump"], 1),
            ("group_c_no_dups", &["C-Thr>St-Bln-tk-Cs1", "C-Thr>F-Forw-sd/2tk-Cd-Jump"], 0),
            ("repeat_construction_group_p", &["P-Knees-SP+K-bb/2ow", "P-Knees-3pA-ne"], 1),
            ("repeat_connection_group_p", &["P-Knees-SP+K-bb/2ow", "P-2S-SP+K-ne/2ey"], 1),
            ("repeat_pos_group_p", &["P-Knees-SP+K-bb/2ow", "P-2S-FA+PF-ow/2ey"], 1),
            ("repeat_bonus_group_p", &["P-Knees-SP+K-bb/2ow-Pos3", "P-2S-FA+PF-ne/2ey-Pos3"], 1),
            ("group_p_no_dups", &["P-Knees-SP+K-bb/2ow-Pos3", "P-2S-FA+PF-ne/2ey-Trav"], 0),
        ];
        for (name, acros, expected) in tests {
            let card = &CoachCard { elements: team_acros(acros), ..Default::default() };
            assert_eq!(check_team_duplicate_acros(card).len(), *expected, "{name}");
        }
    }

    #[test]
    fn test_check_groups_for_acro_routine() {
        let ex_a = "A-Shou-Back-tk-s1";
        let ex_b = "B-St-FS-sd";
        let ex_c = "C-Thr^2F-Forw-bb";
        let ex_p = "P-P-HA-bb/2wi-Porp/Trav";

        let acat = Category { ag: JRSR, event: Acrobatic, free: true };
        let tcat = Category { ag: JRSR, event: Team, free: true };

        let tests: &[(&str, Category, &[&str], usize)] = &[
            ("all_groups", acat, &[ex_a, ex_b, ex_c, ex_p], 0),
            ("missing_a", acat, &[ex_b, ex_c, ex_p], 1),
            ("missing_a_team", tcat, &[ex_b, ex_c, ex_p], 0),
            ("missing_b", acat, &[ex_a, ex_c, ex_p], 1),
            ("missing_c", acat, &[ex_a, ex_b, ex_p], 1),
            ("missing_p", acat, &[ex_a, ex_b, ex_c], 1),
            ("too_many_a", acat, &[ex_a, ex_a, ex_a, ex_b, ex_c, ex_p], 1),
        ];
        for (name, cat, a, expected) in tests {
            let card = &CoachCard { category: *cat, elements: team_acros(a), ..Default::default() };
            assert_eq!(check_groups_for_acro_routine(card).len(), *expected, "{name}");
        }
    }

    #[test]
    fn test_check_dd_limits() {
        const TESTS: &[(&str, Category, &str, usize)] = &[
            ("tech_high", Category { ag: JRSR, event: Team, free: false }, "A-Sq-Back-tk-s2.5", 1),
            ("free_ok", Category { ag: JRSR, event: Team, free: true }, "A-Sq-Back-tk-s2.5", 0),
            ("12u_ok", Category { ag: AG12U, event: Team, free: true }, "P-B-2b/-hp/2ow", 0),
            ("12u_high", Category { ag: AG12U, event: Team, free: true }, "P-B-2b/-hp/2ow-Pos3", 1),
            ("youth_ok", Category { ag: Youth, event: Team, free: true }, "P-B-2b/-hp/2ow-Pos3", 0),
            ("youth_high", Category { ag: Youth, event: Team, free: true }, "P-B-2pBb-qu/2wi", 1),
        ];
        for (name, cat, acro, expected) in TESTS {
            assert_eq!(check_dd_limits(*cat, &acro.parse().unwrap()).len(), *expected, "{name}");
        }
    }

    #[test]
    fn test_check_one_acro_issue() {
        let tests: &[(&str, fn(&TeamAcroKind) -> Box<[CardIssue]>, &str, usize)] = &[
            ("reqs_6_dbl_err", check_num_athletes, "B-2SupD2F-Le-co-Dbl", 1),
            ("reqs_5_dbl_warns", check_num_athletes, "A-Sq-Back-pk/2rg-s1-Dbl", 1),
            ("reqs_4_dbl_warns", check_num_athletes, "A-Thr-Back-pk/2rg-s1-Dbl", 1),
            ("no_dbl_ok", check_num_athletes, "A-Sq-Back-pk/2rg-s1", 0),
            ("missing_2nd_pos", check_team_acro_validity, "A-Sq-Back-pk-Pos3", 1),
            ("pos3_bonus_ok", check_team_acro_validity, "A-Sq-Back-pk/2rg-Pos3", 0),
            ("same_positions", check_team_acro_validity, "A-Sq-Back-pk/2pk", 1),
            ("back_with_cart_err", check_reqs, "A-Sq-Back-ln-ct0.5", 1),
            ("back_with_cart2_err", check_reqs, "A-Sq-Back-ln-C", 1),
            ("side_with_cart_ok", check_reqs, "A-Sq-Side-ln-ct0.5", 0),
            ("side_with_hand_err", check_reqs, "A-Sq-Side-ln-hd", 1),
            ("side_with_hand2_err", check_reqs, "A-Sq-Side-ln-H", 1),
            ("forw_with_hand_ok", check_reqs, "A-Sq-Forw-ln-hd", 0),
            ("up_with_dive_warn", check_reqs, "A-Sq-Up-ln-D", 1),
            ("up_with_twist_ok", check_reqs, "A-Sq-Up-ln-t1", 0),
            ("up_with_somersault_warn", check_reqs, "A-Sq-Up-ln-ss1", 1),
            ("two_sup_wrong_dir_err", check_reqs, "C-2Sup+-Side-sp", 1),
            ("turn_wrong_dir_err", check_reqs, "C-2Sup+-Side-sp-Turn", 2),
            ("two_sup_correct_decl_ok", check_reqs, "C-2Sup+-Up-sp-Turn", 0),
            ("hula_with_side_err", check_reqs, "A-Shou-Side-rg-Hula", 1),
            ("hula_with_up_ok", check_reqs, "A-Shou-Up-rg-Hula", 0),
            ("fs_with_r_err", check_reqs, "B-St-FS-sd-r0.5", 2),
            ("fp_with_r_ok", check_reqs, "B-St-FP-ba-r0.5", 0),
            ("fp_with_r_slash_err", check_reqs, "B-St-FP-ba-r0.5/", 2),
            ("fs_with_r_slash_ok", check_reqs, "B-St-FS-sd-r0.5/", 0),
            ("fs_with_r_plus_err", check_reqs, "B-St-FS-sd-r0.5+", 3),
            ("he_with_r_plus_err", check_reqs, "B-St-FP-he-r0.5+", 1),
            ("fp_with_r_plus_ok", check_reqs, "B-St-FP-ey-r0.5+", 0),
            ("fp_with_rl_err", check_reqs, "B-St-FP-ey-r0.5L", 2),
            ("fp_with_rl_err2", check_reqs, "B-St-FP-ey-r/L", 2),
            ("lih_with_rl_ok", check_reqs, "B-LH-LiH-br-r/L", 0),
            ("sth_with_cr_err", check_reqs, "C-Thr>StH-Forw-ln-Cr0.5", 2),
            ("st_with_cr_ok", check_reqs, "C-Thr>St-Forw-ln-Cr0.5", 0),
            ("st_with_cr_bang_ok", check_reqs, "C-Thr>St-Forw-ln-Cr0.5!", 0),
            ("sth_with_cr_bang_ok", check_reqs, "C-Thr>StH-Forw-ln-Cr0.5!", 0),
            ("two_f_with_crl_err", check_reqs, "C-Thr^2F-Back-br/2tk-Cr0.5L+Cs1", 2),
            ("lh_with_crl_ok", check_reqs, "C-Thr^Lh-Back-br/2tk-Cr0.5L+Cs1", 0),
            ("pair_with_cp_err", check_reqs, "C-Thr>Pair>-Forw-ln-CP0.5", 2),
            ("ff_with_cp_ok", check_reqs, "C-Thr>FF-Forw-ln-CP0.5", 0),
            ("lh_with_2f_err", check_reqs, "C-Thr^Lh-Back-br/2tk-2F0.5+Cs1", 2),
            ("two_f_with_2f_ok", check_reqs, "C-Thr^2F-Back-br/2tk-2F0.5+Cs1", 0),
            ("p_1fa_with_box_err", check_reqs, "P-Box-1FA-he", 2),
            ("p_with_ph_err", check_reqs, "P-P-F2A-sd-P0.5h", 2),
            ("p_with_pr_ok", check_reqs, "P-P-F2A-sd-Pr", 0),
            ("hand_with_pr_err", check_reqs, "P-Hand-F2A-sd-Pr0.5", 2),
            ("hand_with_ph_ok", check_reqs, "P-Hand-F2A-sd-P0.5h", 0),
            ("p_with_p2s_err", check_reqs, "P-P-SiA-mo-P2S", 2),
            ("flower_with_p2s_ok", check_reqs, "P-Flower-SiA-mo-P2S", 0),
            ("b_with_db_err", check_reqs, "P-B-F2A-sd-PDB1", 2),
            ("db_with_db_ok", check_reqs, "P-DB-F2A-sd-PDB1", 0),
            ("only_ln_with_open_warn", check_reqs, "A-Sq-Back-ln-s1.5t0.5o", 2),
            ("open_wo_2ln_warn", check_reqs, "A-Sq-Back-pk-s1.5t0.5o", 1),
            ("arch_with_open_warn", check_reqs, "A-Sq-Back-ja/2ln-s1.5t0.5o", 1),
            ("ln_pk_with_open_ok", check_reqs, "A-Sq-Back-pk/2ln-s1.5t0.5o", 0),
            ("c_with_open_warn", check_reqs, "C-Thr>St-Back-tk-Cs1t1o", 1),
            ("flyabove_with_open_ok", check_reqs, "C-Thr^2F-Bln-ow/2tk-2F1+Cs1t1o-Pos3", 0),
            ("ss_without_ln_err", check_reqs, "A-Sq-Back-pk-ss1", 1),
            ("ss_with_two_positions_err", check_reqs, "A-Sq-Back-pk/2ln-ss1", 1),
            ("ss_with_ln_ok", check_reqs, "A-Sq-Back-ln-ss1", 0),
            ("c_ss_with_ln_ok", check_reqs, "C-Thr^2F-Bln-ow/2ln-2F0.5+Css1t1", 0),
            ("tuck_just_twist_warn", check_reqs, "A-Sq-Back-tk-t1", 1),
            ("tuck_with_jay_twist_ok", check_reqs, "A-Sq-Back-tk/2ja-t1", 0),
            ("line_with_twist_ok", check_reqs, "A-Sq-Up-ln-t1", 0),
            ("st_trans_e_r_bang_ok", check_reqs, "B-St>-E-bb/2ow-r0.5-Pos3", 0),
            ("supu_le_r_err", check_reqs, "B-2SupU-Le-bb/2ow-r0.5", 2),
            ("fp_one_leg_r_ok", check_reqs, "B-St-FP-he/2ba-r0.5", 0),
            ("sp_with_split_err", check_reqs, "A-2Sup-Up-sp-Split", 1),
            ("box_with_porp_err", check_reqs, "P-P-4p-bo-Porp", 1),
            ("dup_bonuses", check_team_acro_validity, "B-LH-LiH-mo-Mov/Mov", 1),
            ("dup_bonuses_ok", check_team_acro_validity, "C-Thr>FF-Forw-ln-CRoll/CRoll", 0),
            ("mut_excl_bonuses", check_exclusive_bonuses, "P-2S-3pA-br-Spider/Climb", 1),
            ("non_mut_excl_bonuses_ok", check_exclusive_bonuses, "P-Hand-3pA-ne-Climb/Fall", 0),
            ("spider_with_p_err", check_reqs, "P-P-BA-br-Spider", 1),
            ("spider_with_2s_ok", check_reqs, "P-2S-BA-br-Spider", 0),
            ("BA_without_bridge_err", check_reqs, "P-2S-BA-sd", 1),
            ("spider_without_bridge_err", check_reqs, "P-Hand-F2A-sd-Spider", 1),
            ("spider_with_bridge_ok", check_reqs, "P-2S-BA-br-Spider", 0),
            ("sdup_with_no_head_down_pos", check_reqs, "B-St-F1S-he/2sa-SdUp", 1),
            ("sdup_with_head_down_pos", check_reqs, "B-St-F1S-ow/2he-SdUp", 0),
            ("feet_with_retpa_err", check_reqs, "A-Feet-Up-sp-RetPa", 1),
            ("somersault_with_retpa_err", check_reqs, "A-Shou-Up-tk-s1-RetPa", 2),
            ("back_with_retpa_warn", check_reqs, "A-Shou-Back-ln-RetPa", 1),
            ("twist_with_retpa_ok", check_reqs, "A-Shou-Up-ln-t1-RetPa", 0),
            ("thr_with_retsq_err", check_reqs, "A-Thr-Up-sp-RetSq", 1),
            ("somersault_with_retsq_err", check_reqs, "A-Sq-Up-tk-s1-RetSq", 2),
            ("back_with_retsq_warn", check_reqs, "A-Sq-Back-sp-RetSq", 1),
            ("twist_with_retsq_ok", check_reqs, "A-Sq-Up-sp-t1-RetSq", 0),
            ("catch_without_dbl_warn", check_reqs, "A-Thr-Forw-ln-Catch", 1),
            ("catch_with_dbl_ok", check_reqs, "A-Thr-Forw-ln-Catch/Dbl", 0),
            ("hold_with_rotation_warn", check_reqs, "B-St-FS-sd-r0.5/-Hold", 1),
            ("hold_with_no_rotation_ok", check_reqs, "B-St-FS-sd-Hold", 0),
            ("rotation_with_no_hold_ok", check_reqs, "B-St-FS-sd-r0.5/", 0),
            ("hula_with_pike_err", check_reqs, "A-Shou-Up-pk-Hula", 1),
            ("hula_with_ja_ok", check_reqs, "A-Shou-Up-ja-Hula", 0),
            ("rotf_with_head_down_err", check_reqs, "B-StH-LayF-wi-RotF", 1),
            ("rotf_with_head_up_conn_err", check_reqs, "B-St-FS-mo-RotF", 1),
            ("rotf_with_horizontal_ok", check_reqs, "B-StH-LayF-co-RotF", 0),
            ("spich_incorrect_pos_err", check_reqs, "P-Knees-SP+K-sh/2ow-Spich", 1),
            ("spich_bb_sh_ok", check_reqs, "P-Knees-SP+K-bb/2sh-Spich", 0),
            ("spich_sh_bb_ok", check_reqs, "P-Knees-SP+K-sh/2bb-Spich", 0),
            ("diva_without_2s_err", check_reqs, "P-B-3pS-ow-Diva", 1),
            ("diva_without_3ps_err", check_reqs, "P-2S-3pbA-ow-Diva", 1),
            ("diva_with_2s_3ps_ok", check_reqs, "P-2S-3pS-ow-Diva", 0),
            ("st_bad_connection", check_reqs, "B-St>-FS-sd", 2),
            ("st_good_connection", check_reqs, "B-St>-F1S-he", 0),
            ("non_st_bad_connection", check_reqs, "B-St-FS-sd", 0),
            ("one_leg_conn_2_leg_pos", check_reqs, "B-St-FS-he", 2),
            ("one_or_two_leg_conn_2_leg_pos", check_reqs, "B-St-FP-sd", 0),
            ("two_leg_conn_2_leg_pos", check_reqs, "B-St-FS-sd", 0),
            ("head_down_conn_head_up_pos", check_reqs, "B-St-Bp-sd", 1),
            ("head_down_conn_head_down_pos", check_reqs, "B-St-PP-bb", 0),
            ("head_up_conn_head_down_pos", check_reqs, "B-St-FS-bb", 1),
            ("head_up_conn_head_up_pos", check_reqs, "B-St-FS-sd", 0),
            ("sit_conn_head_up_pos", check_reqs, "B-St-S+-sd", 1),
            ("sit_conn_head_sit_pos", check_reqs, "B-St-S+-mo", 0),
            ("handstand_conn_without_bb", check_reqs, "B-St-PP-ow", 1),
            ("handstand_conn_with_bb", check_reqs, "B-St-PP-bb/2ow", 0),
            ("le_with_2sup_d_warn", check_reqs, "B-2SupD-Le-so", 3),
            ("le_with_2sup_u_ok", check_reqs, "B-2SupU-Le-so", 0),
            ("ba_with_wi_err", check_reqs, "P-2S-BA-wi", 1),
            ("ba_wtih_br_ok", check_reqs, "P-2S-BA-br", 0),
            ("two_pbb_with_wi_err", check_reqs, "P-B-2pBb-wi", 1),
            ("two_pbb_with_qu_ok", check_reqs, "P-B-2pBb-qu", 0),
            ("one_leg_pos_two_leg_conn", check_reqs, "B-St-FS-he", 2),
            ("one_leg_pos_one_leg_conn", check_reqs, "B-St-F1S-he", 0),
            ("two_leg_pos_one_leg_conn", check_reqs, "B-St-F1S-sd", 1),
            ("two_leg_pos_two_leg_conn", check_reqs, "B-St-FS-sd", 0),
            ("fly_above_airborne_first", check_reqs, "C-Thr^2F-Back-tk/2ow", 1),
            ("fly_above_balance_first", check_reqs, "C-Thr^2F-Back-ow/2tk", 0),
            ("fly_above_with_spl", check_reqs, "C-Thr^2F-Back-spl/2tk", 1),
            ("fly_above_just_balance", check_reqs, "C-Thr^2F-Back-ow", 1),
            ("head_up_with_head_down", check_positions, "B-St-FS-sd/2bb", 1),
            ("head_down_with_head_up", check_positions, "P-Knees-SP+K-bb/2spl", 1),
            ("fly_above_lh_wrong_pos", check_reqs, "C-Thr^Lh-Forw-so/2tk", 1),
            ("fly_above_lh_right_pos", check_reqs, "C-Thr^Lh-Forw-br/2tk", 0),
            ("head_down_to_up_warn", check_positions, "B-St-F1S-ow/2ne", 1),
            ("head_down_to_up_ok", check_reqs, "B-L-Li-ow/2ne-SdUp", 0),
            ("head_down_to_up_ok2", check_reqs, "P-P-3pA-ow/2sd-Dive", 0),
            ("head_up_to_free_ok", check_reqs, "B-L-Li-so/2fl", 0),
            ("free_to_head_up_ok", check_reqs, "B-L-Li-fl/2co", 0),
            ("head_up_to_free_ok", check_reqs, "P-DB-L/SiF+P-pi/2hp", 0),
            ("free_to_head_down_ok", check_reqs, "P-DB-L/SiF+P-hp/2pi", 0),
            ("queen_ok", check_reqs, "P-B-2pBb-qu", 0),
            ("airborne_ln_as_takeoff", check_positions, "A-Sq-Back-ln/2tk-s1", 1),
        ];
        for (name, check, acro, expected) in tests {
            let res = check(&acro.parse().unwrap());
            assert_eq!(res.len(), *expected, "{name}: {res:#?}");
        }
    }

    #[test]
    fn test_check_age_restrictions() {
        let acro = "C-Thr>StH-Forw-ln-1F>1F".parse().unwrap();
        assert_eq!(check_age_restrictions(AG12U, &acro).len(), 1);
        assert_eq!(check_age_restrictions(Youth, &acro).len(), 1);
        assert_eq!(check_age_restrictions(JRSR, &acro).len(), 0);
    }

    #[test]
    fn test_check_duplicate_pair_acros() {
        let category = Category { event: Duet, ..Default::default() };

        let same = pair_acros(&["W!fr1", "W!fr1"]);
        let card = CoachCard { category, elements: same, ..Default::default() };
        assert_eq!(check_duplicate_pair_acros(&card).len(), 1);

        let diff = pair_acros(&["W!fr0.5", "W!fr1"]);
        let card = CoachCard { category, elements: diff, ..Default::default() };
        assert_eq!(check_duplicate_pair_acros(&card).len(), 0);
    }

    #[test]
    fn test_check_pair_acro_common_base_marks() {
        let tests = &[
            (Duet, "Jr0.5", 1),
            (Trio, "Jr0.5", 0),
            (Duet, "W!fr1", 1),
            (Trio, "W!fr1", 0),
            (Duet, "W!s0.5", 1),
            (Duet, "J", 0),
            (Duet, "Jd", 1),
            (Duet, "Jf", 0),
            (Duet, "W!»", 0),
            (Duet, "Jfs1B", 1),
        ];
        for (event, acro, expected) in tests {
            let category = Category { event: *event, ..Default::default() };
            let card = CoachCard { category, elements: pair_acros(&[acro]), ..Default::default() };
            assert_eq!(check_pair_acro_common_base_marks(&card).len(), *expected, "{acro}");
        }
    }

    #[test]
    fn test_run_checks() {
        let category = Category { event: Team, ..Default::default() };
        let team =
            CoachCard { category, elements: team_acros(&["A-Sq-Up-tk-s1"]), ..Default::default() };
        assert_eq!(run_acro_checks(&team).len(), 1);

        let category = Category { event: Duet, ..Default::default() };
        let duet = CoachCard { category, elements: pair_acros(&["L", "L"]), ..Default::default() };
        assert_eq!(run_acro_checks(&duet).len(), 1);

        let solo = &CoachCard {
            category: Category { event: Solo, ..Default::default() },
            ..Default::default()
        };
        assert_eq!(run_acro_checks(solo).len(), 0);
    }

    #[test]
    fn test_for_all_issues() {
        let acros = [
            ("C-Thr>St-Forw-co", 0),
            ("C-Thr>St-Forw-ln-Jump>", 0),
            ("C-Thr>St-Forw-ln/2co-Jump>", 2),
            ("C-Thr>St-Forw-sd/2co-Jump", 0),
            ("C-Thr>StH-Forw-sd/2bb-Jump", 0),
            ("C-Thr>St-Forw-co-Jump>", 1),
            ("C-Thr>St-Forw-co-Jump", 0),
            ("C-Thr>StH-Forw-bb-Jump>", 1),
            ("C-Thr>StH-Forw-bb-Jump", 0),
            ("C-Thr>St-Forw-ln", 0),
            ("C-Thr>StH-Forw-ln", 0),
            ("C-Thr>St-Forw-ln-Jump", 1),
            ("C-Thr>St-Forw-sd/2ja-Jump", 2),
            ("C-Thr>St-Forw-ln/2ja-Jump>", 0),
            ("C-Thr>St-Forw-ow/2ln-Jump>", 2),
            ("C-Thr>St-Forw-ow/2ln-Jump", 2),
            ("C-Thr>St-Forw-ow/2ln-Jump>", 2),
            ("C-Thr>St-Back-mo/2tk", 1),
            ("C-2Sup+-Up-spl", 1),
            ("C-2Sup+-Up-sp", 0),
            ("C-Thr>F-Side-ln", 1),
            ("C-Thr>F-Side-sd", 0),
            ("P-B-L/SiF+P-wi/2ow-Dive", 0),
        ];
        let cat = Category::default();
        for (s, expected_num_issues) in acros {
            let ci = check_one_acro(cat, &s.parse().unwrap());
            assert_eq!(expected_num_issues, ci.len(), "acro {s}: {ci:?}");
        }
    }
}
