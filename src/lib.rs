mod card_checks;
mod card_checks_acros;
mod iss_parser;
mod setup;
mod text_parser;

use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::IssueLevel::{Error, Warning};
pub use crate::iss_parser::parse_excel;
use chrono::NaiveTime;
use regex_lite::Regex;
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum AgeGroups {
    AG12U,
    Youth,
    JRSR,
    #[default]
    Unknown,
}

fn is_12u(txt: &str) -> bool {
    txt.contains("12U")
        || txt.contains("12 U")
        || txt.contains("12-U")
        || txt.contains("12 AND U")
        || txt.contains("12&")
        || (txt.contains("12") && txt.contains("UNDER"))
        || (txt.contains("11") && txt.contains("12"))
}

fn is_youth(txt: &str) -> bool {
    txt.contains("YOUTH") || txt.contains("13-15")
}

fn is_jrsr(txt: &str) -> bool {
    txt.contains("JR")
        || txt.contains("JUNIOR")
        || txt.contains("SR")
        || txt.contains("SENIOR")
        || txt.contains("15") && txt.contains("17")
        || (txt.contains("16") && txt.contains("17"))
        || (txt.contains("16") && txt.contains("19"))
        || (txt.contains("18") && txt.contains("19"))
        || txt.contains("COLLEGIATE")
}

impl AgeGroups {
    const fn as_str(self) -> &'static str {
        match self {
            AG12U => "12-U",
            Youth => "Youth",
            JRSR => "JR/SR",
            Self::Unknown => "Unknown",
        }
    }
}

impl FromStr for AgeGroups {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let txt = input.to_uppercase();
        if is_12u(&txt) {
            return Ok(AG12U);
        } else if is_youth(&txt) {
            return Ok(Youth);
        } else if is_jrsr(&txt) {
            return Ok(JRSR);
        }
        Ok(Self::Unknown)
    }
}

#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum Events {
    Solo,
    Duet,
    MixedDuet,
    Trio,
    Team,
    Acrobatic,
    Combo,
    #[default]
    Unknown,
}

impl Events {
    const fn as_str(self) -> &'static str {
        match self {
            Solo => "Solo",
            Duet => "Duet",
            MixedDuet => "Mixed Duet",
            Trio => "Trio",
            Team => "Team",
            Acrobatic => "Acrobatic",
            Combo => "Combo",
            Self::Unknown => "Unknown",
        }
    }

    const fn is_team_event(self) -> bool {
        matches!(self, Team | Acrobatic | Combo)
    }
}

impl FromStr for Events {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = input.to_uppercase();
        if input.contains("ACRO") {
            return Ok(Acrobatic);
        }
        if input.contains("COMB") {
            return Ok(Combo);
        }
        if input.contains("TEAM") {
            return Ok(Team);
        }
        if input.contains("MIXED") {
            return Ok(MixedDuet);
        }
        if input.contains("DUET") {
            return Ok(Duet);
        }
        if input.contains("TRIO") {
            return Ok(Trio);
        }
        if input.contains("SOLO") {
            return Ok(Solo);
        }
        Ok(Self::Unknown)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AcroGroup {
    Airborne,
    Balance,
    Combined,
    Platform,
}

impl AcroGroup {
    #[must_use]
    pub fn iter() -> core::array::IntoIter<Self, 4> {
        [Self::Airborne, Self::Balance, Self::Combined, Self::Platform].into_iter()
    }
}

impl FromStr for AcroGroup {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::Airborne),
            "B" => Ok(Self::Balance),
            "C" => Ok(Self::Combined),
            "P" => Ok(Self::Platform),
            other => Err(format!("Unknown group '{other}'"))?,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AcroDirection {
    Upwards,
    Forwards,
    Backwards,
    Sideways,
    Reverse,
    Blind, // only for group C
}

impl FromStr for AcroDirection {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "Up" => Ok(Self::Upwards),
            "Forw" => Ok(Self::Forwards),
            "Back" => Ok(Self::Backwards),
            "Side" => Ok(Self::Sideways),
            "Rev" => Ok(Self::Reverse),
            "Bln" => Ok(Self::Blind),
            dir_name => Err(format!("Unknown direction '{dir_name}'")),
        }
    }
}

// TODO should this be an enum and split this into
// common piece + group specific piece?
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TeamAcrobatic {
    pub group: AcroGroup,
    pub construction: String,
    pub direction: Option<AcroDirection>,
    pub connection_grip: String,
    pub positions: Vec<String>,
    pub rotations: Vec<String>,
    pub bonuses: Vec<String>,
}

fn is_rotation(group: AcroGroup, rotation: &str) -> bool {
    Regex::new(match group {
        AcroGroup::Airborne => "^([cdhfst]|[CDH]$)", // or CDH but not things like Dbl
        AcroGroup::Balance => "^r",
        AcroGroup::Combined => "^(C[cdfhrstP]|2F)",
        // consistency is the hobgoblin of little minds
        AcroGroup::Platform => "^P[r|2S|DB|h|0.5h|1h|1.5h|2h]",
    })
    .unwrap()
    .is_match(rotation)
}

impl FromStr for TeamAcrobatic {
    type Err = String;

    fn from_str(code: &str) -> Result<Self, Self::Err> {
        // "-" is to separate parts, except in one place just to screw
        // with you, we'll just rename it internally to make parsing
        // easier since we don't display it anywhere
        let code = code.replace("C-Roll", "CRoll");
        let mut parts = code.split('-');

        let group = parts.next().unwrap_or_default().parse::<AcroGroup>()?;
        let construction = parts.next().unwrap_or_default().to_owned();
        let dir_grip = parts.next().unwrap_or_default();
        let (direction, connection_grip) = match group {
            AcroGroup::Airborne | AcroGroup::Combined => (Some(dir_grip.parse()?), String::new()),
            AcroGroup::Balance | AcroGroup::Platform => (None, dir_grip.into()),
        };

        if group != AcroGroup::Combined && direction == Some(AcroDirection::Blind) {
            return Err(format!("Cannot use Blind direction with {group:?}"));
        }

        // at one point this went through and did to_lowercase on all
        // the positions because the acro code did not check for
        // position validity. Now that check_positions covers all
        // positions we don't need to worry about reporting that an
        // acro is fine just because it uses "Kt" instead of "kt" and
        // a more specific test is only checking against the correct
        // name.
        let positions = parts
            .next()
            .unwrap_or_default()
            .split('/')
            .filter_map(|x| if x.is_empty() { None } else { Some(x.to_owned()) })
            .collect();

        let mut rotations = vec![];
        let mut bonuses = vec![];
        for rotation_or_bonuses in parts {
            if is_rotation(group, rotation_or_bonuses) {
                // group B has r+, while group C uses + for multiple rotations
                if rotation_or_bonuses.ends_with('+') {
                    rotations.push(rotation_or_bonuses.to_owned());
                } else {
                    rotations.extend(rotation_or_bonuses.split('+').map(str::to_owned));
                }
            } else {
                bonuses.extend(rotation_or_bonuses.split('/').map(str::to_owned));
            }
        }

        Ok(Self { group, construction, direction, connection_grip, positions, rotations, bonuses })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ElementKind {
    TeamAcro(TeamAcrobatic, String),
    PairAcro(String),
    ChoHy,
    Hybrid(Vec<String>, String),
    TRE(String, String),
    SuConn,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Element {
    pub number: usize,
    pub start_time: NaiveTime,
    pub stop_time: NaiveTime,
    pub kind: ElementKind,
}

#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Category {
    pub ag: AgeGroups,
    pub free: bool,
    pub event: Events,
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let free_tech = if self.free { "Free" } else { "Tech" };
        write!(f, "{} {} {free_tech}", self.ag.as_str(), self.event.as_str())
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CoachCard {
    pub category: Category,
    pub elements: Vec<Element>,
    pub theme: String,
    pub end_time: NaiveTime,
    pub iss_ver: Option<semver::Version>,
}

#[derive(Clone, Copy, Debug)]
pub enum IssueLevel {
    Warning,
    Error,
}

impl fmt::Display for IssueLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Warning => write!(f, "\u{26A0}"),
            Error => write!(f, "\u{26D4}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CardIssue {
    pub level: IssueLevel,
    pub text: String,
}

impl CardIssue {
    fn new(level: IssueLevel, text: impl Into<String>) -> Self {
        Self { level, text: text.into() }
    }
}

impl fmt::Display for CardIssue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.level, self.text)
    }
}

pub fn ci_err(ci: &mut Vec<CardIssue>, text: impl Into<String>) {
    ci.push(CardIssue::new(Error, text));
}

pub fn ci_errs(text: impl Into<String>) -> Vec<CardIssue> {
    vec![CardIssue::new(Error, text)]
}

pub fn ci_warn(ci: &mut Vec<CardIssue>, text: impl Into<String>) {
    ci.push(CardIssue::new(Warning, text));
}

pub fn ci_warns(text: impl Into<String>) -> Vec<CardIssue> {
    vec![CardIssue::new(Warning, text)]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AcroDirection::Backwards;
    use crate::AcroGroup::Airborne;

    #[test]
    fn test_parse_team_acro() {
        assert!("Q-Sq-Forw-ln".parse::<TeamAcrobatic>().is_err());
        assert!("A-Sq-Bln-ln".parse::<TeamAcrobatic>().is_err());
        assert!("A-Sq-Down-ln".parse::<TeamAcrobatic>().is_err());
        assert_eq!(
            "A-Sq-Back-ln-D".parse(),
            Ok(TeamAcrobatic {
                group: Airborne,
                construction: "Sq".to_string(),
                direction: Some(Backwards),
                connection_grip: "".to_string(),
                positions: vec!["ln".to_string()],
                rotations: vec!["D".to_string()],
                bonuses: vec![],
            })
        );
        assert_eq!(
            "A-Sq-Back-ln-dt0.5".parse(),
            Ok(TeamAcrobatic {
                group: Airborne,
                construction: "Sq".to_string(),
                direction: Some(Backwards),
                connection_grip: "".to_string(),
                positions: vec!["ln".to_string()],
                rotations: vec!["dt0.5".to_string()],
                bonuses: vec![],
            })
        );
        assert_eq!(
            "A-Sq-Back-ln-Dbl".parse(),
            Ok(TeamAcrobatic {
                group: Airborne,
                construction: "Sq".to_string(),
                direction: Some(Backwards),
                connection_grip: "".to_string(),
                positions: vec!["ln".to_string()],
                rotations: vec![],
                bonuses: vec!["Dbl".to_string()],
            })
        )
    }

    #[test]
    fn test_age_groups_from() {
        assert_eq!("12u".parse(), Ok(AG12U));
        assert_eq!("Youth".parse(), Ok(Youth));
        assert_eq!("13-15".parse(), Ok(Youth));
        assert_eq!("1517".parse(), Ok(JRSR));
        assert_eq!("1617".parse(), Ok(JRSR));
        assert_eq!("16-19".parse(), Ok(JRSR));
        assert_eq!("18-19".parse(), Ok(JRSR));
        assert_eq!("collegiate".parse(), Ok(JRSR));
        assert_eq!("jr".parse(), Ok(JRSR));
        assert_eq!("Junior".parse(), Ok(JRSR));
        assert_eq!("sr".parse(), Ok(JRSR));
        assert_eq!("Senior".parse(), Ok(JRSR));
        assert_eq!("".parse(), Ok(AgeGroups::Unknown));
    }

    #[test]
    fn test_event_from() {
        assert_eq!("acro".parse(), Ok(Acrobatic));
        assert_eq!("Acrobatic".parse(), Ok(Acrobatic));
        assert_eq!("combination".parse(), Ok(Combo));
        assert_eq!("combo".parse(), Ok(Combo));
        assert_eq!("duet".parse(), Ok(Duet));
        assert_eq!("MixedDuet".parse(), Ok(MixedDuet));
        assert_eq!("solo".parse(), Ok(Solo));
        assert_eq!("trio".parse(), Ok(Trio));
        assert_eq!("".parse(), Ok(Events::Unknown));
    }
}

const fn routine_time(min: u32, secs: u32) -> NaiveTime {
    NaiveTime::from_hms_opt(0, min, secs).unwrap()
}

pub fn get_expected_routine_time(category: &Category) -> Option<&NaiveTime> {
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

    map.get(category)
}
