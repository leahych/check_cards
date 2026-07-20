mod card_checks;
mod card_checks_acros;
mod element;
mod hybrid;
mod iss_parser;
mod pair_acro;
mod setup;
mod team_acro;
mod text_parser;
mod tre;
mod utils;

use crate::AgeGroups::{AG12U, JRSR, Youth};
use crate::ElementKind::{Hybrid, PairAcro, TeamAcro};
use crate::Events::{Acrobatic, Combo, Duet, MixedDuet, Solo, Team, Trio};
use crate::IssueLevel::{Error, Warning};
pub use crate::element::ElementKind;
pub use crate::hybrid::*;
pub use crate::iss_parser::parse_excel;
pub use crate::pair_acro::PairAcroKind;
pub use crate::team_acro::*;
use chrono::NaiveTime;
use std::fmt;
use std::fmt::Display;
use std::iter::Sum;
use std::ops::{Add, Div, Mul};
use strum_macros::Display;

#[derive(Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct MilliDD(pub u32);

impl Add for MilliDD {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Display for MilliDD {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:.3}", f64::from(self.0) / 1000.0)
    }
}

impl Mul for MilliDD {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl Div for MilliDD {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0 / rhs.0)
    }
}

impl Sum for MilliDD {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self(0), Add::add)
    }
}

pub trait DD {
    // this should be const but can't until
    // rust-project-goals #106 is finished
    fn dd(&self) -> MilliDD;
}

#[allow(clippy::upper_case_acronyms)]
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
        || txt.contains("12AND U")
        || txt.contains("12&")
        || txt.contains("12/U")
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
        || txt.contains("1517")
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

impl From<&str> for AgeGroups {
    fn from(input: &str) -> Self {
        let txt = input.to_uppercase();
        if is_12u(&txt) {
            return AG12U;
        } else if is_youth(&txt) {
            return Youth;
        } else if is_jrsr(&txt) {
            return JRSR;
        }
        Self::Unknown
    }
}

#[derive(Copy, Clone, Debug, Display, Default, Hash, PartialEq, Eq)]
pub enum Events {
    Solo,
    Duet,
    #[strum(to_string = "Mixed Duet")]
    MixedDuet,
    Trio,
    Team,
    Acrobatic,
    Combo,
    #[default]
    Unknown,
}

impl From<&str> for Events {
    fn from(input: &str) -> Self {
        let input = input.to_uppercase();
        for (pattern, kind) in [
            ("ACRO", Acrobatic),
            ("COMB", Combo),
            ("TEAM", Team),
            ("MIXED", MixedDuet),
            ("DUET", Duet),
            ("TRIO", Trio),
            ("SOLO", Solo),
        ] {
            if input.contains(pattern) {
                return kind;
            }
        }
        Self::Unknown
    }
}

#[derive(Debug, PartialEq, Eq)]
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
        write!(f, "{} {} {free_tech}", self.ag.as_str(), self.event)
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CoachCard {
    pub category: Category,
    pub elements: Box<[Element]>,
    pub theme: String,
    pub end_time: NaiveTime,
    pub iss_ver: Option<semver::Version>,
}

impl CoachCard {
    pub fn hybrids(&self) -> impl Iterator<Item = &HybridDecl> {
        self.elements.iter().filter_map(|e| match &e.kind {
            Hybrid(decls, _) => Some(decls),
            _ => None,
        })
    }

    pub fn pair_acros(&self) -> impl Iterator<Item = (usize, &PairAcroKind)> {
        self.elements.iter().filter_map(|e| match &e.kind {
            PairAcro(a, _) => Some((e.number, a)),
            _ => None,
        })
    }

    pub fn team_acros(&self) -> impl Iterator<Item = (usize, &TeamAcroKind)> {
        self.elements.iter().filter_map(|e| match &e.kind {
            TeamAcro(a, _) => Some((e.number, a)),
            _ => None,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

// TODO look at converting these to something more like actual errors
// don't want error because '?' isn't useful but do want an enum that
// implements Display and something like thiserror's #[error("{var}")]
// then test code could check expected errors are being returned
// instead of just checking that the length matches what we expect
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CardIssue {
    pub level: IssueLevel,
    pub text: String,
}

impl CardIssue {
    fn new<T: Into<String>>(level: IssueLevel, text: T) -> Self {
        Self { level, text: text.into() }
    }
}

impl fmt::Display for CardIssue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.level, self.text)
    }
}

pub fn ci_err<T: Into<String>>(ci: &mut Vec<CardIssue>, text: T) {
    ci.push(CardIssue::new(Error, text));
}

pub fn ci_errs<T: Into<String>>(text: T) -> Box<[CardIssue]> {
    [CardIssue::new(Error, text)].into()
}

pub fn ci_warn<T: Into<String>>(ci: &mut Vec<CardIssue>, text: T) {
    ci.push(CardIssue::new(Warning, text));
}

pub fn ci_warns<T: Into<String>>(text: T) -> Box<[CardIssue]> {
    [CardIssue::new(Warning, text)].into()
}

const fn routine_time(min: u32, secs: u32) -> NaiveTime {
    #[allow(clippy::unwrap_used)]
    NaiveTime::from_hms_opt(0, min, secs).unwrap()
}

#[must_use]
#[rustfmt::skip]
pub const fn get_expected_routine_time(category: &Category) -> Option<NaiveTime> {
    #[allow(clippy::match_same_arms)]
    match category {
        // 12-U
        Category { ag: AG12U, event: Solo,      free: true } => Some(routine_time(2,  0)),
        Category { ag: AG12U, event: Duet,      free: true } => Some(routine_time(2, 30)),
        Category { ag: AG12U, event: MixedDuet, free: true } => Some(routine_time(2, 30)),
        Category { ag: AG12U, event: Team,      free: true } => Some(routine_time(3,  0)),
        Category { ag: AG12U, event: Combo,     free: true } => Some(routine_time(3,  0)),
        // Youth
        Category { ag: Youth, event: Solo,      free: true } => Some(routine_time(2,  0)),
        Category { ag: Youth, event: Duet,      free: true } => Some(routine_time(2, 30)),
        Category { ag: Youth, event: MixedDuet, free: true } => Some(routine_time(2, 30)),
        Category { ag: Youth, event: Team,      free: true } => Some(routine_time(3,  0)),
        Category { ag: Youth, event: Combo,     free: true } => Some(routine_time(3,  0)),
        // JR/SR free
        Category { ag: JRSR, event: Solo,       free: true } => Some(routine_time(2, 15)),
        Category { ag: JRSR, event: Duet,       free: true } => Some(routine_time(2, 45)),
        Category { ag: JRSR, event: MixedDuet,  free: true } => Some(routine_time(2, 45)),
        Category { ag: JRSR, event: Trio,       free: true } => Some(routine_time(2, 45)),
        Category { ag: JRSR, event: Team,       free: true } => Some(routine_time(3, 30)),
        Category { ag: JRSR, event: Acrobatic,  free: true } => Some(routine_time(3,  0)),
        Category { ag: JRSR, event: Combo,      free: true } => Some(routine_time(3, 30)),
        // JR/SR tech
        Category { ag: JRSR, event: Solo,       free: false } => Some(routine_time(2,  0)),
        Category { ag: JRSR, event: Duet,       free: false } => Some(routine_time(2, 20)),
        Category { ag: JRSR, event: MixedDuet,  free: false } => Some(routine_time(2, 20)),
        Category { ag: JRSR, event: Team,       free: false } => Some(routine_time(2, 50)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_age_groups_from() {
        assert_eq!(Into::<AgeGroups>::into("12u"), AG12U);
        assert_eq!(Into::<AgeGroups>::into("Youth"), Youth);
        assert_eq!(Into::<AgeGroups>::into("13-15"), Youth);
        assert_eq!(Into::<AgeGroups>::into("1517"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("1617"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("16-19"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("18-19"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("collegiate"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("jr"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("Junior"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("sr"), JRSR);
        assert_eq!(Into::<AgeGroups>::into("Senior"), JRSR);
        assert_eq!(Into::<AgeGroups>::into(""), AgeGroups::Unknown);
    }

    #[test]
    fn test_event_from() {
        assert_eq!(Into::<Events>::into("acro"), Acrobatic);
        assert_eq!(Into::<Events>::into("Acrobatic"), Acrobatic);
        assert_eq!(Into::<Events>::into("combination"), Combo);
        assert_eq!(Into::<Events>::into("combo"), Combo);
        assert_eq!(Into::<Events>::into("duet"), Duet);
        assert_eq!(Into::<Events>::into("MixedDuet"), MixedDuet);
        assert_eq!(Into::<Events>::into("solo"), Solo);
        assert_eq!(Into::<Events>::into("trio"), Trio);
        assert_eq!(Into::<Events>::into(""), Events::Unknown);
    }
}
