use crate::{DD, MilliDD};
use strum_macros::{Display, EnumString};

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum PairAcroKind {
    #[strum(to_string = "L»", serialize = "L>>")]
    LCrash,
    #[strum(to_string = "L!»", serialize = "L!>>")]
    LHeadDownCrash,
    L,
    #[strum(to_string = "Lf»", serialize = "Lf>>")]
    LFlexCrash,
    #[strum(to_string = "L!f»", serialize = "L!f>>")]
    LHeadDownFlexCrash,
    #[strum(to_string = "L!r0.5»", serialize = "L!r0.5>>")]
    LHeadDownr0_5Crash,
    #[strum(to_string = "L!")]
    LHeadDown,
    #[strum(to_string = "Lf")]
    LFlex,
    #[strum(to_string = "L!r1»", serialize = "L!r1>>")]
    LHeadDownr1Crash,
    #[strum(to_string = "L!fr0.5»", serialize = "L!fr0.5>>")]
    LHeadDownFlexr0_5Crash,
    #[strum(to_string = "Lr0.5")]
    Lr0_5,
    #[strum(to_string = "SL>")]
    SLTravel,
    #[strum(to_string = "L!r0.5")]
    LHeadDownr0_5,
    #[strum(to_string = "Lfr0.5")]
    LFlexr0_5,
    #[strum(to_string = "L!f")]
    LHeadDownFlex,
    #[strum(to_string = "SL!>")]
    SLHeadDownTravel,
    Lr1,
    J,
    #[strum(to_string = "W!»", serialize = "W!>>")]
    WHeadDownCrash,
    #[strum(to_string = "L!r1")]
    LHeadDownr1,
    #[strum(to_string = "L!fr0.5")]
    LHeadDownFlexr0_5,
    #[strum(to_string = "SL!f>")]
    SLHeadDownFlexTravel,
    #[strum(to_string = "SL!r0.5>")]
    SLHeadDownr0_5Travel,
    #[strum(to_string = "Jr0.5")]
    Jr0_5,
    #[strum(to_string = "Jf")]
    JFlex,
    #[strum(to_string = "W!d")]
    WHeadDownd,
    #[strum(to_string = "L!fr1")]
    LHeadDownFlexr1,
    #[strum(to_string = "SL!fr0.5>")]
    SLHeadDownFlexr0_5Travel,
    #[strum(to_string = "W!r0.5")]
    WHeadDownr0_5,
    #[strum(to_string = "W!f")]
    WHeadDownFlex,
    Jd,
    #[strum(to_string = "Js0.5B")]
    Js0_5B,
    #[strum(to_string = "W!s0.5")]
    WHeadDowns0_5,
    #[strum(to_string = "W!fr0.5")]
    WHeadDownFlexr0_5,
    Jpd,
    #[strum(to_string = "W!r1")]
    WHeadDownr1,
    #[strum(to_string = "Jdf")]
    JdFlex,
    #[strum(to_string = "W!fr1")]
    WHeadDownFlexr1,
    #[strum(to_string = "Js0.5t0.5")]
    Js0_5t0_5,
    #[strum(to_string = "W!s0.5t0.5")]
    WHeadDowns0_5t0_5,
    #[strum(to_string = "Js1B")]
    Js1B,
    #[strum(to_string = "W!fr1.5")]
    WHeadDownFlexr1_5,
    #[strum(to_string = "JBs1t0.5")]
    JBs1t0_5,
    #[strum(to_string = "Jfs1B")]
    Jfs1B,
    #[strum(to_string = "Js1F")]
    Js1F,
    #[strum(to_string = "Js1B+f")]
    Js1BPlusFlex,
    #[strum(to_string = "SL!f2+r1>")]
    SLHeadDownFlex2Plusr1Travel,
    #[strum(to_string = "Js1B+pf")]
    Js1BPluspFlex,
    #[strum(to_string = "W!s1F")]
    WHeadDowns1F,
    JsF1B,
}

impl DD for PairAcroKind {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::LCrash => 200,
            Self::LHeadDownCrash => 300,
            Self::L => 500,
            Self::LFlexCrash => 500,
            Self::LHeadDownFlexCrash => 500,
            Self::LHeadDownr0_5Crash => 500,
            Self::LHeadDown => 700,
            Self::LFlex => 700,
            Self::LHeadDownr1Crash => 700,
            Self::LHeadDownFlexr0_5Crash => 700,
            Self::Lr0_5 => 700,
            Self::SLTravel => 900,
            Self::LHeadDownr0_5 => 900,
            Self::LFlexr0_5 => 900,
            Self::LHeadDownFlex => 900,
            Self::SLHeadDownTravel => 900,
            Self::Lr1 => 900,
            Self::J => 900,
            Self::WHeadDownCrash => 900,
            Self::LHeadDownr1 => 1100,
            Self::LHeadDownFlexr0_5 => 1100,
            Self::SLHeadDownFlexTravel => 1100,
            Self::SLHeadDownr0_5Travel => 1100,
            Self::Jr0_5 => 1100,
            Self::JFlex => 1100,
            Self::WHeadDownd => 1100,
            Self::LHeadDownFlexr1 => 1300,
            Self::SLHeadDownFlexr0_5Travel => 1300,
            Self::WHeadDownr0_5 => 1300,
            Self::WHeadDownFlex => 1300,
            Self::Jd => 1300,
            Self::Js0_5B => 1300,
            Self::WHeadDowns0_5 => 1500,
            Self::WHeadDownFlexr0_5 => 1500,
            Self::Jpd => 1500,
            Self::WHeadDownr1 => 1500,
            Self::JdFlex => 1600,
            Self::WHeadDownFlexr1 => 1700,
            Self::Js0_5t0_5 => 1700,
            Self::WHeadDowns0_5t0_5 => 1700,
            Self::Js1B => 1900,
            Self::WHeadDownFlexr1_5 => 19000,
            Self::JBs1t0_5 => 2100,
            Self::Jfs1B => 2100,
            Self::Js1F => 2100,
            Self::Js1BPlusFlex => 2200,
            Self::SLHeadDownFlex2Plusr1Travel => 2200,
            Self::Js1BPluspFlex => 2250,
            Self::WHeadDowns1F => 2300,
            Self::JsF1B => 2300,
        })
    }
}

impl PairAcroKind {
    #[must_use]
    pub const fn is_airborne(&self) -> bool {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::LCrash
            | Self::LHeadDownCrash
            | Self::L
            | Self::LFlexCrash
            | Self::LHeadDownFlexCrash
            | Self::LHeadDownr0_5Crash
            | Self::LHeadDown
            | Self::LFlex
            | Self::LHeadDownr1Crash
            | Self::LHeadDownFlexr0_5Crash
            | Self::Lr0_5
            | Self::SLTravel
            | Self::LHeadDownr0_5
            | Self::LFlexr0_5
            | Self::LHeadDownFlex
            | Self::SLHeadDownTravel
            | Self::Lr1 => false,
            Self::J | Self::WHeadDownCrash => true,
            Self::LHeadDownr1
            | Self::LHeadDownFlexr0_5
            | Self::SLHeadDownFlexTravel
            | Self::SLHeadDownr0_5Travel => false,
            Self::Jr0_5 | Self::JFlex | Self::WHeadDownd => true,
            Self::LHeadDownFlexr1 | Self::SLHeadDownFlexr0_5Travel => false,
            Self::WHeadDownr0_5
            | Self::WHeadDownFlex
            | Self::Jd
            | Self::Js0_5B
            | Self::WHeadDowns0_5
            | Self::WHeadDownFlexr0_5
            | Self::Jpd
            | Self::WHeadDownr1
            | Self::JdFlex
            | Self::WHeadDownFlexr1
            | Self::Js0_5t0_5
            | Self::WHeadDowns0_5t0_5
            | Self::Js1B
            | Self::WHeadDownFlexr1_5
            | Self::JBs1t0_5
            | Self::Jfs1B
            | Self::Js1F
            | Self::Js1BPlusFlex => true,
            Self::SLHeadDownFlex2Plusr1Travel => false,
            Self::Js1BPluspFlex | Self::WHeadDowns1F | Self::JsF1B => true,
        }
    }

    #[must_use]
    pub const fn is_crash(&self) -> bool {
        matches!(
            self,
            Self::LCrash
                | Self::LHeadDownCrash
                | Self::LFlexCrash
                | Self::LHeadDownFlexCrash
                | Self::LHeadDownr0_5Crash
                | Self::LHeadDownr1Crash
                | Self::LHeadDownFlexr0_5Crash
                | Self::WHeadDownCrash
                // these can crash on the surface even though they don't end
                // '»', so we'll include them
                | Self::J
                | Self::JFlex
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_pair_acro() {
        assert!("L>>>".parse::<PairAcroKind>().is_err());
        assert_eq!("L>>".parse::<PairAcroKind>().unwrap(), PairAcroKind::LCrash);
        assert_eq!("L»".parse::<PairAcroKind>().unwrap(), PairAcroKind::LCrash);
    }
}
