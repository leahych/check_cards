use crate::{DD, MilliDD};
use anyhow::anyhow;
use std::fmt::Display;
use std::ops::Deref;
use std::str::FromStr;
use strum::ParseError;
use strum_macros::{Display, EnumString};

trait DDPos2: DD {
    fn dd_pos2(&self) -> MilliDD;
}

pub trait Family {
    fn family() -> &'static str;
}

pub trait FeaturedRotation {
    fn group(&self) -> ARotationGroup;
    fn is_open(&self) -> bool;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Positions<Group: DD + FromStr> {
    pub first: Group,
    pub second: Option<Group>,
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum AConst {
    Thr,
    Shou,
    Feet,
    Sq,
    #[strum(to_string = "2Sup")]
    _2Sup,
    #[strum(to_string = "2SupH")]
    _2SupH,
}

impl DD for AConst {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Thr => 600,
            Self::Shou => 900,
            Self::Feet => 1000,
            Self::Sq => 1200,
            Self::_2Sup => 1000,
            Self::_2SupH => 1100,
        })
    }
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum ADir {
    Up,
    Forw,
    Back,
    Side,
    Rev,
}

impl DD for ADir {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Up => 50,
            Self::Forw => 50,
            Self::Back => 100,
            Self::Side => 200,
            Self::Rev => 400,
        })
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum APos {
    tk,
    pk,
    kt,
    ln,
    sp,
    ja,
    rg,
}

impl DD for APos {
    fn dd(&self) -> MilliDD {
        MilliDD(match self {
            Self::tk => 125,
            Self::pk => 200,
            Self::kt => 50,
            Self::ln => 100,
            Self::sp => 250,
            Self::ja => 175,
            Self::rg => 300,
        })
    }
}

impl DDPos2 for APos {
    fn dd_pos2(&self) -> MilliDD {
        self.dd() / MilliDD(2)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Eq, PartialEq)]
pub enum ARotation {
    t0_5,
    t1,
    t1_5,
    t2,
    t2_5,
    t3,
    f1,
    f1_5,
    f2,
    D,
    dt0_5,
    dt1,
    dt1_5,
    dt2,
    s1,
    s1f,
    ss1,
    ss1f,
    s1t0_5,
    s1t0_5f,
    s1t1,
    s1t1f,
    s1t1_5,
    s1t2,
    ss1t0_5,
    ss1t0_5f,
    ss1t1,
    ss1t1f,
    ss1t1_5,
    ss1t2,
    ss1t2_5,
    ss1t3,
    s1_5,
    s1_5f,
    s1_5o,
    s1_5fo,
    s1_5t0_5,
    s1_5t0_5f,
    s1_5t0_5o,
    s1_5t0_5fo,
    s1_5t1fo,
    s1_5t1,
    s1_5t1_5,
    s2,
    s2o,
    s2f,
    s2fo,
    s2t0_5,
    s2t0_5f,
    s2t0_5o,
    s2t0_5fo,
    s2t1,
    s2t1o,
    s2t1fo,
    s2_5,
    s2_5f,
    s3,
    ss1_5,
    C,
    ct0_5,
    ct1,
    H,
    ht0_5,
    ht1,
    hd,
    hs1,
}

// these are based on what sort of checks are needed, not on which letter
// AQUA has at the start, so many are combined into somersault.
#[derive(Eq, PartialEq)]
pub enum ARotationGroup {
    Somersault,
    StraightSomersault,
    Twist,
    Cartwheel,
    Handspring,
}

impl DD for ARotation {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::t0_5 => 25,
            Self::t1 => 50,
            Self::t1_5 => 100,
            Self::t2 => 200,
            Self::t2_5 => 250,
            Self::t3 => 350,
            Self::f1 => 400,
            Self::f1_5 => 700,
            Self::f2 => 1100,
            Self::D => 75,
            Self::dt0_5 => 100,
            Self::dt1 => 150,
            Self::dt1_5 => 250,
            Self::dt2 => 300,
            Self::s1 => 300,
            Self::s1f => 400,
            Self::ss1 => 500,
            Self::ss1f => 600,
            Self::s1t0_5 => 450,
            Self::s1t0_5f => 550,
            Self::s1t1 => 500,
            Self::s1t1f => 600,
            Self::s1t1_5 => 550,
            Self::s1t2 => 700,
            Self::ss1t0_5 => 600,
            Self::ss1t0_5f => 700,
            Self::ss1t1 => 625,
            Self::ss1t1f => 750,
            Self::ss1t1_5 => 900,
            Self::ss1t2 => 1100,
            Self::ss1t2_5 => 1250,
            Self::ss1t3 => 1500,
            Self::s1_5 => 650,
            Self::s1_5f => 750,
            Self::s1_5o => 1000,
            Self::s1_5fo => 1100,
            Self::s1_5t0_5 => 675,
            Self::s1_5t0_5f => 775,
            Self::s1_5t0_5o => 1200,
            Self::s1_5t0_5fo => 1300,
            Self::s1_5t1fo => 1400,
            Self::s1_5t1 => 800,
            Self::s1_5t1_5 => 975,
            Self::s2 => 900,
            Self::s2o => 1700,
            Self::s2f => 1100,
            Self::s2fo => 1800,
            Self::s2t0_5 => 1200,
            Self::s2t0_5f => 1300,
            Self::s2t0_5o => 1750,
            Self::s2t0_5fo => 1850,
            Self::s2t1 => 1600,
            Self::s2t1o => 2100,
            Self::s2t1fo => 2200,
            Self::s2_5 => 1200,
            Self::s2_5f => 1300,
            Self::s3 => 2000,
            Self::ss1_5 => 1200,
            Self::C => 100,
            Self::ct0_5 => 150,
            Self::ct1 => 175,
            Self::H => 100,
            Self::ht0_5 => 150,
            Self::ht1 => 175,
            Self::hd => 125,
            Self::hs1 => 400,
        })
    }
}

impl FromStr for ARotation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "t0.5" => Ok(Self::t0_5),
            "t1" => Ok(Self::t1),
            "t1.5" => Ok(Self::t1_5),
            "t2" => Ok(Self::t2),
            "t2.5" => Ok(Self::t2_5),
            "t3" => Ok(Self::t3),
            "f1" => Ok(Self::f1),
            "f1.5" => Ok(Self::f1_5),
            "f2" => Ok(Self::f2),
            "D" => Ok(Self::D),
            "dt0.5" => Ok(Self::dt0_5),
            "dt1" => Ok(Self::dt1),
            "dt1.5" => Ok(Self::dt1_5),
            "dt2" => Ok(Self::dt2),
            "s1" => Ok(Self::s1),
            "s1f" => Ok(Self::s1f),
            "ss1" => Ok(Self::ss1),
            "ss1f" => Ok(Self::ss1f),
            "s1t0.5" => Ok(Self::s1t0_5),
            "s1t0.5f" => Ok(Self::s1t0_5f),
            "s1t1" => Ok(Self::s1t1),
            "s1t1f" => Ok(Self::s1t1f),
            "s1t1.5" => Ok(Self::s1t1_5),
            "s1t2" => Ok(Self::s1t2),
            "ss1t0.5" => Ok(Self::ss1t0_5),
            "ss1t0.5f" => Ok(Self::ss1t0_5f),
            "ss1t1" => Ok(Self::ss1t1),
            "ss1t1f" => Ok(Self::ss1t1f),
            "ss1t1.5" => Ok(Self::ss1t1_5),
            "ss1t2" => Ok(Self::ss1t2),
            "ss1t2.5" => Ok(Self::ss1t2_5),
            "ss1t3" => Ok(Self::ss1t3),
            "s1.5" => Ok(Self::s1_5),
            "s1.5f" => Ok(Self::s1_5f),
            "s1.5o" => Ok(Self::s1_5o),
            "s1.5fo" => Ok(Self::s1_5fo),
            "s1.5t0.5" => Ok(Self::s1_5t0_5),
            "s1.5t0.5f" => Ok(Self::s1_5t0_5f),
            "s1.5t0.5o" => Ok(Self::s1_5t0_5o),
            "s1.5t0.5fo" => Ok(Self::s1_5t0_5fo),
            "s1.5t1fo" => Ok(Self::s1_5t1fo),
            "s1.5t1" => Ok(Self::s1_5t1),
            "s1.5t1.5" => Ok(Self::s1_5t1_5),
            "s2" => Ok(Self::s2),
            "s2o" => Ok(Self::s2o),
            "s2f" => Ok(Self::s2f),
            "s2fo" => Ok(Self::s2fo),
            "s2t0.5" => Ok(Self::s2t0_5),
            "s2t0.5f" => Ok(Self::s2t0_5f),
            "s2t0.5o" => Ok(Self::s2t0_5o),
            "s2t0.5fo" => Ok(Self::s2t0_5fo),
            "s2t1" => Ok(Self::s2t1),
            "s2t1o" => Ok(Self::s2t1o),
            "s2t1fo" => Ok(Self::s2t1fo),
            "s2.5" => Ok(Self::s2_5),
            "s2.5f" => Ok(Self::s2_5f),
            "s3" => Ok(Self::s3),
            "ss1.5" => Ok(Self::ss1_5),
            "C" => Ok(Self::C),
            "ct0.5" => Ok(Self::ct0_5),
            "ct1" => Ok(Self::ct1),
            "H" => Ok(Self::H),
            "ht0.5" => Ok(Self::ht0_5),
            "ht1" => Ok(Self::ht1),
            "hd" => Ok(Self::hd),
            "hs1" => Ok(Self::hs1),
            &_ => Err(anyhow!("unknown airborne rotation {s}")),
        }
    }
}

impl FeaturedRotation for ARotation {
    fn group(&self) -> ARotationGroup {
        use ARotationGroup::*;

        #[allow(clippy::match_same_arms)]
        match self {
            Self::t0_5 | Self::t1 | Self::t1_5 | Self::t2 | Self::t2_5 | Self::t3 => Twist,
            Self::f1
            | Self::f1_5
            | Self::f2
            | Self::D
            | Self::dt0_5
            | Self::dt1
            | Self::dt1_5
            | Self::dt2
            | Self::s1
            | Self::s1f => Somersault,
            Self::ss1 | Self::ss1f => StraightSomersault,
            Self::s1t0_5 | Self::s1t0_5f | Self::s1t1 | Self::s1t1f | Self::s1t1_5 | Self::s1t2 => {
                Somersault
            }
            Self::ss1t0_5
            | Self::ss1t0_5f
            | Self::ss1t1
            | Self::ss1t1f
            | Self::ss1t1_5
            | Self::ss1t2
            | Self::ss1t2_5
            | Self::ss1t3 => StraightSomersault,
            Self::s1_5
            | Self::s1_5f
            | Self::s1_5o
            | Self::s1_5fo
            | Self::s1_5t0_5
            | Self::s1_5t0_5f
            | Self::s1_5t0_5o
            | Self::s1_5t0_5fo
            | Self::s1_5t1fo
            | Self::s1_5t1
            | Self::s1_5t1_5
            | Self::s2
            | Self::s2o
            | Self::s2f
            | Self::s2fo
            | Self::s2t0_5
            | Self::s2t0_5f
            | Self::s2t0_5o
            | Self::s2t0_5fo
            | Self::s2t1
            | Self::s2t1o
            | Self::s2t1fo
            | Self::s2_5
            | Self::s2_5f
            | Self::s3 => Somersault,
            Self::ss1_5 => StraightSomersault,
            Self::C | Self::ct0_5 | Self::ct1 => Cartwheel,
            Self::H | Self::ht0_5 | Self::ht1 | Self::hd | Self::hs1 => Handspring,
        }
    }

    fn is_open(&self) -> bool {
        matches!(
            self,
            Self::s1_5o
                | Self::s1_5fo
                | Self::s1_5t0_5o
                | Self::s1_5t0_5fo
                | Self::s1_5t1fo
                | Self::s2o
                | Self::s2fo
                | Self::s2t0_5o
                | Self::s2t0_5fo
                | Self::s2t1o
                | Self::s2t1fo
        )
    }
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum ABonus {
    Dbl,
    Pos3,
    Grip,
    Conn,
    Catch,
    Split,
    Hula,
    RetSq,
    RetPa,
    Feet,
}

impl DD for ABonus {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Dbl => 200,
            Self::Pos3 => 50,
            Self::Grip => 100,
            Self::Conn => 100,
            Self::Catch => 150,
            Self::Split => 200,
            Self::Hula => 350,
            Self::RetSq => 800,
            Self::RetPa => 600,
            Self::Feet => 75,
        })
    }
}

impl ABonus {
    #[must_use]
    pub const fn required_consts(&self) -> &'static [AConst] {
        // Feet and Split restrictions aren't explicit but based on my
        // reading of the acro catalog.
        match self {
            Self::Feet => &[AConst::Feet, AConst::_2Sup, AConst::_2SupH],
            Self::RetPa => &[AConst::Shou],
            Self::RetSq => &[AConst::Sq],
            Self::Split => &[AConst::Thr, AConst::_2Sup, AConst::_2SupH],
            _ => &[],
        }
    }

    #[must_use]
    pub const fn required_dir(&self) -> Option<ADir> {
        match self {
            Self::Hula | Self::RetSq | Self::RetPa => Some(ADir::Up),
            _ => None,
        }
    }

    #[must_use]
    pub const fn required_positions(&self) -> &'static [APos] {
        match self {
            Self::Hula => &[APos::ja, APos::rg],
            Self::Split => &[APos::tk, APos::ln, APos::kt],
            _ => &[],
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AcroA {
    pub construction: AConst,
    pub dir: ADir,
    pub positions: Positions<APos>,
    pub bonuses: Box<[ABonus]>,
    pub rotation: Option<ARotation>,
}

impl DD for AcroA {
    fn dd(&self) -> MilliDD {
        self.construction.dd()
            + self.dir.dd()
            + self.positions.dd()
            + self.bonuses.iter().map(DD::dd).sum::<MilliDD>()
            + self.rotation.as_ref().map_or(MilliDD(0), DD::dd)
    }
}

impl Family for &AcroA {
    fn family() -> &'static str {
        "Airborne"
    }
}

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum BConst {
    St,
    StH,
    #[strum(to_string = "2SupU")]
    _2SupU,
    #[strum(to_string = "2SupD")]
    _2SupD,
    #[strum(to_string = "2SupM")]
    _2SupM,
    #[strum(to_string = "2SupD2F")]
    _2SupD2F,
    L,
    #[strum(to_string = "L2F+")]
    L2FPlus,
    #[strum(to_string = "St>")]
    StTransitional,
    LH,
    Lh2F,
}

impl DD for BConst {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::St => 1000,
            Self::StH => 1100,
            Self::_2SupU => 1150,
            Self::_2SupD => 1140,
            Self::_2SupM => 1250,
            Self::_2SupD2F => 1550,
            Self::L => 700,
            Self::L2FPlus => 800,
            Self::StTransitional => 1025,
            Self::LH => 1100,
            Self::Lh2F => 1200,
        })
    }
}

impl BConst {
    #[must_use]
    pub const fn required_conns(&self) -> &'static [BConn] {
        use BConn::*;
        #[allow(clippy::match_same_arms)]
        match self {
            Self::St => &[
                _1P1P, _1PPx, PP, FP, SiSb, Bp, E, PHSlash, AP, SiS, FS, F1S, Tw, SPlus, _1PH,
                _1F1P, _1F1F,
            ],
            Self::StH => &[_1P1F, FF, FFSlash, PF, ShF, LayF, SiF, SPlus, _1F1F, H1FSlash, HTPlus],
            Self::_2SupU => &[Le, _1FHPlus1FP, PP2],
            Self::_2SupD => &[Tow],
            Self::_2SupM => &[Le, Ch],
            Self::_2SupD2F => &[Tow],
            Self::L | Self::L2FPlus => &[Li],
            Self::StTransitional => &[PP, PF, Bp, ShF, E, F1S, LayF, _1P1F, _2pH, PHSlash],
            Self::LH | Self::Lh2F => &[LiH],
        }
    }
}

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum BConn {
    #[strum(to_string = "1P1P")]
    _1P1P,
    #[strum(to_string = "1P1F")]
    _1P1F,
    #[strum(to_string = "1PPx")]
    _1PPx,
    PP,
    FP,
    FF,
    #[strum(to_string = "FF/")]
    FFSlash,
    PF,
    SiSb,
    Bp,
    ShF,
    E,
    #[strum(to_string = "PH/")]
    PHSlash,
    LiH,
    AP,
    SiS,
    FS,
    F1S,
    Le,
    Tow,
    Li,
    Ch,
    Tw,
    LayF,
    SiF,
    #[strum(to_string = "1FH+1FP")]
    _1FHPlus1FP,
    #[strum(to_string = "S+")]
    SPlus,
    #[strum(to_string = "1F1P")]
    _1F1P,
    #[strum(to_string = "1F1F")]
    _1F1F,
    #[strum(to_string = "1PH")]
    _1PH,
    PP2,
    #[strum(to_string = "2pH")]
    _2pH,
    #[strum(to_string = "H1F/")]
    H1FSlash,
    #[strum(to_string = "HT+")]
    HTPlus,
}

impl DD for BConn {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::_1P1P => 1300,
            Self::_1P1F => 1050,
            Self::_1PPx => 1000,
            Self::PP => 800,
            Self::FP => 700,
            Self::FF => 600,
            Self::FFSlash => 275,
            Self::PF => 450,
            Self::SiSb => 500,
            Self::Bp => 150,
            Self::ShF => 400,
            Self::E => 375,
            Self::PHSlash => 850,
            Self::LiH => 450,
            Self::AP => 350,
            Self::SiS => 100,
            Self::FS => 25,
            Self::F1S => 125,
            Self::Le => 150,
            Self::Tow => 250,
            Self::Li => 75,
            Self::Ch => 300,
            Self::Tw => 100,
            Self::LayF => 175,
            Self::SiF => 200,
            Self::_1FHPlus1FP => 550,
            Self::SPlus => 50,
            Self::_1F1P => 1250,
            Self::_1F1F => 1100,
            Self::_1PH => 1400,
            Self::PP2 => 650,
            Self::_2pH => 775,
            Self::H1FSlash => 950,
            Self::HTPlus => 750,
        })
    }
}

impl DDPos2 for BPos {
    fn dd_pos2(&self) -> MilliDD {
        // all other positions are half the DD but these
        // tow are special. Probably typos but since this
        // what is in the manual and what ISS produces,
        // we need to ensure we match. AQUA has been
        // notified, we'll see if they fix it.
        const OW_DD_2POS: MilliDD = MilliDD(175);
        const VS_DD_2POS: MilliDD = MilliDD(100);
        match self {
            Self::ow => OW_DD_2POS,
            Self::vs => VS_DD_2POS,
            _ => self.dd() / MilliDD(2),
        }
    }
}

impl BConn {
    #[must_use]
    // a few of these aren't handstands, but have the same sort of movement
    // so they should also probably start in bamboo
    pub const fn is_handstand(&self) -> bool {
        matches!(
            self,
            Self::_1P1P
                | Self::_1P1F
                | Self::_1PPx
                | Self::PP
                | Self::PF
                | Self::PHSlash
                | Self::PP2
                | Self::_1PH
                | Self::_2pH
                | Self::H1FSlash
                | Self::HTPlus
                | Self::ShF
                | Self::E
        )
    }

    #[must_use]
    pub const fn is_only_one_leg(&self) -> bool {
        matches!(self, Self::F1S | Self::_1F1P | Self::_1F1F)
    }

    #[must_use]
    pub const fn is_one_leg(&self) -> bool {
        self.is_only_one_leg() || matches!(self, Self::FP)
    }

    #[must_use]
    pub const fn expects_head_down_pos(&self) -> bool {
        matches!(
            self,
            Self::_1P1P
                | Self::_1P1F
                | Self::_1PPx
                | Self::PP
                | Self::PF
                | Self::Bp
                | Self::ShF
                | Self::E
                | Self::PHSlash
                | Self::Tw
                | Self::_1PH
                | Self::_2pH
                | Self::H1FSlash
                | Self::HTPlus
        )
    }

    #[must_use]
    pub const fn expects_head_up_pos(&self) -> bool {
        // TODO is this over aggressive? Ex. can owl be done with some of these?
        matches!(
            self,
            Self::FP
                | Self::FF
                | Self::FFSlash
                | Self::SiSb
                | Self::SiF
                | Self::_1FHPlus1FP
                | Self::_1F1P
                | Self::_1F1F
        )
    }

    #[must_use]
    pub const fn required_consts(&self) -> &'static [BConst] {
        use BConst::*;
        #[allow(clippy::match_same_arms)]
        match self {
            Self::_1P1P => &[St],
            Self::_1P1F => &[StH, StTransitional],
            Self::_1PPx => &[St],
            Self::PP => &[St, StTransitional],
            Self::FP => &[St],
            Self::FF => &[StH],
            Self::FFSlash => &[StH],
            Self::PF => &[StH, StTransitional],
            Self::SiSb => &[St],
            Self::Bp => &[St, StTransitional],
            Self::ShF => &[StH, StTransitional],
            Self::E => &[St, StTransitional],
            Self::PHSlash => &[St, StTransitional],
            Self::LiH => &[LH, Lh2F],
            Self::AP => &[St],
            Self::SiS => &[St],
            Self::FS => &[St],
            Self::F1S => &[St, StTransitional],
            Self::Le => &[_2SupU, _2SupM],
            Self::Tow => &[_2SupD, _2SupD2F],
            Self::Li => &[L, L2FPlus],
            Self::Ch => &[_2SupM],
            Self::Tw => &[St],
            Self::LayF => &[StH, StTransitional],
            Self::SiF => &[StH],
            Self::_1FHPlus1FP => &[_2SupU],
            Self::SPlus => &[St, StH],
            Self::_1F1P => &[St],
            Self::_1F1F => &[St, StH],
            Self::_1PH => &[St],
            Self::PP2 => &[_2SupU],
            Self::_2pH => &[StTransitional],
            Self::H1FSlash => &[StH],
            Self::HTPlus => &[StH],
        }
    }

    #[must_use]
    pub const fn required_positions(&self) -> &'static [BPos] {
        match self {
            Self::FF | Self::FFSlash | Self::FS => BPOS_TWO_FOOT,
            _ => &[],
        }
    }

    #[must_use]
    pub const fn required_rotations(&self) -> &'static [BRotationGroup] {
        use BRotationGroup::*;

        #[allow(clippy::match_same_arms)]
        match self {
            Self::_1P1P => &[Bang],
            Self::_1P1F => &[Bang],
            Self::_1PPx => &[Bang],
            Self::PP => &[Plain, Bang],
            Self::FP => &[Plain, Plus],
            Self::FF => &[Bang],
            Self::FFSlash => &[Bang],
            Self::PF => &[Bang],
            Self::SiSb => &[Plain],
            Self::Bp => &[Plain],
            Self::ShF => &[Bang],
            Self::E => &[Plain],
            Self::PHSlash => &[Bang],
            Self::LiH => &[L],
            Self::AP => &[Plain],
            Self::SiS => &[Plain],
            Self::FS => &[Slash],
            Self::F1S => &[Plain, Plus],
            Self::Le => &[],
            Self::Tow => &[],
            Self::Li => &[L],
            Self::Ch => &[],
            Self::Tw => &[Plain],
            Self::LayF => &[Bang],
            Self::SiF => &[Bang],
            Self::_1FHPlus1FP => &[], // doesn't say no rotations, but none listed
            Self::SPlus => &[Plain, Bang],
            Self::_1F1P => &[Plain, Plus],
            Self::_1F1F => &[Plain, Bang],
            Self::_1PH => &[Bang],
            Self::PP2 => &[Bang],
            Self::_2pH => &[Bang],
            Self::H1FSlash => &[Bang],
            Self::HTPlus => &[Bang],
        }
    }
}

pub enum BPosGroup {
    OneLeg,
    TwoLeg,
    Free,
    Horizontal,
    HeadDown,
    ExtremeFlex,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum BPos {
    he,
    vs,
    gl,
    ba,
    sa,
    ne,
    ey,
    sd,
    mo,
    pp,
    ct,
    sh,
    hp,
    fl,
    tu,
    co,
    spl,
    so,
    pi,
    bb,
    bo,
    ff,
    wi,
    br,
    ow,
    ma,
    dr,
    qu,
    sn,
}

const BPOS_TWO_FOOT: &[BPos] = &[BPos::sd, BPos::mo, BPos::sh, BPos::dr];

impl DD for BPos {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::he => 125,
            Self::vs => 300,
            Self::gl => 400,
            Self::ba => 150,
            Self::sa => 325,
            Self::ne => 425,
            Self::ey => 500,
            Self::sd => 50,
            Self::mo => 75,
            Self::pp => 100,
            Self::ct => 25,
            Self::sh => 175,
            Self::hp => 450,
            Self::fl => 225,
            Self::tu => 375,
            Self::co => 150,
            Self::spl => 280,
            Self::so => 275,
            Self::pi => 600,
            Self::bb => 150,
            Self::bo => 200,
            Self::ff => 275,
            Self::wi => 250,
            Self::br => 325,
            Self::ow => 300,
            Self::ma => 350,
            Self::dr => 800,
            Self::qu => 700,
            Self::sn => 475,
        })
    }
}

impl BPos {
    #[must_use]
    pub const fn is_free(&self) -> bool {
        matches!(self, Self::mo | Self::pp | Self::ct | Self::sh | Self::hp | Self::fl | Self::tu)
    }

    #[must_use]
    pub const fn is_horizontal(&self) -> bool {
        self.is_free() || matches!(self, |Self::co| Self::spl | Self::so | Self::pi)
    }

    #[must_use]
    pub const fn is_head_up(&self) -> bool {
        matches!(
            self,
            Self::he
                | Self::vs
                | Self::gl
                | Self::ba
                | Self::sa
                | Self::ne
                | Self::ey
                | Self::sd
                | Self::co
                | Self::spl
                | Self::so
                | Self::pi
        )
    }

    #[must_use]
    pub const fn could_be_head_down(&self) -> bool {
        self.is_free() || self.definitely_head_down()
    }

    #[must_use]
    pub const fn definitely_head_down(&self) -> bool {
        matches!(
            self,
            Self::bb
                | Self::bo
                | Self::ff
                | Self::wi
                | Self::br
                | Self::ow
                | Self::ma
                | Self::dr
                | Self::qu
                | Self::sn
        )
    }

    // The extreme flex positions can be done in a ballet leg platform
    // with a foot under the hips, and that can count as laying on the
    // foot, so we'll add those positions what is allowed.
    //
    // Positions such as willow or bridge can also be done where the
    // torso is vertical but the athlete is laying on a foot, so we'll
    // also allow those positions.
    #[must_use]
    pub const fn is_laying(&self) -> bool {
        self.is_horizontal() || matches!(self, Self::dr | Self::qu | Self::sn | Self::wi | Self::br)
    }

    #[must_use]
    pub const fn is_one_foot(&self) -> bool {
        matches!(
            self,
            Self::he
                | Self::vs
                | Self::gl
                | Self::ba
                | Self::sa
                | Self::ne
                | Self::ey
                | Self::qu
                | Self::sn
        )
    }

    // not the category but positions that can be done by standing w/two feet
    #[must_use]
    pub const fn is_two_foot(&self) -> bool {
        matches!(self, Self::sd | Self::mo | Self::sh | Self::dr)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum BRotation {
    #[strum(to_string = "r0.5/")]
    r0_5Slash,
    #[strum(to_string = "r1/")]
    r1Slash,
    #[strum(to_string = "r1.5/")]
    r1_5Slash,
    #[strum(to_string = "r0.5")]
    r0_5,
    #[strum(to_string = "r1")]
    r1,
    #[strum(to_string = "r1.5")]
    r1_5,
    #[strum(to_string = "r0.5+")]
    r0_5Plus,
    #[strum(to_string = "r1+")]
    r1Plus,
    #[strum(to_string = "r1.5+")]
    r1_5Plus,
    #[strum(to_string = "r2+")]
    r2Plus,
    #[strum(to_string = "r0.5!")]
    r0_5Bang,
    #[strum(to_string = "r1!")]
    r1Bang,
    #[strum(to_string = "r1.5!")]
    r1_5Bang,
    #[strum(to_string = "r2!")]
    r2Bang,
    #[strum(to_string = "r/L")]
    rSlashL,
    #[strum(to_string = "r0.5L")]
    r0_5L,
    r1L,
}

impl DD for BRotation {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::r0_5Slash => 50,
            Self::r1Slash => 100,
            Self::r1_5Slash => 150,
            Self::r0_5 => 100,
            Self::r1 => 200,
            Self::r1_5 => 300,
            Self::r0_5Plus => 125,
            Self::r1Plus => 225,
            Self::r1_5Plus => 325,
            Self::r2Plus => 425,
            Self::r0_5Bang => 150,
            Self::r1Bang => 250,
            Self::r1_5Bang => 350,
            Self::r2Bang => 450,
            Self::rSlashL => 400,
            Self::r0_5L => 500,
            Self::r1L => 800,
        })
    }
}

#[derive(Eq, PartialEq)]
pub enum BRotationGroup {
    Slash,
    Plain,
    Plus,
    Bang,
    L,
}

impl Display for BRotationGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Slash => write!(f, "r/"),
            Self::Plain => write!(f, "r"),
            Self::Plus => write!(f, "r+"),
            Self::Bang => write!(f, "r!"),
            Self::L => write!(f, "rL"),
        }
    }
}

impl BRotationGroup {
    #[must_use]
    pub const fn required_conns(self) -> &'static [BConn] {
        use BConn::*;
        match self {
            Self::Slash => &[FS],
            Self::Plain => &[FP, SiSb, Bp, E, AP, SiS, F1S, Tw, _1F1P, SPlus, PP, _1F1F],
            Self::Plus => &[F1S, _1F1P, _1F1F, FP],
            Self::Bang => &[
                _1P1P, _1P1F, _1PPx, PF, PHSlash, PP2, _2pH, _1PH, PP, FF, FFSlash, ShF, LayF, SiF,
                _1F1F, H1FSlash, HTPlus, _1P1F, PF, SPlus,
            ],
            Self::L => &[LiH, Li],
        }
    }
}

impl BRotation {
    #[must_use]
    pub const fn group(&self) -> BRotationGroup {
        use BRotationGroup::*;
        match self {
            Self::r0_5Slash | Self::r1Slash | Self::r1_5Slash => Slash,
            Self::r0_5 | Self::r1 | Self::r1_5 => Plain,
            Self::r0_5Plus | Self::r1Plus | Self::r1_5Plus | Self::r2Plus => Plus,
            Self::r0_5Bang | Self::r1Bang | Self::r1_5Bang | Self::r2Bang => Bang,
            Self::rSlashL | Self::r0_5L | Self::r1L => L,
        }
    }
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum BBonus {
    Dbl,
    Pos3,
    Twirl,
    RotF,
    SdUp,
    Wave,
    Moon,
    Mov,
    Hold,
}

impl DD for BBonus {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Dbl => 200,
            Self::Pos3 => 50,
            Self::Twirl => 100,
            Self::RotF => 200,
            Self::SdUp => 100,
            Self::Wave => 100,
            Self::Moon => 275,
            Self::Mov => 700,
            Self::Hold => 500,
        })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AcroB {
    pub construction: BConst,
    pub conn: BConn,
    pub positions: Positions<BPos>,
    pub bonuses: Box<[BBonus]>,
    pub rotation: Option<BRotation>,
}

impl DD for AcroB {
    fn dd(&self) -> MilliDD {
        self.construction.dd()
            + self.conn.dd()
            + self.positions.dd()
            + self.bonuses.iter().map(DD::dd).sum::<MilliDD>()
            + self.rotation.as_ref().map_or(MilliDD(0), DD::dd)
    }
}

impl Family for &AcroB {
    fn family() -> &'static str {
        "Balance"
    }
}

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum CConst {
    #[strum(to_string = "Thr>St")]
    ThrOntoSt,
    #[strum(to_string = "Thr>StH")]
    ThrOntoStH,
    #[strum(to_string = "Thr>Pair>")]
    ThrOntoPair,
    #[strum(to_string = "Thr>FF")]
    ThrOntoFF,
    #[strum(to_string = "Thr>F")]
    ThrOntoF,
    #[strum(to_string = "Thr^Lh")]
    ThrAboveLh,
    #[strum(to_string = "Thr^2F")]
    ThrAbove2F,
    #[strum(to_string = "L+spot")]
    LPlusSpot,
    #[strum(to_string = "Thr>hand")]
    ThrOntohand,
    #[strum(to_string = "Thr+Thr")]
    ThrPlusThr,
    #[strum(to_string = "Sn")]
    Sn,
    #[strum(to_string = "Thr>head>")]
    ThrOntohead,
    #[strum(to_string = "2Sup+")]
    _2SupPlus,
    #[strum(to_string = "Thr>Sq")]
    ThrOntoSq,
    #[strum(to_string = "Thr>St2")]
    ThrOntoSt2,
    #[strum(to_string = "Thr>StH>1F")]
    ThrOntoStHOnto1F,
}

impl DD for CConst {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::ThrOntoSt => 1125,
            Self::ThrOntoStH => 1200,
            Self::ThrOntoPair => 675,
            Self::ThrOntoFF => 1000,
            Self::ThrOntoF => 875,
            Self::ThrAboveLh => 1575,
            Self::ThrAbove2F => 1225,
            Self::LPlusSpot => 775,
            Self::ThrOntohand => 800,
            Self::ThrPlusThr => 1250,
            Self::Sn => 1175,
            Self::ThrOntohead => 1075,
            Self::_2SupPlus => 1050,
            Self::ThrOntoSq => 1150,
            Self::ThrOntoSt2 => 1200,
            Self::ThrOntoStHOnto1F => 1300,
        })
    }
}

impl CConst {
    #[must_use]
    pub const fn required_dir(&self) -> Option<CDir> {
        match &self {
            Self::_2SupPlus => Some(CDir::Base(ADir::Up)),
            _ => None,
        }
    }

    #[must_use]
    pub const fn required_rotations(&self) -> &[CBRotationGroup] {
        match self {
            Self::ThrOntoSt | Self::ThrOntoSt2 => &[CBRotationGroup::Plain, CBRotationGroup::Bang],
            Self::ThrOntoStH | Self::ThrOntoStHOnto1F => &[CBRotationGroup::Bang],
            Self::ThrAboveLh => &[CBRotationGroup::L],
            Self::ThrOntoFF | Self::ThrOntoF | Self::ThrOntohand => &[CBRotationGroup::P],
            Self::ThrAbove2F => &[CBRotationGroup::_2F],
            Self::ThrOntoPair
            | Self::ThrPlusThr
            | Self::Sn
            | Self::_2SupPlus
            | Self::ThrOntohead
            | Self::LPlusSpot
            | Self::ThrOntoSq => &[],
        }
    }

    #[must_use]
    pub const fn is_flyabove(&self) -> bool {
        matches!(self, Self::ThrAbove2F | Self::ThrAboveLh)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum CDir {
    Base(ADir),
    Bln,
}

impl Display for CDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Bln => write!(f, "Bln"),
            Self::Base(base) => base.fmt(f),
        }
    }
}

impl FromStr for CDir {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "Bln" { Ok(Self::Bln) } else { Ok(Self::Base(s.parse::<ADir>()?)) }
    }
}

impl DD for CDir {
    fn dd(&self) -> MilliDD {
        match self {
            Self::Base(dir) => dir.dd(),
            Self::Bln => MilliDD(200),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum CPos {
    A(APos),
    B(BPos),
}

impl DD for CPos {
    fn dd(&self) -> MilliDD {
        match self {
            Self::A(pos) => pos.dd(),
            Self::B(pos) => pos.dd(),
        }
    }
}

impl DDPos2 for CPos {
    fn dd_pos2(&self) -> MilliDD {
        match self {
            Self::A(pos) => pos.dd_pos2(),
            Self::B(pos) => pos.dd_pos2(),
        }
    }
}

impl FromStr for CPos {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<APos>().map_or_else(
            |_| {
                s.parse::<BPos>()
                    .map_or_else(|_| Err(anyhow!("unknown position: '{s}'")), |b| Ok(Self::B(b)))
            },
            |pos| Ok(Self::A(pos)),
        )
    }
}

#[derive(Eq, PartialEq)]
pub enum CBRotationGroup {
    Plain,
    Bang,
    L,
    P,
    _2F,
}

impl Display for CBRotationGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plain => write!(f, "Cr"),
            Self::Bang => write!(f, "Cr!"),
            Self::L => write!(f, "CrL"),
            Self::P => write!(f, "CP"),
            Self::_2F => write!(f, "2F"),
        }
    }
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum CBRotation {
    #[strum(to_string = "Cr0.5")]
    Cr0_5,
    Cr1,
    #[strum(to_string = "Cr1.5")]
    Cr1_5,
    #[strum(to_string = "Cr0.5!")]
    Cr0_5Bang,
    #[strum(to_string = "Cr1!")]
    Cr1Bang,
    #[strum(to_string = "Cr1.5!")]
    Cr1_5Bang,
    #[strum(to_string = "Cr0.5L")]
    Cr0_5L,
    #[strum(to_string = "CP0.5")]
    CP0_5,
    #[strum(to_string = "2F0.5")]
    _2F0_5,
    #[strum(to_string = "2F1")]
    _2F1,
}

impl DD for CBRotation {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Cr0_5 => 200,
            Self::Cr1 => 300,
            Self::Cr1_5 => 400,
            Self::Cr0_5Bang => 300,
            Self::Cr1Bang => 400,
            Self::Cr1_5Bang => 500,
            Self::Cr0_5L => 400,
            Self::CP0_5 => 400,
            Self::_2F0_5 => 250,
            Self::_2F1 => 350,
        })
    }
}

impl CBRotation {
    #[must_use]
    pub const fn group(&self) -> CBRotationGroup {
        match self {
            Self::Cr0_5 | Self::Cr1 | Self::Cr1_5 => CBRotationGroup::Plain,
            Self::Cr0_5Bang | Self::Cr1Bang | Self::Cr1_5Bang => CBRotationGroup::Bang,
            Self::Cr0_5L => CBRotationGroup::L,
            Self::CP0_5 => CBRotationGroup::P,
            Self::_2F0_5 | Self::_2F1 => CBRotationGroup::_2F,
        }
    }

    #[must_use]
    pub const fn required_consts(&self) -> &[CConst] {
        use CConst::*;
        match self {
            Self::Cr0_5 | Self::Cr1 | Self::Cr1_5 => &[ThrOntoSt, ThrOntoSt2],
            Self::Cr0_5Bang | Self::Cr1Bang | Self::Cr1_5Bang => {
                &[ThrOntoStH, ThrOntoStHOnto1F, ThrOntoSt, ThrOntoSt2]
            }
            Self::Cr0_5L => &[ThrAboveLh],
            Self::CP0_5 => &[ThrOntoF, ThrOntoFF, ThrOntohand],
            Self::_2F0_5 | Self::_2F1 => &[ThrAbove2F],
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum CFRotation {
    Ct0_5,
    Ct1,
    Ct1_5,
    Ct2,
    Ct2_5,
    Ct3,
    Cd,
    Cdt0_5,
    Cdt1,
    Cdt1_5,
    Cs1,
    Css1,
    Cs1_5,
    Cs1_5o,
    Cf1,
    Cf1_5,
    Cc,
    Cct0_5,
    Cct1,
    Ch,
    Cht0_5,
    Cht1,
    Cs1t0_5,
    Cs1t1,
    Cs1t1_5,
    Cs1t2,
    Css1t0_5,
    Css1t1,
    Css1t1_5,
    Css1t2,
    Cs1t1o,
    Cs1t1_5o,
    Cs1t2o,
    Chs0_5,
}

impl FromStr for CFRotation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Ct0.5" => Ok(Self::Ct0_5),
            "Ct1" => Ok(Self::Ct1),
            "Ct1.5" => Ok(Self::Ct1_5),
            "Ct2" => Ok(Self::Ct2),
            "Ct2_5" => Ok(Self::Ct2_5),
            "Ct3" => Ok(Self::Ct3),
            "Cd" => Ok(Self::Cd),
            "Cdt0.5" => Ok(Self::Cdt0_5),
            "Cdt1" => Ok(Self::Cdt1),
            "Cdt1.5" => Ok(Self::Cdt1_5),
            "Cs1" => Ok(Self::Cs1),
            "Css1" => Ok(Self::Css1),
            "Cs1.5" => Ok(Self::Cs1_5),
            "Cs1.50.5" => Ok(Self::Cs1_5o),
            "Cf1" => Ok(Self::Cf1),
            "Cf1.5" => Ok(Self::Cf1_5),
            "Cc" => Ok(Self::Cc),
            "Cct0.5" => Ok(Self::Cct0_5),
            "Cct1" => Ok(Self::Cct1),
            "Ch" => Ok(Self::Ch),
            "Cht0.5" => Ok(Self::Cht0_5),
            "Cht1" => Ok(Self::Cht1),
            "Cs1t0.5" => Ok(Self::Cs1t0_5),
            "Cs1t1" => Ok(Self::Cs1t1),
            "Cs1t1.5" => Ok(Self::Cs1t1_5),
            "Cs1t2" => Ok(Self::Cs1t2),
            "Css1t0.5" => Ok(Self::Css1t0_5),
            "Css1t1" => Ok(Self::Css1t1),
            "Css1t1.5" => Ok(Self::Css1t1_5),
            "Css1t2" => Ok(Self::Css1t2),
            "Cs1t1o" => Ok(Self::Cs1t1o),
            "Cs1t1_5o" => Ok(Self::Cs1t1_5o),
            "Cs1t2o" => Ok(Self::Cs1t2o),
            "Chs0.5" => Ok(Self::Chs0_5),
            _ => Err(anyhow!("unknown rotation: '{s}'")),
        }
    }
}

impl DD for CFRotation {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Ct0_5 => 50,
            Self::Ct1 => 100,
            Self::Ct1_5 => 200,
            Self::Ct2 => 300,
            Self::Ct2_5 => 400,
            Self::Ct3 => 500,
            Self::Cd => 100,
            Self::Cdt0_5 => 150,
            Self::Cdt1 => 200,
            Self::Cdt1_5 => 300,
            Self::Cs1 => 250,
            Self::Css1 => 350,
            Self::Cs1_5 => 450,
            Self::Cs1_5o => 550,
            Self::Cf1 => 350,
            Self::Cf1_5 => 550,
            Self::Cc => 100,
            Self::Cct0_5 => 150,
            Self::Cct1 => 200,
            Self::Ch => 100,
            Self::Cht0_5 => 150,
            Self::Cht1 => 200,
            Self::Cs1t0_5 => 300,
            Self::Cs1t1 => 400,
            Self::Cs1t1_5 => 500,
            Self::Cs1t2 => 600,
            Self::Css1t0_5 => 400,
            Self::Css1t1 => 500,
            Self::Css1t1_5 => 600,
            Self::Css1t2 => 700,
            Self::Cs1t1o => 750,
            Self::Cs1t1_5o => 900,
            Self::Cs1t2o => 1050,
            Self::Chs0_5 => 200,
        })
    }
}

impl FeaturedRotation for CFRotation {
    fn group(&self) -> ARotationGroup {
        use ARotationGroup::*;
        #[allow(clippy::match_same_arms)]
        match self {
            Self::Ct0_5 | Self::Ct1 | Self::Ct1_5 | Self::Ct2 | Self::Ct2_5 | Self::Ct3 => Twist,
            Self::Cd | Self::Cdt0_5 | Self::Cdt1 | Self::Cdt1_5 | Self::Cs1 => Somersault,
            Self::Css1 => StraightSomersault,
            Self::Cs1_5 | Self::Cs1_5o | Self::Cf1 | Self::Cf1_5 => Somersault,
            Self::Cc | Self::Cct0_5 | Self::Cct1 => Cartwheel,
            Self::Ch | Self::Cht0_5 | Self::Cht1 => Handspring,
            Self::Cs1t0_5 | Self::Cs1t1 | Self::Cs1t1_5 | Self::Cs1t2 => Somersault,
            Self::Css1t0_5 | Self::Css1t1 | Self::Css1t1_5 | Self::Css1t2 => StraightSomersault,
            Self::Cs1t1o | Self::Cs1t1_5o | Self::Cs1t2o => Somersault,
            Self::Chs0_5 => Handspring,
        }
    }

    fn is_open(&self) -> bool {
        matches!(self, Self::Cs1_5o | Self::Cs1t1o | Self::Cs1t1_5o | Self::Cs1t2o)
    }
}

#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum CBonus {
    Pos3,
    Dbl,
    Slip,
    Star,
    Cx,
    Twirl,
    CRoll,
    Turn,
    Run,
    Ju,
    #[strum(to_string = "1P^H")]
    _1POntoH,
    #[strum(to_string = "H^1P")]
    HOnto1P,
    Jump,
    #[strum(to_string = "Jump>")]
    JumpPass,
    On1Foot,
    #[strum(to_string = "1F>1F")]
    _1FOnto1F,
    #[strum(to_string = "1F>1F+")]
    _1FOnto1FPlus,
    #[strum(to_string = "2F>2F")]
    _2FOnto2F,
}

impl DD for CBonus {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Pos3 => 50,
            Self::Dbl => 300,
            Self::Slip => 175,
            Self::Star => 100,
            Self::Cx => 125,
            Self::Twirl => 75,
            Self::CRoll => 150,
            Self::Turn => 250,
            Self::Run => 200,
            Self::Ju => 150,
            Self::_1POntoH => 1100,
            Self::HOnto1P => 900,
            Self::Jump => 275,
            Self::JumpPass => 225,
            Self::On1Foot => 400,
            Self::_1FOnto1F => 700,
            Self::_1FOnto1FPlus => 1000,
            Self::_2FOnto2F => 500,
        })
    }
}

impl CBonus {
    #[must_use]
    pub const fn required_consts(&self) -> &'static [CConst] {
        use CConst::*;
        match &self {
            Self::Turn => &[_2SupPlus],
            Self::Run => &[ThrOntoFF, ThrOntoF],
            Self::Ju => &[ThrOntoFF, ThrOntoF, ThrOntohand, ThrOntoSq],
            Self::Jump => &[ThrOntoSt, ThrOntoStH, ThrOntoSt2],
            Self::JumpPass => {
                &[ThrOntoSt, ThrOntoStH, ThrOntoFF, ThrOntoF, ThrOntohand, ThrOntoSq, ThrOntoSt2]
            }
            // AQUA doesn't have 1F>1F in writing, but nothing else is
            // possible, and it matches the restriction on 2F>2F
            Self::_1FOnto1F | Self::_2FOnto2F => &[ThrOntoStH],
            Self::_1FOnto1FPlus => &[ThrOntoStHOnto1F],
            _ => &[],
        }
    }

    #[must_use]
    pub const fn required_dir(&self) -> Option<CDir> {
        match &self {
            Self::Turn => Some(CDir::Base(ADir::Up)),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AcroCBonusRotation {
    pub bonuses: Box<[CBonus]>,
    pub base: Option<CBRotation>,
    pub featured: Option<CFRotation>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AcroC {
    pub construction: CConst,
    pub dir: CDir,
    pub positions: Positions<CPos>,
    pub bonusrotation: AcroCBonusRotation,
}

impl Deref for AcroC {
    type Target = AcroCBonusRotation;

    fn deref(&self) -> &Self::Target {
        &self.bonusrotation
    }
}

impl DD for AcroC {
    fn dd(&self) -> MilliDD {
        self.construction.dd()
            + self.dir.dd()
            + self.positions.dd()
            + self.bonuses.iter().map(DD::dd).sum::<MilliDD>()
            + self.base.as_ref().map_or(MilliDD(0), DD::dd)
            + self.featured.as_ref().map_or(MilliDD(0), DD::dd)
    }
}

impl Family for &AcroC {
    fn family() -> &'static str {
        "Combined"
    }
}

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum PConst {
    P,
    Box,
    Knees,
    B,
    DB,
    Chariot,
    #[strum(to_string = "2S")]
    _2S,
    Flower,
    Hand,
}

impl DD for PConst {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::P => 1000,
            Self::Box => 1100,
            Self::Knees => 1050,
            Self::B => 1200,
            Self::DB => 1300,
            Self::Chariot => 1150,
            Self::_2S => 1250,
            Self::Flower => 1000,
            Self::Hand => 800,
        })
    }
}

impl PConst {
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub const fn required_conns(&self) -> &'static [PConn] {
        use PConn::*;
        match self {
            Self::P => &[F2A, SiA, _1FA, _3pA, HA, _F1P, _2pASlash, _4p, _3pbA, BA, _2P2P],
            Self::Box => &[_4p, _3pA, SiA, F2A, HA, _3pbA],
            Self::Knees => &[_2pK, _3pbA, _3pK, SPPlusK, F2A, SiA, _3pA, _F1P, _2P2P, BA, _1FA],
            Self::B => &[
                _2pBb,
                _F1P,
                LSlashSiFPlusP,
                SiFPlusPb,
                SPPlusL,
                FAPlusPF,
                F2A,
                SiA,
                HPPlusL,
                FAb,
                _3pA,
                _1FA,
                HA,
                _3pS,
                _3pbA,
                _2bSlash,
            ],
            Self::DB => &[
                LSlashSiFPlusP,
                ShFPlusP,
                SiFPlusPb,
                SPPlusL,
                FAPlusPF,
                _3pK,
                F2A,
                SiA,
                _F1P,
                ShiShiPlus,
                SFPlusTP,
                HPPlusL,
                _3pA,
                _1FA,
                _4p,
                DBB,
                _3pbA,
                _2bSlash,
            ],
            Self::Chariot => &[_2pASlash, _4p, _3pbA, _3pA, FAb, F2A, SiA, _1FA, BA],
            Self::_2S => &[
                _2bSlash,
                _2pBb,
                FAPlusPF,
                _3pbA,
                HA,
                _3pA,
                F2A,
                SiA,
                _1FA,
                SPPlusK,
                _3pS,
                ShFPlusP,
                LSlashSiFPlusP,
                _2pASlash,
                BA,
            ],
            Self::Flower => &[_2pASlash, _3pbA, HA, _3pA, F2A, SiA, _1FA, BA],
            Self::Hand => &[_2pASlash, _3pbA, HA, _1FA, _3pA, F2A, SiA, BA],
        }
    }

    #[must_use]
    pub const fn required_rotation(&self) -> PRotationGroup {
        #[allow(clippy::match_same_arms)]
        match &self {
            Self::P => PRotationGroup::r,
            Self::Box => PRotationGroup::r,
            Self::Knees => PRotationGroup::r,
            Self::B => PRotationGroup::r,
            Self::Chariot => PRotationGroup::r,
            Self::Hand => PRotationGroup::h,
            Self::_2S => PRotationGroup::_2S,
            Self::Flower => PRotationGroup::_2S,
            Self::DB => PRotationGroup::DB,
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum PConn {
    SiA,
    F2A,
    FAb,
    #[strum(to_string = "3pA")]
    _3pA,
    #[strum(to_string = "1FA")]
    _1FA,
    HA,
    #[strum(to_string = "SP+K")]
    SPPlusK,
    #[strum(to_string = "3pK")]
    _3pK,
    #[strum(to_string = "3pS")]
    _3pS,
    #[strum(to_string = "3pbA")]
    _3pbA,
    #[strum(to_string = "FA+PF")]
    FAPlusPF,
    #[strum(to_string = "SP+L")]
    SPPlusL,
    #[strum(to_string = "SiF+Pb")]
    SiFPlusPb,
    #[strum(to_string = "ShF+P")]
    ShFPlusP,
    #[strum(to_string = "L/SiF+P")]
    LSlashSiFPlusP,
    #[strum(to_string = "4p")]
    _4p,
    #[strum(to_string = "2pA/")]
    _2pASlash,
    BA,
    DBB,
    #[strum(to_string = "2pK")]
    _2pK,
    #[strum(to_string = ">F1p")]
    _F1P,
    #[strum(to_string = "2pBb")]
    _2pBb,
    #[strum(to_string = ">2P2P")]
    _2P2P,
    #[strum(to_string = "2b/")]
    _2bSlash,
    #[strum(to_string = "SF+TP")]
    SFPlusTP,
    #[strum(to_string = "ShiShi+")]
    ShiShiPlus,
    #[strum(to_string = "HP+L")]
    HPPlusL,
}

impl DD for PConn {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::SiA => 100,
            Self::F2A => 200,
            Self::FAb => 200,
            Self::_3pA => 150,
            Self::_1FA => 400,
            Self::HA => 100,
            Self::SPPlusK => 250,
            Self::_3pK => 300,
            Self::_3pS => 400,
            Self::_3pbA => 450,
            Self::FAPlusPF => 250,
            Self::SPPlusL => 375,
            Self::SiFPlusPb => 350,
            Self::ShFPlusP => 400,
            Self::LSlashSiFPlusP => 300,
            Self::_4p => 100,
            Self::_2pASlash => 275,
            Self::BA => 300,
            Self::DBB => 500,
            Self::_2pK => 200,
            Self::_F1P => 400,
            Self::_2pBb => 550,
            Self::_2P2P => 600,
            Self::_2bSlash => 450,
            Self::SFPlusTP => 250,
            Self::ShiShiPlus => 700,
            Self::HPPlusL => 425,
        })
    }
}

impl PConn {
    #[must_use]
    pub const fn is_one_leg(&self) -> bool {
        matches!(
            self,
            Self::FAb
                | Self::_3pA
                | Self::_1FA
                | Self::_3pS
                | Self::_3pbA
                | Self::FAPlusPF
                | Self::_F1P
                | Self::ShiShiPlus
                | Self::_2pBb
        )
    }

    #[must_use]
    pub const fn required_consts(&self) -> &'static [PConst] {
        use PConst::*;
        #[allow(clippy::match_same_arms)]
        match self {
            Self::SiA => &[P, Box, Knees, B, DB, Chariot, _2S, Flower, Hand],
            Self::F2A => &[P, Box, Knees, B, DB, Chariot, _2S, Flower, Hand],
            Self::FAb => &[B, Chariot],
            Self::_3pA => &[P, Box, Knees, B, DB, Chariot, _2S, Flower, Hand],
            Self::_1FA => &[P, Knees, B, DB, Chariot, _2S, Flower, Hand],
            Self::HA => &[P, Box, B, _2S, Flower, Hand],
            Self::SPPlusK => &[Knees, _2S],
            Self::_3pK => &[Knees, DB],
            Self::_3pS => &[B, _2S],
            Self::_3pbA => &[P, Box, Knees, B, DB, Chariot, _2S, Flower, Hand],
            Self::FAPlusPF => &[B, DB, _2S],
            Self::SPPlusL => &[B, DB],
            Self::SiFPlusPb => &[B, DB],
            Self::ShFPlusP => &[DB, _2S],
            Self::LSlashSiFPlusP => &[B, DB, _2S],
            Self::_4p => &[P, Box, DB, Chariot],
            Self::_2pASlash => &[P, Chariot, _2S, Flower, Hand],
            Self::BA => &[P, Knees, Chariot, _2S, Flower, Hand],
            Self::DBB => &[DB],
            Self::_2pK => &[Knees],
            Self::_F1P => &[P, Knees, B, DB],
            Self::_2pBb => &[B, _2S],
            Self::_2P2P => &[P, Knees],
            Self::_2bSlash => &[B, DB, _2S],
            Self::SFPlusTP => &[DB],
            Self::ShiShiPlus => &[DB],
            Self::HPPlusL => &[B, DB],
        }
    }

    #[must_use]
    pub const fn required_positions(&self) -> &'static [BPos] {
        match self {
            Self::F2A => BPOS_TWO_FOOT,
            Self::_2pBb => &[BPos::qu],
            Self::BA | Self::DBB => &[BPos::br],
            _ => &[],
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Eq, PartialEq)]
pub enum PRotationGroup {
    r,
    h,
    _2S,
    DB,
}

impl Display for PRotationGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::r => write!(f, "Pr"),
            Self::h => write!(f, "Ph"),
            Self::_2S => write!(f, "P2S"),
            Self::DB => write!(f, "PDB"),
        }
    }
}

impl PRotationGroup {
    #[must_use]
    pub const fn required_consts(&self) -> &'static [PConst] {
        match &self {
            Self::r => &[PConst::P, PConst::Box, PConst::Knees, PConst::B, PConst::Chariot],
            Self::h => &[PConst::Hand],
            Self::_2S => &[PConst::_2S, PConst::Flower],
            Self::DB => &[PConst::DB],
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Display, EnumString, Eq, PartialEq)]
pub enum PRotation {
    Pr,
    #[strum(to_string = "Pr0.5")]
    Pr0_5,
    Pr1,
    #[strum(to_string = "Prq.5")]
    Pr1_5,
    Ph,
    #[strum(to_string = "P0.5h")]
    P0_5h,
    P1h,
    #[strum(to_string = "P1.5h")]
    P1_5h,
    P2h,
    P2S,
    #[strum(to_string = "P2Sr0.5")]
    P2Sr0_5,
    P2Sr1,
    PDB,
    #[strum(to_string = "PDB0.5")]
    PDB0_5,
    PDB1,
}

impl DD for PRotation {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Pr => 200,
            Self::Pr0_5 => 300,
            Self::Pr1 => 500,
            Self::Pr1_5 => 700,
            Self::Ph => 100,
            Self::P0_5h => 200,
            Self::P1h => 300,
            Self::P1_5h => 500,
            Self::P2h => 700,
            Self::P2S => 300,
            Self::P2Sr0_5 => 400,
            Self::P2Sr1 => 600,
            Self::PDB => 350,
            Self::PDB0_5 => 450,
            Self::PDB1 => 700,
        })
    }
}

impl PRotation {
    #[must_use]
    pub const fn group(&self) -> PRotationGroup {
        match self {
            Self::Pr | Self::Pr0_5 | Self::Pr1 | Self::Pr1_5 => PRotationGroup::r,
            Self::Ph | Self::P0_5h | Self::P1h | Self::P1_5h | Self::P2h => PRotationGroup::h,
            Self::P2S | Self::P2Sr0_5 | Self::P2Sr1 => PRotationGroup::_2S,
            Self::PDB | Self::PDB0_5 | Self::PDB1 => PRotationGroup::DB,
        }
    }
}

#[derive(Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum PBonus {
    Dbl,
    Pos3,
    UP,
    Porp,
    Spich,
    Trav,
    Stand,
    Diva,
    PRoll,
    Box,
    Spider,
    Climb,
    Arch,
    Kozak,
    Dive,
    CH,
    Ps1,
    #[strum(to_string = "Ps1t0.5")]
    Ps1t0_5,
    Ps1o,
    #[strum(to_string = "Ps1t0.5o")]
    Ps1t0_5o,
    Ps1t1,
    Pf1,
    Pf1o,
    Mov,
    Mov1,
    #[strum(to_string = "Mov1+T")]
    Mov1PlusT,
    Fall,
    FTurn,
}

impl DD for PBonus {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::Dbl => 300,
            Self::Pos3 => 50,
            Self::UP => 100,
            Self::Porp => 150,
            Self::Spich => 500,
            Self::Trav => 200,
            Self::Stand => 100,
            Self::Diva => 300,
            Self::PRoll => 125,
            Self::Box => 175,
            Self::Spider => 225,
            Self::Climb => 100,
            Self::Arch => 400,
            Self::Kozak => 600,
            Self::Dive => 50,
            Self::CH => 100,
            Self::Ps1 => 100,
            Self::Ps1t0_5 => 150,
            Self::Ps1o => 300,
            Self::Ps1t0_5o => 400,
            Self::Ps1t1 => 350,
            Self::Pf1 => 150,
            Self::Pf1o => 325,
            Self::Mov => 250,
            Self::Mov1 => 150,
            Self::Mov1PlusT => 275,
            Self::Fall => 50,
            Self::FTurn => 150,
        })
    }
}

impl PBonus {
    #[must_use]
    pub const fn required_consts(&self) -> &'static [PConst] {
        use PConst::*;
        match self {
            Self::Spider | Self::Climb | Self::Fall | Self::FTurn => &[_2S, Flower, Hand],
            Self::Diva => &[_2S],
            _ => &[],
        }
    }

    #[must_use]
    pub const fn required_conns(&self) -> Option<PConn> {
        match self {
            Self::Diva => Some(PConn::_3pS),
            _ => None,
        }
    }

    #[must_use]
    pub const fn required_position(&self) -> Option<BPos> {
        match self {
            Self::Porp => Some(BPos::bb),
            Self::Spider => Some(BPos::br),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AcroP {
    pub construction: PConst,
    pub conn: PConn,
    pub positions: Positions<BPos>,
    pub bonuses: Box<[PBonus]>,
    pub rotation: Option<PRotation>,
}

impl DD for AcroP {
    fn dd(&self) -> MilliDD {
        self.construction.dd()
            + self.conn.dd()
            + self.positions.dd()
            + self.bonuses.iter().map(DD::dd).sum::<MilliDD>()
            + self.rotation.as_ref().map_or(MilliDD(0), DD::dd)
    }
}

impl Family for &AcroP {
    fn family() -> &'static str {
        "Platform"
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TeamAcroKind {
    Airborne(AcroA),
    Balance(AcroB),
    Combined(AcroC),
    Platform(AcroP),
}

impl DD for TeamAcroKind {
    fn dd(&self) -> MilliDD {
        const BASE_TEAM_ACRO_DD: MilliDD = MilliDD(500);
        BASE_TEAM_ACRO_DD
            + match self {
                Self::Airborne(acro) => acro.dd(),
                Self::Balance(acro) => acro.dd(),
                Self::Combined(acro) => acro.dd(),
                Self::Platform(acro) => acro.dd(),
            }
    }
}

impl TeamAcroKind {
    #[must_use]
    pub fn family(&self) -> &'static str {
        match self {
            Self::Airborne(_) => <&AcroA>::family(),
            Self::Balance(_) => <&AcroB>::family(),
            Self::Combined(_) => <&AcroC>::family(),
            Self::Platform(_) => <&AcroP>::family(),
        }
    }
}

impl<Group: DDPos2 + FromStr> DD for Positions<Group> {
    fn dd(&self) -> MilliDD {
        self.first.dd() + self.second.as_ref().map(DDPos2::dd_pos2).unwrap_or_default()
    }
}

impl<GroupPos: DD + FromStr> FromStr for Positions<GroupPos> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split('/').collect::<Vec<_>>();
        if parts.is_empty() {
            return Err(anyhow!("no positions"));
        } else if parts.len() > 2 {
            return Err(anyhow!("acros can only have 2 declared positions, found {}", parts.len()));
        }

        let first = parts[0].parse().map_err(|_| anyhow!("unknown position {}", parts[0]))?;
        let second = if let Some(pos2_str) = parts.get(1) {
            Some(
                pos2_str
                    .strip_prefix('2')
                    .ok_or_else(|| anyhow!("second position '{pos2_str}' does not start with '2'"))?
                    .parse::<GroupPos>()
                    .map_err(|_| anyhow!("unknown position {pos2_str}"))?,
            )
        } else {
            None
        };

        Ok(Self { first, second })
    }
}

fn parse_rotation_bonuses<'a, Rotation: FromStr, Bonus: FromStr>(
    iter: impl IntoIterator<Item = &'a str>,
) -> Result<(Option<Rotation>, Box<[Bonus]>), anyhow::Error> {
    let mut r = None;
    let mut bonuses = Vec::new();

    for part in iter {
        if let Ok(rotation) = part.parse::<Rotation>() {
            r = Some(rotation);
        } else {
            for bonus_str in part.split('/') {
                if let Ok(bonus) = bonus_str.parse::<Bonus>() {
                    bonuses.push(bonus);
                } else {
                    return Err(anyhow!("{bonus_str} is not a rotation or bonus"));
                }
            }
        }
    }

    if bonuses.len() > 2 {
        return Err(anyhow!("only 2 bonuses allowed, found {}", bonuses.len()));
    }

    Ok((r, bonuses.into()))
}

fn parse_c_rotation_bonuses<'a>(
    iter: impl IntoIterator<Item = &'a str>,
) -> Result<AcroCBonusRotation, anyhow::Error> {
    let mut base = None;
    let mut featured = None;
    let mut bonuses = Vec::new();

    for part in iter {
        let mut parsed = false;
        for rotate_str in part.split('+') {
            if let Ok(rotation) = rotate_str.parse::<CBRotation>() {
                if base.is_some() {
                    return Err(anyhow!("only one rotation of the base allowed"));
                }
                base = Some(rotation);
                parsed = true;
            } else if let Ok(rotation) = rotate_str.parse::<CFRotation>() {
                if featured.is_some() {
                    return Err(anyhow!("only one rotation of the featured athlete allowed"));
                }
                featured = Some(rotation);
                parsed = true;
            }
        }

        if !parsed {
            for bonus_str in part.split('/') {
                if let Ok(bonus) = bonus_str.parse::<CBonus>() {
                    bonuses.push(bonus);
                } else {
                    return Err(anyhow!("{bonus_str} is not a rotation or bonus"));
                }
            }
        }
    }

    if bonuses.len() > 2 {
        return Err(anyhow!("only 2 bonuses allowed, found {}", bonuses.len()));
    }

    Ok(AcroCBonusRotation { bonuses: bonuses.into(), base, featured })
}

impl FromStr for TeamAcroKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // "-" is to separate parts, except in one place just to screw
        // with you, we'll just rename it internally to make parsing
        // easier
        let code = s.replace("C-Roll", "CRoll");
        let mut parts = code.split('-');

        let group = parts.next().unwrap_or_default();
        let construct = parts.next().unwrap_or_default();
        let dir_conn = parts.next().unwrap_or_default();
        let positions = parts.next().unwrap_or_default();

        match group {
            "A" => {
                let (rotation, bonuses) = parse_rotation_bonuses(parts)?;
                Ok(Self::Airborne(AcroA {
                    construction: construct
                        .parse()
                        .map_err(|_| anyhow!("unknown construction {construct}"))?,
                    dir: dir_conn.parse().map_err(|_| anyhow!("unknown direction {dir_conn}"))?,
                    positions: positions.parse().map_err(anyhow::Error::msg)?,
                    bonuses,
                    rotation,
                }))
            }
            "B" => {
                let (rotation, bonuses) = parse_rotation_bonuses(parts)?;
                Ok(Self::Balance(AcroB {
                    construction: construct
                        .parse()
                        .map_err(|_| anyhow!("unknown construction {construct}"))?,
                    conn: dir_conn.parse().map_err(|_| anyhow!("unknown connection {dir_conn}"))?,
                    positions: positions.parse().map_err(anyhow::Error::msg)?,
                    bonuses,
                    rotation,
                }))
            }
            "C" => Ok(Self::Combined(AcroC {
                construction: construct
                    .parse()
                    .map_err(|_| anyhow!("unknown construction {construct}"))?,
                dir: dir_conn.parse().map_err(|_| anyhow!("unknown direction {dir_conn}"))?,
                positions: positions.parse()?,
                bonusrotation: parse_c_rotation_bonuses(parts)?,
            })),
            "P" => {
                let (rotation, bonuses) = parse_rotation_bonuses(parts)?;
                Ok(Self::Platform(AcroP {
                    construction: construct
                        .parse()
                        .map_err(|_| anyhow!("unknown construction {construct}"))?,
                    conn: dir_conn.parse().map_err(|_| anyhow!("unknown connection {dir_conn}"))?,
                    positions: positions.parse()?,
                    bonuses,
                    rotation,
                }))
            }
            _ => Err(anyhow!("unknown acro group: '{group}'")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_team_acro() {
        assert!("Q-Sq-Forw-ln".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Bln-ln".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Down-ln".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Back-sd".parse::<TeamAcroKind>().is_err());
        assert!("P-Hand-HA-bb/2ow-Ph0.5".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Back".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Back-2ln".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Back-pk/3rg".parse::<TeamAcroKind>().is_err());
        assert!("A-Thr-Forw-ln-Dbbl".parse::<TeamAcroKind>().is_err());
        assert!("B-St-FS-ln-SdU".parse::<TeamAcroKind>().is_err());
        assert!("C-Thr^2F-Back-ow/2tk-Pos4".parse::<TeamAcroKind>().is_err());
        assert!("P-P-F2A-ln-Pss1".parse::<TeamAcroKind>().is_err());
        assert!("A-Sq-Side-ln-ct0.5+s1".parse::<TeamAcroKind>().is_err());
        assert!("C-Thr^2F-Back-ow/2tk-2F0.5+2F1+Ct1".parse::<TeamAcroKind>().is_err());
        assert!("C-Thr^2F-Back-ow/2tk-2F0.5+Cs1+Ct1".parse::<TeamAcroKind>().is_err());
        assert!("C-Thr^2F-Back-ow/2tk-Cs1+Ct1".parse::<TeamAcroKind>().is_err());
        assert!("B-St-FS-ln/2he-Hold/Mov/Dbl".parse::<TeamAcroKind>().is_err());

        assert_eq!(
            "A-Sq-Back-ln-D".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Airborne(AcroA {
                construction: AConst::Sq,
                dir: ADir::Back,
                positions: Positions { first: APos::ln, second: None },
                bonuses: [].into(),
                rotation: Some(ARotation::D),
            })
        );

        assert_eq!(
            "A-Thr-Forw-ln-dt0.5".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Airborne(AcroA {
                construction: AConst::Thr,
                dir: ADir::Forw,
                positions: Positions { first: APos::ln, second: None },
                bonuses: [].into(),
                rotation: Some(ARotation::dt0_5),
            })
        );

        assert_eq!(
            "A-Shou-Up-ja-Dbl".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Airborne(AcroA {
                construction: AConst::Shou,
                dir: ADir::Up,
                positions: Positions { first: APos::ja, second: None },
                bonuses: [ABonus::Dbl].into(),
                rotation: None,
            })
        );

        assert_eq!(
            "B-LH-LiH-mo/2ct-r/L-Hold/SdUp".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Balance(AcroB {
                construction: BConst::LH,
                conn: BConn::LiH,
                positions: Positions { first: BPos::mo, second: Some(BPos::ct) },
                bonuses: [BBonus::Hold, BBonus::SdUp].into(),
                rotation: Some(BRotation::rSlashL),
            })
        );

        assert_eq!(
            "C-2Sup+-Up-spl/2mo-Turn/Dbl".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Combined(AcroC {
                construction: CConst::_2SupPlus,
                dir: CDir::Base(ADir::Up),
                positions: Positions { first: CPos::B(BPos::spl), second: Some(CPos::B(BPos::mo)) },
                bonusrotation: AcroCBonusRotation {
                    bonuses: [CBonus::Turn, CBonus::Dbl].into(),
                    base: None,
                    featured: None,
                }
            })
        );

        assert_eq!(
            "C-Thr^Lh-Bln-br/2rg-Cr0.5L+Cs1t1".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Combined(AcroC {
                construction: CConst::ThrAboveLh,
                dir: CDir::Bln,
                positions: Positions { first: CPos::B(BPos::br), second: Some(CPos::A(APos::rg)) },
                bonusrotation: AcroCBonusRotation {
                    bonuses: [].into(),
                    base: Some(CBRotation::Cr0_5L),
                    featured: Some(CFRotation::Cs1t1),
                }
            })
        );

        assert_eq!(
            "P-Knees-SP+K-ff/2wi-Pr-Porp/Trav".parse::<TeamAcroKind>().unwrap(),
            TeamAcroKind::Platform(AcroP {
                construction: PConst::Knees,
                conn: PConn::SPPlusK,
                positions: Positions { first: BPos::ff, second: Some(BPos::wi) },
                bonuses: [PBonus::Porp, PBonus::Trav].into(),
                rotation: Some(PRotation::Pr),
            })
        );
    }

    #[test]
    fn test_team_acro_dd() {
        const CASES: &[(&str, u32)] = &[
            ("A-Sq-Forw-tk", 1875),
            ("A-Feet-Back-pk/2ln-s1-Feet/Pos3", 2275),
            ("B-St-FS-sd", 1575),
            ("B-St>-F1S-ba/2he", 1862),
            ("C-Thr>St-Side-co", 1975),
            ("C-Thr^2F-Bln-ow/2ja-2F1+Cs1-Pos3/Dbl", 3262),
            ("P-P-F2A-sd", 1750),
            ("P-B-2pBb-qu/2ow-Pr0.5-Mov/Dive", 3725),
            ("A-Shou-Forw-kt/2tk", 1562),
            ("B-St-F1S-ba-r0.5", 1875),
            ("P-2S-3pA-ow-Climb/Fall", 2350),
            ("B-St-FP-sd", 2250),
            ("A-Shou-Forw-tk-t0.5", 1600),
            ("A-Shou-Back-ja-D", 1750),
            ("A-Sq-Back-tk-s1", 2225),
            ("B-St-F1S-ey", 2125),
            ("A-Shou-Back-sp-D", 1825),
            ("B-2SupM-Le-so", 2175),
            ("P-Hand-HA-bb/2ow-P0.5h-Pos3", 1975),
            ("A-Sq-Back-tk/2kt", 1950),
            ("P-P-F2A-sd", 1750),
            ("C-Thr^2F-Bln-ow/2tk-Cs1", 2537),
            ("B-StH-FF-sd", 2250),
            ("P-Knees-SP+K-bb/2ow-Porp/Pos3", 2325),
            ("C-Thr>StH-Forw-he-1F>1F", 2575),
            ("B-St-FP-vs/2he", 2562),
            ("A-Sq-Back-pk-s1", 2300),
        ];
        for (s, expect_dd) in CASES {
            let acro = s.parse::<TeamAcroKind>();
            assert!(acro.is_ok(), "failed to parse {}", s);
            let calc_dd = acro.unwrap().dd();
            assert_eq!(calc_dd, MilliDD(*expect_dd), "{s}: expected DD {expect_dd} got {calc_dd}");
        }
    }
}
