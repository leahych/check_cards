use crate::{DD, MilliDD};
use anyhow::anyhow;
use std::str::FromStr;
use strum_macros::{Display, EnumString};

#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum ThrustLC {
    TB,
    T1,
    T2a,
    T2b,
    T3a,
    T3b,
    T3c,
    T3d,
    T4a,
    T4b,
    T4c,
    T4d,
    T4e,
    T5a,
    T5b,
    T5c,
    T5d,
    T5e,
    T6a,
    T6b,
    T6c,
    T7,
    T8,
    T9a,
    T9b,
}

impl DD for ThrustLC {
    fn dd(&self) -> MilliDD {
        MilliDD(match self {
            Self::TB => 300,
            Self::T1 => 450,
            Self::T2a | Self::T2b => 500,
            Self::T3a | Self::T3b | Self::T3c | Self::T3d => 650,
            Self::T4a | Self::T4b | Self::T4c | Self::T4d | Self::T4e => 800,
            Self::T5a | Self::T5b | Self::T5c | Self::T5d | Self::T5e => 900,
            Self::T6a | Self::T6b | Self::T6c => 1100,
            Self::T7 => 1500,
            Self::T8 => 1700,
            Self::T9a | Self::T9b => 2000,
        })
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum SpinLC {
    SB,
    SCB,
    SCDB,
    S1,
    SC1,
    SCD1,
    S2,
    SC2,
    SCD2,
    S3,
    SC3,
    SCD3,
    S4,
    SC4,
    SCD4,
    S5,
    SC5,
    SCD5,
    S6,
    SC6,
    SCD6,
    S7,
    S8,
    S9,
    S10,
}

impl DD for SpinLC {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::SB => 150,
            Self::SCB => 350,
            Self::SCDB => 400,
            Self::S1 => 350,
            Self::SC1 => 800,
            Self::SCD1 => 850,
            Self::S2 => 750,
            Self::SC2 => 1600,
            Self::SCD2 => 1650,
            Self::S3 => 1150,
            Self::SC3 => 2400,
            Self::SCD3 => 2450,
            Self::S4 => 1550,
            Self::SC4 => 3200,
            Self::SCD4 => 3250,
            Self::S5 => 1950,
            Self::SC5 => 4000,
            Self::SCD5 => 4050,
            Self::S6 => 2350,
            Self::SC6 => 4800,
            Self::SCD6 => 4850,
            Self::S7 => 2750,
            Self::S8 => 3150,
            Self::S9 => 3550,
            Self::S10 => 3950,
        })
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum TwistLC {
    RB,
    #[strum(to_string = "1RB")]
    _1RB,
    #[strum(to_string = "2RB")]
    _2RB,
    ROB,
    RCB,
    R1,
    #[strum(to_string = "1R1")]
    _1R1,
    #[strum(to_string = "2R1")]
    _2R1,
    RD1,
    RU1,
    RO1,
    RC1,
    R2,
    #[strum(to_string = "1R2")]
    _1R2,
    #[strum(to_string = "2R2")]
    _2R2,
    RD2,
    RU2,
    R3,
    #[strum(to_string = "1R3")]
    _1R3,
    #[strum(to_string = "2R3")]
    _2R3,
    RU3,
    R4,
    #[strum(to_string = "1R4")]
    _1R4,
    #[strum(to_string = "2R4")]
    _2R4,
    RD4,
    RU4,
    #[strum(to_string = "1R5")]
    _1R5,
    #[strum(to_string = "2R5")]
    _2R5,
    RU5,
    #[strum(to_string = "1R6")]
    _1R6,
    #[strum(to_string = "2R6")]
    _2R6,
    RD6,
    RU6,
    #[strum(to_string = "2R7")]
    _2R7,
    RU7,
    #[strum(to_string = "2R8")]
    _2R8,
    RU8,
    #[strum(to_string = "2R9")]
    _2R9,
    RU9,
    #[strum(to_string = "2R10")]
    _2R10,
    RU10,
}

impl DD for TwistLC {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::RB => 100,
            Self::_1RB => 150,
            Self::_2RB => 200,
            Self::ROB => 250,
            Self::RCB => 250,
            Self::R1 => 200,
            Self::_1R1 => 350,
            Self::_2R1 => 450,
            Self::RD1 => 500,
            Self::RU1 => 550,
            Self::RO1 => 550,
            Self::RC1 => 550,
            Self::R2 => 400,
            Self::_1R2 => 750,
            Self::_2R2 => 950,
            Self::RD2 => 1050,
            Self::RU2 => 1150,
            Self::R3 => 600,
            Self::_1R3 => 1150,
            Self::_2R3 => 1450,
            Self::RU3 => 1750,
            Self::R4 => 800,
            Self::_1R4 => 1550,
            Self::_2R4 => 1950,
            Self::RD4 => 2150,
            Self::RU4 => 2350,
            Self::_1R5 => 1950,
            Self::_2R5 => 2450,
            Self::RU5 => 2950,
            Self::_1R6 => 2350,
            Self::_2R6 => 2950,
            Self::RD6 => 3350,
            Self::RU6 => 3550,
            Self::_2R7 => 3450,
            Self::RU7 => 4150,
            Self::_2R8 => 3950,
            Self::RU8 => 4750,
            Self::_2R9 => 4450,
            Self::RU9 => 5350,
            Self::_2R10 => 4950,
            Self::RU10 => 5950,
        })
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum AwLC {
    AB,
    A1a,
    A1b,
    A1c,
    A1d,
    A2a,
    A2b,
    A3a,
    A3b,
    A4a,
    A4b,
    A5,
    A6,
    A7,
    A8,
}

impl DD for AwLC {
    fn dd(&self) -> MilliDD {
        MilliDD(match self {
            Self::AB => 50,
            Self::A1a | Self::A1b | Self::A1c | Self::A1d => 100,
            Self::A2a | Self::A2b => 150,
            Self::A3a | Self::A3b => 200,
            Self::A4a | Self::A4b => 450,
            Self::A5 => 650,
            Self::A6 => 1150,
            Self::A7 => 1450,
            Self::A8 => 1650,
        })
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum FlexLC {
    FB,
    F1a,
    F1b,
    F1c,
    F2a,
    F2b,
    F2c,
    F3a,
    F3b,
    F3c,
    F4a,
    F4b,
    F4c,
    F4d,
    F4e,
    F4f,
    F5a,
    F5b,
    F5c,
    F6a,
    F6b,
    F6c,
    F6d,
    F7,
    F8a,
    F8b,
    F9,
    F10,
}

impl DD for FlexLC {
    fn dd(&self) -> MilliDD {
        MilliDD(match self {
            Self::FB => 50,
            Self::F1a | Self::F1b | Self::F1c => 100,
            Self::F2a | Self::F2b | Self::F2c => 200,
            Self::F3a | Self::F3b | Self::F3c => 300,
            Self::F4a | Self::F4b | Self::F4c | Self::F4d | Self::F4e | Self::F4f => 400,
            Self::F5a | Self::F5b | Self::F5c => 500,
            Self::F6a | Self::F6b | Self::F6c | Self::F6d => 650,
            Self::F7 => 750,
            Self::F8a | Self::F8b => 900,
            Self::F9 => 1000,
            Self::F10 => 1300,
        })
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, Eq, Hash, PartialEq)]
pub enum ConnLC {
    CB,
    C1a,
    C1b,
    C2a,
    C2b,
    C2c,
    C3,
    C4,
    C5,
    C6a,
    C6b,
    C7,
}

impl DD for ConnLC {
    fn dd(&self) -> MilliDD {
        MilliDD(match self {
            Self::CB => 100,
            Self::C1a | Self::C1b => 200,
            Self::C2a | Self::C2b | Self::C2c => 300,
            Self::C3 => 400,
            Self::C4 => 500,
            Self::C5 => 1000,
            Self::C6a | Self::C6b => 1250,
            Self::C7 => 1500,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LevelCode {
    Thrust(ThrustLC),
    Spin(SpinLC),
    Twist(TwistLC),
    Aw(AwLC),
    Flex(FlexLC),
    Conn(ConnLC, bool),
}

impl DD for LevelCode {
    fn dd(&self) -> MilliDD {
        match self {
            Self::Thrust(code) => code.dd(),
            Self::Spin(code) => code.dd(),
            Self::Twist(code) => code.dd(),
            Self::Aw(code) => code.dd(),
            Self::Flex(code) => code.dd(),
            Self::Conn(code, plus) => code.dd() + plus.then_some(MilliDD(100)).unwrap_or_default(),
        }
    }
}

impl FromStr for LevelCode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // twists can start with 1R or 2R which makes
        // them different from everything else
        if s.contains('R') {
            return Ok(Self::Twist(
                s.parse().map_err(|_| anyhow!("{s} is not a valid twist declaration"))?,
            ));
        }

        let family = s.chars().next();
        match family {
            Some('T') => Ok(Self::Thrust(
                s.parse().map_err(|_| anyhow!("{s} is not a valid thrust declaration"))?,
            )),
            Some('A') => Ok(Self::Aw(
                s.parse().map_err(|_| anyhow!("{s} is not a valid airborne declaration"))?,
            )),
            Some('S') => Ok(Self::Spin(
                s.parse().map_err(|_| anyhow!("{s} is not a valid spin declaration"))?,
            )),
            Some('F') => Ok(Self::Flex(
                s.parse().map_err(|_| anyhow!("{s} is not a valid flexibility declaration"))?,
            )),
            Some('C') => match s.strip_suffix("+") {
                None => Ok(Self::Conn(
                    s.parse().map_err(|_| anyhow!("{s} is not a valid connection declaration"))?,
                    false,
                )),
                Some(s) => Ok(Self::Conn(
                    s.parse().map_err(|_| anyhow!("{s} is not a valid connection declaration"))?,
                    true,
                )),
            },
            _ => Err(anyhow!("unknown hybrid declaration {s}")),
        }
    }
}

impl std::fmt::Display for LevelCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Thrust(lc) => lc.fmt(f),
            Self::Spin(lc) => lc.fmt(f),
            Self::Twist(lc) => lc.fmt(f),
            Self::Aw(lc) => lc.fmt(f),
            Self::Flex(lc) => lc.fmt(f),
            Self::Conn(lc, plus) => write!(f, "{lc}{}", plus.then_some("+").unwrap_or_default()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Factor {
    No,
    _0_3,
    _0_5,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Decl {
    pub lc: LevelCode,
    pub f: Factor,
}

impl DD for Decl {
    fn dd(&self) -> MilliDD {
        // multiply and then divide so that we can stay with truncating
        // division rather than trying to multiply by 0.3 and have to
        // deal with floating point conversions.
        let factor = MilliDD(match self.f {
            Factor::No => 10,
            Factor::_0_3 => 3,
            Factor::_0_5 => 5,
        });
        self.lc.dd() * factor / MilliDD(10)
    }
}

impl FromStr for Decl {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(s) = s.strip_suffix("*0.3") {
            Ok(Self { lc: s.parse()?, f: Factor::_0_3 })
        } else if let Some(s) = s.strip_suffix("*0.5") {
            Ok(Self { lc: s.parse()?, f: Factor::_0_5 })
        } else {
            Ok(Self { lc: s.parse()?, f: Factor::No })
        }
    }
}

impl std::fmt::Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let factor = match self.f {
            Factor::No => "",
            Factor::_0_3 => "*0.3",
            Factor::_0_5 => "*0.5",
        };
        write!(f, "{}{factor}", self.lc)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct PatternChanges(pub u8);

impl DD for PatternChanges {
    fn dd(&self) -> MilliDD {
        MilliDD(u32::from(self.0)) * MilliDD(200)
    }
}

impl FromStr for PatternChanges {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1PC" => Ok(Self(1)),
            "2PC" => Ok(Self(2)),
            "3PC" => Ok(Self(3)),
            "4PC" => Ok(Self(4)),
            "5PC" => Ok(Self(5)),
            "6PC" => Ok(Self(6)),
            _ => Err(anyhow!("{s} is not a valid pattern change declaration")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct HybridDecl {
    pub decls: Box<[Decl]>,
    pub pc_bonus: Option<PatternChanges>,
}

impl DD for HybridDecl {
    fn dd(&self) -> MilliDD {
        const BASE_DD: MilliDD = MilliDD(500);
        BASE_DD
            + self.decls.iter().map(DD::dd).sum::<MilliDD>()
            + self.pc_bonus.as_ref().map_or(MilliDD(0), DD::dd)
    }
}

impl FromStr for HybridDecl {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut decls = Vec::new();
        let mut pc_bonus = None;
        // using ? means we bail out on the first error, while we could
        // do something to ensure that we can return that as a
        // user-error but continue processing, this would make things
        // like counting families codes harder. There also isn't a great
        // way of solving counting code - ex. we'd have to special
        // handling for A7 A7 A7 A7a.
        //
        // Let's just bail for now in this case and report it as an error.
        // DTCs will have to do more checking but this should be a very
        // rare case now (at least where the checker is used).
        for decl in s.split_whitespace() {
            // This really only should be at the end but having it in
            // the middle of the card would take some work, and should
            // be very obvious to the DTCs. Let's not add checks to see
            // if the PC bonus is last until someone actually messes
            // that up in a way we need to care about.
            if decl.contains("PC") {
                // so far I haven't seen multiple PC bonuses, so rather
                // than adding code for that, we'll just have the last
                // entry win
                pc_bonus = Some(decl.parse()?);
            } else {
                decls.push(decl.parse()?);
            }
        }
        Ok(Self { decls: decls.into(), pc_bonus })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_hybrid() {
        const GOOD_CASES: &[(&str, &str)] = &[
            ("just_pc", "2PC"),
            ("spin_base", "SB"),
            ("spin_ten", "S10*0.3"),
            ("spin_five", "S5*0.5"),
            ("combined_spin_level", "SC4*0.5"),
            ("two_dir_spin_level", "SCD2*0.3"),
            ("conn_factored_plus", "C2b+*0.5"),
            ("flex", "F4e*0.3"),
            ("air", "A7*0.3"),
            ("swirl", "RB*0.5"),
            ("twist", "2R10*0.3"),
            ("unbal", "RU10*0.3"),
            ("open", "ROB"),
            ("one_leg", "1R6"),
        ];

        for (name, s) in GOOD_CASES {
            s.parse::<HybridDecl>().expect(format!("{name}: failed to parse '{s}'").as_str());
        }

        const BASE_CASES: &[(&str, &str)] = &[
            ("factored_pc", "2PC*0.3"),
            ("too_many_pc", "7PC"),
            ("spin_factor", "S5*0.4"),
            ("combined_spin_invalid_level", "SC8"),
            ("two_dir_spin_level", "SCD7"),
            ("conn_bad_option", "C4a"),
            ("flex", "F4k"),
            ("air", "A7a"),
            ("swirl", "R5"),
            ("twist", "2R11"),
            ("unbal", "RU11"),
            ("close", "RC2"),
            ("one_leg", "1R7"),
            ("capital_letter", "A3B"),
        ];
        for (name, s) in BASE_CASES {
            s.parse::<HybridDecl>()
                .err()
                .expect(format!("{name}: unexpectedly parsed '{s}'").as_str());
        }
    }

    #[test]
    fn test_cacl_hybrid_dd() {
        const CASES: &[(&str, u32)] = &[
            ("FB F1c F5a F6c A7 A6 RC1 S1 T6a", 6400),
            ("C4 C3 C4 F6a A8 F1a RCB", 4550),
            ("A1a A6*0.5 A7*0.5 F1b F5a T4e", 3300),
            ("C4+ AB A6 A1d AB F4f F5a SB	1PC", 3700),
            ("A1a A6 A1c F1a RCB", 2200),
            ("A3a SB S1 A1d SB T4b", 2250),
            ("CB+ AB A6 RB F1b F5a", 2600),
            ("T4b AB A6 F1a F2a", 2800),
            ("FB F1c F5a AB A6 A7	1PC", 4000),
            ("A1a F4f F5a F4c A6 F1a RCB	1PC", 3600),
            ("A1c C4+ C4+ AB A6 A1d S1	2PC", 3850),
            ("T5c AB A6 F1b F5a	2PC", 3600),
            ("T6a S1 F1b F5a 2RB RB A1c A8 2RB", 4800),
            ("F4a F3b F6c A6 RB F7 ROB F2a", 4300),
            ("CB C4 C3 C4 AB F4f F5a F5a AB SC2 A1d", 5200),
            ("T6a F7 2RB A1c A7 F8b F7 2RB", 5950),
            ("A3a AB 1R2 A6 A1d F1a RCB", 3100),
            ("CB C4 C3 C4 F6a A3a ROB F2a", 3300),
            ("T5c A3b F4f F5a S1", 2850),
            ("AB SC1 A7 F1a RCB F1a RCB", 3500),
            ("A3a SC1 A6 A1d 2RB F1b F5a", 3550),
            ("F1c F5a F6c A8 2RB F1a F2a", 3900),
            ("T6a S1 A1c A7 F1a RCB", 3850),
            ("CB C4 C3 C4 A1c A8 F6a", 4400),
            ("F4c A6 F1a RC1 F2c*0.5 A1c*0.3 A7*0.3 A1d*0.5 F1b F6c	1PC", 4265),
            ("RB A4b A8 2RB F9 F9 2R1", 5350),
            ("RB A6 A1d A8 A1c RU1 A1c RU1 S2 T4e", 6350),
            ("CB C3 CB AB*0.5 A1c*0.5 A6*0.5 A8*0.5 F7 F7 2RB	1PC", 4475),
            ("T4e A4b F1a RC1 F1a RC1 AB A6 A1d 2RB", 4550),
            ("F7 A6 A1d ROB RC1 T4e", 4100),
            ("A1c S2 S2 F1b F6c", 2850),
            ("CB+ A1a A6 C4+ C4+ A1c	2PC", 3650),
            ("T4e A8 A1c 2RB A1c 2RB A6 A1d	2PC", 5200),
            ("A1c A8 CB C4 CB C4 F1b F8a F1b F8a A1c S2 TB", 6600),
            ("F1b F8a F1b F8a C2b C2b A1c A7 2R1", 5100),
            ("T4e A1c A7 F7 F7", 4350),
            ("RB A1c A7 F1b F8a F1b F8a F2a", 4350),
            ("A1c S2 S1 A1c A7 F7 F7", 4750),
            ("A4b 2RB A1c A7 S1 F1b F6c F1b F6c T4e", 5350),
            ("AB F4e F5a F4f F8a SC1 RC1 F8a", 5000),
            ("F7 F7 S2 S1 A1c A8 2RB", 5050),
            ("CB C4 CB C4 C3 AB SC2 A1d F1a RC1 F2a", 4700),
            ("T4e F6c F6c F6c A1c A7 2R1", 5250),
            ("T3d A1c F1a A1d F1a F1b F6c AB A6", 3500),
            ("A1c A6 1RB SC2 AB F4f F5a F4f F5a", 5350),
            ("CB C3 C5 F1b F6c F6c T6a A3b F1a RC1 F2a", 5550),
            ("F8b F6a F3a A1d SC1 F3a F6c", 4200),
            ("F3b RC1 RB A1c A7 AB A6 A1d S1 T6c", 5750),
            ("F1b F6c F1b F6c RC1 A8 A6 C4+ C4+ C4+ F1b*0.5 F5a*0.5 2R1*0.5 RC1*0.5	2PC", 8350),
            ("A1a A6 A1d 2R1 A1c A7 F4f F6c", 4900),
            ("A4b F1b F6c A1c A7 A1c C4+ C4+ S2*0.5 A1c*0.5 F1a*0.5 RC1*0.5	2PC", 5700),
            ("A1c S2*0.5 A7*0.5 S1 A1d*0.5", 2100),
            ("C4 C3 C4 C3 2R1 F7 F1b F5a 2R1 A7	1PC", 6200),
            ("T4e A8 A6", 4100),
            ("A1a A6 A1c A7 A6", 4450),
            ("2RB AB A6 A8 RC1", 4100),
            ("F7 F1b F5a F1b F5a A8 A1c C3 A1c A8 A1c RD1", 6950),
            ("C4 C3 C4 C3 A1c A7 F7 F1b F5a RD1 S1", 6050),
            ("A4b A7 RD1 A1c 2R1 AB F4f F6c", 4550),
            ("A3a A7 F1b F5a F4f F6c A1c RD1 A1c 2R1", 4950),
            ("A1c A7 A6 F1b F5a F1b F5a", 4400),
            ("A1c RD1 A1c A7 C2b C2b A1c 1RB 2RB A6 F4f F8a", 6150),
            ("A3a A8 RU1 A1c RU1 A1c F1a A1d F1a RC1 F1b F6c", 5250),
            ("T6a S1 F1a RC1 AB F4e F6c AB F4e F6c", 4800),
            ("A6 F1b F6c A7 RU1 2RB", 4600),
            ("A1a F4f F5a F5a F4f F5a 2R1", 3350),
            ("T5e S1 F7 F1b F5a F1b F5a AB A1d A7 2RB", 5500),
            ("RC1 A7 F4e F4e F6c", 3950),
            ("T8 A3b A1d A7 A8 F1b F5a F1b F6c", 6950),
            ("A4b F1b F5a 2R1 F1b F5a RD1 A1a A7 A8", 6300),
            ("A3b A7 RD1 A8 F1b F5a F1b F5a A1d", 5600),
            ("T4e AB A6 A7 RD1", 4450),
            ("S2 S1 S2 A3b A8 2RB", 4400),
            ("T4e AB A1d S2", 2200),
            ("T8 S1 F7 A7 RU2 F7 A3a F9 F8a RC1 RC1 F7 A8 RU2 A1c 2R3 A4b", 15400),
            ("F7 RC1 F7 AB F8a F7 A7 A1c RD1 AB F6c", 7000),
            ("RC1 F7 RU2 AB F4f F5a RC1 F7 A7 F7 RU1 A3a", 8150),
            ("A4b F6c A1c A8 2R1 F7 F5a F7 A7 F5a", 7750),
            ("A1a F4f F5a F6c F7 A8 RU1 F7 RU1", 6400),
            ("RC1 F7 RU1 AB F6c AB F6c F7 A4b A1c A8 F7", 7450),
            ("T8 S1 A1c A8 F7 A7 C2b C2b F7 F5a F7 F5a", 9600),
            ("A4b AB F4f F6c F7 F5a A8 C4 C4 C3 A3a F6c", 7200),
            ("C4 C3 C4 C3 A1c A7 S1 A3b F7 F7 F1b F5a", 6500),
            ("F7 A3a A8 F9 F9 A1c A8 C2b C3 C2b C3", 8250),
            ("A1c A7 AB F8a F7 F5a F7 F5a A4b 2RB", 6150),
            ("F7 F1b F8a F7 A7 A1c A8 C4 C3 C4 C3 F7", 8750),
            ("A1c A7 F7 F1b F8a A1d A8 F1a RC1 2RB", 6400),
        ];
        for (s, expected) in CASES {
            let dd = s.parse::<HybridDecl>().unwrap().dd();
            assert_eq!(MilliDD(*expected), dd, "{s} expected {expected} got {dd}");
        }
    }
}
