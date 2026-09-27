use crate::{DD, Events, MilliDD};
use anyhow::anyhow;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum SoloTRE {
    _1a,
    _1b,
    _2,
    _3a,
    _3b,
    _4a,
    _4b,
    _5a,
    _5b,
}

impl DD for SoloTRE {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::_1a => 2700,
            Self::_1b => 2000,
            Self::_2 => 3500,
            Self::_3a => 3100,
            Self::_3b => 2600,
            Self::_4a => 3600,
            Self::_4b => 3500,
            Self::_5a => 2500,
            Self::_5b => 2200,
        })
    }
}

impl FromStr for SoloTRE {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TRE1a" => Ok(Self::_1a),
            "TRE1b" => Ok(Self::_1b),
            "TRE2" => Ok(Self::_2),
            "TRE3a" => Ok(Self::_3a),
            "TRE3b" => Ok(Self::_3b),
            "TRE4a" => Ok(Self::_4a),
            "TRE4b" => Ok(Self::_4b),
            "TRE5a" => Ok(Self::_5a),
            "TRE5b" => Ok(Self::_5b),
            _ => Err(anyhow!("unknown Solo TRE '{s}'")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum DuetTRE {
    _1a,
    _1b,
    _2a,
    _2b,
    _3,
    _4a,
    _4b,
    _5a,
    _5b,
}

impl DD for DuetTRE {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::_1a => 2300,
            Self::_1b => 2100,
            Self::_2a => 2500,
            Self::_2b => 2400,
            Self::_3 => 3600,
            Self::_4a => 3400,
            Self::_4b => 2900,
            Self::_5a => 2700,
            Self::_5b => 2500,
        })
    }
}

impl FromStr for DuetTRE {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TRE1a" => Ok(Self::_1a),
            "TRE1b" => Ok(Self::_1b),
            "TRE2a" => Ok(Self::_2a),
            "TRE2b" => Ok(Self::_2b),
            "TRE3" => Ok(Self::_3),
            "TRE4a" => Ok(Self::_4a),
            "TRE4b" => Ok(Self::_4b),
            "TRE5a" => Ok(Self::_5a),
            "TRE5b" => Ok(Self::_5b),
            _ => Err(anyhow!("unknown Duet TRE '{s}'")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum MixedDuetTRE {
    _1a,
    _1b,
    _2a,
    _2b,
    _3a,
    _3b,
}

impl DD for MixedDuetTRE {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::_1a => 3100,
            Self::_1b => 2900,
            Self::_2a => 2300,
            Self::_2b => 2200,
            Self::_3a => 3200,
            Self::_3b => 2900,
        })
    }
}

impl FromStr for MixedDuetTRE {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TRE1a" => Ok(Self::_1a),
            "TRE1b" => Ok(Self::_1b),
            "TRE2a" => Ok(Self::_2a),
            "TRE2b" => Ok(Self::_2b),
            "TRE3a" => Ok(Self::_3a),
            "TRE3b" => Ok(Self::_3b),
            _ => Err(anyhow!("unknown Mixed Duet TRE '{s}'")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TeamTRE {
    _1a,
    _1b,
    _2a,
    _2b,
    _3a,
    _3b,
    _4a,
    _4b,
    _5a,
    _5b,
}

impl DD for TeamTRE {
    fn dd(&self) -> MilliDD {
        #[allow(clippy::match_same_arms)]
        MilliDD(match self {
            Self::_1a => 2200,
            Self::_1b => 1900,
            Self::_2a => 2800,
            Self::_2b => 2600,
            Self::_3a => 2100,
            Self::_3b => 1900,
            Self::_4a => 2400,
            Self::_4b => 2300,
            Self::_5a => 3400,
            Self::_5b => 3200,
        })
    }
}

impl FromStr for TeamTRE {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TRE1a" => Ok(Self::_1a),
            "TRE1b" => Ok(Self::_1b),
            "TRE2a" => Ok(Self::_2a),
            "TRE2b" => Ok(Self::_2b),
            "TRE3a" => Ok(Self::_3a),
            "TRE3b" => Ok(Self::_3b),
            "TRE4a" => Ok(Self::_4a),
            "TRE4b" => Ok(Self::_4b),
            "TRE5a" => Ok(Self::_5a),
            "TRE5b" => Ok(Self::_5b),
            _ => Err(anyhow!("unknown Team TRE '{s}'")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TREKind {
    Solo(SoloTRE),
    Duet(DuetTRE),
    MixedDuet(MixedDuetTRE),
    Team(TeamTRE),
}

impl DD for TREKind {
    fn dd(&self) -> MilliDD {
        match self {
            Self::Solo(t) => t.dd(),
            Self::Duet(t) => t.dd(),
            Self::MixedDuet(t) => t.dd(),
            Self::Team(t) => t.dd(),
        }
    }
}

pub fn parse_tre(evt: Events, s: &str) -> Result<TREKind, anyhow::Error> {
    match evt {
        Events::Solo => Ok(TREKind::Solo(s.parse()?)),
        Events::Duet => Ok(TREKind::Duet(s.parse()?)),
        Events::MixedDuet => Ok(TREKind::MixedDuet(s.parse()?)),
        Events::Team => Ok(TREKind::Team(s.parse()?)),
        _ => Err(anyhow!("unexpected TRE in {evt}")),
    }
}

#[cfg(test)]
mod tests {
    use crate::Events;
    use crate::Events::{Combo, MixedDuet, Solo, Team};
    use crate::tre::parse_tre;

    #[test]
    fn test_parse_tre() {
        const GOOD_CASES: &[(&str, Events, &str)] = &[
            ("team_tre4b", Team, "TRE4b"),
            ("solo_tre4a", Solo, "TRE4a"),
            ("md_tre3a", MixedDuet, "TRE3a"),
        ];
        for (name, evt, s) in GOOD_CASES {
            parse_tre(*evt, s).expect(format!("{name}: failed to parse '{s}'").as_str());
        }

        const BAD_CASES: &[(&str, Events, &str)] = &[
            ("solo_invalid_tre", Solo, "TRE5m"),
            ("team_tre4", Team, "TRE4"),
            ("md_tre4a", MixedDuet, "TRE4a"),
            ("tre_in_combo", Combo, "TRE4a"),
        ];
        for (name, evt, s) in BAD_CASES {
            parse_tre(*evt, s).err().expect(format!("{name}: unexpectedly parsed '{s}'").as_str());
        }
    }
}
