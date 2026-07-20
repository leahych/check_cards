use crate::hybrid::HybridDecl;
use crate::pair_acro::PairAcroKind;
use crate::team_acro::TeamAcroKind;
use crate::tre::{TREKind, parse_tre};
use crate::{DD, Events, MilliDD};
use anyhow::anyhow;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Eq, PartialEq)]
pub enum ElementKind {
    TeamAcro(TeamAcroKind, Option<MilliDD>),
    PairAcro(PairAcroKind, Option<MilliDD>),
    ChoHy(Option<MilliDD>),
    Hybrid(HybridDecl, Option<MilliDD>),
    TRE(TREKind, Option<MilliDD>),
    SuConn,
}

impl DD for ElementKind {
    fn dd(&self) -> MilliDD {
        match self {
            Self::TeamAcro(a, _) => a.dd(),
            Self::PairAcro(a, _) => a.dd(),
            Self::ChoHy(_) => MilliDD(1000),
            Self::Hybrid(h, _) => h.dd(),
            Self::TRE(t, _) => t.dd(),
            Self::SuConn => MilliDD(0),
        }
    }
}

impl ElementKind {
    pub(crate) const fn reported_dd(&self) -> Option<&MilliDD> {
        match self {
            Self::TeamAcro(_, r) | Self::PairAcro(_, r) | Self::Hybrid(_, r) | Self::TRE(_, r) => {
                r.as_ref()
            }
            Self::ChoHy(r) => r.as_ref(),
            Self::SuConn => None,
        }
    }
}

pub fn parse_elem_kind(
    evt: Events,
    free: bool,
    s: &str,
    dd: Option<MilliDD>,
) -> Result<ElementKind, anyhow::Error> {
    use ElementKind::*;
    use Events::*;

    let s = s.trim();

    if s == "ChoHy" || s == "ChoHY" {
        return Ok(ChoHy(dd));
    }

    if s == "SuConn" {
        return if evt == MixedDuet { Ok(SuConn) } else { Err(anyhow!("found SuConn in {evt}")) };
    }

    if s.starts_with("TRE") {
        return if free {
            Err(anyhow!("found TRE in free routine"))
        } else {
            Ok(TRE(parse_tre(evt, s)?, dd))
        };
    }

    if s.starts_with(['L', 'J', 'W']) || s.starts_with("SL") {
        return match evt {
            Duet | MixedDuet | Trio => {
                Ok(PairAcro(s.parse().map_err(|_| anyhow!("unknown pair acrobatic {s}"))?, dd))
            }
            _ => Err(anyhow!("found Pair Acro in {evt}")),
        };
    }

    if ["A-", "B-", "C-", "P-"].iter().any(|pre| s.starts_with(pre)) {
        return match evt {
            Acrobatic | Combo | Team => Ok(TeamAcro(s.parse()?, dd)),
            _ => Err(anyhow!("found Team Acro in {evt}")),
        };
    }

    Ok(Hybrid(s.parse()?, dd))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hybrid::{Decl, Factor, FlexLC, LevelCode, PatternChanges};
    use crate::team_acro::{ABonus, AConst, ADir, APos, ARotation, AcroA, Positions};
    use crate::tre::SoloTRE;

    #[test]
    fn test_parse_elem_kind() {
        use ElementKind::*;
        use Events::*;
        let good_cases: &[(Events, bool, &str, ElementKind)] = &[
            (Combo, true, "ChoHy", ChoHy(Some(MilliDD(123)))),
            (MixedDuet, true, "SuConn", SuConn),
            (Solo, false, "TRE1a", TRE(TREKind::Solo(SoloTRE::_1a), Some(MilliDD(123)))),
            (Duet, true, "L!fr1", PairAcro(PairAcroKind::LHeadDownFlexr1, Some(MilliDD(123)))),
            (Trio, true, "Jfs1B", PairAcro(PairAcroKind::Jfs1B, Some(MilliDD(123)))),
            (
                Acrobatic,
                true,
                "A-Sq-Back-tk/2ln-s1-Pos3",
                TeamAcro(
                    TeamAcroKind::Airborne(AcroA {
                        construction: AConst::Sq,
                        dir: ADir::Back,
                        positions: Positions { first: APos::tk, second: Some(APos::ln) },
                        bonuses: [ABonus::Pos3].into(),
                        rotation: Some(ARotation::s1),
                    }),
                    Some(MilliDD(123)),
                ),
            ),
            (
                Team,
                true,
                "F1b F5a*0.5 F6c*0.3 2PC",
                Hybrid(
                    HybridDecl {
                        decls: Box::from([
                            Decl { lc: LevelCode::Flex(FlexLC::F1b), f: Factor::No },
                            Decl { lc: LevelCode::Flex(FlexLC::F5a), f: Factor::_0_5 },
                            Decl { lc: LevelCode::Flex(FlexLC::F6c), f: Factor::_0_3 },
                        ]),
                        pc_bonus: Some(PatternChanges(2)),
                    },
                    Some(MilliDD(123)),
                ),
            ),
        ];
        for (evt, free, s, expected) in good_cases {
            // the reported DD doesn't have to be real, we are testing
            // that whatever is passed to parse_elem_kind() is correctly
            // passed through so that later code can validate the
            // reported DD vs the calculated DD.
            assert_eq!(parse_elem_kind(*evt, *free, s, Some(MilliDD(123))).unwrap(), *expected);
        }

        const BAD_CASES: &[(Events, bool, &str)] = &[
            (Combo, true, "chohy"),
            (MixedDuet, true, "suconn"),
            (Duet, true, "SuConn"),
            (Solo, true, "TRE1a"),
            (Team, true, "Js1F"),
            (Trio, true, "A-Sq-Back-tk"),
            // yes this will bail out at parse time, and we won't do
            // anymore check on the hybrid. See the comment in the
            // hybrid parsing code.
            (Solo, true, "F9a"),
        ];
        for (evt, free, s) in BAD_CASES {
            parse_elem_kind(*evt, *free, *s, None)
                .err()
                .expect(format!("unexpectedly parsed '{s}'").as_str());
        }
    }
}
