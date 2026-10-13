use check_cards::*;
use serde::Deserialize;
use std::any::TypeId;
use std::error::Error;
use std::fmt::Debug;
use std::io::Cursor;
use std::str::FromStr;

#[derive(Debug, Deserialize)]
struct Movement {
    #[serde(rename = "MovementCode")]
    code: String,
    #[serde(rename = "DD")]
    dd: f32,
}

#[derive(Debug, Deserialize)]
struct MovementParent {
    #[serde(default)]
    #[serde(rename = "Movement")]
    movements: Box<[Movement]>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct TeamAcro {
    construction: MovementParent,
    direction: MovementParent,
    support: MovementParent,
    positions: MovementParent,
    bonus: MovementParent,
    #[serde(rename = "Rotation_Base")]
    rotation_base: MovementParent,
    rotation: MovementParent,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct PairAcro {
    elements: MovementParent,
}

#[derive(Debug, Deserialize)]
#[serde(rename = "Artistic_Swimming")]
struct MovementsFile {
    #[serde(rename = "ACRO-A")]
    a: TeamAcro,
    #[serde(rename = "ACRO-B")]
    b: TeamAcro,
    #[serde(rename = "ACRO-C")]
    c: TeamAcro,
    #[serde(rename = "ACRO-P")]
    p: TeamAcro,
    #[serde(rename = "ACRO-PAIRS")]
    pair: PairAcro,
}

#[derive(Debug, Deserialize)]
struct Category {
    #[serde(rename = "CategoryCode")]
    code: String,
    #[serde(rename = "DD")]
    dd: f32,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct Hybrid {
    #[serde(rename = "Category")]
    categories: Box<[Category]>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct Hybrids {
    #[serde(rename = "Hybrid")]
    hybrids: Box<[Hybrid]>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct Element {
    #[serde(rename = "Category")]
    categories: Box<[Category]>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct RequiredElements {
    #[serde(rename = "Element")]
    elements: Box<[Element]>,
}

#[derive(Debug, Deserialize)]
#[serde(rename = "Artistic_Swimming")]
struct DifficultyFile {
    #[serde(rename = "Hybrids")]
    hybrids: Hybrids,
    #[serde(rename = "Required_Elements_Solo")]
    solo: RequiredElements,
    #[serde(rename = "Required_Elements_Men_Solo")]
    mens_solo: RequiredElements,
    #[serde(rename = "Required_Elements_Duet")]
    duet: RequiredElements,
    #[serde(rename = "Required_Elements_Mixed_Duet")]
    mixed_duet: RequiredElements,
    #[serde(rename = "Required_Elements_Team")]
    team: RequiredElements,
}

fn parse_validate_acro<T>(m: MovementParent)
where
    T: FromStr + DD + 'static,
    <T as FromStr>::Err: Debug,
{
    for movement in m.movements {
        let mut code = movement.code.replace("C-Roll", "CRoll");
        if code.starts_with('2') && TypeId::of::<T>() == TypeId::of::<APos>() {
            code.remove(0);
            let parsed: APos =
                code.parse().expect(format!("could not parse {}", movement.code).as_ref());
            let iss_dd = MilliDD((movement.dd * 1000.0).round() as u32);
            assert_eq!(parsed.dd_pos2(), iss_dd, "different dd for {}", movement.code);
        } else if code.starts_with('2') && TypeId::of::<T>() == TypeId::of::<BPos>() {
            code.remove(0);
            let parsed: BPos =
                code.parse().expect(format!("could not parse {}", movement.code).as_ref());
            let iss_dd = MilliDD((movement.dd * 1000.0).round() as u32);
            assert_eq!(parsed.dd_pos2(), iss_dd, "different dd for {}", movement.code);
        } else {
            let parsed: T =
                code.parse().expect(format!("could not parse {}", movement.code).as_ref());
            let mut iss_dd = MilliDD((movement.dd * 1000.0).round() as u32);
            if TypeId::of::<T>() == TypeId::of::<PairAcroKind>() {
                iss_dd = iss_dd + MilliDD(100);
            }
            assert_eq!(parsed.dd(), iss_dd, "different dd for {}", movement.code);
        }
    }
}

fn parse_validate_tre(evt: Events, r: RequiredElements) {
    for element in r.elements {
        for category in element.categories {
            let parsed = parse_tre(evt, format!("TRE{}", category.code).as_str())
                .expect(format!("could not parse {}", category.code).as_ref());
            let iss_dd = MilliDD((category.dd * 1000.0).round() as u32);
            assert_eq!(parsed.dd(), iss_dd, "different dd for {}", category.code);
        }
    }
}

#[test]
fn compare_iss_difficulty_codes() -> Result<(), Box<dyn Error>> {
    let data = include_bytes!("Difficulty_Chart2026-utf8.xml");
    let reader = Cursor::new(data);

    let iss: DifficultyFile = quick_xml::de::from_reader(reader)?;
    parse_validate_tre(Events::Solo(Gender::Man), iss.mens_solo);
    parse_validate_tre(Events::Solo(Gender::Woman), iss.solo);
    parse_validate_tre(Events::Duet, iss.duet);
    parse_validate_tre(Events::MixedDuet, iss.mixed_duet);
    parse_validate_tre(Events::Team, iss.team);

    for hybrid in iss.hybrids.hybrids {
        for category in hybrid.categories {
            let parsed: Decl =
                category.code.parse().expect(format!("could not parse {}", category.code).as_ref());
            let iss_dd = MilliDD((category.dd * 1000.0).round() as u32);
            assert_eq!(parsed.dd(), iss_dd, "different dd for {}", category.code);
        }
    }

    Ok(())
}

#[test]
fn compare_iss_movement_codes() -> Result<(), Box<dyn Error>> {
    let data = include_bytes!("Movements2026-utf8.xml");
    let reader = Cursor::new(data);

    let acros: MovementsFile = quick_xml::de::from_reader(reader)?;
    parse_validate_acro::<AConst>(acros.a.construction);
    parse_validate_acro::<ADir>(acros.a.direction);
    parse_validate_acro::<APos>(acros.a.positions);
    parse_validate_acro::<ABonus>(acros.a.bonus);
    parse_validate_acro::<ARotation>(acros.a.rotation);
    parse_validate_acro::<BConst>(acros.b.construction);
    parse_validate_acro::<BConn>(acros.b.support);
    parse_validate_acro::<BPos>(acros.b.positions);
    parse_validate_acro::<BRotation>(acros.b.rotation_base);
    parse_validate_acro::<BBonus>(acros.b.bonus);
    parse_validate_acro::<CConst>(acros.c.construction);
    parse_validate_acro::<CDir>(acros.c.direction);
    parse_validate_acro::<CBRotation>(acros.c.rotation_base);
    parse_validate_acro::<CFRotation>(acros.c.rotation);
    parse_validate_acro::<CBonus>(acros.c.bonus);
    parse_validate_acro::<PConst>(acros.p.construction);
    parse_validate_acro::<PConn>(acros.p.support);
    parse_validate_acro::<PRotation>(acros.p.rotation_base);
    parse_validate_acro::<PBonus>(acros.p.bonus);

    parse_validate_acro::<PairAcroKind>(acros.pair.elements);

    Ok(())
}
