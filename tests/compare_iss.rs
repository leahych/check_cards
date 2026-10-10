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
    acro_a: TeamAcro,
    #[serde(rename = "ACRO-B")]
    acro_b: TeamAcro,
    #[serde(rename = "ACRO-C")]
    acro_c: TeamAcro,
    #[serde(rename = "ACRO-P")]
    acro_p: TeamAcro,
    #[serde(rename = "ACRO-PAIRS")]
    acro_pairs: PairAcro,
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
    // TODO add men's solo once ISS supports
    #[serde(rename = "Required_Elements_Solo")]
    solo: RequiredElements,
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
    let data = include_bytes!("Difficulty_Chart2024-utf8.xml");
    let reader = Cursor::new(data);

    let iss: DifficultyFile = quick_xml::de::from_reader(reader)?;
    // TODO Men's Solo
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
    let data = include_bytes!("Movements2024-utf8.xml");
    let reader = Cursor::new(data);

    let iss: MovementsFile = quick_xml::de::from_reader(reader)?;
    parse_validate_acro::<AConst>(iss.acro_a.construction);
    parse_validate_acro::<ADir>(iss.acro_a.direction);
    parse_validate_acro::<APos>(iss.acro_a.positions);
    parse_validate_acro::<ABonus>(iss.acro_a.bonus);
    parse_validate_acro::<ARotation>(iss.acro_a.rotation);
    parse_validate_acro::<BConst>(iss.acro_b.construction);
    parse_validate_acro::<BConn>(iss.acro_b.support);
    parse_validate_acro::<BPos>(iss.acro_b.positions);
    parse_validate_acro::<BRotation>(iss.acro_b.rotation_base);
    parse_validate_acro::<BBonus>(iss.acro_b.bonus);
    parse_validate_acro::<CConst>(iss.acro_c.construction);
    parse_validate_acro::<CDir>(iss.acro_c.direction);
    parse_validate_acro::<CBRotation>(iss.acro_c.rotation_base);
    parse_validate_acro::<CFRotation>(iss.acro_c.rotation);
    parse_validate_acro::<CBonus>(iss.acro_c.bonus);
    parse_validate_acro::<PConst>(iss.acro_p.construction);
    parse_validate_acro::<PConn>(iss.acro_p.support);
    parse_validate_acro::<PRotation>(iss.acro_p.rotation_base);
    parse_validate_acro::<PBonus>(iss.acro_p.bonus);

    parse_validate_acro::<PairAcroKind>(iss.acro_pairs.elements);

    Ok(())
}
