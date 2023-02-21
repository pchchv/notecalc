use crate::token_parser::DECIMAL_PI;
use crate::units::units::{UnitInstance, Units};
use crate::units::{Prefix, Unit, UnitPrefixes};
use rust_decimal::prelude::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

const E21: &str = "1000000000000000000000";
const E24: &str = "1000000000000000000000000";
const E27: &str = "1000000000000000000000000000";
const E30: &str = "1000000000000000000000000000000";
const E36: &str = "1000000000000000000000000000000000000";
const E42: &str = "1000000000000000000000000000000000000000000";
const E48: &str = "1000000000000000000000000000000000000000000000000";
pub const BASE_UNIT_DIMENSION_COUNT: usize = 10;
pub const ALL_UNIT_COUNT: usize = 25;
pub const EMPTY_UNIT_DIMENSIONS: [UnitDimensionExponent; BASE_UNIT_DIMENSION_COUNT] =
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
pub(crate) const BASE_UNIT_DIMENSIONS: [[UnitDimensionExponent; BASE_UNIT_DIMENSION_COUNT];
    ALL_UNIT_COUNT] = [
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0], // Mass
    [0, 1, 0, 0, 0, 0, 0, 0, 0, 0], // Length
    [0, 0, 1, 0, 0, 0, 0, 0, 0, 0], // Time
    [0, 0, 0, 1, 0, 0, 0, 0, 0, 0], // Current
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 0], // Temperature
    [0, 0, 0, 0, 0, 1, 0, 0, 0, 0], // LuminousIntensity
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0], // AmountOfSubstance
    [0, 0, 0, 0, 0, 0, 0, 1, 0, 0], // Angle
    [0, 0, 0, 0, 0, 0, 0, 0, 1, 0], // Bit
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1], // Money
    // derived
    [1, 1, -2, 0, 0, 0, 0, 0, 0, 0],  // Force
    [0, 2, 0, 0, 0, 0, 0, 0, 0, 0],   // Surface
    [0, 3, 0, 0, 0, 0, 0, 0, 0, 0],   // Volume
    [1, 2, -2, 0, 0, 0, 0, 0, 0, 0],  // Energy
    [1, 2, -3, 0, 0, 0, 0, 0, 0, 0],  // Power
    [1, -1, -2, 0, 0, 0, 0, 0, 0, 0], // Pressure
    [0, 0, 1, 1, 0, 0, 0, 0, 0, 0],   // ElectricCharge
    [-1, -2, 4, 2, 0, 0, 0, 0, 0, 0], // ElectricCapacitance
    [1, 2, -3, -1, 0, 0, 0, 0, 0, 0], // ElectricPotential
    [1, 2, -3, -2, 0, 0, 0, 0, 0, 0], // ElectricResistance
    [1, 2, -2, -2, 0, 0, 0, 0, 0, 0], // ElectricInductance
    [-1, -2, 3, 2, 0, 0, 0, 0, 0, 0], // ElectricConductance
    [1, 2, -2, -1, 0, 0, 0, 0, 0, 0], // MagneticFlux
    [1, 0, -2, -1, 0, 0, 0, 0, 0, 0], // MagneticFluxDensity
    [0, 0, -1, 0, 0, 0, 0, 0, 0, 0],  // Frequency
];

#[repr(C)]
pub enum UnitType {
    Mass,
    Length,
    Time,
    Current,
    Temperature,
    LuminousIntensity,
    AmountOfSubstance,
    Angle,
    Bit,
    Money,
    Force,
    Surface,
    Volume,
    Energy,
    Power,
    Pressure,
    ElectricCharge,
    ElectricCapacitance,
    ElectricPotential,
    ElectricResistance,
    ElectricInductance,
    ElectricConductance,
    MagneticFlux,
    MagneticFluxDensity,
    Frequency,
}

pub type UnitDimensionExponent = i8;

fn create_prefixes() -> UnitPrefixes {
    UnitPrefixes {
        short: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['d', 'a'], "1e1", false)),
            Rc::new(Prefix::from_scientific(&['h'], "1e2", false)),
            Rc::new(Prefix::from_scientific(&['k'], "1e3", true)),
            Rc::new(Prefix::from_scientific(&['M'], "1e6", true)),
            Rc::new(Prefix::from_scientific(&['G'], "1e9", true)),
            Rc::new(Prefix::from_scientific(&['T'], "1e12", true)),
            Rc::new(Prefix::from_scientific(&['P'], "1e15", true)),
            Rc::new(Prefix::from_scientific(&['E'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['Z'], E21, true)),
            Rc::new(Prefix::from_decimal(&['Y'], E24, true)),
            Rc::new(Prefix::from_scientific(&['d'], "1e-1", false)),
            Rc::new(Prefix::from_scientific(&['c'], "1e-2", false)),
            Rc::new(Prefix::from_scientific(&['m'], "1e-3", true)),
            Rc::new(Prefix::from_scientific(&['u'], "1e-6", true)),
            Rc::new(Prefix::from_scientific(&['n'], "1e-9", true)),
            Rc::new(Prefix::from_scientific(&['p'], "1e-12", true)),
            Rc::new(Prefix::from_scientific(&['f'], "1e-15", true)),
            Rc::new(Prefix::from_scientific(&['a'], "1e-18", true)),
            Rc::new(Prefix::from_scientific(&['z'], "1e-21", true)),
            Rc::new(Prefix::from_scientific(&['y'], "1e-24", true)),
        ])),
        long: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['d', 'e', 'c', 'a'], "1e1", false)),
            Rc::new(Prefix::from_scientific(
                &['h', 'e', 'c', 't', 'o'],
                "1e2",
                false,
            )),
            Rc::new(Prefix::from_scientific(&['k', 'i', 'l', 'o'], "1e3", true)),
            Rc::new(Prefix::from_scientific(&['m', 'e', 'g', 'a'], "1e6", true)),
            Rc::new(Prefix::from_scientific(&['g', 'i', 'g', 'a'], "1e9", true)),
            Rc::new(Prefix::from_scientific(&['t', 'e', 'r', 'a'], "1e12", true)),
            Rc::new(Prefix::from_scientific(&['p', 'e', 't', 'a'], "1e15", true)),
            Rc::new(Prefix::from_scientific(&['e', 'x', 'a'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['z', 'e', 't', 't', 'a'], E21, true)),
            Rc::new(Prefix::from_decimal(&['y', 'o', 't', 't', 'a'], E24, true)),
            Rc::new(Prefix::from_scientific(
                &['d', 'e', 'c', 'i'],
                "1e-1",
                false,
            )),
            Rc::new(Prefix::from_scientific(
                &['c', 'e', 'n', 't', 'i'],
                "1e-2",
                false,
            )),
            Rc::new(Prefix::from_scientific(
                &['m', 'i', 'l', 'l', 'i'],
                "1e-3",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['m', 'i', 'c', 'r', 'o'],
                "1e-6",
                true,
            )),
            Rc::new(Prefix::from_scientific(&['n', 'a', 'n', 'o'], "1e-9", true)),
            Rc::new(Prefix::from_scientific(
                &['p', 'i', 'c', 'o'],
                "1e-12",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['f', 'e', 'm', 't', 'o'],
                "1e-15",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['a', 't', 't', 'o'],
                "1e-18",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['z', 'e', 'p', 't', 'o'],
                "1e-21",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['y', 'o', 'c', 't', 'o'],
                "1e-24",
                true,
            )),
        ])),
        squared: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['d', 'a'], "1e2", false)),
            Rc::new(Prefix::from_scientific(&['h'], "1e4", false)),
            Rc::new(Prefix::from_scientific(&['k'], "1e6", true)),
            Rc::new(Prefix::from_scientific(&['M'], "1e12", true)),
            Rc::new(Prefix::from_scientific(&['G'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['T'], E24, true)),
            Rc::new(Prefix::new2(&['P'], E30, true)),
            Rc::new(Prefix::new2(&['E'], E36, true)),
            Rc::new(Prefix::new2(&['Z'], E42, true)),
            Rc::new(Prefix::new2(&['Y'], E48, true)),
            Rc::new(Prefix::from_scientific(&['d'], "1e-2", false)),
            Rc::new(Prefix::from_scientific(&['c'], "1e-4", false)),
            Rc::new(Prefix::from_scientific(&['m'], "1e-6", true)),
            Rc::new(Prefix::from_scientific(&['u'], "1e-12", true)),
            Rc::new(Prefix::from_scientific(&['n'], "1e-18", true)),
            Rc::new(Prefix::from_scientific(&['p'], "1e-24", true)),
            Rc::new(Prefix::new(&['f'], "1e-30", true)),
            Rc::new(Prefix::new(&['a'], "1e-36", true)),
            Rc::new(Prefix::new(&['z'], "1e-42", true)),
            Rc::new(Prefix::new(&['y'], "1e-48", true)),
        ])),
        cubic: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['d', 'a'], "1e3", false)),
            Rc::new(Prefix::from_scientific(&['h'], "1e6", false)),
            Rc::new(Prefix::from_scientific(&['k'], "1e9", true)),
            Rc::new(Prefix::from_scientific(&['M'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['G'], E27, true)),
            Rc::new(Prefix::new(&['T'], "1e36", true)),
            Rc::new(Prefix::new(&['P'], "1e45", true)),
            Rc::new(Prefix::new(&['E'], "1e54", true)),
            Rc::new(Prefix::new(&['Z'], "1e63", true)),
            Rc::new(Prefix::new(&['Y'], "1e72", true)),
            Rc::new(Prefix::from_scientific(&['d'], "1e-3", false)),
            Rc::new(Prefix::from_scientific(&['c'], "1e-6", false)),
            Rc::new(Prefix::from_scientific(&['m'], "1e-9", true)),
            Rc::new(Prefix::from_scientific(&['u'], "1e-18", true)),
            Rc::new(Prefix::from_scientific(&['n'], "1e-27", true)),
            Rc::new(Prefix::new(&['p'], "1e-36", true)),
            Rc::new(Prefix::new(&['f'], "1e-45", true)),
            Rc::new(Prefix::new(&['a'], "1e-54", true)),
            Rc::new(Prefix::new(&['z'], "1e-63", true)),
            Rc::new(Prefix::new(&['y'], "1e-72", true)),
        ])),
        binary_short_si: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['k'], "1e3", true)),
            Rc::new(Prefix::from_scientific(&['M'], "1e6", true)),
            Rc::new(Prefix::from_scientific(&['G'], "1e9", true)),
            Rc::new(Prefix::from_scientific(&['T'], "1e12", true)),
            Rc::new(Prefix::from_scientific(&['P'], "1e15", true)),
            Rc::new(Prefix::from_scientific(&['E'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['Z'], E21, true)),
            Rc::new(Prefix::from_decimal(&['Y'], E24, true)),
        ])),
        binary_short_iec: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_decimal(&['K', 'i'], "1024", true)),
            Rc::new(Prefix::from_decimal(&['M', 'i'], "1048576", true)),
            Rc::new(Prefix::from_decimal(&['G', 'i'], "1073741824", true)),
            Rc::new(Prefix::from_scientific(&['T', 'i'], "1.0995116e+12", true)),
            Rc::new(Prefix::from_scientific(&['P', 'i'], "1.1258999e+15", true)),
            Rc::new(Prefix::from_scientific(&['E', 'i'], "1.1529215e+18", true)),
            Rc::new(Prefix::from_decimal(
                &['Z', 'i'],
                "1180591620717411303424",
                true,
            )),
            Rc::new(Prefix::from_scientific(&['Z', 'i'], "1.1805916e+21", true)),
            Rc::new(Prefix::from_decimal(
                &['Y', 'i'],
                "1208925819614629174706176",
                true,
            )),
            Rc::new(Prefix::from_scientific(&['Y', 'i'], "1.2089258e+24", true)),
        ])),
        binary_long_si: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_scientific(&['k', 'i', 'l', 'o'], "1e3", true)),
            Rc::new(Prefix::from_scientific(&['m', 'e', 'g', 'a'], "1e6", true)),
            Rc::new(Prefix::from_scientific(&['g', 'i', 'g', 'a'], "1e9", true)),
            Rc::new(Prefix::from_scientific(&['t', 'e', 'r', 'a'], "1e12", true)),
            Rc::new(Prefix::from_scientific(&['p', 'e', 't', 'a'], "1e15", true)),
            Rc::new(Prefix::from_scientific(&['e', 'x', 'a'], "1e18", true)),
            Rc::new(Prefix::from_decimal(&['z', 'e', 't', 't', 'a'], E21, true)),
            Rc::new(Prefix::from_decimal(&['y', 'o', 't', 't', 'a'], E24, true)),
        ])),
        binary_long_iec: Rc::new(Box::new(vec![
            Rc::new(Prefix::from_decimal(&['k', 'i', 'b', 'i'], "1024", true)),
            Rc::new(Prefix::from_decimal(&['m', 'e', 'b', 'i'], "1048576", true)),
            Rc::new(Prefix::from_decimal(
                &['g', 'i', 'b', 'i'],
                "1073741824",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['t', 'e', 'b', 'i'],
                "1.0995116e+12",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['p', 'e', 'b', 'i'],
                "1.1258999e+15",
                true,
            )),
            Rc::new(Prefix::from_scientific(
                &['e', 'x', 'i'],
                "1.1529215e+18",
                true,
            )),
            Rc::new(Prefix::from_decimal(
                &['z', 'e', 'b', 'i'],
                "1180591620717411303424",
                true,
            )),
            Rc::new(Prefix::from_decimal(
                &['y', 'o', 'b', 'i'],
                "1208925819614629174706176",
                true,
            )),
        ])),
        btu: Rc::new(Box::new(vec![Rc::new(Prefix::from_scientific(
            &['M', 'M'],
            "1e6",
            true,
        ))])),
    }
}

pub fn init_units() -> (HashMap<&'static str, Rc<Unit>>, UnitPrefixes) {
    let prefixes = create_prefixes();
    let pi: Decimal = DECIMAL_PI;
    let mut map = HashMap::<&str, Unit>::with_capacity(168);

    map.insert(
        "meter",
        Unit {
            name: &['m', 'e', 't', 'e', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "inch",
        Unit {
            name: &['i', 'n', 'c', 'h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0254").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "foot",
        Unit {
            name: &['f', 'o', 'o', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.3048").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "yard",
        Unit {
            name: &['y', 'a', 'r', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.9144").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mile",
        Unit {
            name: &['m', 'i', 'l', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("1609.344").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "link",
        Unit {
            name: &['l', 'i', 'n', 'k'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.201168").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "rod",
        Unit {
            name: &['r', 'o', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("5.0292").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "chain",
        Unit {
            name: &['c', 'h', 'a', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("20.1168").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "angstrom",
        Unit {
            name: &['a', 'n', 'g', 's', 't', 'r', 'o', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1e-10").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "m",
        Unit {
            name: &['m'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "in",
        Unit {
            name: &['i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0254").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "ft",
        Unit {
            name: &['f', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.3048").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "yd",
        Unit {
            name: &['y', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.9144").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mi",
        Unit {
            name: &['m', 'i'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("1609.344").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "li",
        Unit {
            name: &['l', 'i'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.201168").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "rd",
        Unit {
            name: &['r', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("5.029210").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "ch",
        Unit {
            name: &['c', 'h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("20.1168").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mil",
        Unit {
            name: &['m', 'i', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Length as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0000254").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 1/1000 inch
    map.insert(
        "m2",
        Unit {
            name: &['m', '2'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.squared)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "sqin",
        Unit {
            name: &['s', 'q', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.00064516").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 645.16 mm2
    map.insert(
        "sqft",
        Unit {
            name: &['s', 'q', 'f', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.09290304").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 0.09290304 m2
    map.insert(
        "sqyd",
        Unit {
            name: &['s', 'q', 'y', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.83612736").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 0.83612736 m2
    map.insert(
        "sqmi",
        Unit {
            name: &['s', 'q', 'm', 'i'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("2589988.110336").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 2.589988110336 km2
    map.insert(
        "sqrd",
        Unit {
            name: &['s', 'q', 'r', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("25.29295").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 25.29295 m2
    map.insert(
        "sqch",
        Unit {
            name: &['s', 'q', 'c', 'h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("404.6873").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 404.6873 m2
    map.insert(
        "sqmil",
        Unit {
            name: &['s', 'q', 'm', 'i', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("6.4516e-10").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 6.4516 * 10^-10 m2
    map.insert(
        "acre",
        Unit {
            name: &['a', 'c', 'r', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("4046.86").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 4046.86 m2
    map.insert(
        "hectare",
        Unit {
            name: &['h', 'e', 'c', 't', 'a', 'r', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Surface as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(10000).unwrap(),
            offset: Decimal::zero(),
        },
    ); // 10000 m2
    map.insert(
        "m3",
        Unit {
            name: &['m', '3'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.cubic)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "L",
        Unit {
            name: &['L'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("0.001").unwrap(),
            offset: Decimal::zero(),
        },
    ); // litre
    map.insert(
        "l",
        Unit {
            name: &['l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("0.001").unwrap(),
            offset: Decimal::zero(),
        },
    ); // litre
    map.insert(
        "litre",
        Unit {
            name: &['l', 'i', 't', 'r', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_str("0.001").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "cuin",
        Unit {
            name: &['c', 'u', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1.6387064e-5").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 1.6387064e-5 m3
    map.insert(
        "cuft",
        Unit {
            name: &['c', 'u', 'f', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.028316846592").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 28.316 846 592 L
    map.insert(
        "cuyd",
        Unit {
            name: &['c', 'u', 'y', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.764554857984").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 764.554 857 984 L
    map.insert(
        "teaspoon",
        Unit {
            name: &['t', 'e', 'a', 's', 'p', 'o', 'o', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.000005").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 5 mL
    map.insert(
        "tablespoon",
        Unit {
            name: &['t', 'a', 'b', 'l', 'e', 's', 'p', 'o', 'o', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.000015").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 15 mL
    map.insert(
        "drop",
        Unit {
            name: &['d', 'r', 'o', 'p'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("5e-8").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 0.05 mL = 5e-8 m3
    map.insert(
        "gtt",
        Unit {
            name: &['g', 't', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("5e-8").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 0.05 mL = 5e-8 m3
       // Liquid volume
    map.insert(
        "minim",
        Unit {
            name: &['m', 'i', 'n', 'i', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.00000006161152").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 0.06161152 mL
    map.insert(
        "fluiddram",
        Unit {
            name: &['f', 'l', 'u', 'i', 'd', 'd', 'r', 'a', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0000036966911").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 3.696691 mL
    map.insert(
        "fluidounce",
        Unit {
            name: &['f', 'l', 'u', 'i', 'd', 'o', 'u', 'n', 'c', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.00002957353").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 29.57353 mL
    map.insert(
        "gill",
        Unit {
            name: &['g', 'i', 'l', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0001182941").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 118.2941 mL
    map.insert(
        "cc",
        Unit {
            name: &['c', 'c'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1e-6").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 1e-6 L
    map.insert(
        "cup",
        Unit {
            name: &['c', 'u', 'p'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0002365882").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 236.5882 mL
    map.insert(
        "pint",
        Unit {
            name: &['p', 'i', 'n', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0004731765").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 473.1765 mL
    map.insert(
        "quart",
        Unit {
            name: &['q', 'u', 'a', 'r', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0009463529").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 946.3529 mL
    map.insert(
        "gallon",
        Unit {
            name: &['g', 'a', 'l', 'l', 'o', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.003785412").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 3.785412 L
    map.insert(
        "beerbarrel",
        Unit {
            name: &['b', 'e', 'e', 'r', 'b', 'a', 'r', 'r', 'e', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.1173478").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 117.3478 L
    map.insert(
        "oilbarrel",
        Unit {
            name: &['o', 'i', 'l', 'b', 'a', 'r', 'r', 'e', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.1589873").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 158.9873 L
    map.insert(
        "hogshead",
        Unit {
            name: &['h', 'o', 'g', 's', 'h', 'e', 'a', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.2384810").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 238.4810 L
    map.insert(
        "fldr",
        Unit {
            name: &['f', 'l', 'd', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0000036966911").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 3.696691 mL
    map.insert(
        "floz",
        Unit {
            name: &['f', 'l', 'o', 'z'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.00002957353").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 29.57353 mL
    map.insert(
        "gi",
        Unit {
            name: &['g', 'i'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0001182941").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 118.2941 mL
    map.insert(
        "cp",
        Unit {
            name: &['c', 'p'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0002365882").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 236.5882 mL
    map.insert(
        "pt",
        Unit {
            name: &['p', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0004731765").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 473.1765 mL
    map.insert(
        "qt",
        Unit {
            name: &['q', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.0009463529").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 946.3529 mL
    map.insert(
        "gal",
        Unit {
            name: &['g', 'a', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.003785412").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 3.785412 L
    map.insert(
        "bbl",
        Unit {
            name: &['b', 'b', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.1173478").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 117.3478 L
    map.insert(
        "obl",
        Unit {
            name: &['o', 'b', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Volume as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("0.1589873").unwrap(),
            offset: Decimal::zero(),
        },
    ); // 158.9873 L
       // Mass
    map.insert(
        "g",
        Unit {
            name: &['g'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("0.001").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "gram",
        Unit {
            name: &['g', 'r', 'a', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_str("0.001").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "ton",
        Unit {
            name: &['t', 'o', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("907.18474").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "t",
        Unit {
            name: &['t'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_i64(1000).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "tonne",
        Unit {
            name: &['t', 'o', 'n', 'n', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_i64(1000).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "grain",
        Unit {
            name: &['g', 'r', 'a', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("64.79891e-6").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "dram",
        Unit {
            name: &['d', 'r', 'a', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1.7718451953125e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "ounce",
        Unit {
            name: &['o', 'u', 'n', 'c', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("28.349523125e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "poundmass",
        Unit {
            name: &['p', 'o', 'u', 'n', 'd', 'm', 'a', 's', 's'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("453.59237e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "hundredweight",
        Unit {
            name: &[
                'h', 'u', 'n', 'd', 'r', 'e', 'd', 'w', 'e', 'i', 'g', 'h', 't',
            ],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("45.359237").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "stick",
        Unit {
            name: &['s', 't', 'i', 'c', 'k'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("115e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "stone",
        Unit {
            name: &['s', 't', 'o', 'n', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("6.35029318").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "gr",
        Unit {
            name: &['g', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("64.79891e-6").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "dr",
        Unit {
            name: &['d', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1.7718451953125e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "oz",
        Unit {
            name: &['o', 'z'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("28.349523125e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "lbm",
        Unit {
            name: &['l', 'b', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("453.59237e-3").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "cwt",
        Unit {
            name: &['c', 'w', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Mass as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("45.359237").unwrap(),
            offset: Decimal::zero(),
        },
    );
    // Time
    map.insert(
        "s",
        Unit {
            name: &['s'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "min",
        Unit {
            name: &['m', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(60).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "h",
        Unit {
            name: &['h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(3600).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "second",
        Unit {
            name: &['s', 'e', 'c', 'o', 'n', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "sec",
        Unit {
            name: &['s', 'e', 'c'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "minute",
        Unit {
            name: &['m', 'i', 'n', 'u', 't', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(60).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "hour",
        Unit {
            name: &['h', 'o', 'u', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(3600).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "day",
        Unit {
            name: &['d', 'a', 'y'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(86400).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "week",
        Unit {
            name: &['w', 'e', 'e', 'k'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            // 7 * 86400
            value: Decimal::from_i64(604800).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "month",
        Unit {
            name: &['m', 'o', 'n', 't', 'h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(2629800).unwrap(), // 1/12th of Julian year
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "year",
        Unit {
            name: &['y', 'e', 'a', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(31557600).unwrap(), // Julian year
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "decade",
        Unit {
            name: &['d', 'e', 'c', 'a', 'd', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(315576000).unwrap(), // Julian decade
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "century",
        Unit {
            name: &['c', 'e', 'n', 't', 'u', 'r', 'y'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(3155760000).unwrap(), // Julian century
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "millennium",
        Unit {
            name: &['m', 'i', 'l', 'l', 'e', 'n', 'n', 'i', 'u', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Time as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(31557600000).unwrap(), // Julian millennium
            offset: Decimal::zero(),
        },
    );
    // Frequency
    map.insert(
        "Hertz",
        Unit {
            name: &['H', 'e', 'r', 't', 'z'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Frequency as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "Hz",
        Unit {
            name: &['H', 'z'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Frequency as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Angle
    map.insert(
        "rad",
        Unit {
            name: &['r', 'a', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "radian",
        Unit {
            name: &['r', 'a', 'd', 'i', 'a', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // deg = rad / (2*pi) * 360 = rad / 0.017453292519943295769236907684888
    map.insert(
        "deg",
        Unit {
            name: &['d', 'e', 'g'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: &pi / &Decimal::from_isize(180).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "degree",
        Unit {
            name: &['d', 'e', 'g', 'r', 'e', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: &pi / &Decimal::from_isize(180).unwrap(),
            offset: Decimal::zero(),
        },
    );
    // grad = rad / (2*pi) * 400  = rad / 0.015707963267948966192313216916399
    map.insert(
        "grad",
        Unit {
            name: &['g', 'r', 'a', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: &pi / &Decimal::from_isize(200).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "gradian",
        Unit {
            name: &['g', 'r', 'a', 'd', 'i', 'a', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: &pi / &Decimal::from_isize(200).unwrap(),
            offset: Decimal::zero(),
        },
    );
    // cycle = rad / (2*pi) = rad / 6.2831853071795864769252867665793
    map.insert(
        "cycle",
        Unit {
            name: &['c', 'y', 'c', 'l', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (None, None),
            value: &pi * &Decimal::from_isize(2).unwrap(),
            offset: Decimal::zero(),
        },
    );
    // arcsec = rad / (3600 * (360 / 2 * pi)) = rad / 0.0000048481368110953599358991410235795
    map.insert(
        "arcsec",
        Unit {
            name: &['a', 'r', 'c', 's', 'e', 'c'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (None, None),
            value: &pi / &Decimal::from_isize(648000).unwrap(),
            offset: Decimal::zero(),
        },
    );
    // arcmin = rad / (60 * (360 / 2 * pi)) = rad / 0.00029088820866572159615394846141477
    map.insert(
        "arcmin",
        Unit {
            name: &['a', 'r', 'c', 'm', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Angle as usize],
            prefix_groups: (None, None),
            value: &pi / &Decimal::from_isize(10800).unwrap(),
            offset: Decimal::zero(),
        },
    );
    // Electric current
    map.insert(
        "A",
        Unit {
            name: &['A'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Current as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "ampere",
        Unit {
            name: &['a', 'm', 'p', 'e', 'r', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Current as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Temperature
    // K(C) = °C + 273.15
    // K(F) = (°F + 459.67) / 1.8
    // K(R) = °R / 1.8
    map.insert(
        "K",
        Unit {
            name: &['K'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "degC",
        Unit {
            name: &['d', 'e', 'g', 'C'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one(),
            offset: Decimal::from_str("273.15").unwrap(),
        },
    );
    map.insert(
        "degF",
        Unit {
            name: &['d', 'e', 'g', 'F'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one() / Decimal::from_str("1.8").unwrap(),
            offset: Decimal::from_str("459.67").unwrap(),
        },
    );
    map.insert(
        "degR",
        Unit {
            name: &['d', 'e', 'g', 'R'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one() / Decimal::from_str("1.8").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "kelvin",
        Unit {
            name: &['k', 'e', 'l', 'v', 'i', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "celsius",
        Unit {
            name: &['c', 'e', 'l', 's', 'i', 'u', 's'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one(),
            offset: Decimal::from_str("273.15").unwrap(),
        },
    );
    map.insert(
        "fahrenheit",
        Unit {
            name: &['f', 'a', 'h', 'r', 'e', 'n', 'h', 'e', 'i', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one() / Decimal::from_str("1.8").unwrap(),
            offset: Decimal::from_str("459.67").unwrap(),
        },
    );
    map.insert(
        "rankine",
        Unit {
            name: &['r', 'a', 'n', 'k', 'i', 'n', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize],
            prefix_groups: (None, None),
            value: Decimal::one() / Decimal::from_str("1.8").unwrap(),
            offset: Decimal::zero(),
        },
    );
    // amount of substance
    map.insert(
        "mol",
        Unit {
            name: &['m', 'o', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::AmountOfSubstance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mole",
        Unit {
            name: &['m', 'o', 'l', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::AmountOfSubstance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // luminous intensity
    map.insert(
        "cd",
        Unit {
            name: &['c', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::LuminousIntensity as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "candela",
        Unit {
            name: &['c', 'a', 'n', 'd', 'e', 'l', 'a'],
            base: BASE_UNIT_DIMENSIONS[UnitType::LuminousIntensity as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    ); // Force
    map.insert(
        "N",
        Unit {
            name: &['N'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "newton",
        Unit {
            name: &['n', 'e', 'w', 't', 'o', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "dyn",
        Unit {
            name: &['d', 'y', 'n'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("0.00001").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "dyne",
        Unit {
            name: &['d', 'y', 'n', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_str("0.00001").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "lbf",
        Unit {
            name: &['l', 'b', 'f'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("4.4482216152605").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "poundforce",
        Unit {
            name: &['p', 'o', 'u', 'n', 'd', 'f', 'o', 'r', 'c', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("4.4482216152605").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "kip",
        Unit {
            name: &['k', 'i', 'p'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Force as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_str("4448.2216").unwrap(),
            offset: Decimal::zero(),
        },
    );
    // Energy
    map.insert(
        "J",
        Unit {
            name: &['J'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "joule",
        Unit {
            name: &['j', 'o', 'u', 'l', 'e'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "cal",
        Unit {
            name: &['c', 'a', 'l'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_str("4.1868").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "erg",
        Unit {
            name: &['e', 'r', 'g'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (None, None),
            value: Decimal::from_scientific("1e-7").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "Wh",
        Unit {
            name: &['W', 'h'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_i64(3600).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "BTU",
        Unit {
            name: &['B', 'T', 'U'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.btu)), None),
            value: Decimal::from_str("1055.05585262").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "eV",
        Unit {
            name: &['e', 'V'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::from_scientific("1.602176565e-19").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "electronvolt",
        Unit {
            name: &['e', 'l', 'e', 'c', 't', 'r', 'o', 'n', 'v', 'o', 'l', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Energy as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::from_scientific("1.602176565e-19").unwrap(),
            offset: Decimal::zero(),
        },
    );
    // Power
    map.insert(
        "W",
        Unit {
            name: &['W'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Power as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "watt",
        Unit {
            name: &['w', 'a', 't', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Power as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "hp",
        Unit {
            name: &['h', 'p'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Power as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("745.6998715386").unwrap(),
            offset: Decimal::zero(),
        },
    ); // Electrical power units
    map.insert(
        "VA",
        Unit {
            name: &['V', 'A'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Power as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Pressure
    map.insert(
        "Pa",
        Unit {
            name: &['P', 'a'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "psi",
        Unit {
            name: &['p', 's', 'i'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("6894.75729276459").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "atm",
        Unit {
            name: &['a', 't', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_i64(101325).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "bar",
        Unit {
            name: &['b', 'a', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.short)),
                Some(Rc::clone(&prefixes.long)),
            ),
            value: Decimal::from_i64(100000).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "torr",
        Unit {
            name: &['t', 'o', 'r', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("133.322").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mmHg",
        Unit {
            name: &['m', 'm', 'H', 'g'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("133.322").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "mmH2O",
        Unit {
            name: &['m', 'm', 'H', '2', 'O'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("9.80665").unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "cmH2O",
        Unit {
            name: &['c', 'm', 'H', '2', 'O'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize],
            prefix_groups: (None, None),
            value: Decimal::from_str("98.0665").unwrap(),
            offset: Decimal::zero(),
        },
    );
    // Electric charge
    map.insert(
        "coulomb",
        Unit {
            name: &['c', 'o', 'u', 'l', 'o', 'm', 'b'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricCharge as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "C",
        Unit {
            name: &['C'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricCharge as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Electric capacitance
    map.insert(
        "farad",
        Unit {
            name: &['f', 'a', 'r', 'a', 'd'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricCapacitance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "F",
        Unit {
            name: &['F'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricCapacitance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Electric potential
    map.insert(
        "volt",
        Unit {
            name: &['v', 'o', 'l', 't'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricPotential as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "V",
        Unit {
            name: &['V'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricPotential as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Electric resistance
    map.insert(
        "ohm",
        Unit {
            name: &['o', 'h', 'm'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricResistance as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.short)),
                Some(Rc::clone(&prefixes.long)),
            ), // Both Mohm and megaohm are acceptable
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "Ω",
        Unit {
            name: &['Ω'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricResistance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Electric inductance
    map.insert(
        "henry",
        Unit {
            name: &['h', 'e', 'n', 'r', 'y'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricInductance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "H",
        Unit {
            name: &['H'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricInductance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Electric conductance
    map.insert(
        "siemens",
        Unit {
            name: &['s', 'i', 'e', 'm', 'e', 'n', 's'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricConductance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "S",
        Unit {
            name: &['S'],
            base: BASE_UNIT_DIMENSIONS[UnitType::ElectricConductance as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Magnetic flux
    map.insert(
        "weber",
        Unit {
            name: &['w', 'e', 'b', 'e', 'r'],
            base: BASE_UNIT_DIMENSIONS[UnitType::MagneticFlux as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "Wb",
        Unit {
            name: &['W', 'b'],
            base: BASE_UNIT_DIMENSIONS[UnitType::MagneticFlux as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Magnetic flux density
    map.insert(
        "tesla",
        Unit {
            name: &['t', 'e', 's', 'l', 'a'],
            base: BASE_UNIT_DIMENSIONS[UnitType::MagneticFluxDensity as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.long)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "T",
        Unit {
            name: &['T'],
            base: BASE_UNIT_DIMENSIONS[UnitType::MagneticFluxDensity as usize],
            prefix_groups: (Some(Rc::clone(&prefixes.short)), None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    // Binary
    map.insert(
        "b",
        Unit {
            name: &['b'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Bit as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.binary_short_si)),
                Some(Rc::clone(&prefixes.binary_short_iec)),
            ),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "bits",
        Unit {
            name: &['b', 'i', 't', 's'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Bit as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.binary_long_si)),
                Some(Rc::clone(&prefixes.binary_long_iec)),
            ),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "B",
        Unit {
            name: &['B'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Bit as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.binary_short_si)),
                Some(Rc::clone(&prefixes.binary_short_iec)),
            ),
            value: Decimal::from_i64(8).unwrap(),
            offset: Decimal::zero(),
        },
    );
    map.insert(
        "bytes",
        Unit {
            name: &['b', 'y', 't', 'e', 's'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Bit as usize],
            prefix_groups: (
                Some(Rc::clone(&prefixes.binary_long_si)),
                Some(Rc::clone(&prefixes.binary_long_iec)),
            ),
            value: Decimal::from_i64(8).unwrap(),
            offset: Decimal::zero(),
        },
    );

    map.insert(
        "$",
        Unit {
            name: &['$'],
            base: BASE_UNIT_DIMENSIONS[UnitType::Money as usize],
            prefix_groups: (None, None),
            value: Decimal::one(),
            offset: Decimal::zero(),
        },
    );

    let map = map
        .into_iter()
        .map(|(key, value)| (key, Rc::new(value)))
        .collect();

    return (map, prefixes);
}

pub fn init_aliases() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::<&str, &str>::with_capacity(100);

    map.insert("meters", "meter");
    map.insert("inches", "inch");
    map.insert("feet", "foot");
    map.insert("yards", "yard");
    map.insert("miles", "mile");
    map.insert("links", "link");
    map.insert("rods", "rod");
    map.insert("chains", "chain");
    map.insert("angstroms", "angstrom");

    map.insert("lt", "l");
    map.insert("litres", "litre");
    map.insert("liter", "litre");
    map.insert("liters", "litre");
    map.insert("teaspoons", "teaspoon");
    map.insert("tablespoons", "tablespoon");
    map.insert("minims", "minim");
    map.insert("fluiddrams", "fluiddram");
    map.insert("fluidounces", "fluidounce");
    map.insert("gills", "gill");
    map.insert("cups", "cup");
    map.insert("pints", "pint");
    map.insert("quarts", "quart");
    map.insert("gallons", "gallon");
    map.insert("beerbarrels", "beerbarrel");
    map.insert("oilbarrels", "oilbarrel");
    map.insert("hogsheads", "hogshead");
    map.insert("gtts", "gtt");

    map.insert("grams", "gram");
    map.insert("tons", "ton");
    map.insert("tonnes", "tonne");
    map.insert("grains", "grain");
    map.insert("drams", "dram");
    map.insert("ounces", "ounce");
    map.insert("poundmasses", "poundmass");
    map.insert("hundredweights", "hundredweight");
    map.insert("sticks", "stick");
    map.insert("lb", "lbm");
    map.insert("lbs", "lbm");

    map.insert("kips", "kip");

    map.insert("acres", "acre");
    map.insert("hectares", "hectare");
    map.insert("sqfeet", "sqft");
    map.insert("sqyard", "sqyd");
    map.insert("sqmile", "sqmi");
    map.insert("sqmiles", "sqmi");

    map.insert("mmhg", "mmHg");
    map.insert("mmh2o", "mmH2O");
    map.insert("cmh2o", "cmH2O");

    map.insert("seconds", "second");
    map.insert("secs", "second");
    map.insert("minutes", "minute");
    map.insert("mins", "minute");
    map.insert("hours", "hour");
    map.insert("hr", "hour");
    map.insert("hrs", "hour");
    map.insert("days", "day");
    map.insert("weeks", "week");
    map.insert("months", "month");
    map.insert("years", "year");
    map.insert("decades", "decade");
    map.insert("centuries", "century");
    map.insert("millennia", "millennium");

    map.insert("hertz", "Hertz");

    map.insert("radians", "radian");
    map.insert("degrees", "degree");
    map.insert("gradians", "gradian");
    map.insert("cycles", "cycle");
    map.insert("arcsecond", "arcsec");
    map.insert("arcseconds", "arcsec");
    map.insert("arcminute", "arcmin");
    map.insert("arcminutes", "arcmin");

    map.insert("BTUs", "BTU");
    map.insert("watts", "watt");
    map.insert("joules", "joule");

    map.insert("amperes", "ampere");
    map.insert("coulombs", "coulomb");
    map.insert("volts", "volt");
    map.insert("ohms", "ohm");
    map.insert("farads", "farad");
    map.insert("webers", "weber");
    map.insert("teslas", "tesla");
    map.insert("electronvolts", "electronvolt");
    map.insert("moles", "mole");

    map.insert("bit", "bits");
    map.insert("byte", "bytes");
    return map;
}

pub fn get_base_unit_for(
    units: &Units,
    dimensions: &[UnitDimensionExponent; BASE_UNIT_DIMENSION_COUNT],
) -> Option<UnitInstance> {
    if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Length as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["m"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Mass as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["g"]),
            prefix: Rc::clone(&units.prefixes.short[2]),
            /*k*/
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Time as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["s"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Current as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["A"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Temperature as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["K"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::LuminousIntensity as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["cd"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::AmountOfSubstance as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["mol"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Angle as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["rad"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Bit as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["bits"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Force as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["N"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Energy as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["J"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Power as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["W"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Pressure as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["Pa"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::ElectricCharge as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["C"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::ElectricCapacitance as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["F"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::ElectricResistance as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["ohm"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::ElectricInductance as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["H"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::ElectricConductance as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["S"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::MagneticFlux as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["Wb"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::MagneticFluxDensity as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["T"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Frequency as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["Hz"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else if dimensions == &BASE_UNIT_DIMENSIONS[UnitType::Money as usize] {
        Some(UnitInstance {
            unit: Rc::clone(&units.units["$"]),
            prefix: Rc::clone(&units.no_prefix),
            power: 1,
        })
    } else {
        None
    }
}
