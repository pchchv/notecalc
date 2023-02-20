use crate::matrix::MatrixData;
use crate::token_parser::TokenType;
use crate::units::units::UnitOutput;
use rust_decimal::prelude::*;

const DECIMAL_100: Decimal = Decimal::from_parts(100, 0, 0, false, 0);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CalcResult {
    pub typ: CalcResultType,
    pub index_into_tokens: usize,
    pub index2_into_tokens: Option<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CalcResultType {
    Number(Decimal),
    Percentage(Decimal),
    Unit(UnitOutput),
    Quantity(Decimal, UnitOutput),
    Matrix(MatrixData),
}

impl CalcResult {
    pub fn new(typ: CalcResultType, index: usize) -> CalcResult {
        CalcResult {
            typ,
            index_into_tokens: index,
            index2_into_tokens: None,
        }
    }

    pub fn new2(typ: CalcResultType, index: usize, index2: usize) -> CalcResult {
        CalcResult {
            typ,
            index_into_tokens: index,
            index2_into_tokens: Some(index2),
        }
    }

    pub fn get_index_into_tokens(&self) -> usize {
        self.index_into_tokens
    }

    /// hack_empty creates a cheap CalcResult without memory allocation.
    /// Use it only as a temporary value.
    pub fn hack_empty() -> CalcResult {
        CalcResult {
            typ: CalcResultType::Matrix(MatrixData {
                cells: Vec::new(),
                row_count: 0,
                col_count: 0,
            }),
            index_into_tokens: 0,
            index2_into_tokens: None,
        }
    }

    pub fn zero() -> CalcResult {
        CalcResult::new(CalcResultType::Number(Decimal::zero()), 0)
    }
}

#[derive(Debug)]
pub struct EvaluationResult {
    pub there_was_unit_conversion: bool,
    pub there_was_operation: bool,
    pub assignment: bool,
    pub result: CalcResult,
}

#[derive(Debug, Clone)]
pub struct ShuntingYardResult {
    pub typ: TokenType,
    pub index_into_tokens: usize,
}

impl ShuntingYardResult {
    pub fn new(typ: TokenType, index_into_tokens: usize) -> ShuntingYardResult {
        ShuntingYardResult {
            typ,
            index_into_tokens,
        }
    }
}

pub fn divide_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    let result: Option<CalcResult> = match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Unit(..), CalcResultType::Unit(..))
        | (CalcResultType::Unit(..), CalcResultType::Number(..))
        | (CalcResultType::Unit(..), CalcResultType::Quantity(..))
        | (CalcResultType::Unit(..), CalcResultType::Percentage(..))
        | (CalcResultType::Unit(..), CalcResultType::Matrix(..))
        | (CalcResultType::Matrix(..), CalcResultType::Unit(..)) => None,
        //////////////
        // 30 years * 12/year
        //////////////
        (CalcResultType::Quantity(..), CalcResultType::Unit(rhs_unit)) => divide_op(
            lhs,
            &CalcResult {
                typ: CalcResultType::Quantity(Decimal::one(), rhs_unit.clone()),
                index_into_tokens: 0,
                index2_into_tokens: None,
            },
        ),
        (CalcResultType::Number(num), CalcResultType::Unit(unit)) => {
            let new_unit = unit.pow(-1)?;
            Some(CalcResult::new(
                CalcResultType::Quantity(num.clone(), new_unit),
                0,
            ))
        }
        //////////////
        // 5% / year
        //////////////
        (CalcResultType::Percentage(num), CalcResultType::Unit(unit)) => {
            let new_unit = unit.pow(-1)?;
            Some(CalcResult::new(
                CalcResultType::Quantity(num.checked_div(&DECIMAL_100)?, new_unit),
                0,
            ))
        }
        //////////////
        // 12 / x
        //////////////
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 100 / 2
            Some(CalcResult::new(
                CalcResultType::Number(lhs.checked_div(&rhs)?),
                0,
            ))
        }
        (CalcResultType::Number(lhs_num), CalcResultType::Quantity(rhs_num, unit)) => {
            // 100 / 2km => 100 / (2 km)
            let new_unit = unit.pow(-1)?;

            if rhs_num.is_zero() {
                return None;
            }
            let num_part = (lhs_num.checked_div(rhs_num))?;
            Some(CalcResult::new(
                CalcResultType::Quantity(num_part, new_unit),
                0,
            ))
        }
        (CalcResultType::Number(lhs), CalcResultType::Percentage(rhs)) => {
            if rhs.is_zero() {
                return None;
            }
            // 100 / 50%
            Some(CalcResult::new(
                CalcResultType::Percentage(lhs.checked_div(rhs)?.checked_mul(&DECIMAL_100)?),
                0,
            ))
        }
        (CalcResultType::Number(..), CalcResultType::Matrix(..)) => None,
        //////////////
        // 12km / x
        //////////////
        (CalcResultType::Quantity(lhs_num, lhs_unit), CalcResultType::Number(rhs)) => {
            // 2m / 5
            if rhs.is_zero() {
                return None;
            }
            Some(CalcResult::new(
                CalcResultType::Quantity(lhs_num / rhs, lhs_unit.clone()),
                0,
            ))
        }
        (
            CalcResultType::Quantity(lhs_num, lhs_unit),
            CalcResultType::Quantity(rhs_num, rhs_unit),
        ) => {
            // 12 km / 3s
            if rhs_num.is_zero() {
                return None;
            } else if lhs_unit.unit_count + rhs_unit.unit_count >= MAX_UNIT_COUNT {
                None
            } else {
                return if lhs_unit == rhs_unit {
                    let result = lhs_num.checked_div(rhs_num)?;
                    Some(CalcResult::new(CalcResultType::Number(result), 0))
                } else {
                    let result = lhs_num.checked_div(&rhs_num)?;
                    let (new_unit, k) = lhs_unit.div(rhs_unit)?;
                    let corrected_result = result.checked_div(&k)?;
                    Some(if new_unit.is_unitless() {
                        CalcResult::new(CalcResultType::Number(corrected_result), 0)
                    } else {
                        CalcResult::new(CalcResultType::Quantity(corrected_result, new_unit), 0)
                    })
                };
            }
        }
        (CalcResultType::Quantity(_lhs, _lhs_unit), CalcResultType::Percentage(_rhs)) => {
            // 2m / 50%
            None
        }
        (CalcResultType::Quantity(..), CalcResultType::Matrix(..)) => None,
        //////////////
        // 12% / x
        //////////////
        (CalcResultType::Percentage(_lhs), CalcResultType::Number(_rhs)) => {
            // 5% / 10
            None
        }
        (CalcResultType::Percentage(_lhs), CalcResultType::Quantity(_rhs, _rhs_unit)) => {
            // 5% / 10km
            None
        }
        (CalcResultType::Percentage(_lhs), CalcResultType::Percentage(_rhs)) => {
            // 50% / 50%
            None
        }
        (CalcResultType::Percentage(..), CalcResultType::Matrix(..)) => None,
        (CalcResultType::Matrix(mat), CalcResultType::Number(..))
        | (CalcResultType::Matrix(mat), CalcResultType::Quantity(..))
        | (CalcResultType::Matrix(mat), CalcResultType::Percentage(..)) => mat.div_scalar(rhs),
        (CalcResultType::Matrix(..), CalcResultType::Matrix(..)) => None,
    };
    return match result {
        Some(CalcResult {
            typ: CalcResultType::Quantity(num, unit),
            ..
        }) if unit.is_unitless() => {
            // some operation cancelled out its units, put a simple number on the stack
            Some(CalcResult::new(CalcResultType::Number(num), 0))
        }
        _ => result,
    };
}

