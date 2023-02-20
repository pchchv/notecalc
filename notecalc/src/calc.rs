use crate::matrix::MatrixData;
use crate::units::units::UnitOutput;
use rust_decimal::prelude::*;

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

