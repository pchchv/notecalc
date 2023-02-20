#[allow(variant_size_differences)]
use crate::units::units::{UnitOutput, Units};
use crate::Variable;
use rust_decimal::prelude::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    StringLiteral,
    Header,
    // index to the variable vec
    Variable { var_index: usize },
    LineReference { var_index: usize },
    NumberLiteral(Decimal),
    Operator(OperatorTokenType),
    Unit(UnitTokenType, UnitOutput),
    NumberErr,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnitTokenType {
    ApplyToPrevToken,
    StandInItself,
}
