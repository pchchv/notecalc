use crate::functions::FnType;
use crate::units::units::{UnitOutput, Units};
use crate::{
    tracy_span, FunctionDefinitions, Variable, Variables, FIRST_FUNC_PARAM_VAR_INDEX,
    SUM_VARIABLE_INDEX,
};
use rust_decimal::prelude::*;

pub const APPLY_UNIT_OP_PREC: usize = 5;

#[allow(variant_size_differences)]
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

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Assoc {
    Left,
    Right,
}

#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OperatorTokenType {
    Comma,
    Add,
    UnaryPlus,
    Sub,
    UnaryMinus,
    Mult,
    Div,
    Perc,
    BinAnd,
    BinOr,
    BinXor,
    BinNot,
    Pow,
    ParenOpen,
    ParenClose,
    BracketOpen,
    Semicolon,
    BracketClose,
    ShiftLeft,
    ShiftRight,
    Assign,
    UnitConverter,
    StartLock,
    Matrix { row_count: usize, col_count: usize },
    Fn { arg_count: usize, typ: FnType },
    PercentageIs,
    // 41 is 17% on what
    Percentage_Find_Base_From_Result_Increase_X,
    // what plus 17% is 41
    Percentage_Find_Base_From_X_Icrease_Result,
    // 17% on what is 41
    Percentage_Find_Base_From_Icrease_X_Result,
    // 41 is what % on 35
    Percentage_Find_Incr_Rate_From_Result_X_Base,
    // 41 is 17% off what
    Percentage_Find_Base_From_Result_Decrease_X,
    // what minus 17% is 41
    Percentage_Find_Base_From_X_Decrease_Result,
    // 17% off what is 41
    Percentage_Find_Base_From_Decrease_X_Result,
    // 35 is what % off 41
    Percentage_Find_Decr_Rate_From_Result_X_Base,

    // 20 is what percent of 60
    Percentage_Find_Rate_From_Result_Base,
    // 5 is 25% of what
    Percentage_Find_Base_From_Result_Rate,
}

impl OperatorTokenType {
    pub fn precedence(&self) -> usize {
        match self {
            OperatorTokenType::Add => 2,
            OperatorTokenType::UnaryPlus => 4,
            OperatorTokenType::Sub => 2,
            OperatorTokenType::UnaryMinus => 4,
            OperatorTokenType::Mult => 3,
            OperatorTokenType::Div => 3,
            OperatorTokenType::Perc => 6,
            OperatorTokenType::BinAnd => 0,
            OperatorTokenType::BinOr => 0,
            OperatorTokenType::BinXor => 0,
            OperatorTokenType::BinNot => 4,
            OperatorTokenType::Pow => APPLY_UNIT_OP_PREC + 1,
            OperatorTokenType::ParenOpen => 0,
            OperatorTokenType::ParenClose => 0,
            OperatorTokenType::ShiftLeft => 0,
            OperatorTokenType::ShiftRight => 0,
            OperatorTokenType::Assign => 0,
            OperatorTokenType::UnitConverter => 0,
            OperatorTokenType::Semicolon | OperatorTokenType::Comma => 0,
            OperatorTokenType::BracketOpen => 0,
            OperatorTokenType::StartLock => 0,
            OperatorTokenType::BracketClose => 0,
            OperatorTokenType::Matrix { .. } => 0,
            OperatorTokenType::Fn { .. } => 0,
            OperatorTokenType::PercentageIs => 20,
            OperatorTokenType::Percentage_Find_Base_From_Result_Increase_X => 10,
            OperatorTokenType::Percentage_Find_Base_From_X_Icrease_Result => 10,
            OperatorTokenType::Percentage_Find_Base_From_Icrease_X_Result => 10,
            OperatorTokenType::Percentage_Find_Incr_Rate_From_Result_X_Base => 10,
            OperatorTokenType::Percentage_Find_Base_From_Result_Decrease_X => 10,
            OperatorTokenType::Percentage_Find_Base_From_X_Decrease_Result => 10,
            OperatorTokenType::Percentage_Find_Base_From_Decrease_X_Result => 10,
            OperatorTokenType::Percentage_Find_Decr_Rate_From_Result_X_Base => 10,
            OperatorTokenType::Percentage_Find_Rate_From_Result_Base => 10,
            OperatorTokenType::Percentage_Find_Base_From_Result_Rate => 10,
        }
    }

    pub fn assoc(&self) -> Assoc {
        match self {
            OperatorTokenType::ParenClose => Assoc::Left,
            OperatorTokenType::Add => Assoc::Left,
            OperatorTokenType::UnaryPlus => Assoc::Left,
            OperatorTokenType::Sub => Assoc::Left,
            OperatorTokenType::UnaryMinus => Assoc::Left,
            OperatorTokenType::Mult => Assoc::Left,
            OperatorTokenType::Div => Assoc::Left,
            OperatorTokenType::Perc => Assoc::Left,
            OperatorTokenType::BinAnd => Assoc::Left,
            OperatorTokenType::BinOr => Assoc::Left,
            OperatorTokenType::BinXor => Assoc::Left,
            OperatorTokenType::BinNot => Assoc::Left,
            OperatorTokenType::Pow => Assoc::Right,
            OperatorTokenType::ParenOpen => Assoc::Left,
            OperatorTokenType::ShiftLeft => Assoc::Left,
            OperatorTokenType::ShiftRight => Assoc::Left,
            OperatorTokenType::Assign => Assoc::Left,
            OperatorTokenType::UnitConverter => Assoc::Left,
            // Right, so 1 comma won't replace an other on the operator stack
            OperatorTokenType::Semicolon | OperatorTokenType::Comma => Assoc::Right,
            OperatorTokenType::BracketOpen => Assoc::Left,
            OperatorTokenType::BracketClose => Assoc::Left,
            OperatorTokenType::Matrix { .. } => Assoc::Left,
            OperatorTokenType::StartLock => Assoc::Left,
            OperatorTokenType::Fn { .. } => Assoc::Left,
            OperatorTokenType::PercentageIs => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_Result_Increase_X => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_X_Icrease_Result => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_Icrease_X_Result => Assoc::Left,
            OperatorTokenType::Percentage_Find_Incr_Rate_From_Result_X_Base => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_Result_Decrease_X => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_X_Decrease_Result => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_Decrease_X_Result => Assoc::Left,
            OperatorTokenType::Percentage_Find_Decr_Rate_From_Result_X_Base => Assoc::Left,
            OperatorTokenType::Percentage_Find_Rate_From_Result_Base => Assoc::Left,
            OperatorTokenType::Percentage_Find_Base_From_Result_Rate => Assoc::Left,
        }
    }
}
