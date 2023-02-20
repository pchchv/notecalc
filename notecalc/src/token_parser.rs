use crate::functions::FnType;
use crate::units::units::{UnitOutput, Units};
use crate::{
    tracy_span, FunctionDefinitions, Variable, Variables, FIRST_FUNC_PARAM_VAR_INDEX,
    SUM_VARIABLE_INDEX,
};
use bumpalo::Bump;
use rust_decimal::prelude::*;

pub const APPLY_UNIT_OP_PREC: usize = 5;

/// 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989
pub const DECIMAL_PI: Decimal = Decimal::from_parts(1102470953, 185874565, 1703060790, false, 28);

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

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub ptr: &'a [char],
    pub typ: TokenType,
    pub has_error: bool,
}

impl<'text_ptr> Token<'text_ptr> {
    pub fn is_number(&self) -> bool {
        matches!(self.typ, TokenType::NumberLiteral(..))
    }

    pub fn is_string(&self) -> bool {
        matches!(self.typ, TokenType::StringLiteral)
    }

    pub fn has_error(&self) -> bool {
        self.has_error
    }

    pub fn set_token_error_flag_by_index(index: usize, tokens: &mut [Token]) {
        if let Some(t) = tokens.get_mut(index) {
            t.has_error = true
        }
    }
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

pub struct TokenParser {}

impl TokenParser {
    pub fn parse_line<'text_ptr>(
        line: &[char],
        variable_names: &Variables,
        dst: &mut Vec<Token<'text_ptr>>,
        units: &Units,
        line_index: usize,
        allocator: &'text_ptr Bump,
        function_param_count: usize,
        func_defs: &FunctionDefinitions<'text_ptr>,
    ) {
        tracy_span("parse_line", file!(), line!());
        let mut index = 0;
        let mut can_be_unit = None;
        let mut can_be_unit_converter = false;
        if line.starts_with(&['#']) {
            dst.push(Token {
                ptr: allocator.alloc_slice_fill_iter(line.iter().map(|it| *it)),
                typ: TokenType::Header,
                has_error: false,
            });
            return;
        }
        while index < line.len() {
            let parse_result = TokenParser::try_extract_token(
                &line[index..],
                variable_names,
                &dst,
                units,
                line_index,
                can_be_unit,
                can_be_unit_converter,
                allocator,
                function_param_count,
                func_defs,
            );

            match &parse_result.typ {
                TokenType::Header => {
                    // the functions already returned in this case
                    panic!();
                }
                TokenType::StringLiteral => {
                    if parse_result.ptr[0].is_ascii_whitespace() {
                        // keep can_be_unit as it was
                    } else {
                        can_be_unit = None;
                        can_be_unit_converter = false;
                    }
                }
                TokenType::NumberLiteral(..) | TokenType::NumberErr => {
                    can_be_unit = Some(UnitTokenType::ApplyToPrevToken);
                    can_be_unit_converter = false;
                }
                TokenType::Unit(..) => {
                    can_be_unit = Some(UnitTokenType::StandInItself);
                    can_be_unit_converter = true;
                }
                TokenType::Operator(typ) => {
                    match typ {
                        OperatorTokenType::ParenClose => {
                            can_be_unit = Some(UnitTokenType::ApplyToPrevToken);
                        }
                        OperatorTokenType::BracketClose => {
                            // keep can_be_unit as it was
                        }
                        OperatorTokenType::UnitConverter => {
                            can_be_unit = Some(UnitTokenType::StandInItself);
                            can_be_unit_converter = false;
                        }
                        OperatorTokenType::Div => can_be_unit = Some(UnitTokenType::StandInItself),
                        _ => {
                            can_be_unit = None;
                            can_be_unit_converter = false;
                        }
                    }
                }
                TokenType::Variable { .. } | TokenType::LineReference { .. } => {
                    can_be_unit = Some(UnitTokenType::ApplyToPrevToken);
                    can_be_unit_converter = true;
                }
            }
            index += parse_result.ptr.len();
            dst.push(parse_result);
        }
    }

    #[inline]
    fn try_extract_unit<'text_ptr>(
        str: &[char],
        unit: &Units,
        can_be_unit: Option<UnitTokenType>,
        can_be_unit_converter: bool,
        allocator: &'text_ptr Bump,
    ) -> Option<Token<'text_ptr>> {
        tracy_span("try_extract_unit", file!(), line!());
        if can_be_unit.is_none() || str[0].is_ascii_whitespace() {
            return None;
        }
        let (unit, parsed_len) = unit.parse(str);
        let result = if parsed_len == 0 {
            None
        } else {
            // remove trailing spaces
            let mut i = parsed_len;
            while i > 0 && str[i - 1].is_ascii_whitespace() {
                i -= 1;
            }
            let ptr = allocator.alloc_slice_fill_iter(str.iter().map(|it| *it).take(i));
            if ptr == &['i', 'n'] && can_be_unit_converter {
                // 'in' is always a UnitConverter, the shunting_yard will convert it to
                // ApplyUnit or String if necessary.
                // It is required "&[1] in", here, we don't know yet if 'in' is a converter or a unit
                // for the referenced value
                // However with 'can_be_unit_converter' we try to reduce its occurance
                // as much as possible
                Some(Token {
                    typ: TokenType::Operator(OperatorTokenType::UnitConverter),
                    ptr,
                    has_error: false,
                })
            } else {
                if let Some(can_be_unit) = can_be_unit {
                    Some(Token {
                        typ: TokenType::Unit(can_be_unit, unit),
                        ptr,
                        has_error: false,
                    })
                } else {
                    panic!("impossible")
                }
            }
        };
        return result;
    }

    #[inline]
    fn try_extract_comment<'text_ptr>(
        line: &[char],
        allocator: &'text_ptr Bump,
    ) -> Option<Token<'text_ptr>> {
        return if line.starts_with(&['/', '/']) {
            Some(Token {
                typ: TokenType::StringLiteral,
                ptr: allocator.alloc_slice_fill_iter(line.iter().map(|it| *it)),
                has_error: false,
            })
        } else {
            None
        };
    }

    #[inline]
    fn try_extract_variable_name<'text_ptr>(
        line: &[char],
        vars: &Variables,
        parsed_row_index: usize,
        allocator: &'text_ptr Bump,
        prev_was_lineref: bool,
        function_param_count: usize,
        func_defs: &FunctionDefinitions<'text_ptr>,
    ) -> Option<Token<'text_ptr>> {
        tracy_span("try_extract_variable_name", file!(), line!());
        if line.starts_with(&['s', 'u', 'm'])
            && line
                .get(3)
                .map(|it| !it.is_alphanumeric() && *it != '_' && *it != '(')
                .unwrap_or(true)
        {
            return Some(Token {
                typ: TokenType::Variable {
                    var_index: SUM_VARIABLE_INDEX,
                },
                ptr: allocator.alloc_slice_fill_iter(line.iter().map(|it| *it).take(3)),
                has_error: false,
            });
        }

        let mut longest_match_index = 0;
        let mut longest_match = 0;
        // first try to find the var in 'vars' so parameters can be shadowed
        'outer: for (var_index, var) in vars[0..parsed_row_index].iter().enumerate().rev() {
            // avoid variable declarations that are in a function we are not part of
            for investigated_line_i in (0..=var_index).rev() {
                if let Some(fd) = func_defs[investigated_line_i].as_ref() {
                    let investigated_line_is_part_of_that_func =
                        var_index <= fd.last_row_index.as_usize();
                    let parsed_line_is_not_part_of_that_func =
                        parsed_row_index > fd.last_row_index.as_usize();
                    if investigated_line_is_part_of_that_func {
                        if parsed_line_is_not_part_of_that_func {
                            // if this line belongs to a function we are not part of
                            continue 'outer;
                        } else {
                            // first check the lines inside the function
                            // then check the parameters
                            // then the lines above the function
                            if investigated_line_i == var_index {
                                for (var_index, var) in vars[FIRST_FUNC_PARAM_VAR_INDEX
                                    ..FIRST_FUNC_PARAM_VAR_INDEX + function_param_count]
                                    .iter()
                                    .enumerate()
                                {
                                    TokenParser::find_variable_match(
                                        line,
                                        &mut longest_match_index,
                                        &mut longest_match,
                                        FIRST_FUNC_PARAM_VAR_INDEX + var_index,
                                        var,
                                    )
                                }
                                continue 'outer;
                            }
                        }
                    }
                    break;
                }
            }

            TokenParser::find_variable_match(
                line,
                &mut longest_match_index,
                &mut longest_match,
                var_index,
                var,
            );
        }

        let result = if longest_match > 0 {
            let is_line_ref = longest_match > 2 && line[0] == '&' && line[1] == '[';
            let typ = if is_line_ref {
                if prev_was_lineref {
                    return None;
                } else {
                    TokenType::LineReference {
                        var_index: longest_match_index,
                    }
                }
            } else {
                TokenType::Variable {
                    var_index: longest_match_index,
                }
            };
            Some(Token {
                typ,
                ptr: allocator.alloc_slice_fill_iter(line.iter().map(|it| *it).take(longest_match)),
                has_error: false,
            })
        } else {
            None
        };
        return result;
    }

    fn find_variable_match(
        line: &[char],
        longest_match_index: &mut usize,
        longest_match: &mut usize,
        var_index: usize,
        var: &Option<Variable>,
    ) {
        if var.is_none() {
            return;
        }
        let var = var.as_ref().unwrap();

        for (i, ch) in var.name.iter().enumerate() {
            if i >= line.len() || line[i] != *ch {
                return;
            }
        }
        // if the next char is '(', it can't be a var name
        if line
            .get(var.name.len())
            .map(|it| *it == '(')
            .unwrap_or(false)
        {
            return;
        }
        // only full match allowed e.g. if there is variable 'b', it should not match "b0" as 'b' and '0'
        let not_full_match = line
            .get(var.name.len())
            .map(|it| it.is_alphanumeric())
            .unwrap_or(false);
        if not_full_match {
            return;
        }
        if var.name.len() > *longest_match {
            *longest_match = var.name.len();
            *longest_match_index = var_index;
        }
    }

    #[inline]
    fn try_extract_string_literal<'text_ptr>(
        str: &[char],
        allocator: &'text_ptr Bump,
    ) -> Token<'text_ptr> {
        tracy_span("try_extract_string_literal", file!(), line!());
        let mut i = 0;
        for ch in str {
            if "=%/+-*^()[],".chars().any(|it| it == *ch) || ch.is_ascii_whitespace() {
                break;
            }
            // it means somwewhere we passed an invalid slice
            debug_assert!(*ch as u8 != 0);
            i += 1;
        }
        let result = if i > 0 {
            // alphabetical literal
            Token {
                typ: TokenType::StringLiteral,
                ptr: allocator.alloc_slice_fill_iter(str.iter().map(|it| *it).take(i)),
                has_error: false,
            }
        } else {
            for ch in &str[0..] {
                if !ch.is_ascii_whitespace() {
                    break;
                }
                i += 1;
            }
            if i > 0 {
                // whitespace
                Token {
                    typ: TokenType::StringLiteral,
                    // ptr: &str[0..i],
                    ptr: allocator.alloc_slice_fill_iter(str.iter().map(|it| *it).take(i)),
                    has_error: false,
                }
            } else {
                panic!("cannot happen")
            }
        };
        return result;
    }
