use crate::borrow_checker_fighter::create_vars;
use crate::editor::editor_content::EditorContent;
use crate::functions::FnType;
use crate::helper::{content_y, AppTokens, BitFlag256};
use crate::matrix::MatrixData;
use crate::token_parser::{debug_print, OperatorTokenType, TokenType, UnitTokenType};
use crate::units::consts::EMPTY_UNIT_DIMENSIONS;
use crate::units::units::{UnitOutput, Units, MAX_UNIT_COUNT};
use crate::{
    tracy_span, FunctionDefinitions, LineData, Variable, Variables, FIRST_FUNC_PARAM_VAR_INDEX,
    MAX_TOKEN_COUNT_PER_LINE, SUM_VARIABLE_INDEX, VARIABLE_ARR_SIZE,
};
use rust_decimal::prelude::*;
use std::ops::{BitXor, Neg, Not, Shl, Shr};
use tinyvec::ArrayVec;

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

#[derive(Debug)]
pub struct EvalErr {
    pub token_index: usize,
    pub token_index_lhs_1: Option<usize>,
    pub token_index_lhs_2: Option<usize>,
    pub token_index_rhs_1: Option<usize>,
    pub token_index_rhs_2: Option<usize>,
    pub reason: String,
}
impl EvalErr {
    pub fn new(str: String, token_index: usize) -> EvalErr {
        EvalErr {
            token_index,
            token_index_lhs_1: None,
            token_index_lhs_2: None,
            token_index_rhs_1: None,
            token_index_rhs_2: None,
            reason: str,
        }
    }

    pub fn new2(str: String, param: &CalcResult) -> EvalErr {
        EvalErr {
            token_index: param.index_into_tokens,
            token_index_lhs_1: param.index2_into_tokens,
            token_index_lhs_2: None,
            token_index_rhs_1: None,
            token_index_rhs_2: None,
            reason: str,
        }
    }

    pub fn new3(str: String, token_index: usize, lhs: &CalcResult, rhs: &CalcResult) -> EvalErr {
        EvalErr {
            token_index,
            reason: str,
            token_index_lhs_1: Some(lhs.index_into_tokens),
            token_index_lhs_2: lhs.index2_into_tokens,
            token_index_rhs_1: Some(rhs.index_into_tokens),
            token_index_rhs_2: rhs.index2_into_tokens,
        }
    }
}

pub fn evaluate_tokens<'text_ptr>(
    editor_y: usize,
    apptokens: &AppTokens,
    vars: &Variables,
    func_defs: &FunctionDefinitions,
    units: &Units,
    editor_content: &EditorContent<LineData>,
    call_depth: usize,
    fn_context_index: Option<usize>,
) -> (BitFlag256, Result<Option<EvaluationResult>, EvalErr>) {
    let _span = tracy_span("calc", file!(), line!());
    let mut wrong_type_token_indices: BitFlag256 = BitFlag256::empty();

    if call_depth >= 5 {
        return (
            wrong_type_token_indices,
            Err(EvalErr::new("too much recursion TODO index".to_string(), 0)),
        );
    }

    let mut stack: Vec<CalcResult> = vec![];
    let mut there_was_unit_conversion = false;
    let mut assignment = false;
    let mut last_success_operation_result_index = None;

    let mut last_op_was_fn_call = false;

    let len = apptokens[content_y(editor_y)]
        .as_ref()
        .unwrap()
        .shunting_output_stack
        .len();
    let mut lock_stack: ArrayVec<[u16; 64]> = ArrayVec::new();
    for i in 0..len {
        let mut op_is_fn_call = false;
        let token_type = apptokens[content_y(editor_y)]
            .as_ref()
            .unwrap()
            .shunting_output_stack[i]
            .typ
            .clone(); // I hate rust;
        debug_print(&format!("calc> {:?}", token_type));
        match &token_type {
            TokenType::NumberLiteral(num) => {
                let shunting_tokens = &apptokens[content_y(editor_y)]
                    .as_ref()
                    .unwrap()
                    .shunting_output_stack;
                let token = &shunting_tokens[i];
                stack.push(CalcResult::new(
                    CalcResultType::Number(num.clone()),
                    token.index_into_tokens,
                ))
            }
            TokenType::NumberErr => {
                let shunting_tokens = &apptokens[content_y(editor_y)]
                    .as_ref()
                    .unwrap()
                    .shunting_output_stack;
                let token = &shunting_tokens[i];
                return (
                    wrong_type_token_indices,
                    Err(EvalErr::new(
                        "Number parsin/format".to_string(),
                        token.index_into_tokens,
                    )),
                );
            }
            TokenType::Unit(unit_typ, target_unit) => {
                // next token must be a UnitConverter or Div
                if *unit_typ == UnitTokenType::ApplyToPrevToken
                    || (*unit_typ == UnitTokenType::StandInItself && last_op_was_fn_call)
                {
                    let operand = stack.last();
                    if let Some(CalcResult {
                        typ: CalcResultType::Number(operand_num),
                        index_into_tokens,
                        index2_into_tokens: _index2_into_tokens,
                    }) = operand
                    {
                        if let Some(result) = apply_unit_to_num(
                            operand_num,
                            target_unit,
                            *index_into_tokens,
                            get_token_index_into_tokens(apptokens, editor_y, i),
                        ) {
                            stack.pop();
                            debug_print(&format!("calc> result = {:?}", &result));
                            stack.push(result);
                            last_success_operation_result_index = Some(stack.len() - 1);
                        } else {
                            return (
                                wrong_type_token_indices,
                                Err(EvalErr::new(
                                    format!(
                                        "Could not apply '{}' to '{}'",
                                        target_unit, operand_num
                                    ),
                                    get_token_index_into_tokens(apptokens, editor_y, i),
                                )),
                            );
                        }
                    } else {
                        return (
                            wrong_type_token_indices,
                            Err(EvalErr::new(
                                format!("There is no operand to apply '{}' on", target_unit),
                                get_token_index_into_tokens(apptokens, editor_y, i),
                            )),
                        );
                    }
                } else {
                    let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
                    let shunting_tokens = &tokens.shunting_output_stack;
                    let token = &shunting_tokens[i];
                    if shunting_tokens
                        .get(i + 1)
                        .map(|it| {
                            matches!(
                                it.typ,
                                TokenType::Operator(OperatorTokenType::UnitConverter)
                                    | TokenType::Operator(OperatorTokenType::Div)
                            )
                        })
                        .unwrap_or(false)
                    {
                        stack.push(CalcResult::new(
                            CalcResultType::Unit(target_unit.clone()),
                            token.index_into_tokens,
                        ));
                    } else {
                        if token.index_into_tokens <= MAX_TOKEN_COUNT_PER_LINE {
                            wrong_type_token_indices.set(token.index_into_tokens);
                        }
                    }
                }
            }
            TokenType::Operator(OperatorTokenType::StartLock) => {
                lock_stack.push(stack.len() as u16);
            }
            TokenType::Operator(OperatorTokenType::Fn {
                typ: FnType::UserDefined(fn_index),
                arg_count,
            }) => {
                let fd = func_defs[*fn_index].as_ref().unwrap();
                let expected_arg_count = fd.param_count;
                if *arg_count != expected_arg_count || stack.len() < expected_arg_count {
                    let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
                    let shunting_tokens = &tokens.shunting_output_stack;
                    let index_into_tokens = shunting_tokens[i].index_into_tokens;
                    return (
                        wrong_type_token_indices,
                        Err(EvalErr::new(
                            format!(
                                "Expected {} arguments, provided {}",
                                expected_arg_count, *arg_count
                            ),
                            index_into_tokens,
                        )),
                    );
                }
                // fill variables from the stack
                let mut local_vars = create_vars();

                local_vars[0..SUM_VARIABLE_INDEX].clone_from_slice(&vars[0..SUM_VARIABLE_INDEX]);
                local_vars[SUM_VARIABLE_INDEX + 1..VARIABLE_ARR_SIZE]
                    .clone_from_slice(&vars[SUM_VARIABLE_INDEX + 1..VARIABLE_ARR_SIZE]);
                for i in (0..expected_arg_count).rev() {
                    local_vars[FIRST_FUNC_PARAM_VAR_INDEX + i] = Some(Variable {
                        name: Box::from(fd.param_names[i]),
                        value: stack.pop().ok_or(()),
                    });
                }
                let mut result: Result<Option<EvaluationResult>, EvalErr> =
                    Err(EvalErr::new(String::new(), 0));
                let mut sum_is_null = true;
                debug_print("calc> evaluate function");
                for i in (fd.first_row_index.as_usize() + 1)..=fd.last_row_index.as_usize() {
                    result = evaluate_tokens(
                        i,
                        apptokens,
                        &local_vars,
                        &func_defs,
                        units,
                        editor_content,
                        if fn_context_index.map(|it| it == *fn_index).unwrap_or(false) {
                            call_depth + 1
                        } else {
                            call_depth
                        },
                        Some(*fn_index),
                    )
                    .1;
                    process_variable_assignment_or_line_ref(
                        &result,
                        &mut local_vars,
                        i,
                        editor_content,
                    );
                    if let Ok(Some(result)) = result.as_ref() {
                        if sum_is_null {
                            sum_is_null = false;
                            local_vars[SUM_VARIABLE_INDEX].as_mut().unwrap().value =
                                Ok(result.result.clone());
                        }
                    }
                }
                debug_print("calc> evaluate end");
                if let Ok(Some(result)) = result {
                    stack.push(result.result);
                    last_success_operation_result_index = Some(stack.len() - 1);
                    op_is_fn_call = true;
                } else {
                    let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
                    let shunting_tokens = &tokens.shunting_output_stack;
                    let index_into_tokens = shunting_tokens[i].index_into_tokens;
                    return (
                        wrong_type_token_indices,
                        Err(EvalErr::new("No result".to_owned(), index_into_tokens)),
                    );
                }
            }
            TokenType::Operator(typ) => {
                if *typ == OperatorTokenType::Assign {
                    assignment = true;
                    continue;
                }
                let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
                let shunting_tokens = &tokens.shunting_output_stack;
                let token = &shunting_tokens[i];

                if let Err(eval_err) = apply_operation(
                    &mut stack,
                    &typ,
                    token.index_into_tokens,
                    units,
                    &mut lock_stack,
                ) {
                    return (wrong_type_token_indices, Err(eval_err));
                } else {
                    if matches!(typ, OperatorTokenType::UnitConverter) {
                        there_was_unit_conversion = true;
                    } else if matches!(typ, OperatorTokenType::Fn { .. }) {
                        op_is_fn_call = true;
                    }
                    if !stack.is_empty() {
                        last_success_operation_result_index = Some(stack.len() - 1);
                    }
                }
            }
            TokenType::StringLiteral | TokenType::Header => panic!(),
            TokenType::Variable { var_index } | TokenType::LineReference { var_index } => {
                match &vars[*var_index]
                    .as_ref()
                    .expect("var_index should be valid!")
                    .value
                {
                    Ok(value) => {
                        let shunting_tokens = &apptokens[content_y(editor_y)]
                            .as_ref()
                            .unwrap()
                            .shunting_output_stack;
                        let token = &shunting_tokens[i];
                        stack.push(CalcResult::new(value.typ.clone(), token.index_into_tokens));
                    }
                    Err(_) => {
                        let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
                        let shunting_tokens = &tokens.shunting_output_stack;
                        let index_into_tokens = shunting_tokens[i].index_into_tokens;
                        return (
                            wrong_type_token_indices,
                            Err(EvalErr::new(
                                "Variable contains error".to_owned(),
                                index_into_tokens,
                            )),
                        );
                    }
                }
            }
        }
        last_op_was_fn_call = op_is_fn_call;
    }

    let result = match last_success_operation_result_index {
        Some(last_success_operation_index) => {
            // e.g. "1+2 some text 3"
            // in this case prefer the result of 1+2 and convert 3 to String
            for (i, stack_elem) in stack.iter().enumerate() {
                if last_success_operation_index != i {
                    if stack_elem.index_into_tokens <= MAX_TOKEN_COUNT_PER_LINE {
                        wrong_type_token_indices.set(stack_elem.index_into_tokens);
                    }
                }
            }
            Ok(Some(EvaluationResult {
                there_was_unit_conversion,
                there_was_operation: true,
                assignment,
                result: stack[last_success_operation_index].clone(),
            }))
        }
        None => Ok(stack.pop().map(|it| EvaluationResult {
            there_was_operation: false,
            there_was_unit_conversion,
            assignment,
            result: it,
        })),
    };

    return (wrong_type_token_indices, result);
}

pub fn process_variable_assignment_or_line_ref<'a, 'b>(
    result: &Result<Option<EvaluationResult>, EvalErr>,
    vars: &mut Variables,
    editor_y: usize,
    editor_content: &EditorContent<LineData>,
) {
    if let Ok(Some(result)) = &result {
        fn replace_or_insert_var(
            vars: &mut Variables,
            var_name: &[char],
            result: CalcResult,
            editor_y: usize,
        ) {
            if let Some(var) = &mut vars[editor_y] {
                var.name = Box::from(var_name);
                var.value = Ok(result);
            } else {
                vars[editor_y] = Some(Variable {
                    name: Box::from(var_name),
                    value: Ok(result),
                });
            };
        }

        if result.assignment {
            let var_name = get_var_name_from_assignment(editor_y, editor_content);
            if !var_name.is_empty() {
                debug_print(&format!(
                    "eval> assign {:?} at {} to {:?}",
                    var_name, editor_y, &result.result
                ));
                replace_or_insert_var(vars, var_name, result.result.clone(), editor_y);
            }
        } else {
            let line_data = editor_content.get_data(editor_y);
            debug_assert!(line_data.line_id > 0);
            let line_id = line_data.line_id;
            let var_name: Vec<char> = format!("&[{}]", line_id).chars().collect();
            replace_or_insert_var(vars, &var_name, result.result.clone(), editor_y);
        }
    } else if let Some(var) = &mut vars[editor_y] {
        let line_data = editor_content.get_data(editor_y);
        debug_assert!(line_data.line_id > 0);
        let line_id = line_data.line_id;
        let var_name: Vec<char> = format!("&[{}]", line_id).chars().collect();
        var.name = Box::from(var_name);
        var.value = Err(());
    } else {
        vars[editor_y] = None;
    }
}

pub fn get_var_name_from_assignment(
    editor_y: usize,
    editor_content: &EditorContent<LineData>,
) -> &[char] {
    let line = editor_content.get_line_valid_chars(editor_y);
    let mut i = 0;
    if line[0] == '=' {
        // it might happen that there are more '=' in a line.
        // To avoid panic, start the index from 1, so if the first char is
        // '=', it will be ignored.
        i += 1;
    }
    // skip whitespaces
    while line[i].is_ascii_whitespace() {
        i += 1;
    }
    let start = i;
    // take until '='
    while i < line.len() && line[i] != '=' {
        i += 1;
    }
    if i == line.len() {
        return &[];
    }
    // remove trailing whitespaces
    i -= 1;
    while i > start && line[i].is_ascii_whitespace() {
        i -= 1;
    }
    let end = i;
    return &line[start..=end];
}

fn get_token_index_into_tokens(apptokens: &AppTokens, editor_y: usize, i: usize) -> usize {
    let tokens = apptokens[content_y(editor_y)].as_ref().unwrap();
    let shunting_tokens = &tokens.shunting_output_stack;
    let token = &shunting_tokens[i];
    return token.index_into_tokens;
}

pub fn divide_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    let result: Option<CalcResult> = match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Unit(..), CalcResultType::Unit(..))
        | (CalcResultType::Unit(..), CalcResultType::Number(..))
        | (CalcResultType::Unit(..), CalcResultType::Quantity(..))
        | (CalcResultType::Unit(..), CalcResultType::Percentage(..))
        | (CalcResultType::Unit(..), CalcResultType::Matrix(..))
        | (CalcResultType::Matrix(..), CalcResultType::Unit(..)) => None,
        // 30 years * 12/year
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
        // 5% / year
        (CalcResultType::Percentage(num), CalcResultType::Unit(unit)) => {
            let new_unit = unit.pow(-1)?;
            Some(CalcResult::new(
                CalcResultType::Quantity(num.checked_div(&DECIMAL_100)?, new_unit),
                0,
            ))
        }
        // 12 / x
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
        // 12km / x
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
        // 12% / x
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

pub fn multiply_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    let result = match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Unit(..), CalcResultType::Unit(..))
        | (CalcResultType::Unit(..), CalcResultType::Number(..))
        | (CalcResultType::Unit(..), CalcResultType::Quantity(..))
        | (CalcResultType::Unit(..), CalcResultType::Percentage(..))
        | (CalcResultType::Unit(..), CalcResultType::Matrix(..))
        | (CalcResultType::Number(..), CalcResultType::Unit(..))
        | (CalcResultType::Quantity(..), CalcResultType::Unit(..))
        | (CalcResultType::Percentage(..), CalcResultType::Unit(..))
        | (CalcResultType::Matrix(..), CalcResultType::Unit(..)) => None,
        // 12 * x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 12 * 2
            lhs.checked_mul(rhs)
                .map(|num| CalcResult::new(CalcResultType::Number(num), 0))
        }
        (CalcResultType::Number(lhs), CalcResultType::Quantity(rhs, unit)) => {
            // 12 * 2km
            lhs.checked_mul(rhs)
                .map(|num| CalcResult::new(CalcResultType::Quantity(num, unit.clone()), 0))
        }
        (CalcResultType::Number(lhs), CalcResultType::Percentage(rhs)) => {
            // 100 * 50%
            Some(CalcResult::new(
                CalcResultType::Number(percentage_of(rhs, lhs)?),
                0,
            ))
        }
        (CalcResultType::Number(..), CalcResultType::Matrix(mat)) => mat.mult_scalar(lhs),
        // 12km * x
        (CalcResultType::Quantity(lhs_num, lhs_unit), CalcResultType::Number(rhs_num)) => {
            // 2m * 5
            lhs_num
                .checked_mul(rhs_num)
                .map(|num| CalcResult::new(CalcResultType::Quantity(num, lhs_unit.clone()), 0))
        }
        (
            CalcResultType::Quantity(lhs_num, lhs_unit),
            CalcResultType::Quantity(rhs_num, rhs_unit),
        ) => {
            // 2s * 3s
            if lhs_unit.unit_count + rhs_unit.unit_count >= MAX_UNIT_COUNT {
                None
            } else {
                let result = lhs_num.checked_mul(&rhs_num)?;
                let (new_unit, k) = lhs_unit.mul(rhs_unit)?;
                let corrected_result = result.checked_mul(&k)?;

                Some(if new_unit.is_unitless() {
                    CalcResult::new(CalcResultType::Number(corrected_result), 0)
                } else {
                    CalcResult::new(CalcResultType::Quantity(corrected_result, new_unit), 0)
                })
            }
        }
        (CalcResultType::Quantity(lhs, lhs_unit), CalcResultType::Percentage(rhs)) => {
            // e.g. 2m * 50%
            Some(CalcResult::new(
                CalcResultType::Quantity(percentage_of(rhs, lhs)?, lhs_unit.clone()),
                0,
            ))
        }
        (CalcResultType::Quantity(..), CalcResultType::Matrix(mat)) => mat.mult_scalar(lhs),
        // 12% * x
        (CalcResultType::Percentage(lhs), CalcResultType::Number(rhs)) => {
            // 5% * 10
            Some(CalcResult::new(
                CalcResultType::Number(percentage_of(lhs, rhs)?),
                0,
            ))
        }
        (CalcResultType::Percentage(lhs), CalcResultType::Quantity(rhs, rhs_unit)) => {
            // 5% * 10km
            Some(CalcResult::new(
                CalcResultType::Quantity(percentage_of(lhs, rhs)?, rhs_unit.clone()),
                0,
            ))
        }
        (CalcResultType::Percentage(lhs), CalcResultType::Percentage(rhs)) => {
            // 50% * 50%

            Some(CalcResult::new(
                CalcResultType::Percentage(
                    (lhs.checked_div(&DECIMAL_100)?)
                        .checked_mul(&rhs.checked_div(&DECIMAL_100)?)?,
                ),
                0,
            ))
        }
        (CalcResultType::Percentage(..), CalcResultType::Matrix(..)) => None,
        // Matrix
        (CalcResultType::Matrix(mat), CalcResultType::Number(..))
        | (CalcResultType::Matrix(mat), CalcResultType::Quantity(..))
        | (CalcResultType::Matrix(mat), CalcResultType::Percentage(..)) => mat.mult_scalar(rhs),
        (CalcResultType::Matrix(a), CalcResultType::Matrix(b)) => {
            if a.col_count != b.row_count {
                return None;
            }
            let mut result = Vec::with_capacity(a.row_count * b.col_count);
            for row in 0..a.row_count {
                for col in 0..b.col_count {
                    let mut sum = if let Some(r) = multiply_op(a.cell(row, 0), b.cell(0, col)) {
                        r
                    } else {
                        return None;
                    };
                    for i in 1..a.col_count {
                        if let Some(r) = multiply_op(a.cell(row, i), b.cell(i, col)) {
                            if let Some(s) = add_op(&sum, &r) {
                                sum = s;
                            } else {
                                return None;
                            }
                        }
                    }
                    result.push(sum);
                }
            }
            Some(CalcResult::new(
                CalcResultType::Matrix(MatrixData::new(result, a.row_count, b.col_count)),
                0,
            ))
        }
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

fn sub_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Unit(..), CalcResultType::Unit(..))
        | (CalcResultType::Unit(..), CalcResultType::Number(..))
        | (CalcResultType::Unit(..), CalcResultType::Quantity(..))
        | (CalcResultType::Unit(..), CalcResultType::Percentage(..))
        | (CalcResultType::Unit(..), CalcResultType::Matrix(..))
        | (CalcResultType::Number(..), CalcResultType::Unit(..))
        | (CalcResultType::Quantity(..), CalcResultType::Unit(..))
        | (CalcResultType::Percentage(..), CalcResultType::Unit(..))
        | (CalcResultType::Matrix(..), CalcResultType::Unit(..)) => None,
        // 12 - x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 12 - 3
            Some(CalcResult::new(
                CalcResultType::Number(lhs.checked_sub(&rhs)?),
                0,
            ))
        }
        (CalcResultType::Number(_lhs), CalcResultType::Quantity(_rhs, _unit)) => {
            // 12 - 3 km
            None
        }
        (CalcResultType::Number(lhs), CalcResultType::Percentage(rhs)) => {
            // 100 - 50%
            let x_percent_of_left_hand_side = lhs
                .checked_div(&DECIMAL_100)
                .and_then(|it| it.checked_mul(rhs))?;
            Some(CalcResult::new(
                CalcResultType::Number(lhs.checked_sub(&x_percent_of_left_hand_side)?),
                0,
            ))
        }
        (CalcResultType::Number(..), CalcResultType::Matrix(..)) => None,
        // 12km - x
        (CalcResultType::Quantity(_lhs, _lhs_unit), CalcResultType::Number(_rhs)) => {
            // 2m - 5
            None
        }
        (
            CalcResultType::Quantity(lhs_num, lhs_unit),
            CalcResultType::Quantity(rhs_num, rhs_unit),
        ) => {
            // 2s - 3s
            if !lhs_unit.is_compatible(rhs_unit) {
                None
            } else {
                return if lhs_unit == rhs_unit {
                    let result = lhs_num.checked_sub(rhs_num)?;
                    Some(CalcResult::new(
                        CalcResultType::Quantity(result, lhs_unit.clone()),
                        0,
                    ))
                } else {
                    let same_unit_rhs_num =
                        UnitOutput::convert_same_powers(rhs_unit, lhs_unit, rhs_num)?;
                    let result = lhs_num.checked_sub(&same_unit_rhs_num)?;
                    Some(CalcResult::new(
                        CalcResultType::Quantity(result, lhs_unit.clone()),
                        0,
                    ))
                };
            }
        }
        (CalcResultType::Quantity(lhs, lhs_unit), CalcResultType::Percentage(rhs)) => {
            // e.g. 2m - 50%
            let x_percent_of_left_hand_side = lhs
                .checked_div(&DECIMAL_100)
                .and_then(|it| it.checked_mul(rhs))?;
            Some(CalcResult::new(
                CalcResultType::Quantity(
                    lhs.checked_sub(&x_percent_of_left_hand_side)?,
                    lhs_unit.clone(),
                ),
                0,
            ))
        }
        (CalcResultType::Quantity(..), CalcResultType::Matrix(..)) => None,
        // 12% - x
        (CalcResultType::Percentage(_lhs), CalcResultType::Number(_rhs)) => {
            // 5% - 10
            None
        }
        (CalcResultType::Percentage(_lhs), CalcResultType::Quantity(_rhs, _rhs_unit)) => {
            // 5% - 10km
            None
        }
        (CalcResultType::Percentage(lhs), CalcResultType::Percentage(rhs)) => {
            // 50% - 50%
            Some(CalcResult::new(
                CalcResultType::Percentage(lhs.checked_sub(&rhs)?),
                0,
            ))
        }
        (CalcResultType::Percentage(..), CalcResultType::Matrix(..)) => None,
        // Matrix
        (CalcResultType::Matrix(..), CalcResultType::Number(..)) => None,
        (CalcResultType::Matrix(..), CalcResultType::Quantity(..)) => None,
        (CalcResultType::Matrix(..), CalcResultType::Percentage(..)) => None,
        (CalcResultType::Matrix(lhs), CalcResultType::Matrix(rhs)) => {
            if lhs.row_count != rhs.row_count || lhs.col_count != rhs.col_count {
                return None;
            }
            let cells: Option<Vec<CalcResult>> = lhs
                .cells
                .iter()
                .zip(rhs.cells.iter())
                .map(|(a, b)| sub_op(a, b))
                .collect();
            cells.map(|it| {
                CalcResult::new(
                    CalcResultType::Matrix(MatrixData::new(it, lhs.row_count, lhs.col_count)),
                    0,
                )
            })
        }
    }
}

fn pow_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        // 1^x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 2^3
            rhs.to_i64()
                .and_then(|rhs| {
                    let p = pow(lhs.clone(), rhs);
                    p
                })
                .map(|pow| CalcResult::new(CalcResultType::Number(pow), 0))
        }
        (CalcResultType::Quantity(lhs, lhs_unit), CalcResultType::Number(rhs)) => {
            let p = rhs.to_i64()?;
            let num_powered = pow(lhs.clone(), p)?;
            let unit_powered = lhs_unit.pow(p);
            Some(CalcResult::new(
                CalcResultType::Quantity(num_powered, unit_powered?),
                0,
            ))
        }
        _ => None,
    }
}

pub fn add_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Unit(..), CalcResultType::Unit(..))
        | (CalcResultType::Unit(..), CalcResultType::Number(..))
        | (CalcResultType::Unit(..), CalcResultType::Quantity(..))
        | (CalcResultType::Unit(..), CalcResultType::Percentage(..))
        | (CalcResultType::Unit(..), CalcResultType::Matrix(..))
        | (CalcResultType::Number(..), CalcResultType::Unit(..))
        | (CalcResultType::Quantity(..), CalcResultType::Unit(..))
        | (CalcResultType::Percentage(..), CalcResultType::Unit(..))
        | (CalcResultType::Matrix(..), CalcResultType::Unit(..)) => None,
        // 12 + x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 12 + 3
            Some(CalcResult::new(
                CalcResultType::Number(lhs.checked_add(&rhs)?),
                0,
            ))
        }
        (CalcResultType::Number(_lhs), CalcResultType::Quantity(_rhs, _unit)) => {
            // 12 + 3 km
            None
        }
        (CalcResultType::Number(lhs), CalcResultType::Percentage(rhs)) => {
            // 100 + 50%
            let x_percent_of_left_hand_side = lhs
                .checked_div(&DECIMAL_100)
                .and_then(|it| it.checked_mul(rhs))?;
            Some(CalcResult::new(
                CalcResultType::Number(lhs.checked_add(&x_percent_of_left_hand_side)?),
                0,
            ))
        }
        (CalcResultType::Number(_lhs), CalcResultType::Matrix(..)) => None,
        // 12km + x
        (CalcResultType::Quantity(_lhs, _lhs_unit), CalcResultType::Number(_rhs)) => {
            // 2m + 5
            None
        }
        (
            CalcResultType::Quantity(lhs_num, lhs_unit),
            CalcResultType::Quantity(rhs_num, rhs_unit),
        ) => {
            // 2s + 3s
            if !lhs_unit.is_compatible(rhs_unit) {
                None
            } else {
                return if lhs_unit == rhs_unit {
                    let result = rhs_num.checked_add(lhs_num)?;
                    Some(CalcResult::new(
                        CalcResultType::Quantity(result, lhs_unit.clone()),
                        0,
                    ))
                } else {
                    let same_unit_rhs_num =
                        UnitOutput::convert_same_powers(rhs_unit, lhs_unit, rhs_num)?;
                    let result = lhs_num.checked_add(&same_unit_rhs_num)?;

                    Some(CalcResult::new(
                        CalcResultType::Quantity(result, lhs_unit.clone()),
                        0,
                    ))
                };
            }
        }
        (CalcResultType::Quantity(lhs, lhs_unit), CalcResultType::Percentage(rhs)) => {
            // e.g. 2m + 50%
            let x_percent_of_left_hand_side = lhs
                .checked_div(&DECIMAL_100)
                .and_then(|it| it.checked_mul(rhs))?;
            Some(CalcResult::new(
                CalcResultType::Quantity(lhs + x_percent_of_left_hand_side, lhs_unit.clone()),
                0,
            ))
        }
        (CalcResultType::Quantity(..), CalcResultType::Matrix(..)) => None,
        // 12% + x
        (CalcResultType::Percentage(_lhs), CalcResultType::Number(_rhs)) => {
            // 5% + 10
            None
        }
        (CalcResultType::Percentage(_lhs), CalcResultType::Quantity(_rhs, _rhs_unit)) => {
            // 5% + 10km
            None
        }
        (CalcResultType::Percentage(lhs), CalcResultType::Percentage(rhs)) => {
            // 50% + 50%
            Some(CalcResult::new(CalcResultType::Percentage(lhs + rhs), 0))
        }
        (CalcResultType::Percentage(..), CalcResultType::Matrix(..)) => None,
        // Matrix
        (CalcResultType::Matrix(..), CalcResultType::Number(..)) => None,
        (CalcResultType::Matrix(..), CalcResultType::Quantity(..)) => None,
        (CalcResultType::Matrix(..), CalcResultType::Percentage(..)) => None,
        (CalcResultType::Matrix(lhs), CalcResultType::Matrix(rhs)) => {
            if lhs.row_count != rhs.row_count || lhs.col_count != rhs.col_count {
                return None;
            }
            let cells: Option<Vec<CalcResult>> = lhs
                .cells
                .iter()
                .zip(rhs.cells.iter())
                .map(|(a, b)| add_op(a, b))
                .collect();
            cells.map(|it| {
                CalcResult::new(
                    CalcResultType::Matrix(MatrixData::new(it, lhs.row_count, lhs.col_count)),
                    0,
                )
            })
        }
    }
}

fn bitwise_and_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        // 12 and x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 0b01 and 0b10
            let lhs = lhs.to_u64()?;
            let rhs = rhs.to_u64()?;
            Some(CalcResult::new(CalcResultType::Number(dec(lhs & rhs)), 0))
        }
        _ => None,
    }
}

fn unary_minus_op(lhs: &CalcResult) -> Option<CalcResult> {
    match &lhs.typ {
        CalcResultType::Number(lhs_num) => {
            // -12
            Some(CalcResult::new(
                CalcResultType::Number(lhs_num.neg()),
                lhs.index_into_tokens,
            ))
        }
        CalcResultType::Quantity(lhs_num, unit) => {
            // -12km
            Some(CalcResult::new(
                CalcResultType::Quantity(lhs_num.neg(), unit.clone()),
                lhs.index_into_tokens,
            ))
        }
        CalcResultType::Percentage(lhs_num) => {
            // -50%
            Some(CalcResult::new(
                CalcResultType::Percentage(lhs_num.neg()),
                lhs.index_into_tokens,
            ))
        }
        _ => None,
        CalcResultType::Matrix(mat) => CalcResultType::Matrix(mat.neg()),
    }
}

fn apply_operation<'text_ptr>(
    stack: &mut Vec<CalcResult>,
    op: &OperatorTokenType,
    op_token_index: usize,
    units: &Units,
    lock_stack: &mut ArrayVec<[u16; 64]>,
) -> Result<(), EvalErr> {
    let succeed = match &op {
        OperatorTokenType::Mult
        | OperatorTokenType::Div
        | OperatorTokenType::Add
        | OperatorTokenType::Sub
        | OperatorTokenType::BinAnd
        | OperatorTokenType::BinOr
        | OperatorTokenType::BinXor
        | OperatorTokenType::Pow
        | OperatorTokenType::ShiftLeft
        | OperatorTokenType::ShiftRight
        | OperatorTokenType::Percentage_Find_Base_From_Result_Increase_X
        | OperatorTokenType::Percentage_Find_Base_From_X_Icrease_Result
        | OperatorTokenType::Percentage_Find_Base_From_Icrease_X_Result
        | OperatorTokenType::Percentage_Find_Incr_Rate_From_Result_X_Base
        | OperatorTokenType::Percentage_Find_Base_From_Result_Decrease_X
        | OperatorTokenType::Percentage_Find_Base_From_X_Decrease_Result
        | OperatorTokenType::Percentage_Find_Base_From_Decrease_X_Result
        | OperatorTokenType::Percentage_Find_Decr_Rate_From_Result_X_Base
        | OperatorTokenType::Percentage_Find_Rate_From_Result_Base
        | OperatorTokenType::Percentage_Find_Base_From_Result_Rate
        | OperatorTokenType::UnitConverter => {
            if stack.len() > 1 {
                let (lhs, rhs) = (&stack[stack.len() - 2], &stack[stack.len() - 1]);
                if let Some(result) = binary_operation(op, lhs, rhs, units) {
                    stack.truncate(stack.len() - 2);
                    stack.push(result);
                    Ok(())
                } else {
                    Err(EvalErr::new("Op failed".to_owned(), op_token_index))
                }
            } else {
                Err(EvalErr::new("Not enugh operand".to_owned(), 0))
            }
        }
        OperatorTokenType::UnaryMinus
        | OperatorTokenType::UnaryPlus
        | OperatorTokenType::Perc
        | OperatorTokenType::BinNot => {
            let maybe_top = stack.last();
            let result = maybe_top.and_then(|top| unary_operation(&op, top, op_token_index));
            debug_print(&format!(
                "calc> {:?} {:?} = {:?}",
                &op,
                &maybe_top.as_ref().map(|it| &it.typ),
                &result
            ));
            if let Some(result) = result {
                stack.pop();
                stack.push(result);
                Ok(())
            } else {
                Err(EvalErr::new("??".to_owned(), 0))
            }
        }
        OperatorTokenType::Matrix {
            row_count,
            col_count,
        } => {
            let lock_offset = lock_stack.pop().expect("must");
            let arg_count = row_count * col_count;
            if stack.len() - lock_offset as usize >= arg_count {
                let matrix_args = stack.drain(stack.len() - arg_count..).collect::<Vec<_>>();
                stack.push(CalcResult::new(
                    CalcResultType::Matrix(MatrixData::new(matrix_args, *row_count, *col_count)),
                    op_token_index,
                ));
                debug_print("calc> Matrix");
                Ok(())
            } else {
                Err(EvalErr::new(
                    "Not enough argument for matrix creation".to_owned(),
                    op_token_index,
                ))
            }
        }
        OperatorTokenType::Fn { arg_count, typ } => {
            debug_print(&format!("calc> call Fn {:?}", typ));
            typ.execute(*arg_count, stack, op_token_index, units)
        }
        OperatorTokenType::Semicolon | OperatorTokenType::Comma => {
            // ignore
            Ok(())
        }
        OperatorTokenType::Assign | OperatorTokenType::StartLock => {
            panic!("handled in the main loop above")
        }
        OperatorTokenType::ParenOpen
        | OperatorTokenType::ParenClose
        | OperatorTokenType::BracketOpen
        | OperatorTokenType::BracketClose => {
            // this branch was executed during fuzz testing, don't panic here
            // check test_panic_fuzz_3
            return Err(EvalErr::new(String::new(), op_token_index));
        }
        OperatorTokenType::PercentageIs => {
            // ignore
            Ok(())
        }
    };
    return succeed;
}

fn binary_operation(
    op: &OperatorTokenType,
    lhs: &CalcResult,
    rhs: &CalcResult,
    units: &Units,
) -> Option<CalcResult> {
    let result = match &op {
        OperatorTokenType::Mult => multiply_op(lhs, rhs),
        OperatorTokenType::Div => divide_op(lhs, rhs),
        OperatorTokenType::Add => add_op(lhs, rhs),
        OperatorTokenType::Sub => sub_op(lhs, rhs),
        OperatorTokenType::BinAnd => bitwise_and_op(lhs, rhs),
        OperatorTokenType::BinOr => bitwise_or_op(lhs, rhs),
        OperatorTokenType::BinXor => bitwise_xor_op(lhs, rhs),
        OperatorTokenType::Pow => pow_op(lhs, rhs),
        OperatorTokenType::ShiftLeft => bitwise_shift_left(lhs, rhs),
        OperatorTokenType::ShiftRight => bitwise_shift_right(lhs, rhs),
        OperatorTokenType::Percentage_Find_Base_From_Result_Increase_X => {
            perc_num_is_xperc_on_what(lhs, rhs)
        }
        OperatorTokenType::Percentage_Find_Base_From_X_Icrease_Result => {
            perc_num_is_xperc_on_what(rhs, lhs)
        }
        OperatorTokenType::Percentage_Find_Base_From_Icrease_X_Result => {
            perc_num_is_xperc_on_what(rhs, lhs)
        }
        OperatorTokenType::Percentage_Find_Incr_Rate_From_Result_X_Base => {
            perc_num_is_what_perc_on_num(lhs, rhs)
        }
        //
        OperatorTokenType::Percentage_Find_Base_From_Result_Decrease_X => {
            perc_num_is_xperc_off_what(lhs, rhs)
        }
        OperatorTokenType::Percentage_Find_Base_From_X_Decrease_Result => {
            perc_num_is_xperc_off_what(rhs, lhs)
        }
        OperatorTokenType::Percentage_Find_Base_From_Decrease_X_Result => {
            perc_num_is_xperc_off_what(rhs, lhs)
        }
        OperatorTokenType::Percentage_Find_Decr_Rate_From_Result_X_Base => {
            perc_num_is_what_perc_off_num(lhs, rhs)
        }
        OperatorTokenType::Percentage_Find_Rate_From_Result_Base => {
            percentage_find_rate_from_result_base(lhs, rhs)
        }
        OperatorTokenType::Percentage_Find_Base_From_Result_Rate => {
            percentage_find_base_from_result_rate(lhs, rhs)
        }
        OperatorTokenType::UnitConverter => {
            return match (&lhs.typ, &rhs.typ) {
                (
                    CalcResultType::Quantity(lhs_num, source_unit),
                    CalcResultType::Unit(target_unit),
                ) => {
                    if source_unit.is_compatible(target_unit) {
                        let converted_num = UnitOutput::convert(source_unit, target_unit, lhs_num)?;
                        Some(CalcResult::new(
                            CalcResultType::Quantity(converted_num, target_unit.clone()),
                            0,
                        ))
                    } else {
                        None
                    }
                }
                (CalcResultType::Matrix(mat), CalcResultType::Unit(..)) => {
                    let cells: Option<Vec<CalcResult>> = mat
                        .cells
                        .iter()
                        .map(|cell| binary_operation(op, cell, rhs, units))
                        .collect();
                    cells.map(|it| {
                        CalcResult::new(
                            CalcResultType::Matrix(MatrixData::new(
                                it,
                                mat.row_count,
                                mat.col_count,
                            )),
                            0,
                        )
                    })
                }
                _ => None,
            };
        }
        _ => panic!(),
    };
    debug_print(&format!(
        "calc====\n  {:?}\n  {:?}\n  {:?}\n  = {:?}",
        &lhs.typ, op, &rhs.typ, &result
    ));
    result
}

fn percentage_operator(lhs: &CalcResult, op_token_index: usize) -> Option<CalcResult> {
    match &lhs.typ {
        CalcResultType::Number(lhs_num) => {
            // 5%
            Some(CalcResult::new2(
                CalcResultType::Percentage(lhs_num.clone()),
                lhs.index_into_tokens,
                op_token_index,
            ))
        }
        _ => None,
    }
}

fn percentage_find_rate_from_result_base(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // lhs is what percent of lhs
    // 20 is what percent of 60
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Number(x)) => {
            let p = y.checked_div(x)?.checked_mul(&DECIMAL_100)?;
            Some(CalcResult::new(CalcResultType::Percentage(p), 0))
        }
        _ => None,
    }
}

fn percentage_find_base_from_result_rate(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // lhs is rhs% of what
    // 5 is 25% of what
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Percentage(p)) => {
            let x = y.checked_div(p)?.checked_mul(&DECIMAL_100)?;
            Some(CalcResult::new(CalcResultType::Number(x), 0))
        }
        _ => None,
    }
}

fn bitwise_not(lhs: &CalcResult) -> Option<CalcResult> {
    match &lhs.typ {
        CalcResultType::Number(lhs_num) => {
            // 0b01 and 0b10
            let lhs_num = lhs_num.to_u64()?;
            Some(CalcResult::new(
                CalcResultType::Number(dec(lhs_num.not())),
                lhs.index_into_tokens,
            ))
        }
        _ => None,
    }
}

fn bitwise_xor_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        // 12 and x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 0b01 and 0b10
            let lhs = lhs.to_u64()?;
            let rhs = rhs.to_u64()?;
            Some(CalcResult::new(
                CalcResultType::Number(dec(lhs.bitxor(rhs))),
                0,
            ))
        }
        _ => None,
    }
}

fn bitwise_or_op(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        // 12 and x
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            // 0b01 and 0b10
            let lhs = lhs.to_u64()?;
            let rhs = rhs.to_u64()?;
            Some(CalcResult::new(CalcResultType::Number(dec(lhs | rhs)), 0))
        }
        _ => None,
    }
}

fn bitwise_shift_right(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            let lhs = lhs.to_u64()?;
            let rhs = rhs.to_u32()?;
            if rhs > 63 {
                None
            } else {
                Some(CalcResult::new(
                    CalcResultType::Number(dec(lhs.shr(rhs))),
                    0,
                ))
            }
        }
        _ => None,
    }
}

fn bitwise_shift_left(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(lhs), CalcResultType::Number(rhs)) => {
            let lhs = lhs.to_u64()?;
            let rhs = rhs.to_u32()?;
            if rhs > 63 {
                None
            } else {
                Some(CalcResult::new(
                    CalcResultType::Number(dec(lhs.shl(rhs))),
                    0,
                ))
            }
        }
        _ => None,
    }
}

fn perc_num_is_xperc_on_what(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // 'lhs' is 'rhs' on what
    // 41 is 17% on what
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Percentage(p)) => {
            let x = y
                .checked_mul(&DECIMAL_100)?
                .checked_div(&p.checked_add(&DECIMAL_100)?)?;
            Some(CalcResult::new(CalcResultType::Number(x), 0))
        }
        _ => None,
    }
}

fn perc_num_is_xperc_off_what(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // 'lhs' is 'rhs' off what
    // 41 is 17% off what
    // x = (y*100)/(100-p)
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Percentage(p)) => {
            let x = y
                .checked_mul(&DECIMAL_100)?
                .checked_div(&DECIMAL_100.checked_sub(&p)?)?;
            Some(CalcResult::new(CalcResultType::Number(x), 0))
        }
        _ => None,
    }
}

fn perc_num_is_what_perc_on_num(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // lhs is what % on rhs
    // 41 is what % on 35
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Number(x)) => {
            let p = y
                .checked_mul(&DECIMAL_100)?
                .checked_div(x)?
                .checked_sub(&DECIMAL_100)?;
            Some(CalcResult::new(CalcResultType::Percentage(p), 0))
        }
        _ => None,
    }
}

fn perc_num_is_what_perc_off_num(lhs: &CalcResult, rhs: &CalcResult) -> Option<CalcResult> {
    // lhs is what % off rhs
    // 35 is what % off 41
    match (&lhs.typ, &rhs.typ) {
        (CalcResultType::Number(y), CalcResultType::Number(x)) => {
            let p = y
                .checked_mul(&DECIMAL_100)?
                .checked_div(x)?
                .checked_sub(&DECIMAL_100)?
                .neg();
            Some(CalcResult::new(CalcResultType::Percentage(p), 0))
        }
        _ => None,
    }
}

fn apply_unit_to_num(
    num: &Decimal,
    target_unit: &UnitOutput,
    operand_token_index: usize,
    unit_token_index: usize,
) -> Option<CalcResult> {
    if target_unit.dimensions == EMPTY_UNIT_DIMENSIONS {
        // the units cancelled each other, e.g. 1 km/m
        let k = target_unit.get_unit_coeff()?;
        Some(CalcResult::new(
            CalcResultType::Number(num.checked_mul(&k)?),
            operand_token_index,
        ))
    } else {
        Some(CalcResult::new2(
            CalcResultType::Quantity(num.clone(), target_unit.clone()),
            operand_token_index,
            unit_token_index,
        ))
    }
}

fn unary_operation(
    op: &OperatorTokenType,
    top: &CalcResult,
    op_token_index: usize,
) -> Option<CalcResult> {
    return match &op {
        OperatorTokenType::UnaryPlus => Some(top.clone()),
        OperatorTokenType::UnaryMinus => unary_minus_op(top),
        OperatorTokenType::Perc => percentage_operator(top, op_token_index),
        OperatorTokenType::BinNot => bitwise_not(top),
        _ => None,
    };
}

pub fn pow(this: Decimal, mut exp: i64) -> Option<Decimal> {
    if this.is_zero() && exp.is_negative() {
        return None;
    }

    let mut base = this.clone();
    let mut acc = Decimal::one();
    let neg = exp < 0;

    exp = exp.abs();

    while exp > 1 {
        if (exp & 1) == 1 {
            acc = acc.checked_mul(&base)?;
        }
        exp /= 2;
        base = base.checked_mul(&base)?;
    }

    if exp == 1 {
        acc = acc.checked_mul(&base)?;
    }

    Some(if neg {
        Decimal::one().checked_div(&acc)?
    } else {
        acc
    })
}

pub fn dec<T: Into<Decimal>>(num: T) -> Decimal {
    num.into()
}

fn percentage_of(this: &Decimal, base: &Decimal) -> Option<Decimal> {
    base.checked_div(&DECIMAL_100)?.checked_mul(this)
}

#[cfg(test)]
mod tests {
    use crate::shunting_yard::tests::{
        apply_to_prev_token_unit, apply_to_prev_token_unit_with_err, num, num_with_err, op, op_err,
        str, unit,
    };
    use crate::units::units::Units;
    use crate::{ResultFormat, Tokens, Variable, Variables, MAX_LINE_COUNT};
    use std::str::FromStr;

    use crate::borrow_checker_fighter::create_vars;
    use crate::calc::{CalcResult, CalcResultType, EvaluationResult};
    use crate::editor::editor_content::EditorContent;
    use crate::functions::FnType;
    use crate::helper::{content_y, AppTokens};
    use crate::renderer::render_result;
    use crate::token_parser::{OperatorTokenType, Token};
    use bumpalo::Bump;
    use rust_decimal::prelude::*;

    const DECIMAL_COUNT: usize = 4;

    fn test_tokens(text: &str, expected_tokens: &[Token]) {
        println!("===================================================");
        println!("{}", text);
        let units = Units::new();
        let temp = text.chars().collect::<Vec<char>>();
        let mut tokens = vec![];
        let vars = create_vars();
        let arena = Bump::new();
        let shunting_output = crate::shunting_yard::tests::do_shunting_yard_for_tests(
            &temp,
            &units,
            &mut tokens,
            &vars,
            &arena,
        );
        let mut apptokens = AppTokens::new();
        apptokens[content_y(0)] = Some(Tokens {
            tokens,
            shunting_output_stack: shunting_output,
        });
        let fds = [None; MAX_LINE_COUNT];
        let (_, result) = crate::calc::evaluate_tokens(
            0,
            &mut apptokens,
            &vars,
            &fds,
            &units,
            &EditorContent::new(120, 120),
            0,
            None,
        );
        if let Err(err) = result {
            Token::set_token_error_flag_by_index(
                err.token_index,
                &mut apptokens[content_y(0)].as_mut().unwrap().tokens,
            );
            for i in &[
                err.token_index_lhs_1,
                err.token_index_lhs_2,
                err.token_index_rhs_1,
                err.token_index_rhs_2,
            ] {
                if let Some(i) = i {
                    Token::set_token_error_flag_by_index(
                        *i,
                        &mut apptokens[content_y(0)].as_mut().unwrap().tokens,
                    );
                }
            }
        }

        crate::shunting_yard::tests::compare_tokens(
            text,
            &expected_tokens,
            &apptokens[content_y(0)].as_ref().unwrap().tokens,
        );
    }

    fn test_vars(vars: &Variables, text: &str, expected: &str, dec_count: usize) {
        dbg!("===========================================================");
        dbg!(text);
        let temp = text.chars().collect::<Vec<char>>();

        let units = Units::new();

        let mut tokens = vec![];
        let arena = Bump::new();
        let shunting_output = crate::shunting_yard::tests::do_shunting_yard_for_tests(
            &temp,
            &units,
            &mut tokens,
            vars,
            &arena,
        );

        let mut all_lines_tokens = AppTokens::new();
        all_lines_tokens[content_y(0)] = Some(Tokens {
            tokens,
            shunting_output_stack: shunting_output,
        });
        let fds = [None; MAX_LINE_COUNT];
        let (_, result) = crate::calc::evaluate_tokens(
            0,
            &mut all_lines_tokens,
            vars,
            &fds,
            &units,
            &EditorContent::new(120, 120),
            0,
            None,
        );

        if let Err(..) = &result {
            assert_eq!("Err", expected);
        } else if let Ok(Some(EvaluationResult {
            there_was_unit_conversion,
            there_was_operation: _,
            assignment: _assignment,
            result:
                CalcResult {
                    typ: CalcResultType::Quantity(_num, _unit),
                    ..
                },
        })) = &result
        {
            assert_eq!(
                render_result(
                    &units,
                    &result.as_ref().unwrap().as_ref().unwrap().result,
                    &ResultFormat::Dec,
                    *there_was_unit_conversion,
                    Some(dec_count),
                    false,
                ),
                expected
            );
        } else if let Ok(..) = &result {
            assert_eq!(
                result
                    .unwrap()
                    .map(|it| render_result(
                        &units,
                        &it.result,
                        &ResultFormat::Dec,
                        false,
                        Some(dec_count),
                        false
                    ))
                    .unwrap_or(" ".to_string()),
                expected,
            );
        }
    }

    fn test(text: &str, expected: &str) {
        test_vars(&create_vars(), text, expected, DECIMAL_COUNT);
    }

    fn test_with_dec_count(dec_count: usize, text: &str, expected: &'static str) {
        test_vars(&create_vars(), text, expected, dec_count);
    }

    #[test]
    fn calc_tests() {
        test("2^-2", "0.25");
        test_with_dec_count(5, "5km + 5cm", "5.00005 km");
        test("0.000001 km2 in m2", "1 m2");
        test("0.000000001 km3 in m3", "1 m3");

        test("0.000000002 km^3 in m^3", "2 m^3");
        test("0.000000002 km3 in m^3", "2 m^3");

        test("2 - -1", "3");

        test("24 bla + 0", "24");

        // should test whether two units are equal
        test("100 cm in m", "1 m");
        test("5000 cm in m", "50 m");

        test("100 ft * lbf in (in*lbf)", "1200 in lbf");
        test("100 N in kg*m / s ^ 2", "100 (kg m) / s^2");
        test("100 cm in m", "1 m");
        test("100 Hz in 1/s", "100 / s");
        test("() Hz", "Err");

        test("1 ft * lbf * 2 rad", "2 ft lbf rad");
        test("1 ft * lbf * 2 rad in in*lbf*rad", "24 in lbf rad");
        test("(2/3)m", "0.6667 m");
        test_with_dec_count(50, "(2/3)m", "0.6667 m");
        test_with_dec_count(50, "2/3m", "0.6667 / m");

        test("123 N in (kg m)/s^2", "123 (kg m) / s^2");

        test("1 km / 3000000 mm", "0.3333");
        test_with_dec_count(100, "1 km / 3000000 mm", "0.3333");

        test("5kg * 1", "5 kg");
        test("5 kg * 1", "5 kg");
        test(" 5 kg  * 1", "5 kg");
        test("-5kg  * 1", "-5 kg");
        test("+5kg  * 1", "5 kg");
        test(".5kg  * 1", "0.5 kg");
        test_with_dec_count(6, "-5mg in kg", "-0.000005 kg");
        test("5.2mg * 1", "5.2 mg");

        test("981 cm/s^2 in m/s^2", "9.81 m / s^2");
        test("5exabytes in bytes", "5000000000000000000 bytes");
        test(
            "8.314 kg*(m^2 / (s^2 / (K^-1 / mol))) * 1",
            "8.314 (kg m^2) / (s^2 K mol)",
        );

        test("9.81 meters/second^2 * 1", "9.81 meter / second^2");
        test("10 decades in decade", "10 decade");
        test("10 centuries in century", "10 century");
        test("10 millennia in millennium", "10 millennium");

        test("(10 + 20)km", "30 km");
    }

    #[test]
    fn simple_add() {
        test("1m + 2cm", "1.02 m");
        test("1cm + 2m", "201 cm");
        test("1cm^2 + 2m^2", "201 cm^2");
        test("1m^2 + 2cm^2", "1.02 m^2");

        test("1 h + 2 s", "1.0006 h");
        test("1 h^-1 + 2 s^-1", "7201 / h");
        test("1 km/h + 2km/h", "3 km / h");
        test("1 km/h + 2m/s", "8.2 km / h");

        test("2 N + 3 kg * m * s^-2", "5 N");
    }

    #[test]
    fn simple_mul() {
        test("3 m * 2m", "6 m^2");
        test("1 m * 2cm", "0.02 m^2");
        test("1 cm * 2m", "200 cm^2");
        test("1 cm^2 * 2m", "200 cm^3");
        test("1 m * 2cm^2", "0.0002 m^3");
        test("1 m^2 * 2cm^2", "0.0002 m^4");
        test("1cm^2 * 2m^2", "20000 cm^4");

        test("1 m^2 * 2cm^-1", "200 m");

        test("2 N * 3 kg * m * s^-2", "6 (kg^2 m^2) / s^4");
        test("2 N * 3 kg * m * s^-2 / 3 N", "2 N");
    }

    #[test]
    fn simple_div() {
        test("1 m / 2m", "0.5");
        test("1 m / 2cm", "50");
        test("1 cm / 2m", "0.005");
        test("1 cm^2 / 2m", "0.005 cm");
        test("1 m / 2cm^2", "5000 / m");
        test("1 m^2 / 2cm^2", "5000");
        test_with_dec_count(100, "1cm^2 / 2m^2", "0.00005");

        test("4 N / 2 kg * m * s^-2", "2");
    }

    #[test]
    fn simple_sub() {
        test("1m - 2cm", "0.98 m");
        test("2m - 1cm", "1.99 m");
        test("200cm - 0.01m", "199 cm");
        test("2m^2 - 1cm^2", "1.99 m^2");
        test("200cm^2 - 0.01m^2", "199 cm^2");
        test("1m^2 - 2cm^2", "0.98 m^2");

        test("1 h - 2 s", "0.9994 h");
        test("1 h^-1 - 2 s^-1", "-7199 / h");
        test("1 km/h - 3km/h", "-2 km / h");
        test("1 km/h - 2m/s", "-6.2 km / h");

        test("5 N - 3 kg * m * s^-2", "2 N");
    }

    #[test]
    fn calc_exp_test() {
        // exp, binary and hex does not work with units
        // test("5e3kg", "5000 kg");
        // test("3 kg^1.0e0 * m^1.0e0 * s^-2e0", "3 (kg m) / s^2");

        test_with_dec_count(5, "2.3e-4 + 0", "0.00023");
        test("2.8e-4 + 0", "0.0003");

        // TODO rust_decimal's range is too small for this :(
        test("1.23e50 + 0", "Err");
        // test(
        //     "1.23e50 + 0",
        //     "123000000000000000000000000000000000000000000000000",
        // );

        test("3 e + 0", "3");
        test("3e + 0", "3");
        test("33e + 0", "33");
        test("3e3 + 0", "3000");

        // it interprets it as 3 - (-3)
        test("3e--3", "6");

        // invalid input tests
        test("2.3e4e5 + 0", "23000");
    }

    #[test]
    fn test_percentages() {
        test("200 km/h * 10%", "20 km / h");
        test("200 km/h * 0%", "0 km / h");
        test("200 km/h + 10%", "220 km / h");
        test("200 km/h - 10%", "180 km / h");
        test("200 km/h + 0%", "200 km / h");
        test("200 km/h - 0%", "200 km / h");

        test("0 + 10%", "0");
        test("200 - 10%", "180");
        test("200 - 0%", "200");
        test("0 - 10%", "0");
        test("200 * 10%", "20");
        test("200 * 0%", "0");
        test("10% * 200", "20");
        test("0% * 200", "0");
        test("(10 + 20)%", "30 %");

        test("30/200%", "15 %");
    }

    #[test]
    fn test_longer_texts3() {
        test("I traveled 13km at a rate / 40km/h in min", "19.5 min");
    }

    #[test]
    fn test_longer_texts() {
        test(
            "I traveled 24 miles and rode my bike  / 2 hours",
            "12 mile / hour",
        );
        test(
            "Now let's say you rode your bike at a rate of 10 miles/h for * 4 h in mile",
            "40 mile",
        );
        test(
            "Now let's say you rode your bike at a rate of 10 miles/h for * 4 h",
            "40 mile",
        );
        test(
            "Now let's say you rode your bike at a rate of 10 miles/h for * 4 h in m",
            "64373.76 m",
        );
        test(
            " is a unit but should not be handled here so... 37.5MB*1 of DNA information in it.",
            "37.5 MB",
        );
    }

    #[test]
    fn test_longer_texts2() {
        test(
            "transfer of around 1.587GB in about / 3 seconds",
            "0.529 GB / second",
        );
    }

    #[test]
    fn test_result_heuristics() {
        // 2 numbers but no oepration, select none
        test("2.3e4.0e5", "23000");

        // ignore "15" and return with the last successful operation
        test("75-15 euróból kell adózni mert 15 EUR adómentes", "60");

        test("15 EUR adómentes azaz 75-15 euróból kell adózni", "60");
    }

    #[test]
    fn test_dont_count_zeroes() {
        test("1k * 1", "1000");
        test("2k * 1", "2000");
        test("3k - 2k", "1000");

        test("1k*1", "1000");
        test("2k*1", "2000");
        test("3k-2k", "1000");

        test("1M * 1", "1000000");
        test("2M * 1", "2000000");
        test("3M - 2M", "1000000");

        test("3M + 1k", "3001000");
        test("3M * 2k", "6000000000");
        // missing digit
        test("3M + k", "3000000");

        test("2kalap * 1", "2");
    }

    #[test]
    fn test_quant_vs_non_quant() {
        test("12 km/h * 5 ", "60 km / h");
        test("200kg alma + 300 kg banán ", "500 kg");

        test("3000/50ml", "60 / ml");
        test("(3000/50)ml", "60 ml");
        test("3000/(50ml)", "60 / ml");
        test("1/(2km/h)", "0.5 h / km");
    }

    #[test]
    fn tests_for_invalid_input() {
        test("3", "3");
        test("3e-3-", "0.003");

        test_tokens(
            "[2, asda]",
            &[
                op(OperatorTokenType::Matrix {
                    row_count: 1,
                    col_count: 2,
                }),
                num(2),
                op(OperatorTokenType::Comma),
                str(" "),
                str("asda"),
                op_err(OperatorTokenType::BracketClose),
            ],
        );
        test("[2, asda]", "Err");

        test(
            "2+3 - this minus sign is part of the text, should not affect the result",
            "5",
        );

        test_tokens(
            "1szer sem jött el + *megjegyzés 2 éve...",
            &[
                num(1),
                str("szer"),
                str(" "),
                str("sem"),
                str(" "),
                str("jött"),
                str(" "),
                str("el"),
                str(" "),
                str("+"),
                str(" "),
                str("*"),
                str("megjegyzés"),
                str(" "),
                str("2"),
                str(" "),
                str("éve..."),
            ],
        );
        test("1szer sem jött el + *megjegyzés 2 éve...", "1");

        test("100 Hz in s", "Err");

        test("12m/h * 45s ^^", "0.15 m");
        test("12km/h * 45s ^^", "0.15 km");
        test("12m/h * 45s ^^", "0.15 m");
        test_tokens(
            "12km/h * 45s ^^",
            &[
                num(12),
                apply_to_prev_token_unit("km / h"),
                str(" "),
                op(OperatorTokenType::Mult),
                str(" "),
                num(45),
                apply_to_prev_token_unit("s"),
                str(" "),
                str("^"),
                str("^"),
            ],
        );

        // there are no empty vectors

        // matrix
        test_tokens(
            "1 + [2,]",
            &[
                num(1),
                str(" "),
                op(OperatorTokenType::Add),
                str(" "),
                op(OperatorTokenType::Matrix {
                    row_count: 1,
                    col_count: 2,
                }),
                num(2),
                op(OperatorTokenType::Comma),
                op_err(OperatorTokenType::BracketClose),
            ],
        );
        test("1 + [2,]", "Err");

        // multiply operator must be explicit, "5" is ignored here
        test("5(1+2)", "3");

        // invalid
        test("[[2 * 1]]", "[2]");
        test("[[2 * 3, 4]]", "[6, 4]");
        test("[[2 * 1, 3], [4, 5]]", "[4, 5]");
    }

    #[test]
    fn calc_simplify_units() {
        test("128PiB / 30Mb/s", "38430716586.6667 s");
        test_with_dec_count(39, "128PiB / 30Mb/s", "38430716586.666666661101237895 s");
        test_with_dec_count(40, "128PiB / 30Mb/s", "38430716586.666666661101237895 s");
    }

    #[test]
    fn calc_indentify_derived_unit() {
        // simplify from base to derived units if possible
        test("3 kg * m * 1 s^-2", "3 N");
        test("5kg*m / 1s^2", "5 N");
        // should skip automatic simplification if created directly in the constructor
        test("9.81 kg*m/s^2 * 1", "9.81 N");
    }

    #[test]
    fn unit_calcs() {
        test_with_dec_count(5, "50km + 50mm", "50.00005 km");
        test_with_dec_count(5, "50km - 50mm", "49.99995 km");
        test("5kg * 5g", "0.025 kg^2");
        test_with_dec_count(100, "5km * 5mm", "0.000025 km^2");
        test("5km * 5mm in m^2", "25 m^2");
        test("5km * 5mm in cm^2", "250000 cm^2");
        test_with_dec_count(100, "5km^-3 * 5mm in m^-2", "0.000000000025 m^-2");
        test("5km * 5mm^-3 in cm^-2", "2500000000 cm^-2");
    }

    #[test]
    fn test_calc_angles() {
        test("1 radian in rad", "1 rad");
        test_with_dec_count(51, "1 deg in rad", "0.0174532925199432957692369077 rad");
    }

    #[test]
    fn test_cancelling_out() {
        test("3 (s^-1) * 4 s", "12");
        test("60 minute / 1 s", "3600");
        test_with_dec_count(
            303,
            "60 km/h*h/h/h * 1 in m/s^2",
            "0.00462962962962962962962963 m / s^2",
        );
        // it is a very important test, if it gets converted wrongly
        // then 60 km/h is converted to m/s, which is 16.6666...7 m/s,
        // and it causes inaccuracies
        test("60km/h * 2h", "120 km");
        test("60km/h * 2h in m", "120000 m");
        test("1s * 2s^-1", "2");
        test("2s * 3(s^-1)", "6");
        test("2s * 3(1/s)", "6");

        test("1m*km in m^2", "1000 m^2");
        test("1cm*km in m^2", "10 m^2");
        test("3600cm/h in m/s", "0.01 m / s");
    }

    #[test]
    fn test_cancelling_out_derived() {
        test("40 m * 40 N / 40 J", "40");
        test("(8.314 J / mol / K) ^ 0", "1");
    }

    #[test]
    fn test_cancelling_out_try_simplifying_at_render() {
        test_with_dec_count(
            303,
            "60 km/h*h/h/h * 1",
            "0.00462962962962962962962963 m / s^2",
        );
        test("2 m*km", "2000 m^2");
        test("2 cm*km", "20 m^2");
    }

    #[test]
    fn test_calc_inside_matrix() {
        test("[2 * 1]", "[2]");
        test("[2 * 1, 3]", "[2, 3]");
        test("[2 * 1, 3, 4, 5, 6]", "[2, 3, 4, 5, 6]");

        test("[2+3]", "[5]");
        test("[2+3, 4 - 1, 5*2, 6/3, 2^4]", "[5, 3, 10, 2, 16]");

        test("[2 * 1]", "[2]");
        test("[2 * 3; 4]", "[6; 4]");
        test("[2 * 1, 3; 4, 5]", "[2, 3; 4, 5]");
    }

    #[test]
    fn test_matrix_addition() {
        test("[2] + [3]", "[5]");
        test("[2, 3] + [4, 5]", "[6, 8]");
        test("[2, 3, 4] + [5, 6, 7]", "[7, 9, 11]");
        test("[2; 3] + [4; 5]", "[6; 8]");
        test(
            "[2, 3, 4; 5, 6, 7] + [8, 9, 10; 11, 12, 13]",
            "[10, 12, 14; 16, 18, 20]",
        );

        test("2 km + [3]", "Err");
        test("[2 km] + [3]", "Err");
    }

    #[test]
    fn test_matrix_sub() {
        test("[2] - [3]", "[-1]");
        test("[2, 3] - [4, 5]", "[-2, -2]");
        test("[2, 3, 4] - [5, 6, 7]", "[-3, -3, -3]");
        test("[4; 5] - [2; 3]", "[2; 2]");

        test("[2 km] - [3]", "Err");
    }

    #[test]
    fn test_matrix_scalar_mult() {
        test("3 * [2]", "[6]");
        test("[2] * 6", "[12]");

        test("2 * [2, 3]", "[4, 6]");
        test("2 * [2, 3, 4]", "[4, 6, 8]");
        test("2 * [2; 3]", "[4; 6]");
        test("2 * [2, 3; 4, 5]", "[4, 6; 8, 10]");
        test("[2, 3; 4, 5] * 2", "[4, 6; 8, 10]");

        test("2km * [2]", "[4 km]");
    }

    #[test]
    fn div_by_zero() {
        test("1 / 0", "Err");
        test("1kg / 0", "Err");
        test("1m / 0s", "Err");
        test("1% / 0", "Err");
        test("10 / 0%", "Err");
    }

    #[test]
    fn test_matrix_scalar_div() {
        test("3 / [2]", "Err");
        test("[6] / 2", "[3]");

        test("[6, 10] / 2", "[3, 5]");
        test("[2, 3, 4] / 2", "[1, 1.5, 2]");
        test("[2; 3] / 2", "[1; 1.5]");
        test("[2, 3; 4, 5] / 2", "[1, 1.5; 2, 2.5]");

        test("[100g] / 2g", "[50]");
    }

    #[test]
    fn test_matrix_matrix_mult() {
        test("[3] * [2]", "[6]");
        test("[2;3] * [4, 5]", "[8, 10; 12, 15]");

        test(
            "[1,2,3,4; 5,6,7,8; 9,10,11,12; 13,14,15,16] * [30;40;50;60]",
            "[500; 1220; 1940; 2660]",
        );

        test(
            "[2,3,4,5] * [2,3,4,5; 6,7,8,9; 10,11,12,13; 14,15,16,17]",
            "[132, 146, 160, 174]",
        );
        test("[3m] * [2cm]", "[0.06 m^2]");

        test("[2,3] * [4]", "Err");
    }

    #[test]
    fn matrix_unit() {
        test("[2cm,3mm; 4m,5km] in m", "[0.02 m, 0.003 m; 4 m, 5000 m]");
    }

    #[test]
    fn kcal_unit_tokens() {
        test_tokens(
            "1 cal in J",
            &[
                num(1),
                str(" "),
                apply_to_prev_token_unit("cal"),
                str(" "),
                op(OperatorTokenType::UnitConverter),
                str(" "),
                unit("J"),
            ],
        );
    }

    #[test]
    fn kcal_unit() {
        test("1 cal in J", "4.1868 J");
        test("3kcal in J", "12560.4 J");
    }

    #[test]
    fn test_eval_failure_changes_token_type() {
        test_tokens(
            "1 - not_variable",
            &[num(1), str(" "), str("-"), str(" "), str("not_variable")],
        );
    }

    #[test]
    fn test_matrix_wont_take_operands_from_outside_its_scope() {
        test("[1, 2] + [2,]", "Err");
        test("1 + [2, asda]", "Err");
        test("[1, 2] + [3,4]", "[4, 6]");
    }

    #[test]
    fn test_percent_div_period() {
        test("3.7%/year", "0.037 / year");
        test("350 000$ * 20%", "70000 $");
        test("350 000$ - 70 000$", "280000 $");
    }

    #[test]
    fn test_bitwise_ops() {
        test("0xFF AND 0b111", "7");

        test_tokens(
            "0xFF AND(0b11 OR 0b1111)",
            &[
                num(0xff),
                str(" "),
                op(OperatorTokenType::BinAnd),
                op(OperatorTokenType::ParenOpen),
                num(0b11),
                str(" "),
                op(OperatorTokenType::BinOr),
                str(" "),
                num(0b1111),
                op(OperatorTokenType::ParenClose),
            ],
        );

        test("0xFF AND(0b11 OR 0b1111)", "15");
    }

    #[test]
    fn test_unfinished_operators() {
        test_tokens(
            "0xFF AND 0b11 AND",
            &[
                num(0xff),
                str(" "),
                op(OperatorTokenType::BinAnd),
                str(" "),
                num(0b11),
                str(" "),
                str("AND"),
            ],
        );
    }

    #[test]
    fn test_binary() {
        ///// binary
        // Kibi BIT!
        test("1 Kib in bits", "1024 bits");
        test("1 Kib in bytes", "128 bytes");
        test("1 Kib/s in b/s", "1024 b / s");

        test("1kb in bytes", "125 bytes");
    }

    #[test]
    fn test_variables() {
        let mut vars = create_vars();
        vars[0] = Some(Variable {
            name: Box::from(&['v', 'a', 'r'][..]),
            value: Ok(CalcResult::new(
                CalcResultType::Number(Decimal::from_str("12").unwrap()),
                0,
            )),
        });
        test_vars(&vars, "var * 2", "24", 0);
        test_vars(&vars, "var - var", "0", 0);
    }

    #[test]
    fn test_unit_cancelling() {
        test("1 km/m", "1000");
        test("2 cm/mm", "20");
        test_with_dec_count(100, "3 mm/km", "0.000003");
        test("1 km / 50m", "20");

        test_tokens(
            "1 km/m",
            &[num(1), str(" "), apply_to_prev_token_unit("km / m")],
        );
        test("1 km/m", "1000");
        test("1 m/km", "0.001");
        test_with_dec_count(100, "140k h/ month", "191.6495550992470910335272");
        test("100 / 2km", "50 / km");
        test("100 / 2km/m", "0.05");
    }

    #[test]
    fn test_financial_without_dollar_sign() {
        test("2 year / 1 month", "24");
    }

    #[test]
    fn test_unit_money() {
        test_tokens(
            "10 $/month",
            &[num(10), str(" "), apply_to_prev_token_unit("$ / month")],
        );
        test("1 $/month", "1 $ / month");
        test("140k $ / month * 3 years", "5040000 $");
    }

    #[test]
    fn test_func_nth() {
        test("nth([5, 6, 7], 0)", "5");
        test("nth([5, 6, 7], 1)", "6");
        test("nth([5, 6, 7], 2)", "7");
    }

    #[test]
    fn test_out_of_index_nth() {
        test_tokens(
            "nth([1],5)",
            &[
                op(OperatorTokenType::Fn {
                    arg_count: 0,
                    typ: FnType::Nth,
                }),
                op(OperatorTokenType::ParenOpen),
                op(OperatorTokenType::Matrix {
                    row_count: 1,
                    col_count: 1,
                }),
                num(1),
                op(OperatorTokenType::BracketClose),
                op(OperatorTokenType::Comma),
                num_with_err(5),
                op(OperatorTokenType::ParenClose),
            ],
        )
    }

    #[test]
    fn test_func_sum() {
        test("sum([5, 6, 7])", "18");
    }

    #[test]
    fn test_bitwise_not() {
        test("NOT(0b11)", "18446744073709551612");
        test("13 AND NOT(4 - 1)", "12");
    }

    #[test]
    fn test_func_transpose() {
        test("transpose([5, 6, 7])", "[5; 6; 7]");
        test("transpose([1, 2; 3, 4])", "[1, 3; 2, 4]");
        test("transpose([1, 2; 3, 4; 5, 6])", "[1, 3, 5; 2, 4, 6]");
    }

    #[test]
    fn test_func_pi() {
        test_with_dec_count(1000, "pi()", "3.1415926535897932384626433833");
        test("pi(1)", "Err");
    }

    #[test]
    fn test_func_e() {
        test_with_dec_count(1000, "e()", "2.7182818284590452353602874714");
        test("e(1)", "Err");
    }

    #[test]
    fn test_func_ln() {
        test_with_dec_count(1000, "ln(2)", "0.693147180559945");
        test_with_dec_count(1000, "ln(100)", "4.60517018598809");
        test("ln()", "Err");
        test("ln(2, 3)", "Err");
    }

    #[test]
    fn test_func_lg() {
        test_with_dec_count(1000, "lg(2)", "1");
        test_with_dec_count(1000, "lg(100)", "6.64385618977472");
        test("lg()", "Err");
        test("lg(2, 3)", "Err");
    }

    #[test]
    fn test_func_log() {
        test_with_dec_count(1000, "log(3, 2)", "0.630929753571457");
        test_with_dec_count(1000, "log(2, 100)", "6.64385618977473");
        test("log()", "Err");
        test("log(1)", "Err");
        test("log(1, 2, 3)", "Err");
    }

    #[test]
    fn test_func_cos() {
        test_with_dec_count(1000, "cos(2 degree)", "0.999390827019096");
        test_with_dec_count(1000, "cos(1 degree)", "0.999847695156391");
        test_with_dec_count(1000, "cos(1 rad)", "0.54030230586814");
        test("cos()", "Err");
        test("cos(1)", "Err");
        test("cos(1 rad^2)", "Err");
        test("cos(1, 2)", "Err");
    }

    #[test]
    fn test_func_sin() {
        test_with_dec_count(1000, "sin(2 degree)", "0.03489949670250097");
        test_with_dec_count(1000, "sin(1 degree)", "0.01745240643728351");
        test_with_dec_count(1000, "sin(1 rad)", "0.841470984807897");
        test("sin()", "Err");
        test("sin(1)", "Err");
        test("sin(1 rad^2)", "Err");
        test("sin(1, 2)", "Err");
        test("sin(1 m)", "Err");
        test_tokens(
            "sin(1 m)",
            &[
                op(OperatorTokenType::Fn {
                    arg_count: 0,
                    typ: FnType::Sin,
                }),
                op(OperatorTokenType::ParenOpen),
                num_with_err(1),
                str(" "),
                apply_to_prev_token_unit_with_err("m"),
                op(OperatorTokenType::ParenClose),
            ],
        );
    }

    #[test]
    fn test_func_acos() {
        test_with_dec_count(1000, "acos(1)", "0 rad");
        test_with_dec_count(1000, "acos(0.5)", "1.047197551196598 rad");
        test_with_dec_count(1000, "acos(-0.5)", "2.094395102393196 rad");
        test("acos()", "Err");
        test("acos(1 rad)", "Err");
        test("acos(1 degree)", "Err");
        test("acos(2)", "Err");
        test("acos(-2)", "Err");
        test("acos(1 rad^2)", "Err");
        test("acos(1, 2)", "Err");
    }

    #[test]
    fn test_func_asin() {
        test_with_dec_count(1000, "asin(1)", "1.570796326794897 rad");
        test_with_dec_count(1000, "asin(0.5)", "0.523598775598299 rad");
        test_with_dec_count(1000, "asin(-0.5)", "-0.523598775598299 rad");
        test("asin()", "Err");
        test("asin(1 rad)", "Err");
        test("asin(1 degree)", "Err");
        test("asin(2)", "Err");
        test("asin(-2)", "Err");
        test("asin(1 rad^2)", "Err");
        test("asin(1, 2)", "Err");
    }

    #[test]
    fn test_func_tan() {
        test_with_dec_count(1000, "tan(2 degree)", "0.03492076949174773");
        test_with_dec_count(1000, "tan(1 degree)", "0.01745506492821759");
        test_with_dec_count(1000, "tan(1 rad)", "1.557407724654902");
        test("tan()", "Err");
        test("tan(1)", "Err");
        test("tan(1 rad^2)", "Err");
        test("tan(1, 2)", "Err");
    }

    #[test]
    fn test_func_atan() {
        test_with_dec_count(1000, "atan(1)", "0.785398163397448 rad");
        test_with_dec_count(1000, "atan(0.5)", "0.463647609000806 rad");
        test_with_dec_count(1000, "atan(-0.5)", "-0.463647609000806 rad");
        test("atan()", "Err");
        test("atan(1 rad)", "Err");
        test("atan(1 degree)", "Err");
        test("atan(2)", "Err");
        test("atan(-2)", "Err");
        test("atan(1 rad^2)", "Err");
        test("atan(1, 2)", "Err");
    }

    #[test]
    fn test_func_abs() {
        test_with_dec_count(1000, "abs(10)", "10");
        test_with_dec_count(1000, "abs(-10)", "10");
        test("abs()", "Err");
        test("abs(1, 2)", "Err");
    }

    #[test]
    fn test_fraction_reduction_rounding() {
        test_with_dec_count(1000, "0.0030899999999999999999999999", "0.003090");
    }

    #[test]
    fn test_fraction_reduction_rounding2() {
        test_with_dec_count(1000, "5 m^2/s in km^2/h", "0.0180 km^2 / h");
    }

    #[test]
    fn test_single_brackets() {
        test("[", " ");
        test("]", " ");
        test("(", " ");
        test(")", " ");
        test("=", " ");
    }

    #[test]
    fn test_error_for_pow_percent() {
        test_tokens(
            "30^5%",
            &[
                num(30),
                op_err(OperatorTokenType::Pow),
                num(5),
                op(OperatorTokenType::Perc),
            ],
        );
    }

    #[test]
    fn test_zero_negativ_pow() {
        test("0^-1", "Err");
    }

    #[test]
    fn test_simple_unit() {
        test("30 years", "30 year");
    }

    #[test]
    fn test_error_wrong_result_year_multiply() {
        test("30 years * 12(1/year)", "360");
        test("30 years * 12/year", "360");
    }

    #[test]
    fn test_unit_in_denominator() {
        test("12/year", "12 / year");
    }

    #[test]
    fn test_unit_in_denominator_tokens() {
        test_tokens(
            "12/year",
            &[num(12), op(OperatorTokenType::Div), unit("year")],
        );
    }

    #[test]
    fn test_unit_in_denominator_tokens2() {
        test_tokens(
            "1/12/year",
            &[
                num(1),
                op(OperatorTokenType::Div),
                num(12),
                op(OperatorTokenType::Div),
                unit("year"),
            ],
        );
    }

    #[test]
    fn test_unit_in_denominator_tokens_with_parens() {
        test_tokens(
            "(12/year)",
            &[
                op(OperatorTokenType::ParenOpen),
                num(12),
                op(OperatorTokenType::Div),
                unit("year"),
                op(OperatorTokenType::ParenClose),
            ],
        );
    }

    #[test]
    fn test_that_pow_has_higher_precedence_than_unit() {
        test_tokens(
            "10^24kg",
            &[
                num(10),
                op(OperatorTokenType::Pow),
                num(24),
                apply_to_prev_token_unit("kg"),
            ],
        );
    }

    #[test]
    fn test_huge_nums_in_scientific_form() {
        test("1e28", "10000000000000000000000000000");
        for i in 0..=28 {
            let input = format!("1e{}", i);
            let expected = format!("1{}", "0".repeat(i));
            test(&input, &expected);
        }
    }

    #[test]
    fn test_pi() {
        test("π", "3.1416");
    }

    #[test]
    fn test_multiple_equal_signs2() {
        test("=(Blq9h/Oq=7y^$o[/kR]*$*oReyMo-M++]", "7");
    }

    #[test]
    fn no_panic_huge_num_vs_num() {
        test(
            "79 228 162 514 264 337 593 543 950 335",
            "79228162514264337593543950335",
        );
        test(
            "79228162514264337593543950335 + 79228162514264337593543950335",
            "Err",
        );
        test(
            "-79228162514264337593543950335 - 79228162514264337593543950335",
            "Err",
        );
        test("10^28 * 10^28", "Err");
        test("10^28 / 10^-28", "Err");
    }

    #[test]
    fn no_panic_huge_num_vs_perc() {
        test("10^28 + 1000%", "Err");
        test("79228162514264337593543950335 + 1%", "Err");
        test("-79228162514264337593543950335 - (-1%)", "Err");
        test("10^28 - 1000%", "Err");
        test("10^28 * 1000%", "Err");
        test("10^28 / 1000%", "1000000000000000000000000000 %");
    }

    #[test]
    fn no_panic_huge_unit_vs_perc() {
        test("10^28m + 1000%", "Err");
        test("10^28m - 1000%", "Err");
        test("-79228162514264337593543950335m - (-1%)", "Err");
        test("10^28m * 1000%", "Err");
        test("10^28m / 1000%", "Err");
    }

    #[test]
    fn no_panic_huge_perc_vs_perc() {
        test("10^28% + 1000%", "Err");
        test("10^28% - 1000%", "Err");
        test("10^28% * 1000%", "Err");
        test("10^28% / 1000%", "Err");
        test("-79228162514264337593543950335% - 1%", "Err");
    }

    #[test]
    fn no_panic_huge_unit_vs_unit() {
        test(
            "79228162514264337593543950335s + 79228162514264337593543950335s",
            "Err",
        );
        test(
            "-79228162514264337593543950335s - 79228162514264337593543950335s",
            "Err",
        );
    }

    #[test]
    fn test_multiplying_bug_numbers_via_unit_no_panic() {
        test("909636Yl", "909636 Yl");
    }

    #[test]
    fn test_huge_unit_exponent() {
        test("6K^61595", "Err");
    }

    #[test]
    fn test_fuzzing_issue() {
        test("90-/9b^72^4", "Err");
    }

    #[test]
    fn calc_bug_period_calc() {
        test("(1000/month) + (2000/year) in year^-1", "14000 / year");
        test("(1000/month) + (2000/year)", "1166.6667 / month");
        test("(1000/year) + (2000/month)", "25000 / year");
        test("(1000/year) + (2000/year)", "3000 / year");
        test("(1000/month) + (2000/month)", "3000 / month");
    }

    #[test]
    fn calc_bug_period_calc2() {
        test("((1000/month) + (2000/year)) * 12 month", "14000");
    }

    #[test]
    fn calc_bug_period_calc3() {
        test("50 000 / month * 1 year", "600000");
    }

    #[test]
    fn calc_period_avoid_precision_loss_y_m_y() {
        test_with_dec_count(100, "((50000/year) + (25000/month)) * 10 year", "3500000");
    }

    #[test]
    fn calc_period_avoid_precision_loss_m_y_y() {
        test_with_dec_count(100, "((50000/month) + (25000/year)) * 10 year", "6250000");
    }
    #[test]
    fn calc_period_avoid_precision_loss_y_y_y() {
        test_with_dec_count(100, "((50000/year) + (25000/year)) * 10 year", "750000");
    }

    #[test]
    fn calc_period_avoid_precision_loss_m_m_m() {
        test_with_dec_count(100, "((50000/month) + (25000/month)) * 10 month", "750000");
    }

    #[test]
    fn calc_period_avoid_precision_loss_y_y_m() {
        test_with_dec_count(
            100,
            "((50000/year) + (25000/year)) * 10 month",
            "62500.0000",
        );
    }

    #[test]
    fn calc_period_avoid_precision_loss_m_m_y() {
        test_with_dec_count(100, "((50000/month) + (25000/month)) * 10 year", "9000000");
    }

    ///////////////////////////////////
    // dollar
    ///////////////////////////////////
    #[test]
    fn calc_period_avoid_precision_loss_dollar_y_m_y() {
        test_with_dec_count(
            100,
            "((50000$/year) + (25000$/month)) * 10 year",
            "3500000 $",
        );
    }

    #[test]
    fn test_u64_hex_bitwise_and() {
        test("0xFF AND 0xFFFFFFFFFFFFFFFF", &0xFFu64.to_string());
    }

    #[test]
    fn test_u64_hex_bitwise_or() {
        test(
            "0xFF OR 0xFFFFFFFFFFFFFFFF",
            &0xFFFFFFFFFFFFFFFFu64.to_string(),
        );
    }

    #[test]
    fn test_u64_hex_bitwise_xor() {
        test(
            "0xFF XOR 0xFFFFFFFFFFFFFFFF",
            &0xFFFFFFFFFFFFFF00u64.to_string(),
        );
    }

    #[test]
    fn test_u64_hex_bitwise_shift_left() {
        test(
            "0x00FFFFFF_FFFFFFFF << 8",
            &0xFFFFFFFF_FFFFFF00u64.to_string(),
        );
    }

    #[test]
    fn test_u64_hex_bitwise_shift_right() {
        test(
            "0xFFFFFFFF_FFFFFFFF >> 8",
            &0x00FFFFFF_FFFFFFFFu64.to_string(),
        );
    }

    #[test]
    fn test_calc_num_perc_on_what() {
        test("41 is 17% on what", "35.0427");
    }

    #[test]
    fn test_calc_num_perc_on_what_tokens() {
        test_tokens(
            "41 is 17% on what",
            &[
                num(41),
                str(" "),
                op(OperatorTokenType::PercentageIs),
                str(" "),
                num(17),
                op(OperatorTokenType::Perc),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Base_From_Result_Increase_X),
            ],
        );
    }

    #[test]
    fn test_calc_num_perc_on_what_2() {
        test("41 is (16%+1%) on what", "35.0427");
    }

    #[test]
    fn test_calc_num_perc_on_what_3() {
        test("41 is (16+1)% on what", "35.0427");
    }

    #[test]
    fn test_calc_percentage_what_plus() {
        test("what plus 17% is 41", "35.0427");
    }

    #[test]
    fn test_calc_percentage_what_plus_2() {
        test("what plus (16%+1%) is 41", "35.0427");
    }
    #[test]
    fn test_calc_percentage_what_plus_3() {
        test("what plus (16+1)% is 41", "35.0427");
    }

    #[test]
    fn test_calc_perc_on_what_is() {
        test("17% on what is 41", "35.0427");
    }

    #[test]
    fn test_calc_perc_on_what_is_tokens() {
        test_tokens(
            "17% on what is 41",
            &[
                num(17),
                op(OperatorTokenType::Perc),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Base_From_Icrease_X_Result),
                str(" "),
                num(41),
            ],
        );
    }

    #[test]
    fn test_calc_perc_on_what_is_2() {
        test("(16%+1%) on what is 41", "35.0427");
    }

    #[test]
    fn test_calc_perc_on_what_is_3() {
        test("(16+1)% on what is 41", "35.0427");
    }

    #[test]
    fn test_calc_num_what_perc_on_num_tokens() {
        test("41 is what % on 35", "17.1429 %");
    }

    #[test]
    fn test_calc_num_perc_off_what() {
        test("41 is 17% off what", "49.3976");
    }

    #[test]
    fn test_calc_num_perc_off_what_tokens() {
        test_tokens(
            "41 is 17% off what",
            &[
                num(41),
                str(" "),
                op(OperatorTokenType::PercentageIs),
                str(" "),
                num(17),
                op(OperatorTokenType::Perc),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Base_From_Result_Decrease_X),
            ],
        );
    }

    #[test]
    fn test_calc_num_perc_off_what_2() {
        test("41 is (16%+1%) off what", "49.3976");
    }

    #[test]
    fn test_calc_num_perc_off_what_3() {
        test("41 is (16+1)% off what", "49.3976");
    }

    #[test]
    fn test_calc_percentage_what_minus() {
        test("what minus 17% is 41", "49.3976");
    }

    #[test]
    fn test_calc_percentage_what_minus_2() {
        test("what minus (16%+1%) is 41", "49.3976");
    }
    #[test]
    fn test_calc_percentage_what_minus_3() {
        test("what minus (16+1)% is 41", "49.3976");
    }

    #[test]
    fn test_calc_perc_off_what_is() {
        test("17% off what is 41", "49.3976");
    }

    #[test]
    fn test_calc_perc_off_what_is_tokens() {
        test_tokens(
            "17% off what is 41",
            &[
                num(17),
                op(OperatorTokenType::Perc),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Base_From_Decrease_X_Result),
                str(" "),
                num(41),
            ],
        );
    }

    #[test]
    fn test_calc_perc_off_what_is_2() {
        test("(16%+1%) off what is 41", "49.3976");
    }

    #[test]
    fn test_calc_perc_off_what_is_3() {
        test("(16+1)% off what is 41", "49.3976");
    }

    #[test]
    fn test_calc_num_what_perc_off_num_tokens() {
        test("35 is what % off 41", "14.6341 %");
    }

    #[test]
    fn test_percent_complex_1() {
        test("44 is (220 is what % on 200) on what", "40");
    }

    #[test]
    fn test_percent_complex_2() {
        test("44 is (180 is what % off 200) on what", "40");
    }

    #[test]
    fn test_percent_complex_3() {
        test("(44 is 10% on what) is 60% on what", "25");
    }

    #[test]
    fn test_percent_complex_4() {
        test("what plus (180 is what % off 200) is 44", "40");
    }

    #[test]
    fn test_percent_complex_5() {
        test("(180 is what % off 200) on what is 44", "40");
    }

    #[test]
    fn test_percent_complex_6() {
        test(
            "44 is what % on ((180 is what % off 200) on what is 44)",
            "10 %",
        );
    }

    #[test]
    fn test_percent_complex_7() {
        test(
            "44 is what % on ((180 is what % off (what plus 10% is 220)) on what is 44)",
            "10 %",
        );
    }

    #[test]
    fn test_calc_percentage_find_rate_from_result_base() {
        test("20 is what percent of 60", "33.3333 %");
    }

    #[test]
    fn test_calc_percentage_find_rate_from_result_base_tokens() {
        test_tokens(
            "20 is what percent of 60",
            &[
                num(20),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Rate_From_Result_Base),
                str(" "),
                num(60),
            ],
        );
    }

    #[test]
    fn test_calc_percentage_find_base_from_result_rate() {
        test("5 is 25% of what", "20");
    }

    #[test]
    fn test_calc_percentage_find_base_from_result_rate_tokens() {
        test_tokens(
            "5 is 25% of what",
            &[
                num(5),
                str(" "),
                op(OperatorTokenType::PercentageIs),
                str(" "),
                num(25),
                op(OperatorTokenType::Perc),
                str(" "),
                op(OperatorTokenType::Percentage_Find_Base_From_Result_Rate),
            ],
        );
    }

    #[test]
    fn test_invalid_left_shift_to_string2() {
        test("1 << 200", "Err");
    }

    #[test]
    fn test_invalid_left_shift_to_string() {
        test_tokens(
            "1 << 200",
            &[
                num(1),
                str(" "),
                op_err(OperatorTokenType::ShiftLeft),
                str(" "),
                num(200),
            ],
        );
    }

    #[test]
    fn test_invalid_right_shift_to_string2() {
        test("1 >> 64", "Err");
    }

    #[test]
    fn test_invalid_right_shift_to_string() {
        test_tokens(
            "1 >> 64",
            &[
                num(1),
                str(" "),
                op_err(OperatorTokenType::ShiftRight),
                str(" "),
                num(64),
            ],
        );
    }

    #[test]
    fn test_multiplying_too_much_units() {
        test("1 km*h*s*b*J*A*ft * 2 L*mi", "Err");
    }

    #[test]
    fn test_dividing_too_much_units() {
        test("1 km*h*s*b*J*A*ft / 2 L*mi", "Err");
    }

    #[test]
    fn test_unit_conversion_26() {
        test_tokens(
            "(256byte * 120) in MiB",
            &[
                op(OperatorTokenType::ParenOpen),
                num(256),
                apply_to_prev_token_unit("bytes"),
                str(" "),
                op(OperatorTokenType::Mult),
                str(" "),
                num(120),
                op(OperatorTokenType::ParenClose),
                str(" "),
                op(OperatorTokenType::UnitConverter),
                str(" "),
                unit("MiB"),
            ],
        );
    }

    #[test]
    fn test_explicit_multipl_is_mandatory_before_units() {
        test_tokens(
            "2m^4kg/s^3",
            &[num(2), apply_to_prev_token_unit("m^4"), unit("kg / s^3")],
        );
        // it is the accepted form
        test_tokens(
            "2m^4*kg/s^3",
            &[num(2), apply_to_prev_token_unit("(m^4 kg) / s^3")],
        );
    }

    #[test]
    fn not_in_must_be_str_if_we_are_sure_it_cant_be_unit() {
        test_tokens(
            "12 m in",
            &[
                num(12),
                str(" "),
                apply_to_prev_token_unit("m"),
                str(" "),
                str("in"),
            ],
        );
        test("12 m in", "12 m");
    }

    #[test]
    fn test_bug_no_paren_around_100() {
        test_tokens(
            "1+e()^(100)",
            &[
                num(1),
                op(OperatorTokenType::Add),
                op(OperatorTokenType::Fn {
                    arg_count: 0,
                    typ: FnType::E,
                }),
                op(OperatorTokenType::ParenOpen),
                op(OperatorTokenType::ParenClose),
                op_err(OperatorTokenType::Pow),
                op(OperatorTokenType::ParenOpen),
                num(100),
                op(OperatorTokenType::ParenClose),
            ],
        );
    }

    #[test]
    fn test_fuzz_bug_201220() {
        test(")5)t[Mr/(K)", "5 t");
    }

    #[test]
    fn test_fuzz_bug_201221_2_no_panic_if_arg_is_not_valid_token() {
        test("e(R())", "2.7183");
        test("e(e())", "Err");
    }

    #[test]
    fn test_fuzz_bug_201221_3_no_panic_if_arg_is_not_valid_token() {
        test("sin(R())", "Err");
    }

    #[test]
    fn test_fuzz_bug_201221_4_no_panic_if_arg_is_not_valid_token() {
        test("ln(R())", "Err");
        test("ln(R(), R())", "Err");
        test("ln()", "Err");
    }

    #[test]
    fn test_fuzz_bug_201221_5_no_panic_if_arg_is_not_valid_token() {
        test("log(R(), R())", "Err");
    }

    #[test]
    fn test_fuzz_bug_201221_6_no_panic_if_arg_is_not_valid_token() {
        test("ceil(R())", "Err");
    }

    #[test]
    fn test_aply_unit_to_func_result() {
        test("(12+3)m", "15 m");
        test("sin(2 degree) m", "0.0349 m");
        test("sin(pi() rad)", "0");
        test("sin(0.5*pi() rad)", "1");
        test("sin(0.3*pi() rad)", "0.809");
    }
}
