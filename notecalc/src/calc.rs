use crate::borrow_checker_fighter::create_vars;
use crate::editor::editor_content::EditorContent;
use crate::functions::FnType;
use crate::helper::{content_y, AppTokens, BitFlag256};
use crate::matrix::MatrixData;
use crate::token_parser::{debug_print, OperatorTokenType, TokenType, UnitTokenType};
use crate::units::units::{UnitOutput, Units, MAX_UNIT_COUNT};
use crate::{
    tracy_span, FunctionDefinitions, LineData, Variable, Variables, FIRST_FUNC_PARAM_VAR_INDEX,
    MAX_TOKEN_COUNT_PER_LINE, SUM_VARIABLE_INDEX, VARIABLE_ARR_SIZE,
};
use rust_decimal::prelude::*;
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
                        // TODO clone
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
                // TODO avoid copy
                let mut local_vars = create_vars();

                // don't clone sum
                local_vars[0..SUM_VARIABLE_INDEX].clone_from_slice(&vars[0..SUM_VARIABLE_INDEX]);
                local_vars[SUM_VARIABLE_INDEX + 1..VARIABLE_ARR_SIZE]
                    .clone_from_slice(&vars[SUM_VARIABLE_INDEX + 1..VARIABLE_ARR_SIZE]);
                for i in (0..expected_arg_count).rev() {
                    local_vars[FIRST_FUNC_PARAM_VAR_INDEX + i] = Some(Variable {
                        // TODO: absolutely not, it would mean an alloc in hot path
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
                // TODO clone :(
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
                // nocheckin TODO: remove clone
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
        //////////////
        // 12 * x
        //////////////
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
        //////////////
        // 12km * x
        //////////////
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
        //////////////
        // 12% * x
        //////////////
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
        //////////////
        // Matrix
        //////////////
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

