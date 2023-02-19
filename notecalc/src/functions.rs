#[derive(PartialEq, Eq, Clone, Copy, Debug, EnumIter)]
pub enum FnType {
    UserDefined(usize),
    Nth,
    Sum,
    Transpose,
    Pi,
    E,
    Ceil,
    Ln,
    Lg,
    Log,
    Abs,
    Sin,
    Asin,
    Cos,
    Acos,
    Tan,
    Atan,
}

impl FnType {
    #[inline]
    pub fn value_of(ptr: &[char]) -> Option<FnType> {
        for fn_type in FnType::iter() {
            if ptr == fn_type.name() {
                return Some(fn_type);
            }
        }
        return None;
    }

    #[inline]
    pub fn name(&self) -> &'static [char] {
        match self {
            FnType::Abs => &['a', 'b', 's'],
            FnType::Sin => &['s', 'i', 'n'],
            FnType::Cos => &['c', 'o', 's'],
            FnType::Asin => &['a', 's', 'i', 'n'],
            FnType::Acos => &['a', 'c', 'o', 's'],
            FnType::Tan => &['t', 'a', 'n'],
            FnType::Atan => &['a', 't', 'a', 'n'],
            FnType::Nth => &['n', 't', 'h'],
            FnType::Sum => &['s', 'u', 'm'],
            FnType::Transpose => &['t', 'r', 'a', 'n', 's', 'p', 'o', 's', 'e'],
            FnType::Pi => &['p', 'i'],
            FnType::E => &['e'],
            FnType::Ceil => &['c', 'e', 'i', 'l'],
            FnType::Ln => &['l', 'n'],
            FnType::Lg => &['l', 'g'],
            FnType::Log => &['l', 'o', 'g'],
            FnType::UserDefined(_) => &[],
        }
    }

    #[inline]
    pub fn execute<'text_ptr>(
        &self,
        arg_count: usize,
        stack: &mut Vec<CalcResult>,
        fn_token_index: usize,
        units: &Units,
    ) -> Result<(), EvalErr> {
        match self {
            FnType::Abs => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_single_param_decimal(stack, Decimal::abs)
            }),
            FnType::Nth => arg_count_limited_fn(2, arg_count, stack, fn_token_index, fn_nth),
            FnType::Sum => arg_count_limited_fn(1, arg_count, stack, fn_token_index, fn_sum),
            FnType::Transpose => {
                arg_count_limited_fn(1, arg_count, stack, fn_token_index, fn_transpose)
            }
            FnType::Pi => arg_count_limited_fn(0, arg_count, stack, fn_token_index, |stack| {
                fn_const(stack, fn_token_index, DECIMAL_PI)
            }),
            FnType::E => arg_count_limited_fn(0, arg_count, stack, fn_token_index, |stack| {
                fn_const(stack, fn_token_index, DECIMAL_E)
            }),
            FnType::Sin => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_rad_to_num(stack, units, f64::sin)
            }),
            FnType::Asin => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_num_to_rad(stack, f64::asin, units)
            }),
            FnType::Cos => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_rad_to_num(stack, units, f64::cos)
            }),
            FnType::Acos => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_num_to_rad(stack, f64::acos, units)
            }),
            FnType::Tan => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_rad_to_num(stack, units, f64::tan)
            }),
            FnType::Atan => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_f64_num_to_rad(stack, f64::atan, units)
            }),
            FnType::Ceil => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_single_param_decimal(stack, Decimal::ceil)
            }),

            FnType::Ln => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_single_param_f64(stack, f64::ln)
            }),
            FnType::Lg => arg_count_limited_fn(1, arg_count, stack, fn_token_index, |stack| {
                fn_single_param_f64(stack, f64::log2)
            }),
            FnType::Log => arg_count_limited_fn(2, arg_count, stack, fn_token_index, |stack| {
                fn_double_param_f64(stack, fn_token_index, |a, b| f64::log(b, a))
            }),
            FnType::UserDefined(_i) => {
                panic!("User fn is handled manually")
            }
        }
    }
}

fn fn_const<'text_ptr>(
    stack: &mut Vec<CalcResult>,
    token_index: usize,
    const_value: Decimal,
) -> Result<(), EvalErr> {
    stack.push(CalcResult::new(
        CalcResultType::Number(const_value),
        token_index,
    ));

    Ok(())
}

fn fn_arg_count_limited<F>(
    expected_arg_count: usize,
    arg_count: usize,
    stack: &mut Vec<CalcResult>,
    fn_token_index: usize,
    action: F,
) -> Result<(), EvalErr>
where
    F: Fn(&mut Vec<CalcResult>) -> Result<(), EvalErr>,
{
    let arg_count = arg_count.min(stack.len());

    return if expected_arg_count != arg_count {
        if arg_count > 0 {
            Err(EvalErr::new(
                "Illegal argument".to_owned(),
                stack[stack.len() - 1].get_index_into_tokens(),
            ))
        } else {
            Err(EvalErr::new("Illegal argument".to_owned(), fn_token_index))
        }
    } else {
        action(stack)
    };
}
