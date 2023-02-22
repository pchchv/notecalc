use crate::token_parser::{debug_print, OperatorTokenType, Token, TokenParser, TokenType};

use bumpalo::Bump;
use helper::*;

pub mod token_parser;

pub const SCROLLBAR_WIDTH: usize = 1;
pub const MAX_FUNCTION_PARAM_COUNT: usize = 6;
pub const MAX_VAR_NAME_LEN: usize = 32;

pub const RENDERED_RESULT_PRECISION: usize = 28;
pub const MAX_EDITOR_WIDTH: usize = 120;
pub const LEFT_GUTTER_MIN_WIDTH: usize = 2;

// Currently y coords are transmitted as u8 to the frontend, if you raise this value,
// don't forget to update  the communication layer as well
pub const MAX_LINE_COUNT: usize = 256;
pub const MAX_TOKEN_COUNT_PER_LINE: usize = MAX_LINE_COUNT;
pub const RIGHT_GUTTER_WIDTH: usize = 2;
pub const MIN_RESULT_PANEL_WIDTH: usize = 7;

// There are some optimizationts (stack allocated arrays etc), where we have to know
// the maximum lines rendered at once, so it is limited to 64
pub const MAX_CLIENT_HEIGHT: usize = 64;
pub const DEFAULT_RESULT_PANEL_WIDTH_PERCENT: usize = 30;
pub const SUM_VARIABLE_INDEX: usize = MAX_LINE_COUNT;
#[allow(dead_code)]
pub const FIRST_FUNC_PARAM_VAR_INDEX: usize = SUM_VARIABLE_INDEX + 1;
pub const VARIABLE_ARR_SIZE: usize = MAX_LINE_COUNT + 1 + MAX_FUNCTION_PARAM_COUNT;
pub const MATRIX_ASCII_HEADER_FOOTER_LINE_COUNT: usize = 2;
pub const ACTIVE_LINE_REF_HIGHLIGHT_COLORS: [u32; 9] = [
    0xFFD300FF, 0xDE3163FF, 0x73c2fbFF, 0xc7ea46FF, 0x702963FF, 0x997950FF, 0x777b73FF, 0xFC6600FF,
    0xED2939FF,
];

#[allow(dead_code)]
pub struct Theme {
    pub bg: u32,
    pub func_bg: u32,
    pub result_bg_color: u32,
    pub selection_color: u32,
    pub sum_bg_color: u32,
    pub sum_text_color: u32,
    pub reference_pulse_start: u32,
    pub reference_pulse_end: u32,
    pub number: u32,
    pub number_error: u32,
    pub operator: u32,
    pub unit: u32,
    pub variable: u32,
    pub result_text: u32,
    pub header: u32,
    pub text: u32,
    pub cursor: u32,
    pub matrix_edit_active_bg: u32,
    pub matrix_edit_active_text: u32,
    pub matrix_edit_inactive_text: u32,
    pub result_gutter_bg: u32,
    pub left_gutter_bg: u32,
    pub line_num_active: u32,
    pub line_num_simple: u32,
    pub scrollbar_hovered: u32,
    pub scrollbar_normal: u32,
    pub line_ref_bg: u32,
    pub line_ref_text: u32,
    pub line_ref_selector: u32,
    pub referenced_matrix_text: u32,
    pub change_result_pulse_start: u32,
    pub change_result_pulse_end: u32,
    pub current_line_bg: u32,
    pub parenthesis: u32,
}

#[allow(dead_code)]
impl Theme {
    const DRACULA_BG: u32 = 0x282a36_FF;
    const DRACULA_CURRENT_LINE: u32 = 0x44475a_FF;
    const DRACULA_FG: u32 = 0xf8f8f2_FF;
    const DRACULA_CYAN: u32 = 0x8be9fd_FF;
    const DRACULA_COMMENT: u32 = 0x6272a4_FF;
    const DRACULA_GREEN: u32 = 0x50fa7b_FF;
    const DRACULA_ORANGE: u32 = 0xffb86c_FF;
    const DRACULA_PINK: u32 = 0xff79c6_FF;
    const DRACULA_PURPLE: u32 = 0xbd93f9_FF;
    const DRACULA_RED: u32 = 0xff5555_FF;
    const DRACULA_YELLOW: u32 = 0xf1fa8c_FF;
}

pub const THEMES: [Theme; 2] = [
    // LIGHT
    Theme {
        bg: 0xFFFFFF_FF,
        func_bg: 0xEFEFEF_FF,
        result_bg_color: 0xF2F2F2_FF,
        result_gutter_bg: 0xD2D2D2_FF,
        selection_color: 0xA6D2FF_FF,
        sum_bg_color: 0x008a0d_FF,
        sum_text_color: 0x000000_FF,
        reference_pulse_start: 0x00FF7F_33,
        reference_pulse_end: 0x00FF7F_00,
        number: 0x8963c4_FF,
        number_error: 0xde353d_FF,
        operator: 0x3a88e8_FF,
        unit: 0x048395_FF,
        variable: 0xc26406_FF,
        result_text: 0x000000_FF,
        header: 0x000000_FF,
        text: 0x8393c7_FF,
        cursor: 0x000000_FF,
        matrix_edit_active_bg: 0xBBBBBB_55,
        matrix_edit_active_text: 0x000000_FF,
        matrix_edit_inactive_text: 0x000000_FF,
        left_gutter_bg: 0xF2F2F2_FF,
        line_num_active: 0x000000_FF,
        line_num_simple: 0xADADAD_FF,
        scrollbar_hovered: 0xFFBBBB_FF,
        scrollbar_normal: 0xFFCCCC_FF,
        line_ref_text: 0x000000_FF,
        line_ref_bg: 0xDCE2F7_FF,
        line_ref_selector: 0xDCE2F7_FF,
        referenced_matrix_text: 0x000000_FF,
        change_result_pulse_start: 0xFF88FF_AA,
        change_result_pulse_end: 0xFFFFFF_55,
        current_line_bg: 0xFFFFCC_FF,
        parenthesis: 0x565869_FF,
    },
    // DARK
    Theme {
        bg: Theme::DRACULA_BG,
        func_bg: 0x292B37_FF,
        result_bg_color: 0x3c3f41_FF,
        result_gutter_bg: 0x313335_FF,
        selection_color: 0x214283_FF,
        sum_bg_color: Theme::DRACULA_GREEN,
        sum_text_color: 0x000000_FF,
        reference_pulse_start: 0x00FF7F_33,
        reference_pulse_end: 0x00FF7F_00,
        number: Theme::DRACULA_PURPLE,
        number_error: Theme::DRACULA_RED,
        operator: 0x5bb0ff_FF, // Theme::DRACULA_YELLOW,
        unit: Theme::DRACULA_CYAN,
        variable: Theme::DRACULA_ORANGE,
        result_text: Theme::DRACULA_FG,
        header: Theme::DRACULA_FG,
        text: Theme::DRACULA_COMMENT + 0x444444_00,
        cursor: Theme::DRACULA_FG,
        matrix_edit_active_bg: 0xBBBBBB_55,
        matrix_edit_active_text: 0x000000_FF,
        matrix_edit_inactive_text: 0x000000_FF,
        left_gutter_bg: 0x3c3f41_FF,
        line_num_active: 0xa3a2a0_FF,
        line_num_simple: 0x4e6164_FF,
        scrollbar_hovered: 0x4f4f4f_FF,
        scrollbar_normal: 0x4b4b4b_FF,
        line_ref_bg: 0x7C92A7_FF,
        line_ref_text: 0x000000_FF,
        line_ref_selector: Theme::DRACULA_BG + 0x333300_00,
        referenced_matrix_text: 0x000000_FF,
        change_result_pulse_start: 0xFF88FF_AA,
        change_result_pulse_end: Theme::DRACULA_BG - 0xFF,
        current_line_bg: Theme::DRACULA_CURRENT_LINE,
        parenthesis: Theme::DRACULA_PINK,
    },
];

#[allow(non_snake_case)]
#[inline]
pub fn NOT(a: bool) -> bool {
    !a
}

pub enum Click {
    Simple(Pos),
    Drag(Pos),
}

fn get_function_index_for_line(
    line_index: usize,
    func_defs: &FunctionDefinitions,
) -> Option<usize> {
    for investigated_line_i in (0..=line_index).rev() {
        if let Some(fd) = func_defs[investigated_line_i].as_ref() {
            let func_end_index = fd.last_row_index.as_usize();
            let line_index_is_part_of_that_func = line_index <= func_end_index;
            return if line_index_is_part_of_that_func {
                Some(investigated_line_i)
            } else {
                None
            };
        }
    }
    None
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub func_name: &'a [char],
    pub param_names: [&'a [char]; MAX_FUNCTION_PARAM_COUNT],
    pub param_count: usize,
    pub first_row_index: ContentIndex,
    pub last_row_index: ContentIndex,
}

pub fn try_extract_function_def<'b>(
    parsed_tokens: &mut [Token<'b>],
    allocator: &'b Bump,
) -> Option<FunctionDef<'b>> {
    if parsed_tokens.len() < 4
        || (!parsed_tokens[0].ptr[0].is_alphabetic() && parsed_tokens[0].ptr[0] != '_')
        || parsed_tokens[1].typ != TokenType::Operator(OperatorTokenType::ParenOpen)
        || parsed_tokens.last().unwrap().ptr != &[':']
    {
        return None;
    }

    let mut fd = FunctionDef {
        func_name: parsed_tokens[0].ptr,
        param_names: [&[]; MAX_FUNCTION_PARAM_COUNT],
        param_count: 0,
        first_row_index: content_y(0),
        last_row_index: content_y(0),
    };

    fn skip_whitespace_tokens(parsed_tokens: &[Token], token_index: &mut usize) {
        while *token_index < parsed_tokens.len()
            && parsed_tokens[*token_index].ptr[0].is_whitespace()
        {
            *token_index += 1;
        }
    }

    fn close_var_name_parsing<'b>(
        param_index: &mut usize,
        fd: &mut FunctionDef<'b>,
        var_name: &[char],
        allocator: &'b Bump,
    ) {
        fd.param_names[*param_index] =
            allocator.alloc_slice_fill_iter(var_name.iter().map(|it| *it));
        *param_index += 1;
    }

    let mut param_index = 0;
    let mut token_index = 2;
    let mut token_indices_for_params = BitFlag256::empty();
    let mut tmp_var_name: ArrayVec<[char; MAX_VAR_NAME_LEN]> = ArrayVec::new();

    loop {
        if token_index == parsed_tokens.len() - 2
            && parsed_tokens[token_index].typ == TokenType::Operator(OperatorTokenType::ParenClose)
            && parsed_tokens[token_index + 1].ptr == &[':']
        {
            if !tmp_var_name.is_empty() {
                close_var_name_parsing(&mut param_index, &mut fd, &tmp_var_name, allocator);
            }
            break;
        } else if parsed_tokens[token_index].typ == TokenType::Operator(OperatorTokenType::Comma) {
            close_var_name_parsing(&mut param_index, &mut fd, &tmp_var_name, allocator);
            tmp_var_name.clear();

            token_index += 1; // skip ','
            skip_whitespace_tokens(parsed_tokens, &mut token_index);
        } else if matches!(
            parsed_tokens[token_index].typ,
            TokenType::StringLiteral | TokenType::Variable { .. }
        ) {
            if tmp_var_name.len() + parsed_tokens[token_index].ptr.len() > MAX_VAR_NAME_LEN {
                return None;
            }
            tmp_var_name.extend_from_slice(parsed_tokens[token_index].ptr);
            token_indices_for_params.set(token_index);
            token_index += 1;
        } else {
            return None;
        }
    }

    fd.param_count = param_index;

    parsed_tokens[0].typ = TokenType::Operator(OperatorTokenType::Fn {
        arg_count: fd.param_count,
        typ: FnType::UserDefined(0),
    });
    // set ':' to operator
    parsed_tokens.last_mut().unwrap().typ = TokenType::Operator(OperatorTokenType::Add);
    // set param names to variables
    for i in 0..parsed_tokens.len() {
        if token_indices_for_params.is_true(i) {
            parsed_tokens[i].typ = TokenType::Variable {
                var_index: FIRST_FUNC_PARAM_VAR_INDEX + i,
            };
        }
    }

    return Some(fd);
}

fn set_editor_and_result_panel_widths(
    client_width: usize,
    result_panel_width_percent: usize,
    gr: &mut GlobalRenderData,
) {
    let mut result_gutter_x: isize =
        (client_width * (100 - result_panel_width_percent) / 100) as isize;
    {
        // the editor pushes the gutter to right
        let editor_width =
            (result_gutter_x - SCROLLBAR_WIDTH as isize - LEFT_GUTTER_MIN_WIDTH as isize) - 1;
        let diff = gr.longest_visible_editor_line_len as isize - editor_width;
        if diff > 0 {
            result_gutter_x += diff;
        }
    }
    {
        // the result panel pushes the gutter to left, with higher priority
        let result_panel_w: isize =
            client_width as isize - result_gutter_x - RIGHT_GUTTER_WIDTH as isize;
        let diff = gr.longest_visible_result_len as isize - result_panel_w;
        if diff > 0 {
            result_gutter_x -= diff;
        }
    }
    // result panel width has a minimum required width
    let result_panel_w = client_width as isize - result_gutter_x - RIGHT_GUTTER_WIDTH as isize;
    let result_gutter_x = (client_width as isize - result_panel_w - RIGHT_GUTTER_WIDTH as isize)
        .max(MIN_RESULT_PANEL_WIDTH as isize) as usize;
    gr.set_result_gutter_x(result_gutter_x);
}
pub fn default_result_gutter_x(client_width: usize) -> usize {
    (client_width * (100 - DEFAULT_RESULT_PANEL_WIDTH_PERCENT) / 100)
        .max(LEFT_GUTTER_MIN_WIDTH + SCROLLBAR_WIDTH)
}
