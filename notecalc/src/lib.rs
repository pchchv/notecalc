#![feature(drain_filter)]
#![feature(type_alias_impl_trait)]
#![deny(
    warnings,
    anonymous_parameters,
    unused_extern_crates,
    unused_import_braces,
    trivial_casts,
    variant_size_differences,
    trivial_numeric_casts,
    unused_qualifications,
    clippy::all
)]

use crate::calc::{
    add_op, evaluate_tokens, get_var_name_from_assignment, process_variable_assignment_or_line_ref,
    CalcResult, CalcResultType, EvalErr, EvaluationResult, ShuntingYardResult,
};
use crate::editor::editor::{
    Editor, EditorInputEvent, InputModifiers, Pos, RowModificationType, Selection,
};
use crate::editor::editor_content::EditorContent;
use crate::functions::FnType;
use crate::matrix::MatrixData;
use crate::renderer::{get_int_frac_part_len, render_result, render_result_into};
use crate::token_parser::{debug_print, OperatorTokenType, Token, TokenParser, TokenType};
use crate::units::units::Units;

use bumpalo::Bump;
use const_fn;
use const_panic;
use helper::*;
use std::io::Cursor;
use std::ops::Range;
use std::time::Duration;
use strum_macros::EnumDiscriminants;
use symbol::sym::const_in_array_repeat_expressions;
use tinyvec::ArrayVec;

pub mod borrow_checker_fighter;
pub mod calc;
pub mod constants;
pub mod editor;
pub mod functions;
pub mod matrix;
pub mod renderer;
pub mod shunting_yard;
pub mod test_common;
pub mod token_parser;
pub mod units;

#[inline]
fn _readonly_<T: ?Sized>(e: &mut T) -> &T {
    return e;
}

#[inline]
#[cfg(feature = "tracy")]
fn tracy_span(name: &str, file: &str, line: u32) -> tracy_client::Span {
    return tracy_client::Span::new(name, name, file, line, 100);
}

#[inline]
#[cfg(not(feature = "tracy"))]
fn tracy_span(_name: &str, _file: &str, _line: u32) -> () {}

pub const SCROLLBAR_WIDTH: usize = 1;
pub const MAX_FUNCTION_PARAM_COUNT: usize = 6;
pub const MAX_VAR_NAME_LEN: usize = 32;
pub const EMPTY_FILE_DEFUALT_CONTENT: &str = "\n\n\n\n\n\n\n\n\n\n";
pub const MAX_VISIBLE_HEADER_COUNT: usize = 16;

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

// "0,0,0;0,0,0;0,0,0".split("").map(x => '\'' + x + '\'').join(',')
const AUTOCOMPLETION_CONSTS: [AutoCompletionConst; 5] = [
    AutoCompletionConst {
        abbrev: &['p', 'o', 'w'],
        replace_to: &['^'],
        relative_new_cursor_pos: None,
    },
    AutoCompletionConst {
        abbrev: &['m', 'a', 't', '3'],
        replace_to: &[
            '[', '0', ',', '0', ',', '0', ';', '0', ',', '0', ',', '0', ';', '0', ',', '0', ',',
            '0', ']',
        ],
        relative_new_cursor_pos: Some(1),
    },
    AutoCompletionConst {
        abbrev: &['m', 'a', 't', '4'],
        replace_to: &[
            '[', '0', ',', '0', ',', '0', ',', '0', ';', '0', ',', '0', ',', '0', ',', '0', ';',
            '0', ',', '0', ',', '0', ',', '0', ';', '0', ',', '0', ',', '0', ',', '0', ']',
        ],
        relative_new_cursor_pos: Some(1),
    },
    AutoCompletionConst {
        abbrev: &['m', 'a', 't'],
        replace_to: &['[', '0', ']'],
        relative_new_cursor_pos: Some(1),
    },
    AutoCompletionConst {
        abbrev: &['p', 'i'],
        replace_to: &['π'],
        relative_new_cursor_pos: None,
    },
];

static mut RESULT_BUFFER: [u8; 2048] = [0; 2048];

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

pub mod helper {
    use crate::calc::CalcResultType;
    pub use crate::{MAX_LINE_COUNT, *};
    use std::ops::{Index, IndexMut};

    #[derive(Debug)]
    pub struct EditorObjects(Vec<Vec<EditorObject>>);

    impl EditorObjects {
        pub fn new() -> EditorObjects {
            EditorObjects(
                std::iter::repeat(Vec::with_capacity(8))
                    .take(MAX_LINE_COUNT)
                    .collect::<Vec<_>>(),
            )
        }

        pub fn clear(&mut self) {
            self.0.clear();
        }

        pub fn push(&mut self, d: Vec<EditorObject>) {
            self.0.push(d);
        }
    }

    impl Index<ContentIndex> for EditorObjects {
        type Output = Vec<EditorObject>;

        fn index(&self, index: ContentIndex) -> &Self::Output {
            &self.0[index.0]
        }
    }

    impl IndexMut<ContentIndex> for EditorObjects {
        fn index_mut(&mut self, index: ContentIndex) -> &mut Self::Output {
            &mut self.0[index.0]
        }
    }

    pub struct Results([LineResult; MAX_LINE_COUNT]);

    impl Results {
        pub fn new() -> Results {
            Results([Ok(None); MAX_LINE_COUNT])
        }
        pub fn as_slice(&self) -> &[LineResult] {
            &self.0[..]
        }

        pub fn as_mut_slice(&mut self) -> &mut [LineResult] {
            &mut self.0[..]
        }
    }

    impl Index<ContentIndex> for Results {
        type Output = LineResult;

        fn index(&self, index: ContentIndex) -> &Self::Output {
            &self.0[index.0]
        }
    }

    impl IndexMut<ContentIndex> for Results {
        fn index_mut(&mut self, index: ContentIndex) -> &mut Self::Output {
            &mut self.0[index.0]
        }
    }

    #[derive(Debug)]
    pub struct AppTokens<'a>([Option<Tokens<'a>>; MAX_LINE_COUNT]);

    impl<'a> AppTokens<'a> {
        pub fn new() -> AppTokens<'a> {
            AppTokens([None; MAX_LINE_COUNT])
        }

        pub fn iter(&self) -> std::slice::Iter<Option<Tokens<'a>>> {
            self.0.iter()
        }
    }

    impl<'a> Index<ContentIndex> for AppTokens<'a> {
        type Output = Option<Tokens<'a>>;

        fn index(&self, index: ContentIndex) -> &Self::Output {
            &self.0[index.0]
        }
    }

    impl<'a> IndexMut<ContentIndex> for AppTokens<'a> {
        fn index_mut(&mut self, index: ContentIndex) -> &mut Self::Output {
            &mut self.0[index.0]
        }
    }

    #[derive(Copy, Clone)]
    pub struct BitFlag256 {
        pub bitset: [u128; 2],
    }

    impl BitFlag256 {
        pub fn empty() -> BitFlag256 {
            BitFlag256 { bitset: [0; 2] }
        }

        fn get_index(row_index: usize) -> (usize, usize) {
            let array_index = (row_index & (!127) > 0) as usize;
            let index_inside_u128 = row_index & 127;
            return (array_index, index_inside_u128);
        }

        pub fn set(&mut self, row_index: usize) {
            let (array_index, index_inside_u128) = BitFlag256::get_index(row_index);
            self.bitset[array_index] |= 1u128 << index_inside_u128;
        }

        pub fn single_row(row_index: usize) -> BitFlag256 {
            let (array_index, index_inside_u128) = BitFlag256::get_index(row_index);
            let mut bitset = [0; 2];
            bitset[array_index] = 1u128 << index_inside_u128;
            BitFlag256 { bitset }
        }

        #[inline]
        pub fn clear(&mut self) {
            self.bitset[0] = 0;
            self.bitset[1] = 0;
        }

        pub fn all_rows_starting_at(row_index: usize) -> BitFlag256 {
            if row_index >= MAX_LINE_COUNT {
                return BitFlag256::empty();
            }
            let mut bitset = [0; 2];

            let (array_index, index_inside_u128) = BitFlag256::get_index(row_index);
            let s = 1u128 << index_inside_u128;
            let right_to_s_bits = s - 1;
            let left_to_s_and_s_bits = !right_to_s_bits;
            bitset[array_index] = left_to_s_and_s_bits;
            // the other int is either fully 1s (if the array_index is 0) or 0s (if array_index is 1)
            let other_index = array_index ^ 1;
            bitset[other_index] = std::u128::MAX * other_index as u128;

            BitFlag256 { bitset }
        }
        pub fn multiple(indices: &[usize]) -> BitFlag256 {
            let mut b = [0; 2];
            for i in indices {
                let (array_index, index_inside_u128) = BitFlag256::get_index(*i);
                b[array_index] |= 1 << index_inside_u128;
            }
            let bitset = b;

            BitFlag256 { bitset }
        }

        pub fn range_incl(from: usize, to: usize) -> BitFlag256 {
            debug_assert!(to >= from);
            if from >= MAX_LINE_COUNT {
                return BitFlag256::empty();
            } else if to >= MAX_LINE_COUNT {
                return BitFlag256::range_incl(from, MAX_LINE_COUNT - 1);
            }
            fn set_range_u128(from: usize, to: usize) -> u128 {
                let top = 1 << to;
                let right_to_top_bits = top - 1;
                let bottom = 1 << from;
                let right_to_bottom_bits = bottom - 1;
                return (right_to_top_bits ^ right_to_bottom_bits) | top;
            }
            let mut b = BitFlag256::empty();
            if from < 128 {
                b.bitset[0] = set_range_u128(from, to.min(127));
            }
            if to >= 128 {
                b.bitset[1] = set_range_u128(((from as isize) - 128).max(0) as usize, to - 128);
            }

            return b;
        }

        #[inline]
        pub fn merge(&mut self, other: BitFlag256) {
            self.bitset[0] |= other.bitset[0];
            self.bitset[1] |= other.bitset[1];
        }

        #[inline]
        pub fn need(&self, line_index: ContentIndex) -> bool {
            let (array_index, index_inside_u128) = BitFlag256::get_index(line_index.0);
            ((1 << index_inside_u128) & self.bitset[array_index]) != 0
        }

        #[inline]
        pub fn is_true(&self, line_index: usize) -> bool {
            return self.need(content_y(line_index));
        }

        #[inline]
        pub fn is_false(&self, line_index: usize) -> bool {
            return !self.is_true(line_index);
        }

        #[inline]
        pub fn is_non_zero(&self) -> bool {
            (self.bitset[0] | self.bitset[1]) != 0
        }
    }

    #[derive(Clone, Debug)]
    pub struct GlobalRenderData {
        pub client_height: usize,
        pub client_width: usize,
        pub scroll_y: usize,
        pub result_gutter_x: usize,
        pub left_gutter_width: usize,
        pub longest_visible_result_len: usize,
        pub longest_visible_editor_line_len: usize,
        pub current_editor_width: usize,
        pub current_result_panel_width: usize,
        editor_y_to_render_y: [Option<CanvasY>; MAX_LINE_COUNT],
        editor_y_to_rendered_height: [usize; MAX_LINE_COUNT],
        pub theme_index: usize,
    }

    impl GlobalRenderData {
        pub fn new(
            client_width: usize,
            client_height: usize,
            result_gutter_x: usize,
            left_gutter_width: usize,
            right_gutter_width: usize,
        ) -> GlobalRenderData {
            let min_req_width =
                MIN_RESULT_PANEL_WIDTH + RIGHT_GUTTER_WIDTH + LEFT_GUTTER_MIN_WIDTH + 4;
            if client_width < min_req_width {
                panic!(
                    "client width is too small, it must be at least {} but it is {}",
                    min_req_width, client_width
                );
            }
            let mut r = GlobalRenderData {
                scroll_y: 0,
                longest_visible_result_len: 0,
                longest_visible_editor_line_len: 0,
                result_gutter_x,
                left_gutter_width,
                current_editor_width: 0,
                current_result_panel_width: 0,
                editor_y_to_render_y: [None; MAX_LINE_COUNT],
                editor_y_to_rendered_height: [0; MAX_LINE_COUNT],
                client_height: client_height.min(MAX_CLIENT_HEIGHT),
                client_width,
                theme_index: 0,
            };
            r.current_editor_width = (result_gutter_x - left_gutter_width) - 1;
            r.current_result_panel_width = client_width - result_gutter_x - right_gutter_width;
            // so tests without calling "paste" work
            r.editor_y_to_rendered_height[0] = 1;
            r
        }

        pub fn set_result_gutter_x(&mut self, x: usize) {
            self.result_gutter_x = x;
            // - 1 so that the last visible character in the editor is '…' if the content is to long
            self.current_editor_width = (x - self.left_gutter_width) - 1;
            self.current_result_panel_width = self.client_width - x - RIGHT_GUTTER_WIDTH;
        }

        pub fn set_left_gutter_width(&mut self, new_width: usize) {
            self.left_gutter_width = new_width;
            // - 1 so that the last visible character in the editor is '…' if the content is to long
            self.current_editor_width = (self.result_gutter_x - new_width) - 1;
        }

        pub fn calc_bottom_y(&self, content_len: usize) -> CanvasY {
            let bottom_i = content_y(content_len - 1);
            return if let Some(y) = self.get_render_y(bottom_i) {
                y.add(self.get_rendered_height(bottom_i))
            } else {
                canvas_y(self.client_height as isize)
            };
        }

        pub fn clear_editor_y_to_render_y(&mut self) {
            for e in self.editor_y_to_render_y.iter_mut() {
                *e = None;
            }
        }

        pub fn clear(&mut self) {
            for e in self.editor_y_to_render_y.iter_mut() {
                *e = None;
            }
            for e in self.editor_y_to_rendered_height.iter_mut() {
                *e = 0;
            }
            self.scroll_y = 0;
        }

        pub fn is_visible(&self, y: ContentIndex) -> bool {
            let top = match self.get_render_y(content_y(self.scroll_y)) {
                Some(y) => y.as_isize(),
                None => {
                    return false;
                }
            };
            return if let Some(y) = self.get_render_y(y) {
                let y = y.as_isize();
                y >= top && y < (top + self.client_height as isize)
            } else {
                false
            };
        }

        pub fn get_render_y(&self, y: ContentIndex) -> Option<CanvasY> {
            self.editor_y_to_render_y[y.0]
        }

        pub fn set_render_y(&mut self, y: ContentIndex, newy: Option<CanvasY>) {
            self.editor_y_to_render_y[y.0] = newy;
        }

        pub fn editor_y_to_render_y(&self) -> &[Option<CanvasY>] {
            &self.editor_y_to_render_y
        }

        pub fn get_rendered_height(&self, y: ContentIndex) -> usize {
            self.editor_y_to_rendered_height[y.0]
        }

        pub fn set_rendered_height(&mut self, y: ContentIndex, h: usize) {
            self.editor_y_to_rendered_height[y.0] = h;
        }
    }

    #[derive(Debug)]
    pub struct PerLineRenderData {
        pub editor_x: usize,
        pub editor_y: ContentIndex,
        pub render_x: usize,
        pub render_y: CanvasY,
        // contains the y position for each editor line
        pub rendered_row_height: usize,
        pub vert_align_offset: usize,
        pub cursor_render_x_offset: isize,
        // for rendering line number
        pub line_num_digit_0: u8,
        pub line_num_digit_1: u8,
        pub line_num_digit_2: u8,
    }

    impl PerLineRenderData {
        pub fn new() -> PerLineRenderData {
            let r = PerLineRenderData {
                editor_x: 0,
                editor_y: content_y(0),
                render_x: 0,
                render_y: canvas_y(0),
                rendered_row_height: 0,
                vert_align_offset: 0,
                cursor_render_x_offset: 0,
                line_num_digit_0: 0,
                line_num_digit_1: 0,
                line_num_digit_2: 0,
            };
            r
        }

        pub fn inc_editor_y(&mut self) {
            self.editor_y.0 += 1;
        }

        pub fn new_line_started(&mut self) {
            self.editor_x = 0;
            self.render_x = 0;
            self.cursor_render_x_offset = 0;
        }

        pub fn line_render_ended(&mut self, row_height: usize) {
            self.render_y.0 += row_height as isize;
            self.editor_y.0 += 1;
        }

        pub fn set_fix_row_height(&mut self, height: usize) {
            self.rendered_row_height = height;
            self.vert_align_offset = 0;
        }

        pub fn calc_rendered_row_height(
            result: &LineResult,
            tokens: &[Token],
            vars: &Variables,
            active_mat_edit_height: Option<usize>,
        ) -> usize {
            let mut max_height = active_mat_edit_height.unwrap_or(1);
            // determine max height based on result's height
            let result_row_height = if let Ok(result) = result {
                if let Some(result) = result {
                    let result_row_height = match &result.typ {
                        CalcResultType::Matrix(mat) => mat.render_height(),
                        _ => max_height,
                    };
                    result_row_height
                } else {
                    max_height
                }
            } else {
                max_height
            };

            // determine max height based on tokens' height
            for token in tokens {
                let token_height = match token.typ {
                    TokenType::Operator(OperatorTokenType::Matrix {
                        row_count,
                        col_count: _,
                    }) => MatrixData::calc_render_height(row_count),
                    TokenType::LineReference { var_index } => {
                        let var = &vars[var_index];
                        match &var {
                            Some(Variable {
                                value:
                                    Ok(CalcResult {
                                        typ: CalcResultType::Matrix(mat),
                                        ..
                                    }),
                                ..
                            }) => mat.render_height(),
                            _ => 1,
                        }
                    }
                    _ => 1,
                };
                if token_height > max_height {
                    max_height = token_height;
                }
            }
            return max_height.max(result_row_height);
        }

        pub fn token_render_done(&mut self, editor_len: usize, render_len: usize, x_offset: isize) {
            self.render_x += render_len;
            self.editor_x += editor_len;
            self.cursor_render_x_offset += x_offset;
        }
    }

    #[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
    pub struct ContentIndex(usize);

    #[inline]
    pub fn content_y(y: usize) -> ContentIndex {
        ContentIndex(y)
    }

    impl ContentIndex {
        #[inline]
        pub fn new(n: usize) -> ContentIndex {
            ContentIndex(n)
        }

        #[inline]
        pub fn as_usize(self) -> usize {
            self.0
        }

        pub fn add(&self, n: usize) -> ContentIndex {
            ContentIndex(self.0 + n)
        }

        pub fn sub(&self, n: usize) -> ContentIndex {
            ContentIndex(self.0 - n)
        }
    }

    #[derive(Clone, Copy, Eq, PartialEq, Debug, Ord, PartialOrd)]
    pub struct CanvasY(isize);

    pub fn canvas_y(y: isize) -> CanvasY {
        CanvasY(y)
    }

    impl CanvasY {
        pub fn new(n: isize) -> CanvasY {
            CanvasY(n)
        }

        pub fn as_usize(self) -> usize {
            self.0 as usize
        }

        pub fn as_isize(self) -> isize {
            self.0
        }

        pub fn add(&self, n: usize) -> CanvasY {
            CanvasY(self.0 + n as isize)
        }

        pub fn sub(&self, n: usize) -> CanvasY {
            CanvasY(self.0 - n as isize)
        }
    }
}

//, α, Ω, β
// γ - 	Greek Small Letter Gamma[1]
// δ Greek Small Letter Delta
// ε Greek Small Letter Epsilon
// ζ Greek Small Letter Zeta[
// η Greek Small Letter Eta
// θ Greek Small Letter Theta
// λ Greek Small Letter Lamda
// μ Greek Small Letter Mu
// φ Greek Small Letter Phi
// ω Greek Small Letter Omega
// ψ Greek Small Letter Psi
// τ Greek Small Letter Tau
// ϕ Greek Phi Symbol
struct AutoCompletionConst {
    //const PREFIX: char = '.';
    abbrev: &'static [char],
    replace_to: &'static [char],
    relative_new_cursor_pos: Option<usize>,
}

struct ScrollBarRenderInfo {
    scroll_bar_render_y: usize,
    scroll_bar_render_h: usize,
    max_scroll_y: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RenderStringMsg {
    pub text: String,
    pub row: CanvasY,
    pub column: usize,
}

#[derive(Debug, PartialEq)]
pub struct PulsingRectangle {
    pub x: usize,
    pub y: CanvasY,
    pub w: usize,
    pub h: usize,
    pub start_color: u32,
    pub end_color: u32,
    pub animation_time: Duration,
    pub repeat: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RenderUtf8TextMsg<'a> {
    pub text: &'a [char],
    pub row: CanvasY,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderChar {
    pub col: usize,
    pub row: CanvasY,
    pub char: char,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RenderAsciiTextMsg<'a> {
    pub text: &'a [u8],
    pub row: CanvasY,
    pub column: usize,
}

#[derive(Debug, PartialEq)]
pub struct Rect {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

#[repr(C)]
pub enum Layer {
    // function background
    BehindTextBehindCursor,
    // cursor
    BehindTextCursor,
    // highlighting words, matrix editor bg
    BehindTextAboveCursor,
    Text,
    AboveText,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextStyle {
    Normal,
    Bold,
    Underline,
    Italy,
}

#[repr(C)]
#[derive(Debug, Clone, EnumDiscriminants, PartialEq)]
#[strum_discriminants(name(OutputMessageCommandId))]
pub enum OutputMessage<'a> {
    SetStyle(TextStyle),
    SetColor(u32),
    RenderChar(RenderChar),
    RenderUtf8Text(RenderUtf8TextMsg<'a>),
    RenderAsciiText(RenderAsciiTextMsg<'a>),
    RenderString(RenderStringMsg),
    RenderRectangle {
        x: usize,
        y: CanvasY,
        w: usize,
        h: usize,
    },
    FollowingTextCommandsAreHeaders(bool),
    RenderUnderline {
        x: usize,
        y: CanvasY,
        w: usize,
    },
    UpdatePulses,
}

#[derive(Debug)]
pub struct RenderBuckets<'a> {
    pub left_gutter_bg: Rect,
    pub right_gutter_bg: Rect,
    pub result_panel_bg: Rect,
    pub scroll_bar: Option<(u32, Rect)>,
    pub ascii_texts: Vec<RenderAsciiTextMsg<'a>>,
    pub utf8_texts: Vec<RenderUtf8TextMsg<'a>>,
    pub headers: Vec<RenderUtf8TextMsg<'a>>,
    pub numbers: Vec<RenderUtf8TextMsg<'a>>,
    pub number_errors: Vec<RenderUtf8TextMsg<'a>>,
    pub units: Vec<RenderUtf8TextMsg<'a>>,
    pub operators: Vec<RenderUtf8TextMsg<'a>>,
    pub parenthesis: Vec<RenderChar>,
    pub variable: Vec<RenderUtf8TextMsg<'a>>,
    pub line_ref_results: Vec<RenderStringMsg>,
    pub custom_commands: [Vec<OutputMessage<'a>>; 5],
    pub pulses: Vec<PulsingRectangle>,
    pub clear_pulses: bool,
}

impl<'a> RenderBuckets<'a> {
    pub fn new() -> RenderBuckets<'a> {
        RenderBuckets {
            left_gutter_bg: Rect {
                x: 0,
                y: 0,
                w: 0,
                h: 0,
            },
            right_gutter_bg: Rect {
                x: 0,
                y: 0,
                w: 0,
                h: 0,
            },
            result_panel_bg: Rect {
                x: 0,
                y: 0,
                w: 0,
                h: 0,
            },
            scroll_bar: None,
            ascii_texts: Vec::with_capacity(128),
            utf8_texts: Vec::with_capacity(128),
            headers: Vec::with_capacity(16),
            custom_commands: [
                Vec::with_capacity(128),
                Vec::with_capacity(128),
                Vec::with_capacity(128),
                Vec::with_capacity(128),
                Vec::with_capacity(128),
            ],
            numbers: Vec::with_capacity(32),
            number_errors: Vec::with_capacity(32),
            units: Vec::with_capacity(32),
            operators: Vec::with_capacity(32),
            parenthesis: Vec::with_capacity(32),
            variable: Vec::with_capacity(32),
            line_ref_results: Vec::with_capacity(32),
            pulses: Vec::with_capacity(8),
            clear_pulses: false,
        }
    }

    pub fn custom_commands<'b>(&'b self, layer: Layer) -> &'b Vec<OutputMessage<'a>> {
        &self.custom_commands[layer as usize]
    }

    pub fn clear(&mut self) {
        self.ascii_texts.clear();
        self.utf8_texts.clear();
        self.headers.clear();
        for bucket in self.custom_commands.iter_mut() {
            bucket.clear();
        }
        self.numbers.clear();
        self.number_errors.clear();
        self.units.clear();
        self.operators.clear();
        self.variable.clear();
        self.line_ref_results.clear();
        self.pulses.clear();
        self.parenthesis.clear();
        self.clear_pulses = false;
    }

    pub fn set_color(&mut self, layer: Layer, color: u32) {
        self.custom_commands[layer as usize].push(OutputMessage::SetColor(color));
    }

    pub fn draw_rect(&mut self, layer: Layer, x: usize, y: CanvasY, w: usize, h: usize) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderRectangle { x, y, w, h });
    }

    pub fn draw_char(&mut self, layer: Layer, col: usize, row: CanvasY, char: char) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderChar(RenderChar {
            col,
            row,
            char,
        }));
    }

    pub fn draw_text(&mut self, layer: Layer, x: usize, y: CanvasY, text: &'a [char]) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderUtf8Text(
            RenderUtf8TextMsg {
                text,
                row: y,
                column: x,
            },
        ));
    }

    pub fn draw_ascii_text(&mut self, layer: Layer, x: usize, y: CanvasY, text: &'a [u8]) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderAsciiText(
            RenderAsciiTextMsg {
                text,
                row: y,
                column: x,
            },
        ));
    }

    pub fn draw_underline(&mut self, layer: Layer, x: usize, y: CanvasY, w: usize) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderUnderline { x, y, w });
    }

    pub fn draw_string(&mut self, layer: Layer, x: usize, y: CanvasY, text: String) {
        self.custom_commands[layer as usize].push(OutputMessage::RenderString(RenderStringMsg {
            text: text.clone(),
            row: y,
            column: x,
        }));
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum ResultFormat {
    Bin,
    Dec,
    Hex,
}

#[derive(Clone, Debug)]
pub struct LineData {
    pub line_id: usize,
    result_format: ResultFormat,
}

impl Default for LineData {
    fn default() -> Self {
        LineData {
            line_id: 0,
            result_format: ResultFormat::Dec,
        }
    }
}

#[derive(Debug)]
pub struct MatrixEditing {
    pub editor_content: EditorContent<LineData>,
    pub editor: Editor<LineData>,
    row_count: usize,
    col_count: usize,
    current_cell: Pos,
    start_text_index: usize,
    end_text_index: usize,
    row_index: ContentIndex,
    cell_strings: Vec<String>,
}

impl MatrixEditing {
    pub fn new<'a>(
        row_count: usize,
        col_count: usize,
        src_canvas: &[char],
        row_index: ContentIndex,
        start_text_index: usize,
        end_text_index: usize,
        step_in_pos: Pos,
    ) -> MatrixEditing {
        let current_cell = if step_in_pos.row == row_index.as_usize() {
            if step_in_pos.column > start_text_index {
                // from right
                Pos::from_row_column(0, col_count - 1)
            } else {
                // from left
                Pos::from_row_column(0, 0)
            }
        } else if step_in_pos.row < row_index.as_usize() {
            // from above
            Pos::from_row_column(0, 0)
        } else {
            // from below
            Pos::from_row_column(row_count - 1, 0)
        };

        let mut editor_content = EditorContent::new(32, 1);
        let mut mat_edit = MatrixEditing {
            row_index,
            start_text_index,
            end_text_index,
            editor: Editor::new(&mut editor_content),
            editor_content,
            row_count,
            col_count,
            current_cell,
            cell_strings: Vec::with_capacity((row_count * col_count).max(4)),
        };
        let mut str: String = String::with_capacity(8);
        let mut can_ignore_ws = true;
        for ch in src_canvas {
            match ch {
                '[' => {
                    //ignore
                }
                ']' => {
                    break;
                }
                ',' => {
                    mat_edit.cell_strings.push(str);
                    str = String::with_capacity(8);
                    can_ignore_ws = true;
                }
                ';' => {
                    mat_edit.cell_strings.push(str);
                    str = String::with_capacity(8);
                    can_ignore_ws = true;
                }
                _ if ch.is_ascii_whitespace() && can_ignore_ws => {
                    // ignore
                }
                _ => {
                    can_ignore_ws = false;
                    str.push(*ch);
                }
            }
        }
        mat_edit.cell_strings.push(str);

        let cell_index = mat_edit.current_cell.row * col_count + mat_edit.current_cell.column;
        mat_edit
            .editor_content
            .init_with(&mat_edit.cell_strings[cell_index]);
        // select all
        mat_edit.editor.set_cursor_range(
            Pos::from_row_column(0, 0),
            Pos::from_row_column(0, mat_edit.editor_content.line_len(0)),
        );

        mat_edit
    }

    fn add_column(&mut self) {
        if self.col_count == 6 {
            return;
        }
        self.cell_strings
            .reserve(self.row_count * (self.col_count + 1));
        for row_i in (0..self.row_count).rev() {
            let index = row_i * self.col_count + self.col_count;
            // TODO alloc :(, but at least not in the hot path
            self.cell_strings.insert(index, "0".to_owned());
        }
        self.col_count += 1;
    }

    fn add_row(&mut self) {
        if self.row_count == 6 {
            return;
        }
        self.cell_strings
            .reserve((self.row_count + 1) * self.col_count);
        self.row_count += 1;
        for _ in 0..self.col_count {
            // TODO alloc :(, but at least not in the hot path
            self.cell_strings.push("0".to_owned());
        }
    }

    fn remove_column(&mut self) {
        self.col_count -= 1;
        if self.current_cell.column >= self.col_count {
            self.move_to_cell(self.current_cell.with_column(self.col_count - 1));
        }
        for row_i in (0..self.row_count).rev() {
            let index = row_i * (self.col_count + 1) + self.col_count;
            self.cell_strings.remove(index);
        }
    }

    fn remove_row(&mut self) {
        self.row_count -= 1;
        if self.current_cell.row >= self.row_count {
            self.move_to_cell(self.current_cell.with_row(self.row_count - 1));
        }
        for _ in 0..self.col_count {
            self.cell_strings.pop();
        }
    }

    fn move_to_cell(&mut self, new_pos: Pos) {
        self.save_editor_content();

        let new_content = &self.cell_strings[new_pos.row * self.col_count + new_pos.column];
        self.editor_content.init_with(new_content);

        self.current_cell = new_pos;
        // select all
        self.editor.set_cursor_range(
            Pos::from_row_column(0, 0),
            Pos::from_row_column(0, self.editor_content.line_len(0)),
        );
    }

    fn save_editor_content(&mut self) {
        let mut backend = &mut self.cell_strings
            [self.current_cell.row * self.col_count + self.current_cell.column];
        backend.clear();
        self.editor_content.write_content_into(&mut backend);
    }

    fn render<'b>(
        &self,
        mut render_x: usize,
        render_y: CanvasY,
        current_editor_width: usize,
        left_gutter_width: usize,
        render_buckets: &mut RenderBuckets<'b>,
        rendered_row_height: usize,
        theme: &Theme,
    ) -> usize {
        let vert_align_offset =
            (rendered_row_height - MatrixData::calc_render_height(self.row_count)) / 2;

        render_matrix_left_brackets(
            render_x + left_gutter_width,
            render_y,
            self.row_count,
            render_buckets,
            vert_align_offset,
            false,
        );
        render_x += 1;

        for col_i in 0..self.col_count {
            if render_x >= current_editor_width {
                return render_x;
            }
            let max_width: usize = (0..self.row_count)
                .map(|row_i| {
                    if self.current_cell == Pos::from_row_column(row_i, col_i) {
                        self.editor_content.line_len(0)
                    } else {
                        self.cell_strings[row_i * self.col_count + col_i].len()
                    }
                })
                .max()
                .unwrap();
            for row_i in 0..self.row_count {
                // the content of the matrix starts from the second row
                let matrix_ascii_header_offset = if self.row_count == 1 { 0 } else { 1 };
                let dst_y = render_y.add(row_i + vert_align_offset + matrix_ascii_header_offset);
                let len: usize = if self.current_cell == Pos::from_row_column(row_i, col_i) {
                    self.editor_content.line_len(0)
                } else {
                    self.cell_strings[row_i * self.col_count + col_i].len()
                };
                let padding_x = max_width - len;
                let text_len = len.min(
                    (current_editor_width as isize - (render_x + padding_x) as isize).max(0)
                        as usize,
                );

                if self.current_cell == Pos::from_row_column(row_i, col_i) {
                    render_buckets
                        .set_color(Layer::BehindTextAboveCursor, theme.matrix_edit_active_bg);
                    render_buckets.draw_rect(
                        Layer::BehindTextAboveCursor,
                        render_x + padding_x + left_gutter_width,
                        dst_y,
                        text_len,
                        1,
                    );
                    let chars = &self.editor_content.lines().next().unwrap();
                    render_buckets.set_color(Layer::Text, theme.matrix_edit_active_text);
                    for (i, char) in chars.iter().enumerate() {
                        render_buckets.draw_char(
                            Layer::Text,
                            render_x + padding_x + left_gutter_width + i,
                            dst_y,
                            *char,
                        );
                    }
                    let sel = self.editor.get_selection();
                    if let Some((first, second)) = sel.is_range_ordered() {
                        let len = second.column - first.column;
                        render_buckets
                            .set_color(Layer::BehindTextAboveCursor, theme.selection_color);
                        render_buckets.draw_rect(
                            Layer::BehindTextAboveCursor,
                            render_x + padding_x + left_gutter_width + first.column,
                            dst_y,
                            len,
                            1,
                        );
                    }
                } else {
                    let chars = &self.cell_strings[row_i * self.col_count + col_i];
                    render_buckets.set_color(Layer::Text, theme.matrix_edit_inactive_text);
                    render_buckets.draw_string(
                        Layer::Text,
                        render_x + padding_x + left_gutter_width,
                        dst_y,
                        (&chars[0..text_len]).to_owned(),
                    );
                }

                if self.current_cell == Pos::from_row_column(row_i, col_i)
                    && self.editor.is_cursor_shown()
                {
                    render_buckets.set_color(Layer::Text, theme.cursor);
                    render_buckets.draw_char(
                        Layer::Text,
                        (self.editor.get_selection().get_cursor_pos().column + left_gutter_width)
                            + render_x
                            + padding_x,
                        dst_y,
                        '▏',
                    );
                }
            }
            render_x += if col_i + 1 < self.col_count {
                max_width + MATRIX_ASCII_HEADER_FOOTER_LINE_COUNT
            } else {
                max_width
            };
        }

        render_matrix_right_brackets(
            render_x + left_gutter_width,
            render_y,
            self.row_count,
            render_buckets,
            vert_align_offset,
            false,
        );

        render_x += 1;
        render_x
    }
}

fn draw_line_refs_and_vars_referenced_from_cur_row<'b>(
    editor_objs: &[EditorObject],
    gr: &GlobalRenderData,
    render_buckets: &mut RenderBuckets<'b>,
) {
    let mut color_index = 0;
    let mut highlighted = BitFlag256::empty();
    for editor_obj in editor_objs {
        match editor_obj.typ {
            EditorObjectType::LineReference { var_index }
            | EditorObjectType::Variable { var_index } => {
                if var_index >= SUM_VARIABLE_INDEX {
                    continue;
                }
                let color = if highlighted.is_true(var_index) {
                    continue;
                } else {
                    highlighted.set(var_index);
                    let color = ACTIVE_LINE_REF_HIGHLIGHT_COLORS[color_index];
                    color_index = if color_index < 8 { color_index + 1 } else { 0 };
                    color
                };
                let defined_at = content_y(var_index);
                if let Some(render_y) = gr.get_render_y(defined_at) {
                    // render a rectangle on the *left gutter*
                    render_buckets.custom_commands[Layer::BehindTextAboveCursor as usize]
                        .push(OutputMessage::SetColor(color));
                    render_buckets.custom_commands[Layer::BehindTextAboveCursor as usize].push(
                        OutputMessage::RenderRectangle {
                            x: 0,
                            y: render_y,
                            w: gr.left_gutter_width,
                            h: gr.get_rendered_height(defined_at),
                        },
                    );
                    // render a rectangle on the *right gutter*
                    render_buckets.custom_commands[Layer::BehindTextAboveCursor as usize].push(
                        OutputMessage::RenderRectangle {
                            x: gr.result_gutter_x,
                            y: render_y,
                            w: RIGHT_GUTTER_WIDTH,
                            h: gr.get_rendered_height(defined_at),
                        },
                    );
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum EditorObjectType {
    Matrix { row_count: usize, col_count: usize },
    LineReference { var_index: usize },
    Variable { var_index: usize },
    SimpleTokens,
}

#[derive(Clone, Debug)]
pub struct EditorObject {
    pub typ: EditorObjectType,
    pub row: ContentIndex,
    pub start_x: usize,
    pub end_x: usize,
    pub rendered_x: usize,
    pub rendered_y: CanvasY,
    pub rendered_w: usize,
    pub rendered_h: usize,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Box<[char]>,
    pub value: Result<CalcResult, ()>,
}

pub type LineResult = Result<Option<CalcResult>, EvalErr>;
pub type Variables = [Option<Variable>];
pub type FunctionDefinitions<'a> = [Option<FunctionDef<'a>>];

#[derive(Debug)]
pub struct Tokens<'a> {
    pub tokens: Vec<Token<'a>>,
    pub shunting_output_stack: Vec<ShuntingYardResult>,
}

pub enum MouseClickType {
    ClickedInEditor,
    ClickedInScrollBar {
        original_click_y: CanvasY,
        original_scroll_y: usize,
    },
    RightGutterIsDragged,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MouseHoverType {
    Normal,
    Scrollbar,
    RightGutter,
    Result,
}

#[derive(Debug)]
pub struct EditorObjId {
    content_index: ContentIndex,
    var_index: usize,
}

#[derive(Debug, Default)]
pub struct ResultLengths {
    int_part_len: usize,
    frac_part_len: usize,
    unit_part_len: usize,
}

impl ResultLengths {
    fn set_max(&mut self, other: &ResultLengths) {
        if self.int_part_len < other.int_part_len {
            self.int_part_len = other.int_part_len;
        }
        if self.frac_part_len < other.frac_part_len {
            self.frac_part_len = other.frac_part_len;
        }
        if self.unit_part_len < other.unit_part_len {
            self.unit_part_len = other.unit_part_len;
        }
    }
}

#[derive(Default)]
struct ResultTmp {
    buffer_ptr: Option<Range<usize>>,
    editor_y: ContentIndex,
    lengths: ResultLengths,
}

struct ResultRender {
    result_ranges: ArrayVec<[ResultTmp; MAX_CLIENT_HEIGHT]>,
    max_len: usize,
    max_lengths: [ResultLengths; MAX_VISIBLE_HEADER_COUNT],
    result_counts_in_regions: [usize; MAX_VISIBLE_HEADER_COUNT],
}

impl ResultRender {
    pub fn new(vec: ArrayVec<[ResultTmp; MAX_CLIENT_HEIGHT]>) -> ResultRender {
        return ResultRender {
            result_ranges: vec,
            max_len: 0,
            max_lengths: [ResultLengths {
                int_part_len: 0,
                frac_part_len: 0,
                unit_part_len: 0,
            }; 16],
            result_counts_in_regions: [0; MAX_VISIBLE_HEADER_COUNT],
        };
    }
}

fn draw_cursor(
    render_buckets: &mut RenderBuckets,
    r: &PerLineRenderData,
    gr: &GlobalRenderData,
    editor: &Editor<LineData>,
    matrix_editing: &Option<MatrixEditing>,
    theme: &Theme,
) {
    let cursor_pos = editor.get_selection().get_cursor_pos();
    if cursor_pos.row == r.editor_y.as_usize() {
        render_buckets.set_color(Layer::AboveText, theme.cursor);
        if editor.is_cursor_shown()
            && matrix_editing.is_none()
            && ((cursor_pos.column as isize + r.cursor_render_x_offset) as usize)
                <= gr.current_editor_width
        {
            render_buckets.draw_char(
                Layer::AboveText,
                ((cursor_pos.column + gr.left_gutter_width) as isize + r.cursor_render_x_offset)
                    as usize,
                r.render_y.add(r.vert_align_offset),
                '▏',
            );
        }
    }
}

pub fn pulse_modified_line_references(
    render_buckets: &mut RenderBuckets,
    gr: &GlobalRenderData,
    updated_line_ref_obj_indices: &[EditorObjId],
    editor_objects: &EditorObjects,
    theme: &Theme,
) {
    // Pulsing changed line references
    for id in updated_line_ref_obj_indices {
        for ed_obj in &editor_objects[id.content_index] {
            match ed_obj {
                EditorObject {
                    typ: EditorObjectType::LineReference { var_index },
                    rendered_x,
                    rendered_y,
                    rendered_w,
                    rendered_h,
                    ..
                } if *var_index == id.var_index => {
                    render_buckets.pulses.push(PulsingRectangle {
                        x: gr.left_gutter_width + *rendered_x,
                        y: *rendered_y,
                        w: *rendered_w,
                        h: *rendered_h,
                        start_color: theme.change_result_pulse_start,
                        end_color: theme.change_result_pulse_end,
                        animation_time: Duration::from_millis(2000),
                        repeat: false,
                    });
                }
                _ => {}
            }
        }
    }
}

fn render_results_into_buf_and_calc_len<'text_ptr>(
    units: &Units,
    results: &[LineResult],
    tmp: &mut ResultRender,
    editor_content: &EditorContent<LineData>,
    gr: &GlobalRenderData,
    decimal_count: Option<usize>,
) {
    let mut result_buffer_index = 0;
    let result_buffer = unsafe { &mut RESULT_BUFFER };
    // calc max length and render results into buffer
    let mut region_index = 0;
    let mut region_count_offset = 0;
    for (editor_y, result) in results.iter().enumerate() {
        let editor_y = content_y(editor_y);
        let render_y = if let Some(render_y) = gr.get_render_y(editor_y) {
            render_y
        } else {
            continue;
        };
        if !gr.is_visible(editor_y) {
            continue;
        } else if editor_y.as_usize() >= editor_content.line_count() {
            break;
        }
        if render_y.as_usize() != 0 && editor_content.get_char(editor_y.as_usize(), 0) == '#' {
            let max_lens = &tmp.max_lengths[region_index];
            tmp.max_len = (max_lens.int_part_len
                + max_lens.frac_part_len
                + if max_lens.unit_part_len > 0 {
                    max_lens.unit_part_len + 1
                } else {
                    0
                })
            .max(tmp.max_len);
            tmp.result_counts_in_regions[region_index] =
                tmp.result_ranges.len() - region_count_offset;
            region_count_offset = tmp.result_ranges.len();
            if region_index < MAX_VISIBLE_HEADER_COUNT - 1 {
                region_index += 1;
            }
            continue;
        }

        if let Err(..) = result {
            result_buffer[result_buffer_index] = b'E';
            result_buffer[result_buffer_index + 1] = b'r';
            result_buffer[result_buffer_index + 2] = b'r';
            tmp.result_ranges.push(ResultTmp {
                buffer_ptr: Some(result_buffer_index..result_buffer_index + 3),
                editor_y,
                lengths: ResultLengths {
                    int_part_len: 999,
                    frac_part_len: 0,
                    unit_part_len: 0,
                },
            });
            result_buffer_index += 3;
        } else if let Ok(Some(result)) = result {
            match &result.typ {
                CalcResultType::Matrix(_mat) => {
                    tmp.result_ranges.push(ResultTmp {
                        buffer_ptr: None,
                        editor_y,
                        lengths: ResultLengths {
                            int_part_len: 0,
                            frac_part_len: 0,
                            unit_part_len: 0,
                        },
                    });
                }
                _ => {
                    let start = result_buffer_index;
                    let mut c = Cursor::new(&mut result_buffer[start..]);
                    let lens = render_result_into(
                        &units,
                        &result,
                        &editor_content.get_data(editor_y.as_usize()).result_format,
                        false,
                        &mut c,
                        decimal_count,
                        true,
                    );
                    let len = c.position() as usize;
                    let range = start..start + len;
                    tmp.max_lengths[region_index].set_max(&lens);
                    tmp.result_ranges.push(ResultTmp {
                        buffer_ptr: Some(range),
                        editor_y,
                        lengths: lens,
                    });
                    result_buffer_index += len;
                }
            };
        } else {
            tmp.result_ranges.push(ResultTmp {
                buffer_ptr: None,
                editor_y,
                lengths: ResultLengths {
                    int_part_len: 0,
                    frac_part_len: 0,
                    unit_part_len: 0,
                },
            });
        }
    }
    result_buffer[result_buffer_index] = 0; // tests depend on it

    tmp.result_counts_in_regions[region_index] = tmp.result_ranges.len() - region_count_offset;
    tmp.max_len = (tmp.max_lengths[region_index].int_part_len
        + tmp.max_lengths[region_index].frac_part_len
        + if tmp.max_lengths[region_index].unit_part_len > 0 {
            tmp.max_lengths[region_index].unit_part_len + 1
        } else {
            0
        })
    .max(tmp.max_len);
}

fn create_render_commands_for_results_and_render_matrices<'text_ptr>(
    tmp: &ResultRender,
    units: &Units,
    results: &[LineResult],
    render_buckets: &mut RenderBuckets<'text_ptr>,
    gr: &GlobalRenderData,
    decimal_count: Option<usize>,
    theme: &Theme,
) -> usize {
    let mut result_matrix_length = None;
    let mut matrix_len = 0;
    let result_buffer = unsafe { &RESULT_BUFFER };
    let mut region_index = 0;
    let mut result_count_in_this_region = tmp.result_counts_in_regions[0];
    let mut max_lens = &tmp.max_lengths[0];

    for result_tmp in tmp.result_ranges.iter() {
        while result_count_in_this_region == 0 {
            region_index += 1;
            result_count_in_this_region = tmp.result_counts_in_regions[region_index];
            max_lens = &tmp.max_lengths[region_index];
        }
        let rendered_row_height = gr.get_rendered_height(result_tmp.editor_y);
        let render_y = gr.get_render_y(result_tmp.editor_y).expect("");

        if let Some(result_range) = &result_tmp.buffer_ptr {
            let lengths = &result_tmp.lengths;
            let from = result_range.start;
            let vert_align_offset = (rendered_row_height - 1) / 2;
            let row = render_y.add(vert_align_offset);
            enum ResultOffsetX {
                Err,
                Ok(usize),
                TooLong,
            }
            let offset_x = if max_lens.int_part_len < lengths.int_part_len {
                ResultOffsetX::Err
            } else {
                let offset_x = max_lens.int_part_len - lengths.int_part_len;
                let sum_len =
                    lengths.int_part_len + max_lens.frac_part_len + max_lens.unit_part_len;
                if offset_x + sum_len > gr.current_result_panel_width {
                    if sum_len > gr.current_result_panel_width {
                        ResultOffsetX::TooLong
                    } else {
                        ResultOffsetX::Ok(gr.current_result_panel_width - sum_len)
                    }
                } else {
                    ResultOffsetX::Ok(offset_x)
                }
            };
            let x = gr.result_gutter_x
                + RIGHT_GUTTER_WIDTH
                + match offset_x {
                    ResultOffsetX::Err => 0,
                    ResultOffsetX::TooLong => 0,
                    ResultOffsetX::Ok(n) => n,
                };
            let int_w = match offset_x {
                ResultOffsetX::Err => 3,
                _ => lengths.int_part_len,
            };
            render_buckets.ascii_texts.push(RenderAsciiTextMsg {
                text: &result_buffer[from..from + int_w],
                row,
                column: x,
            });
            if lengths.frac_part_len > 0 {
                let from = result_range.start + lengths.int_part_len;
                render_buckets.ascii_texts.push(RenderAsciiTextMsg {
                    text: &result_buffer[from..from + lengths.frac_part_len],
                    row,
                    column: x + lengths.int_part_len,
                });
            }
            if lengths.unit_part_len > 0 {
                let from = result_range.start + lengths.int_part_len + lengths.frac_part_len + 1;
                // e.g. in case of 2 units mm and m, m should be 1 coordinates right
                let offset_x = max_lens.unit_part_len - lengths.unit_part_len;
                render_buckets.ascii_texts.push(RenderAsciiTextMsg {
                    text: &result_buffer[from..result_range.end],
                    row,
                    column: gr.result_gutter_x
                        + RIGHT_GUTTER_WIDTH
                        + max_lens.int_part_len
                        + max_lens.frac_part_len
                        + 1
                        + offset_x,
                });
            }
            match offset_x {
                ResultOffsetX::TooLong => {
                    render_buckets.set_color(Layer::AboveText, theme.result_bg_color);
                    render_buckets.draw_char(
                        Layer::AboveText,
                        gr.result_gutter_x + RIGHT_GUTTER_WIDTH + gr.current_result_panel_width - 1,
                        row,
                        '█',
                    );
                    render_buckets.set_color(Layer::AboveText, theme.cursor);
                    render_buckets.draw_char(
                        Layer::AboveText,
                        gr.result_gutter_x + RIGHT_GUTTER_WIDTH + gr.current_result_panel_width - 1,
                        row,
                        '…',
                    );
                }
                _ => {}
            }
            result_matrix_length = None;
        } else {
            match &results[result_tmp.editor_y.as_usize()] {
                Ok(Some(CalcResult {
                    typ: CalcResultType::Matrix(mat),
                    ..
                })) => {
                    if result_matrix_length.is_none() {
                        result_matrix_length = calc_consecutive_matrices_max_lengths(
                            units,
                            &results[result_tmp.editor_y.as_usize()..],
                        );
                    }
                    let width = render_matrix_result(
                        units,
                        gr.result_gutter_x + RIGHT_GUTTER_WIDTH,
                        render_y,
                        gr.client_width,
                        mat,
                        render_buckets,
                        result_matrix_length.as_ref(),
                        gr.get_rendered_height(result_tmp.editor_y),
                        decimal_count,
                        theme.result_text,
                    );
                    if width > matrix_len {
                        matrix_len = width;
                    }
                }
                _ => {
                    result_matrix_length = None;
                }
            }
        }
        result_count_in_this_region -= 1;
        if result_count_in_this_region == 0 {
            region_index += 1;
            result_count_in_this_region = tmp.result_counts_in_regions[region_index];
            max_lens = &tmp.max_lengths[region_index];
        }
    }
    return matrix_len;
}

fn calc_consecutive_matrices_max_lengths(
    units: &Units,
    results: &[LineResult],
) -> Option<ResultLengths> {
    let mut max_lengths: Option<ResultLengths> = None;
    for result in results.iter() {
        match result {
            Ok(Some(CalcResult {
                typ: CalcResultType::Matrix(mat),
                ..
            })) => {
                let lengths = calc_matrix_max_lengths(units, mat);
                if let Some(max_lengths) = &mut max_lengths {
                    max_lengths.set_max(&lengths);
                } else {
                    max_lengths = Some(lengths);
                }
            }
            _ => {
                break;
            }
        }
    }
    return max_lengths;
}

fn calc_matrix_max_lengths(units: &Units, mat: &MatrixData) -> ResultLengths {
    let cells_strs = {
        let mut tokens_per_cell: ArrayVec<[String; 32]> = ArrayVec::new();

        for cell in mat.cells.iter() {
            let result_str = render_result(
                units,
                cell,
                &ResultFormat::Dec,
                false,
                Some(RENDERED_RESULT_PRECISION),
                true,
            );
            tokens_per_cell.push(result_str);
        }
        tokens_per_cell
    };
    let max_lengths = {
        let mut max_lengths = ResultLengths {
            int_part_len: 0,
            frac_part_len: 0,
            unit_part_len: 0,
        };
        for cell_str in &cells_strs {
            let lengths = get_int_frac_part_len(cell_str);
            max_lengths.set_max(&lengths);
        }
        max_lengths
    };
    return max_lengths;
}

fn draw_token<'text_ptr>(
    token: &Token<'text_ptr>,
    render_x: usize,
    render_y: CanvasY,
    current_editor_width: usize,
    left_gutter_width: usize,
    render_buckets: &mut RenderBuckets<'text_ptr>,
    is_bold: bool,
    theme: &Theme,
) {
    let dst = if token.has_error() {
        &mut render_buckets.number_errors
    } else {
        match &token.typ {
            TokenType::StringLiteral => &mut render_buckets.utf8_texts,
            TokenType::Header => &mut render_buckets.headers,
            TokenType::Variable { .. } => &mut render_buckets.variable,
            TokenType::LineReference { .. } => &mut render_buckets.variable,
            TokenType::NumberLiteral(_) => &mut render_buckets.numbers,
            TokenType::NumberErr => &mut render_buckets.number_errors,
            TokenType::Unit(_, _) => &mut render_buckets.units,
            TokenType::Operator(OperatorTokenType::ParenClose) => {
                if current_editor_width <= render_x {
                    return;
                }
                if is_bold {
                    render_buckets.set_color(Layer::Text, theme.line_ref_bg);
                    render_buckets.draw_rect(
                        Layer::Text,
                        render_x + left_gutter_width,
                        render_y,
                        1,
                        1,
                    );

                    render_buckets.set_color(Layer::Text, theme.parenthesis);

                    let b = &mut render_buckets.custom_commands[Layer::Text as usize];
                    b.push(OutputMessage::FollowingTextCommandsAreHeaders(true));
                    b.push(OutputMessage::RenderChar(RenderChar {
                        col: render_x + left_gutter_width,
                        row: render_y,
                        char: ')',
                    }));
                    b.push(OutputMessage::FollowingTextCommandsAreHeaders(false));
                } else {
                    render_buckets.parenthesis.push(RenderChar {
                        col: render_x + left_gutter_width,
                        row: render_y,
                        char: ')',
                    });
                }
                return;
            }
            TokenType::Operator(OperatorTokenType::ParenOpen) => {
                if current_editor_width <= render_x {
                    return;
                }
                if is_bold {
                    render_buckets.set_color(Layer::Text, theme.line_ref_bg);
                    render_buckets.draw_rect(
                        Layer::Text,
                        render_x + left_gutter_width,
                        render_y,
                        1,
                        1,
                    );
                    render_buckets.set_color(Layer::Text, theme.parenthesis);

                    render_buckets.custom_commands[Layer::Text as usize]
                        .push(OutputMessage::FollowingTextCommandsAreHeaders(true));
                    &mut render_buckets.custom_commands[Layer::Text as usize].push(
                        OutputMessage::RenderChar(RenderChar {
                            col: render_x + left_gutter_width,
                            row: render_y,
                            char: '(',
                        }),
                    );
                    render_buckets.custom_commands[Layer::Text as usize]
                        .push(OutputMessage::FollowingTextCommandsAreHeaders(false));
                } else {
                    &mut render_buckets.parenthesis.push(RenderChar {
                        col: render_x + left_gutter_width,
                        row: render_y,
                        char: '(',
                    });
                }
                return;
            }
            TokenType::Operator(_) => &mut render_buckets.operators,
        }
    };
    let text_len = token
        .ptr
        .len()
        .min((current_editor_width as isize - render_x as isize).max(0) as usize);
    dst.push(RenderUtf8TextMsg {
        text: &token.ptr[0..text_len],
        row: render_y,
        column: render_x + left_gutter_width,
    });
}

fn render_buckets_into(buckets: &RenderBuckets, canvas: &mut [[char; 256]]) {
    fn write_char_slice(canvas: &mut [[char; 256]], row: CanvasY, col: usize, src: &[char]) {
        let str = &mut canvas[row.as_usize()];
        for (dst_char, src_char) in str[col..].iter_mut().zip(src.iter()) {
            *dst_char = *src_char;
        }
    }

    fn write_str(canvas: &mut [[char; 256]], row: CanvasY, col: usize, src: &str) {
        let str = &mut canvas[row.as_usize()];
        for (dst_char, src_char) in str[col..].iter_mut().zip(src.chars()) {
            *dst_char = src_char;
        }
    }

    fn write_char(canvas: &mut [[char; 256]], row: CanvasY, col: usize, char: char) {
        let str = &mut canvas[row.as_usize()];
        str[col] = char;
    }

    fn write_ascii(canvas: &mut [[char; 256]], row: CanvasY, col: usize, src: &[u8]) {
        let str = &mut canvas[row.as_usize()];
        for (dst_char, src_char) in str[col..].iter_mut().zip(src.iter()) {
            *dst_char = *src_char as char;
        }
    }

    fn write_command(canvas: &mut [[char; 256]], command: &OutputMessage) {
        match command {
            OutputMessage::RenderUtf8Text(text) => {
                write_char_slice(canvas, text.row, text.column, text.text);
            }
            OutputMessage::SetStyle(..) => {}
            OutputMessage::SetColor(..) => {}
            OutputMessage::RenderRectangle { .. } => {}
            OutputMessage::RenderChar(r) => {
                write_char(canvas, r.row, r.col, r.char);
            }
            OutputMessage::RenderString(text) => {
                write_str(canvas, text.row, text.column, &text.text);
            }
            OutputMessage::RenderAsciiText(text) => {
                write_ascii(canvas, text.row, text.column, &text.text);
            }
            OutputMessage::FollowingTextCommandsAreHeaders { .. } => {}
            OutputMessage::RenderUnderline { .. } => {}
            OutputMessage::UpdatePulses => {}
        }
    }

    for command in &buckets.custom_commands[Layer::BehindTextBehindCursor as usize] {
        write_command(canvas, command);
    }

    for command in &buckets.custom_commands[Layer::BehindTextCursor as usize] {
        write_command(canvas, command);
    }
    for command in &buckets.custom_commands[Layer::BehindTextAboveCursor as usize] {
        write_command(canvas, command);
    }

    write_command(canvas, &OutputMessage::UpdatePulses);

    for command in &buckets.utf8_texts {
        write_char_slice(canvas, command.row, command.column, command.text);
    }
    for text in &buckets.ascii_texts {
        write_ascii(canvas, text.row, text.column, text.text);
    }
    for command in &buckets.numbers {
        write_char_slice(canvas, command.row, command.column, command.text);
    }

    for command in &buckets.number_errors {
        write_char_slice(canvas, command.row, command.column, command.text);
    }

    for command in &buckets.units {
        write_char_slice(canvas, command.row, command.column, command.text);
    }

    for command in &buckets.operators {
        write_char_slice(canvas, command.row, command.column, command.text);
    }
    for command in &buckets.parenthesis {
        write_char(canvas, command.row, command.col, command.char);
    }

    for command in &buckets.line_ref_results {
        write_str(canvas, command.row, command.column, &command.text);
    }

    for command in &buckets.headers {
        write_char_slice(canvas, command.row, command.column, command.text);
    }

    for command in &buckets.variable {
        write_char_slice(canvas, command.row, command.column, command.text);
    }

    for command in &buckets.custom_commands[Layer::Text as usize] {
        write_command(canvas, command);
    }

    for command in &buckets.custom_commands[Layer::AboveText as usize] {
        write_command(canvas, command);
    }
}

fn render_selection_and_its_sum<'text_ptr>(
    units: &Units,
    render_buckets: &mut RenderBuckets<'text_ptr>,
    results: &Results,
    editor: &Editor<LineData>,
    editor_content: &EditorContent<LineData>,
    gr: &GlobalRenderData,
    vars: &Variables,
    func_defs: &FunctionDefinitions<'text_ptr>,
    allocator: &'text_ptr Bump,
    theme: &Theme,
    apptokens: &AppTokens,
) {
    render_buckets.set_color(Layer::BehindTextAboveCursor, theme.selection_color);
    if let Some((start, end)) = editor.get_selection().is_range_ordered() {
        if end.row > start.row {
            // first line
            if let Some(start_render_y) = gr.get_render_y(content_y(start.row)) {
                let height = gr.get_rendered_height(content_y(start.row));
                let w = editor_content.line_len(start.row);
                if w > start.column {
                    render_buckets.draw_rect(
                        Layer::BehindTextAboveCursor,
                        start.column + gr.left_gutter_width,
                        start_render_y,
                        (w - start.column).min(gr.current_editor_width),
                        height,
                    );
                }
            }
            // full lines
            for i in start.row + 1..end.row {
                if let Some(render_y) = gr.get_render_y(content_y(i)) {
                    let height = gr.get_rendered_height(content_y(i));
                    render_buckets.draw_rect(
                        Layer::BehindTextAboveCursor,
                        gr.left_gutter_width,
                        render_y,
                        editor_content.line_len(i).min(gr.current_editor_width),
                        height,
                    );
                }
            }
            // last line
            if let Some(end_render_y) = gr.get_render_y(content_y(end.row)) {
                let height = gr.get_rendered_height(content_y(end.row));
                render_buckets.draw_rect(
                    Layer::BehindTextAboveCursor,
                    gr.left_gutter_width,
                    end_render_y,
                    end.column.min(gr.current_editor_width),
                    height,
                );
            }
        } else if let Some(start_render_y) = gr.get_render_y(content_y(start.row)) {
            let height = gr.get_rendered_height(content_y(start.row));
            render_buckets.draw_rect(
                Layer::BehindTextAboveCursor,
                start.column + gr.left_gutter_width,
                start_render_y,
                (end.column - start.column).min(gr.current_editor_width),
                height,
            );
        }
        // evaluated result of selection, selected text
        if let Some(mut partial_result) = evaluate_selection(
            &units,
            editor,
            editor_content,
            &vars,
            func_defs,
            results.as_slice(),
            allocator,
            apptokens,
        ) {
            if start.row == end.row {
                if let Some(start_render_y) = gr.get_render_y(content_y(start.row)) {
                    let selection_center = start.column + ((end.column - start.column) / 2);
                    partial_result.insert_str(0, "= ");
                    let result_w = partial_result.chars().count();
                    let centered_x =
                        (selection_center as isize - (result_w / 2) as isize).max(0) as usize;
                    render_buckets.set_color(Layer::AboveText, theme.sum_bg_color);
                    let rect_y = if start.row == 0 {
                        start_render_y.add(1)
                    } else {
                        start_render_y.sub(1)
                    };
                    render_buckets.draw_rect(
                        Layer::AboveText,
                        gr.left_gutter_width + centered_x,
                        rect_y,
                        result_w,
                        1,
                    );
                    render_buckets.set_color(Layer::AboveText, theme.sum_text_color);
                    render_buckets.draw_string(
                        Layer::AboveText,
                        gr.left_gutter_width + centered_x,
                        rect_y,
                        partial_result,
                    );
                }
            } else {
                partial_result.insert_str(0, " ∑ = ");
                let result_w = partial_result.chars().count();
                let x = (start.row..=end.row)
                    .map(|it| editor_content.line_len(it))
                    .max_by(|a, b| a.cmp(b))
                    .unwrap()
                    + 3;
                let frist_visible_row_index = content_y(start.row.max(gr.scroll_y));
                let last_visible_row_index =
                    content_y(end.row.min(gr.scroll_y + gr.client_height - 1));
                let inner_height = gr
                    .get_render_y(last_visible_row_index)
                    .expect("")
                    .as_usize()
                    - gr.get_render_y(frist_visible_row_index)
                        .expect("")
                        .as_usize();
                render_buckets.set_color(Layer::AboveText, theme.sum_bg_color);
                render_buckets.draw_rect(
                    Layer::AboveText,
                    gr.left_gutter_width + x,
                    gr.get_render_y(frist_visible_row_index).expect(""),
                    result_w + 1,
                    inner_height + 1,
                );
                // draw the parenthesis
                render_buckets.set_color(Layer::AboveText, theme.sum_text_color);

                render_buckets.draw_char(
                    Layer::AboveText,
                    gr.left_gutter_width + x,
                    gr.get_render_y(frist_visible_row_index).expect(""),
                    if frist_visible_row_index.as_usize() == start.row {
                        '⎫'
                    } else {
                        '⎪'
                    },
                );

                render_buckets.draw_char(
                    Layer::AboveText,
                    gr.left_gutter_width + x,
                    gr.get_render_y(last_visible_row_index).expect(""),
                    if last_visible_row_index.as_usize() == end.row {
                        '⎭'
                    } else {
                        '⎪'
                    },
                );

                for i in 1..inner_height {
                    render_buckets.draw_char(
                        Layer::AboveText,
                        gr.left_gutter_width + x,
                        gr.get_render_y(frist_visible_row_index).expect("").add(i),
                        '⎪',
                    );
                }
                // center
                render_buckets.draw_string(
                    Layer::AboveText,
                    gr.left_gutter_width + x,
                    gr.get_render_y(frist_visible_row_index)
                        .expect("")
                        .add(inner_height / 2),
                    partial_result,
                );
            }
        }
    }
}

fn calc_rendered_height<'b>(
    editor_y: ContentIndex,
    matrix_editing: &Option<MatrixEditing>,
    tokens: &AppTokens,
    results: &Results,
    vars: &Variables,
) -> usize {
    return if let Some(tokens) = &tokens[editor_y] {
        let h = PerLineRenderData::calc_rendered_row_height(
            &results[editor_y],
            &tokens.tokens,
            vars,
            matrix_editing
                .as_ref()
                .filter(|it| it.row_index == editor_y)
                .map(|it| MatrixData::calc_render_height(it.row_count)),
        );
        h
    } else {
        1
    };
}

pub fn end_matrix_editing(
    matrix_editing: &mut Option<MatrixEditing>,
    editor: &mut Editor<LineData>,
    editor_content: &mut EditorContent<LineData>,
    new_cursor_pos: Option<Pos>,
) {
    let mat_editor = {
        let mat_editor = matrix_editing.as_mut().unwrap();
        mat_editor.save_editor_content();
        mat_editor
    };
    let mut concat = String::with_capacity(32);
    concat.push('[');
    for row_i in 0..mat_editor.row_count {
        if row_i > 0 {
            concat.push(';');
        }
        for col_i in 0..mat_editor.col_count {
            if col_i > 0 {
                concat.push(',');
            }
            let cell_str = &mat_editor.cell_strings[row_i * mat_editor.col_count + col_i];
            concat += &cell_str;
        }
    }
    concat.push(']');

    if editor_content.line_len(mat_editor.row_index.as_usize()) + concat.len()
        - (mat_editor.end_text_index - mat_editor.start_text_index)
        < MAX_EDITOR_WIDTH
    {
        let selection = Selection::range(
            Pos::from_row_column(mat_editor.row_index.as_usize(), mat_editor.start_text_index),
            Pos::from_row_column(mat_editor.row_index.as_usize(), mat_editor.end_text_index),
        );
        editor.set_selection_save_col(selection);
        editor.handle_input_undoable(
            EditorInputEvent::Del,
            InputModifiers::none(),
            editor_content,
        );
        editor.insert_text_undoable(&concat, editor_content);
    }
    *matrix_editing = None;

    if let Some(new_cursor_pos) = new_cursor_pos {
        editor.set_selection_save_col(Selection::single(
            new_cursor_pos.with_column(
                new_cursor_pos
                    .column
                    .min(editor_content.line_len(new_cursor_pos.row) - 1),
            ),
        ));
    }
    editor.blink_cursor();
}

fn get_scroll_y_after_cursor_movement(
    prev_row: usize,
    current_row: usize,
    render_data: &GlobalRenderData,
) -> Option<usize> {
    if prev_row != current_row {
        if current_row < render_data.scroll_y {
            // scroll up
            Some(current_row)
        } else {
            // scroll down
            // if the new pos is 5. line and its height is 1, this var is 6
            let new_pos_bottom_y =
                if let Some(new_row_y) = render_data.get_render_y(content_y(current_row)) {
                    let new_h = render_data.get_rendered_height(content_y(current_row));
                    new_row_y.add(new_h)
                } else {
                    // find the last rendered line at the bottom
                    let mut assumed_heights = 1;
                    let mut prev_row_y = None;
                    let mut prev_row_i = current_row as isize - 1;
                    while prev_row_y.is_none() && prev_row_i >= 0 {
                        prev_row_y = render_data.get_render_y(content_y(prev_row_i as usize));
                        assumed_heights += 1;
                        prev_row_i -= 1;
                    }
                    // we assume that the non-yet-rendered lines' height will be 1
                    prev_row_y.unwrap_or(canvas_y(0)).add(assumed_heights)
                };
            let new_scroll_y = new_pos_bottom_y.as_isize() + render_data.scroll_y as isize
                - (render_data.client_height as isize);
            if new_scroll_y > render_data.scroll_y as isize {
                Some(new_scroll_y as usize)
            } else {
                None
            }
        }
    } else {
        None
    }
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

fn is_pos_inside_an_obj(editor_objects: &EditorObjects, pos: Pos) -> Option<&EditorObject> {
    for obj in &editor_objects[content_y(pos.row)] {
        if (obj.start_x + 1..obj.end_x).contains(&pos.column) {
            return Some(obj);
        }
    }
    return None;
}

fn sum_result(sum_var: &mut Variable, result: &CalcResult, sum_is_null: &mut bool) {
    if *sum_is_null {
        sum_var.value = Ok(result.clone());
        *sum_is_null = false;
    } else {
        sum_var.value = match &sum_var.value {
            Ok(current_sum) => {
                if let Some(ok) = add_op(current_sum, &result) {
                    Ok(ok)
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
    }
}
