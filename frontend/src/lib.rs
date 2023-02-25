#![deny(
warnings,
anonymous_parameters,
unused_extern_crates,
unused_import_braces,
trivial_casts,
variant_size_differences,
missing_debug_implementations,
trivial_numeric_casts,
unused_qualifications,
clippy::all
)]
#![feature(const_in_array_repeat_expressions)]

use wasm_bindgen::prelude::*;

use crate::utils::set_panic_hook;
use notecalc::borrow_checker_fighter::{to_box_ptr, BorrowCheckerFighter};
use notecalc::editor::editor::{EditorInputEvent, InputModifiers};
use notecalc::helper::*;
use notecalc::{
    Layer, OutputMessage, OutputMessageCommandId, RenderAsciiTextMsg, RenderBuckets,
    RenderStringMsg, RenderUtf8TextMsg,
};

mod utils;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const RENDER_COMMAND_BUFFER_SIZE: usize = 1024 * 100;
static mut RENDER_COMMAND_BUFFER: [u8; RENDER_COMMAND_BUFFER_SIZE] =
    [0; RENDER_COMMAND_BUFFER_SIZE];

#[wasm_bindgen]
extern "C" {
    pub fn js_log(s: &str);
}

#[wasm_bindgen]
pub unsafe fn create_app(client_width: usize, client_height: usize) -> usize {
    set_panic_hook();
    js_log(&format!("client_width: {}", client_width));
    js_log(&format!("client_height: {}", client_height));
    return to_box_ptr(BorrowCheckerFighter::new(client_width, client_height));
}

#[wasm_bindgen]
pub fn get_command_buffer_ptr() -> *const u8 {
    unsafe {
        return RENDER_COMMAND_BUFFER.as_ptr();
    }
}

#[wasm_bindgen]
pub fn alt_key_released(app_ptr: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let rb = bcf.mut_render_bucket();

    bcf.mut_app().alt_key_released(
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_editor_objects(),
        rb,
    );
}

#[wasm_bindgen]
pub fn handle_resize(app_ptr: usize, new_client_width: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    bcf.mut_app().handle_resize(
        new_client_width,
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn set_theme(app_ptr: usize, theme_index: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.mut_app();
    app.set_theme(
        theme_index,
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn get_compressed_encoded_content(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.mut_app();
    let content = app.get_line_ref_normalized_content();
    {
        use flate2::write::ZlibEncoder;
        use flate2::Compression;
        use std::io::prelude::*;
        
        let mut e = ZlibEncoder::new(Vec::new(), Compression::default());
        e.write_all(content.as_bytes()).expect("");
        let compressed_encoded = e
            .finish()
            .map(|it| base64::encode_config(it, base64::URL_SAFE_NO_PAD));
        
        return compressed_encoded.unwrap_or("".to_owned());
    }
}

#[wasm_bindgen]
pub fn set_compressed_encoded_content(app_ptr: usize, compressed_encoded: String) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let content = {
        use flate2::write::ZlibDecoder;
        use std::io::prelude::*;

        let decoded = base64::decode_config(&compressed_encoded, base64::URL_SAFE_NO_PAD);
        decoded.ok().and_then(|it| {
            let mut writer = Vec::with_capacity(compressed_encoded.len() * 2);
            let mut z = ZlibDecoder::new(writer);
            z.write_all(&it[..]).expect("");
            writer = z.finish().unwrap_or(Vec::new());
            String::from_utf8(writer).ok()
        })
    };
    if let Some(content) = content {
        let app = bcf.mut_app();

        app.set_normalized_content(
            &content.trim_end(),
            bcf.units(),
            bcf.allocator(),
            bcf.mut_tokens(),
            bcf.mut_results(),
            bcf.mut_vars(),
            bcf.mut_func_defs(),
            bcf.mut_editor_objects(),
            bcf.mut_render_bucket(),
        );
    }
}

#[wasm_bindgen]
pub fn get_clipboard_text(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.app();
    return app.editor.clipboard.clone();
}

#[wasm_bindgen]
pub fn get_selected_text_and_clear_app_clipboard(app_ptr: usize) -> Option<String> {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    return bcf.mut_app().get_selected_text_and_clear_app_clipboard();
}

#[wasm_bindgen]
pub fn get_allocated_bytes_count(app_ptr: usize) -> usize {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    return bcf.allocator().allocated_bytes();
}

#[wasm_bindgen]
pub fn handle_time(app_ptr: usize, now: u32) -> bool {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let rerender_needed = bcf.mut_app().handle_time(
        now,
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_editor_objects(),
        bcf.mut_render_bucket(),
    );

    return rerender_needed;
}

#[wasm_bindgen]
pub fn handle_mouse_move(app_ptr: usize, x: usize, y: usize) -> usize {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    return bcf.mut_app().handle_mouse_move(
        x,
        CanvasY::new(y as isize),
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.tokens(),
        bcf.results(),
        bcf.vars(),
        bcf.func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn handle_drag(app_ptr: usize, x: usize, y: usize) -> bool {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    return bcf.mut_app().handle_drag(
        x,
        CanvasY::new(y as isize),
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.tokens(),
        bcf.results(),
        bcf.vars(),
        bcf.func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn handle_click(app_ptr: usize, x: usize, y: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    bcf.mut_app().handle_click(
        x,
        CanvasY::new(y as isize),
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn handle_wheel(app_ptr: usize, dir: usize) -> bool {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    return bcf.mut_app().handle_wheel(
        dir,
        bcf.mut_editor_objects(),
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn handle_mouse_up(app_ptr: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    bcf.mut_app().handle_mouse_up();
}

#[wasm_bindgen]
pub fn handle_paste(app_ptr: usize, input: String) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    bcf.mut_app().handle_paste(
        input,
        bcf.units(),
        bcf.allocator(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_editor_objects(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn handle_input(app_ptr: usize, input: u32, modifiers: u8) -> bool {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let modifiers = InputModifiers {
        shift: modifiers & 1 != 0,
        ctrl: modifiers & 2 != 0,
        alt: modifiers & 4 != 0,
    };
    let input = match input {
        1 => EditorInputEvent::Backspace,
        2 => EditorInputEvent::Enter,
        3 => EditorInputEvent::Home,
        4 => EditorInputEvent::End,
        5 => EditorInputEvent::Up,
        6 => EditorInputEvent::Down,
        7 => EditorInputEvent::Left,
        8 => EditorInputEvent::Right,
        9 => EditorInputEvent::Del,
        10 => EditorInputEvent::Esc,
        11 => EditorInputEvent::PageUp,
        12 => EditorInputEvent::PageDown,
        13 => EditorInputEvent::Tab,
        _ => {
            let ch = std::char::from_u32(input);
            if let Some(ch) = ch {
                EditorInputEvent::Char(ch)
            } else {
                return false;
            }
        }
    };
    let app = bcf.mut_app();
    let modif = app.handle_input(
        input,
        modifiers,
        bcf.allocator(),
        bcf.units(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_editor_objects(),
        bcf.mut_render_bucket(),
    );

    return modif.is_some();
}

// The application has a memory leak, so call this method every N seconds,
// which will clear the allocator, but this is only possible if everything is reparsed and re-rendered after that.
// The reasons are that Tokens and RenderCommands both refer to the editor's canvas as a slice because of the Rust borrowing check,
// so we had to allocate them separately, and this separate allocation must be released.
// But unfortunately, the selections from parsing and rendering are mixed, so it can't just be freed anywhere.
// Could have freed it in the library, but that would require a mut allocator, and again, Rust's borrowing checker doesn't like that.
#[wasm_bindgen]
pub fn reparse_everything(app_ptr: usize) {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    bcf.mut_allocator().reset();
    let app = bcf.mut_app();

    app.reparse_everything(
        bcf.allocator(),
        bcf.units(),
        bcf.mut_tokens(),
        bcf.mut_results(),
        bcf.mut_vars(),
        bcf.mut_func_defs(),
        bcf.mut_editor_objects(),
        bcf.mut_render_bucket(),
    );
}

#[wasm_bindgen]
pub fn get_selected_rows_with_results(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.mut_app();
    let units = bcf.units();
    return app.copy_selected_rows_with_result_to_clipboard(
        units,
        bcf.mut_render_bucket(),
        bcf.mut_tokens(),
        bcf.mut_vars(),
        bcf.mut_results(),
    );
}

#[wasm_bindgen]
pub fn get_plain_content(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.app();
    app.editor_content.get_content()
}

#[wasm_bindgen]
pub fn get_cursor(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.app();
    let sel = app.editor.get_selection();
    format!("{:?}", sel)
}

#[wasm_bindgen]
pub fn get_top_of_undo_stack(app_ptr: usize) -> String {
    let bcf = BorrowCheckerFighter::from_ptr(app_ptr);
    let app = bcf.app();
    format!("{:?}", app.editor.undo_stack.last())
}
