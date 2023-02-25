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

mod utils;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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
