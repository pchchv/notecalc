use wasm_bindgen::prelude::*;

use crate::utils::set_panic_hook;
use notecalc::borrow_checker_fighter::{to_box_ptr, BorrowCheckerFighter};

mod utils;

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

