pub fn set_panic_hook() {
    // When `console_error_panic_hook` is enabled,
    // the `set_panic_hook` function can be called at least once during initialization,
    // and then better error messages will be received if the code ever panics.
    //
    // For more information, see https://github.com/rustwasm/console_error_panic_hook#readme
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}