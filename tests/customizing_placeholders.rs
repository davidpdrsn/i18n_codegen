i18n_codegen::i18n!("tests/locales_with_different_placeholders", open: "%{", close: "}");

#[test]
fn it_works_with_interpolations() {
    assert_eq!("Hello Bob", Locale::En.greeting_different_placeholder(PercentPlaceholder("Bob")));
    assert_eq!("Hej Bob", Locale::Da.greeting_different_placeholder(PercentPlaceholder("Bob")));
}
