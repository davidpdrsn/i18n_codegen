i18n_codegen::i18n!("tests/locales");

#[test]
fn it_works() {
    assert_eq!("Hello, World!", Locale::En.hello());
    assert_eq!("Hej, Verden!", Locale::Da.hello());
}

#[test]
fn it_works_with_interpolations() {
    assert_eq!("Hello Bob", Locale::En.greeting(Name("Bob")));
    assert_eq!("Hej Bob", Locale::Da.greeting(Name("Bob")));
}

#[test]
fn it_works_when_some_locales_are_missing_interpolations() {
    assert_eq!(
        "en foo word",
        Locale::En.missing_interpolation_da(Word("word")),
    );
    assert_eq!("da foo", Locale::Da.missing_interpolation_da(Word("word")));

    assert_eq!("en foo", Locale::En.missing_interpolation_en(Word("word")));
    assert_eq!(
        "da foo word",
        Locale::Da.missing_interpolation_en(Word("word")),
    );
}

#[test]
fn it_works_when_placeholders_are_rust_keywords() {
    assert_eq!("yo dawg", Locale::En.rust_keyword(Type("dawg")));
    assert_eq!("hva så hund", Locale::Da.rust_keyword(Type("hund")));
}

#[test]
fn it_works_for_duplicate_placeholders() {
    assert_eq!(
        "Hey Bob. Is your name Bob?",
        Locale::En.duplicate_placeholders(Name("Bob"))
    );

    assert_eq!(
        "Hej Bob. Er dit navn Bob?",
        Locale::Da.duplicate_placeholders(Name("Bob"))
    );
}

#[test]
fn it_works_for_multiple_placeholders() {
    assert_eq!(
        "en one 0x42",
        Locale::En.two_placeholders(One("one"), 66)
    );
    assert_eq!(
        "en one 0x42",
        Locale::En.two_placeholders(One("one"), 0x42)
    );

    assert_eq!(
        "da one 0x42",
        Locale::Da.two_placeholders(One("one"), 0x42)
    );
}

#[test]
fn it_supports_strings_with_newlines() {
    assert_eq!("Hello\nWorld!", Locale::En.hello_newline());
    assert_eq!("Hej\nVerden!", Locale::Da.hello_newline());
}
