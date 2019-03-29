i18n_codegen::i18n!("tests/locales");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!("Hello, World!", I18n::new(Locale::En).t(Strings::Hello));
        assert_eq!("Hej, Verden!", I18n::new(Locale::Da).t(Strings::Hello));
    }

    #[test]
    fn it_works_with_interpolations() {
        assert_eq!(
            "Hello Bob",
            I18n::new(Locale::En).t(Strings::Greeting {
                name_: "Bob".to_string()
            })
        );

        assert_eq!(
            "Hej Bob",
            I18n::new(Locale::Da).t(Strings::Greeting {
                name_: "Bob".to_string()
            })
        );
    }

    #[test]
    fn it_works_when_some_locales_are_missing_interpolations() {
        assert_eq!(
            "en foo word",
            en().t(Strings::MissingInterpolationDa {
                word_: "word".to_string()
            }),
        );
        assert_eq!(
            "da foo",
            da().t(Strings::MissingInterpolationDa {
                word_: "word".to_string()
            }),
        );

        assert_eq!(
            "en foo",
            en().t(Strings::MissingInterpolationEn {
                word_: "word".to_string()
            }),
        );
        assert_eq!(
            "da foo word",
            da().t(Strings::MissingInterpolationEn {
                word_: "word".to_string()
            }),
        );
    }

    #[test]
    fn it_works_when_placeholders_are_rust_keywords() {
        assert_eq!(
            "yo dawg",
            en().t(Strings::RustKeyword {
                type_: "dawg".to_string()
            }),
        );
        assert_eq!(
            "hva så hund",
            da().t(Strings::RustKeyword {
                type_: "hund".to_string()
            }),
        );
    }

    #[test]
    fn it_works_for_duplicate_placeholders() {
        assert_eq!(
            "Hey Bob. Is your name Bob?",
            en().t(Strings::DuplicatePlaceholders {
                name_: "Bob".to_string()
            }),
        );
        assert_eq!(
            "Hej Bob. Er dit navn Bob?",
            da().t(Strings::DuplicatePlaceholders {
                name_: "Bob".to_string()
            }),
        );
    }

    // error when strings are missing in some languages

    fn en() -> I18n {
        I18n::new(Locale::En)
    }

    fn da() -> I18n {
        I18n::new(Locale::Da)
    }
}
