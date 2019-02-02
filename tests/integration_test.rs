use i18n::i18n;

i18n!("tests/locales");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!("Hello, World!", I18n::hello(&Locale::En));
        assert_eq!("Hej, Verden!", I18n::hello(&Locale::Da));
    }

    #[test]
    fn it_works_with_interpolations() {
        assert_eq!("Hello Bob", I18n::greeting(&Locale::En, "Bob"));
        assert_eq!("Hej Bob", I18n::greeting(&Locale::Da, "Bob"));
    }
}
