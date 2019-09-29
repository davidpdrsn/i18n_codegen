use crate::{
    error::{Error, Result},
    LocaleName,
};
use std::collections::HashSet;

pub(crate) fn find_placeholders(
    s: &str,
    start: &str,
    end: &str,
    locale_name: &LocaleName,
) -> Result<HashSet<String>> {
    // TODO: Escaping of {}

    let tokens = tokenize(s, start, end, locale_name)?;

    let mut acc = HashSet::new();

    let mut inside_placeholder = false;
    let mut current_placeholder = String::new();

    for token in tokens {
        if token.is_start() {
            inside_placeholder = true;
        } else if token.is_end() {
            inside_placeholder = false;

            // This is necessary to allow placeholder to be Rust keywords
            current_placeholder.push('_');

            acc.insert(current_placeholder);
            current_placeholder = String::new();
        } else if inside_placeholder {
            current_placeholder.push_str(token.token())
        }
    }

    Ok(acc)
}

#[derive(Debug, Eq, PartialEq)]
enum Token<'a> {
    PlaceholderStart,
    PlaceholderEnd,
    Char(&'a str),
}

impl<'a> Token<'a> {
    fn is_start(&self) -> bool {
        if let Token::PlaceholderStart = self {
            true
        } else {
            false
        }
    }

    fn is_end(&self) -> bool {
        if let Token::PlaceholderEnd = self {
            true
        } else {
            false
        }
    }

    fn token(&self) -> &str {
        match self {
            Token::PlaceholderStart => panic!("token start"),
            Token::PlaceholderEnd => panic!("token end"),
            Token::Char(token) => token,
        }
    }
}

fn tokenize<'a>(
    string: &'a str,
    start: &str,
    end: &str,
    locale_name: &LocaleName,
) -> Result<Vec<Token<'a>>> {
    let mut tokens = vec![];
    let mut idx = 0;
    let start = split_into_slices(start);
    let end = split_into_slices(end);
    let s = split_into_slices(string);

    loop {
        if !within_bounds(&s, idx) {
            break;
        }

        if within_bounds(&s, idx + start.len() - 1) {
            let maybe_start = &s[idx..idx + start.len()];
            if maybe_start == start.as_slice() {
                tokens.push(Token::PlaceholderStart);
                idx += start.len();
                continue;
            }
        }

        if within_bounds(&s, idx + end.len() - 1) {
            let maybe_end = &s[idx..idx + end.len()];
            if maybe_end == end.as_slice() {
                tokens.push(Token::PlaceholderEnd);
                idx += end.len();
                continue;
            }
        }

        let c = &s[idx];
        tokens.push(Token::Char(c));
        idx += 1;
    }

    if balanced(&tokens) {
        Ok(tokens)
    } else {
        Err(Error::UnbalancedPlaceholders {
            locale_name: locale_name.clone(),
            string: string.to_string(),
        })
    }
}

fn balanced(tokens: &[Token]) -> bool {
    let mut start_count = 0;
    let mut end_count = 0;
    for token in tokens.iter() {
        if token.is_start() {
            start_count += 1;
        } else if token.is_end() {
            end_count += 1;
        }
    }
    start_count == end_count
}

fn split_into_slices(s: &str) -> Vec<&str> {
    let mut v = s.split("").collect::<Vec<&str>>();
    v.remove(0);
    v.remove(v.len() - 1);
    v
}

fn within_bounds(s: &[&str], idx: usize) -> bool {
    idx < s.len()
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    macro_rules! hashset {
        ( $( $item:expr ),* ) => {
            {
                let mut s = std::collections::HashSet::new();
                $( s.insert($item); )*
                    s
            }
        }
    }

    #[test]
    fn test_tokenize_empty_input() {
        assert_eq!(tokenize("", "{", "}", &test_locale()).unwrap(), vec![]);
    }

    #[test]
    fn test_tokenize_without_placeholders() {
        assert_eq!(
            tokenize("123", "{", "}", &test_locale()).unwrap(),
            vec![Token::Char("1"), Token::Char("2"), Token::Char("3"),]
        );
    }

    #[test]
    fn test_tokenize_with_simple_placeholders() {
        assert_eq!(
            tokenize("{bob}", "{", "}", &test_locale()).unwrap(),
            vec![
                Token::PlaceholderStart,
                Token::Char("b"),
                Token::Char("o"),
                Token::Char("b"),
                Token::PlaceholderEnd,
            ]
        );
    }

    #[test]
    fn test_tokenize_with_multi_char_placeholders() {
        assert_eq!(
            tokenize("[{bob}]", "[{", "}]", &test_locale()).unwrap(),
            vec![
                Token::PlaceholderStart,
                Token::Char("b"),
                Token::Char("o"),
                Token::Char("b"),
                Token::PlaceholderEnd,
            ]
        );
    }

    #[test]
    fn test_tokenize_with_different_length_char_placeholders() {
        assert_eq!(
            tokenize("%{bob}", "%{", "}", &test_locale()).unwrap(),
            vec![
                Token::PlaceholderStart,
                Token::Char("b"),
                Token::Char("o"),
                Token::Char("b"),
                Token::PlaceholderEnd,
            ]
        );
    }

    #[test]
    fn test_tokenize_with_emojis() {
        assert_eq!(
            tokenize("%{bob} 😅", "%{", "}", &test_locale()).unwrap(),
            vec![
                Token::PlaceholderStart,
                Token::Char("b"),
                Token::Char("o"),
                Token::Char("b"),
                Token::PlaceholderEnd,
                Token::Char(" "),
                Token::Char("😅"),
            ]
        );
    }

    #[test]
    fn error_when_unbalanced() {
        match tokenize("%{bob", "%{", "}", &test_locale()) {
            Err(Error::UnbalancedPlaceholders { .. }) => {}
            other => panic!("{:?}", other),
        }
    }

    #[test]
    fn test_parsing_placeholders() {
        assert_eq!(
            find_placeholders("Hello", "{", "}", &test_locale()).unwrap(),
            HashSet::<String>::new()
        );
        assert_eq!(
            find_placeholders("Hello {name}", "{", "}", &test_locale()).unwrap(),
            hashset!["name_".to_string()]
        );

        assert_eq!(
            find_placeholders("{greeting} {name}", "{", "}", &test_locale()).unwrap(),
            hashset!["greeting_".to_string(), "name_".to_string()],
        );
    }

    fn test_locale() -> LocaleName {
        LocaleName::new("test")
    }
}
