use std::collections::HashSet;

pub fn find_placeholders(s: &str, start: &str, end: &str) -> HashSet<String> {
    // TODO: Handle invalid input with unbalanced braces
    // TODO: Escaping of {}

    let tokens = tokenize(s, start, end);

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

    acc
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
            Token::PlaceholderStart => unimplemented!("token start"),
            Token::PlaceholderEnd => unimplemented!("token end"),
            Token::Char(token) => token,
        }
    }
}

fn tokenize<'a>(s: &'a str, start: &str, end: &str) -> Vec<Token<'a>> {
    let mut tokens = vec![];
    let mut idx = 0;
    let start = split_into_slices(start);
    let end = split_into_slices(end);
    let s = split_into_slices(s);

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

    tokens
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
        assert_eq!(tokenize("", "{", "}"), vec![]);
    }

    #[test]
    fn test_tokenize_without_placeholders() {
        assert_eq!(
            tokenize("123", "{", "}"),
            vec![Token::Char("1"), Token::Char("2"), Token::Char("3"),]
        );
    }

    #[test]
    fn test_tokenize_with_simple_placeholders() {
        assert_eq!(
            tokenize("{bob}", "{", "}"),
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
            tokenize("[{bob}]", "[{", "}]"),
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
            tokenize("%{bob}", "%{", "}"),
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
            tokenize("%{bob} 😅", "%{", "}"),
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
    fn test_parsing_placeholders() {
        assert_eq!(
            find_placeholders("Hello", "{", "}"),
            HashSet::<String>::new()
        );
        assert_eq!(
            find_placeholders("Hello {name}", "{", "}"),
            hashset!["name_".to_string()]
        );

        assert_eq!(
            find_placeholders("{greeting} {name}", "{", "}"),
            hashset!["greeting_".to_string(), "name_".to_string()],
        );
    }

}
