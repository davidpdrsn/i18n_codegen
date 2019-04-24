use std::collections::HashSet;

pub fn find_placeholders(s: &str) -> HashSet<String> {
    let mut acc = HashSet::new();
    let mut current_placeholder = String::new();
    let mut inside_placeholder = false;
    for token in tokenize(s) {
        match token {
            Token::OpenBrace => {
                if inside_placeholder {
                    panic!("Nested placeholders are not allowed");
                }
                inside_placeholder = true;
            }
            Token::CloseBrace => {
                if !inside_placeholder {
                    panic!("Should have been inside placeholder but wasn't");
                }
                inside_placeholder = false;
                acc.insert(current_placeholder.clone());
                current_placeholder.clear();
            }
            Token::Char(c) => {
                if inside_placeholder {
                    current_placeholder.push(c);
                }
            }
        }
    }
    acc
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut acc = vec![];

    let mut open_count = 0;
    let mut close_count = 0;

    for c in input.chars() {
        if c == '{' {
            open_count += 1;
        } else if c == '}' {
            close_count += 1;
        } else {
            add_braces(
                open_count,
                &mut acc,
                Token::Char('{'),
                Token::OpenBrace,
                false,
            );
            open_count = 0;

            add_braces(
                close_count,
                &mut acc,
                Token::Char('}'),
                Token::CloseBrace,
                true,
            );
            close_count = 0;

            acc.push(Token::Char(c));
        }
    }

    add_braces(
        open_count,
        &mut acc,
        Token::Char('{'),
        Token::OpenBrace,
        false,
    );
    add_braces(
        close_count,
        &mut acc,
        Token::Char('}'),
        Token::CloseBrace,
        true,
    );

    acc
}

fn add_braces(count: i32, acc: &mut Vec<Token>, t1: Token, t2: Token, is_close: bool) {
    if count > 0 {
        if is_even(count) {
            repeat(count / 2, || {
                acc.push(t1);
            });
        } else {
            if is_close {
                acc.push(t2);
                repeat((count - 1) / 2, || {
                    acc.push(t1);
                });
            } else {
                repeat((count - 1) / 2, || {
                    acc.push(t1);
                });
                acc.push(t2);
            }
        }
    }
}

fn repeat<F>(n: i32, mut f: F)
where
    F: FnMut(),
{
    for _ in 0..n {
        f()
    }
}

fn is_even(n: i32) -> bool {
    n % 2 == 0
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Token {
    OpenBrace,
    CloseBrace,
    Char(char),
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_all_chars() {
        assert_eq!(
            tokenize("bar"),
            vec![Token::Char('b'), Token::Char('a'), Token::Char('r'),]
        );
    }

    #[test]
    fn test_open_close() {
        assert_eq!(
            tokenize("{bar}"),
            vec![
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
            ],
            "Just placeholder",
        );

        assert_eq!(
            tokenize("f {bar}"),
            vec![
                Token::Char('f'),
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
            ],
            "Text in front",
        );

        assert_eq!(
            tokenize("{bar} f"),
            vec![
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char(' '),
                Token::Char('f'),
            ],
            "Text behind",
        );

        assert_eq!(
            tokenize("g {bar} f"),
            vec![
                Token::Char('g'),
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char(' '),
                Token::Char('f'),
            ],
            "Text around",
        );
    }

    #[test]
    fn test_escaping_braces() {
        assert_eq!(
            tokenize("{{bar}}"),
            vec![
                Token::Char('{'),
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::Char('}'),
            ]
        );
    }

    #[test]
    fn test_escape_and_brace() {
        assert_eq!(
            tokenize("{{{bar}}}"),
            vec![
                Token::Char('{'),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char('}'),
            ],
            "Just placeholder",
        );

        assert_eq!(
            tokenize("f {{{bar}}}"),
            vec![
                Token::Char('f'),
                Token::Char(' '),
                Token::Char('{'),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char('}'),
            ],
            "Text in front",
        );

        assert_eq!(
            tokenize("{{{bar}}} f"),
            vec![
                Token::Char('{'),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char('}'),
                Token::Char(' '),
                Token::Char('f'),
            ],
            "Text behind",
        );

        assert_eq!(
            tokenize("g {{{bar}}} f"),
            vec![
                Token::Char('g'),
                Token::Char(' '),
                Token::Char('{'),
                Token::OpenBrace,
                Token::Char('b'),
                Token::Char('a'),
                Token::Char('r'),
                Token::CloseBrace,
                Token::Char('}'),
                Token::Char(' '),
                Token::Char('f'),
            ],
            "Text around",
        );
    }

    #[test]
    fn multiple_placeholders() {
        assert_eq!(
            tokenize("I am {fn} {ln}."),
            vec![
                Token::Char('I'),
                Token::Char(' '),
                Token::Char('a'),
                Token::Char('m'),
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('f'),
                Token::Char('n'),
                Token::CloseBrace,
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('l'),
                Token::Char('n'),
                Token::CloseBrace,
                Token::Char('.'),
            ]
        );
    }

    #[test]
    fn nested_escaped() {
        assert_eq!(
            tokenize("{{I am {fn} {ln}.}}"),
            vec![
                Token::Char('{'),
                Token::Char('I'),
                Token::Char(' '),
                Token::Char('a'),
                Token::Char('m'),
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('f'),
                Token::Char('n'),
                Token::CloseBrace,
                Token::Char(' '),
                Token::OpenBrace,
                Token::Char('l'),
                Token::Char('n'),
                Token::CloseBrace,
                Token::Char('.'),
                Token::Char('}'),
            ]
        );
    }
}
