use std::collections::HashSet;

pub struct DefaultPlaceholderParser {
    start: char,
    end: char,
}

impl Default for DefaultPlaceholderParser {
    fn default() -> Self {
        Self {
            start: '{',
            end: '}',
        }
    }
}

pub trait PlaceholderParser {
    fn find_placeholders(&self, s: &str) -> HashSet<String>;
}

impl PlaceholderParser for DefaultPlaceholderParser {
    fn find_placeholders(&self, s: &str) -> HashSet<String> {
        // TODO: Handle invalid input with unbalanced braces
        // TODO: Escaping of {}

        let mut acc = HashSet::new();

        let mut inside_placeholder = false;
        let mut current_placeholder = String::new();

        for c in s.chars() {
            if c == self.start {
                inside_placeholder = true;
            } else if c == self.end {
                inside_placeholder = false;

                // This is necessary to allow placeholder to be Rust keywords
                current_placeholder.push('_');

                acc.insert(current_placeholder);
                current_placeholder = String::new();
            } else if inside_placeholder {
                current_placeholder.push(c)
            }
        }

        acc
    }
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
    fn test_parsing_placeholders() {
        assert_eq!(find_placeholders("Hello"), HashSet::<String>::new());
        assert_eq!(
            find_placeholders("Hello {name}"),
            hashset!["name_".to_string()]
        );

        assert_eq!(
            find_placeholders("{greeting} {name}"),
            hashset!["greeting_".to_string(), "name_".to_string()],
        );
    }

    fn find_placeholders(s: &str) -> HashSet<String> {
        let parser = DefaultPlaceholderParser::default();
        parser.find_placeholders(&s)
    }
}
