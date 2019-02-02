extern crate proc_macro;
extern crate proc_macro2;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use serde_json::Value;
use std::{
    collections::{hash_map::Keys, HashMap, HashSet},
    path::{Path, PathBuf},
};
use syn::Type;

#[proc_macro]
pub fn i18n(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let input = input.to_string().replace("\"", "");

    let mut output = TokenStream::new();

    let locale_files = find_locale_files(input);

    let translations = build_translations_from_files(&locale_files);
    let locales = build_locale_names_from_files(&locale_files);
    gen_code(locales, translations, &mut output);

    output.into_iter().collect::<TokenStream>().into()
}

fn gen_code(locales: Vec<LocaleName>, translations: Translations, out: &mut TokenStream) {
    gen_locale_enum(locales, out);
    gen_i18n_struct(translations, out);
}

fn gen_locale_enum(locales: Vec<LocaleName>, out: &mut TokenStream) {
    let variants = locales.iter().map(|key| ident(&key.0));

    out.extend(quote! {
        #[allow(missing_docs)]
        pub enum Locale {
            #(#variants),*
        }
    });
}

type Translations = HashMap<Key, HashMap<LocaleName, (Translation, Placeholders)>>;

fn gen_i18n_struct(translations: Translations, out: &mut TokenStream) {
    let methods = translations.into_iter().map(|(key, translation_for_key)| {
        let method_name = ident(&key.0);

        let placeholders: Placeholders = (translation_for_key.iter().find(|_| true).unwrap().1)
            .1
            .clone();
        let placeholders = placeholders.0.iter().map(|name| {
            let name = ident(name);
            quote! { #name: &str }
        });

        let branches =
            translation_for_key
                .into_iter()
                .map(|(locale_name, (translation, pladeholders))| {
                    let locale_name = ident(&locale_name.0);
                    let translation = translation.0;

                    let body = if pladeholders.0.is_empty() {
                        quote! { #translation.to_string() }
                    } else {
                        let pladeholders = pladeholders.0.iter().map(|p| {
                            let name = ident(p);
                            quote! { #name = #name }
                        });

                        quote! {
                            format!(#translation, #(#pladeholders),*)
                        }
                    };

                    quote! {
                        Locale::#locale_name => { #body }
                    }
                });

        quote! {
            fn #method_name(locale: &Locale, #(#placeholders),*) -> String {
                match locale {
                    #(#branches),*
                }
            }
        }
    });

    out.extend(quote! {
        #[allow(missing_docs)]
        pub struct I18n;

        impl I18n {
            #(#methods)*
        }
    });
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

fn build_translations_from_files(files: &Vec<PathBuf>) -> Translations {
    let keys_per_locale: Vec<(LocaleName, I18nKey)> = files
        .into_iter()
        .flat_map(|file| {
            let locale_name = locale_name_from_translations_file_path(&file);
            let keys_in_file = build_keys_from_json(read_translations_file(file));

            keys_in_file
                .into_iter()
                .map(|key| (locale_name.clone(), key))
                .collect::<Vec<(LocaleName, I18nKey)>>()
        })
        .collect();

    let keys_per_locale: HashMap<(LocaleName, Key), (Translation, Placeholders)> = keys_per_locale
        .into_iter()
        .map(|(locale, key)| ((locale, key.key), (key.translation, key.placeholders)))
        .collect();

    let mut acc: Translations = HashMap::new();

    for ((locale_name, key), (translation, placeholders)) in keys_per_locale {
        let entry = acc.entry(key).or_insert_with(HashMap::new);
        entry.insert(locale_name, (translation, placeholders));
    }

    acc
}

fn build_locale_names_from_files(files: &Vec<PathBuf>) -> Vec<LocaleName> {
    files
        .into_iter()
        .map(|file| locale_name_from_translations_file_path(&file))
        .collect()
}

fn find_locale_files<P: AsRef<Path>>(locales_path: P) -> Vec<PathBuf> {
    let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
    let locales_dir = crate_root_path.join(locales_path);

    std::fs::read_dir(locales_dir)
        .unwrap()
        .map(|entry| {
            let entry = entry.unwrap();
            let path = entry.path();

            if path.is_dir() {
                // TODO: Skip directory and just emit warning
                panic!("Directories not allowed")
            } else {
                path
            }
        })
        .collect()
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Key(String);

#[derive(Debug)]
struct Translation(String);

#[derive(Debug, Clone)]
struct Placeholders(Vec<String>);

#[derive(Debug)]
struct I18nKey {
    key: Key,
    translation: Translation,
    placeholders: Placeholders,
}

fn read_translations_file(path: &PathBuf) -> HashMap<String, String> {
    let contents = std::fs::read_to_string(path).unwrap();
    serde_json::from_str(&contents).expect("failed to parse json")
}

fn build_keys_from_json(map: HashMap<String, String>) -> Vec<I18nKey> {
    map.into_iter()
        .map(|(key, value)| {
            let placeholders = find_placeholders(&value);

            I18nKey {
                key: Key(key),
                translation: Translation(value),
                placeholders: Placeholders(placeholders),
            }
        })
        .collect()
}

fn find_placeholders(s: &str) -> Vec<String> {
    // TODO: Handle invalid input with unbalanced braces
    // TODO: Escaping of {}

    let mut acc = vec![];

    let mut inside_placeholder = false;
    let mut current_placeholder = String::new();

    for c in s.chars() {
        if c == '{' {
            inside_placeholder = true;
        } else if c == '}' {
            inside_placeholder = false;
            acc.push(current_placeholder);
            current_placeholder = String::new();
        } else if inside_placeholder {
            current_placeholder.push(c)
        }
    }

    acc
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct LocaleName(String);

fn locale_name_from_translations_file_path(path: &PathBuf) -> LocaleName {
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let name = uppercase_first_letter(file_stem);
    LocaleName(name)
}

fn uppercase_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_find_locale_files() {
        let input = "tests/locales";

        let locale_files = find_locale_files(input);

        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));

        let expected_paths = vec![
            crate_root_path.join(input).join(PathBuf::from("en.json")),
            crate_root_path.join(input).join(PathBuf::from("da.json")),
        ];

        assert_eq!(locale_files, expected_paths,);
    }

    #[test]
    fn test_reading_files() {
        let input = "tests/locales";
        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let locale_path = crate_root_path.join(input).join(PathBuf::from("en.json"));

        let map = read_translations_file(&locale_path);
        let mut keys = build_keys_from_json(map);
        keys.sort_by_key(|key| key.key.0.clone());

        assert_eq!(keys[0].key.0, "greeting");
        assert_eq!(keys[0].translation.0, "Hello {name}");
        assert_eq!(keys[0].placeholders.0, vec!["name"]);

        assert_eq!(keys[1].key.0, "hello");
        assert_eq!(keys[1].translation.0, "Hello, World!");
        assert_eq!(keys[1].placeholders.0, Vec::<String>::new());
    }

    #[test]
    fn test_parsing_placeholders() {
        assert_eq!(find_placeholders("Hello"), Vec::<String>::new());
        assert_eq!(find_placeholders("Hello {name}"), vec!["name"]);
        assert_eq!(
            find_placeholders("{greeting} {name}"),
            vec!["greeting", "name"]
        );
    }

    #[test]
    fn test_finding_locale_names() {
        let input = "tests/locales";
        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let locale_path = crate_root_path.join(input).join(PathBuf::from("en.json"));

        let locale_name = locale_name_from_translations_file_path(&locale_path);

        assert_eq!(locale_name.0, "En");
    }

    #[test]
    fn test_building_translations() {
        let input = "tests/locales";
        let locale_files = find_locale_files(input);

        let translations = build_translations_from_files(&locale_files);

        assert_eq!(
            (translations[&Key("greeting".to_string())][&LocaleName("En".to_string())].0).0,
            "Hello {name}",
        );
    }
}
