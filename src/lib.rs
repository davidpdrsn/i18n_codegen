#![deny(unused_imports, dead_code, unused_variables)]

extern crate proc_macro;
extern crate proc_macro2;

mod parse;

use heck::CamelCase;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::{
    collections::{HashMap, HashSet},
    env,
    path::{Path, PathBuf},
};

#[proc_macro]
pub fn i18n(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let input = input.to_string().replace("\"", "");

    let mut output = TokenStream::new();

    let locale_files = find_locale_files(input);

    let translations = build_translations_from_files(&locale_files);

    let locales = build_locale_names_from_files(&locale_files);
    gen_code(locales, translations, &mut output);

    if env::var("I18N_CODE_GEN_DEBUG").ok() == Some("1".to_string()) {
        println!("{}", output);
    }

    output.into()
}

type Translations = HashMap<Key, HashMap<LocaleName, (Translation, Placeholders)>>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct LocaleName(String);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Key(String);

#[derive(Debug)]
struct Translation(String);

#[derive(Debug, Clone)]
struct Placeholders(HashSet<String>);

#[derive(Debug)]
struct I18nKey {
    key: Key,
    translation: Translation,
    placeholders: Placeholders,
}

fn gen_code(locales: Vec<LocaleName>, translations: Translations, out: &mut TokenStream) {
    gen_locale_enum(locales, out);
    gen_i18n_struct(translations, out);
}

fn gen_locale_enum(locales: Vec<LocaleName>, out: &mut TokenStream) {
    let variants = locales.iter().map(|key| ident(&key.0));

    out.extend(quote! {
        #[allow(missing_docs)]
        #[derive(Copy, Clone, Debug)]
        pub enum Locale {
            #(#variants),*
        }
    });
}

fn gen_i18n_struct(translations: Translations, out: &mut TokenStream) {
    let mut methods = vec![];
    let mut all_unique_placeholders = HashSet::<Ident>::new();

    for (key, translations) in translations {
        let name = ident(&key.0);

        let mut placeholders = translations
            .iter()
            .flat_map(|(_, (_, placeholders))| placeholders.0.iter().map(|p| ident(p)))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        placeholders.sort();

        for placeholder in &placeholders {
            all_unique_placeholders.insert(placeholder.clone());
        }

        let args = placeholders.iter().map(|placeholder| {
            let type_name = ident(&placeholder.to_string().to_camel_case());
            quote! { #placeholder: #type_name<'_> }
        });

        let match_arms = translations.iter().map(|(locale_name, (translation, _))| {
            let locale_name = ident(&locale_name.0);
            let translation = translation.0.to_string();

            let body = if placeholders.is_empty() {
                quote! { format!(#translation) }
            } else {
                let fields = placeholders.iter().filter_map(|placeholder| {
                    let mut format_key = placeholder.to_string();
                    format_key.truncate(format_key.len() - 1);

                    if translation.contains(&format!("{{{}}}", format_key)) {
                        let format_key = ident(&format_key);
                        Some(quote! { #format_key = #placeholder.0 })
                    } else {
                        None
                    }
                });
                quote! { format!(#translation, #(#fields),*) }
            };

            quote! {
                Locale::#locale_name => #body
            }
        });

        methods.push(quote! {
            #[allow(missing_docs)]
            pub fn #name(self, #(#args),*) -> String {
                match self {
                    #(#match_arms),*
                }
            }
        });
    }

    let placeholder_newtypes = all_unique_placeholders.into_iter().map(|placeholder| {
        let placeholder = ident(&placeholder.to_string().to_camel_case());
        quote! {
            #[allow(missing_docs)]
            pub struct #placeholder<'a>(pub &'a str);
        }
    });

    out.extend(quote! {
        #(#placeholder_newtypes)*

        impl Locale {
            #(#methods)*
        }
    });
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

fn build_translations_from_files(files: &[PathBuf]) -> Translations {
    let keys_per_locale: Vec<(LocaleName, I18nKey)> = files
        .iter()
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

fn build_locale_names_from_files(files: &[PathBuf]) -> Vec<LocaleName> {
    files
        .iter()
        .map(|file| locale_name_from_translations_file_path(&file))
        .collect()
}

fn find_locale_files<P: AsRef<Path>>(locales_path: P) -> Vec<PathBuf> {
    let cargo_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("Env var `CARGO_MANIFEST_DIR` was missing");
    let pwd = PathBuf::from(cargo_dir);
    let full_locales_path = pwd.join(locales_path);

    std::fs::read_dir(full_locales_path)
        .expect("read dir")
        .map(|entry| {
            let entry = entry.expect("entry");
            let path = entry.path();

            if path.is_dir() {
                // TODO: Skip directory and just emit warning
                panic!("Directories not allowed")
            } else {
                path
            }
        })
        .filter(|path| {
            path.extension()
                .map(|ext| ext == "json")
                .unwrap_or_else(|| false)
        })
        .collect()
}

fn read_translations_file(path: &PathBuf) -> HashMap<String, String> {
    let contents = std::fs::read_to_string(path).expect("read file");
    serde_json::from_str(&contents).expect("failed to parse JSON file into HashMap<String, String>")
}

fn build_keys_from_json(map: HashMap<String, String>) -> Vec<I18nKey> {
    map.into_iter()
        .map(|(key, value)| {
            let placeholders = find_placeholders(&value);

            I18nKey {
                key: Key(key.replace(".", "_")),
                translation: Translation(value),
                placeholders: Placeholders(placeholders),
            }
        })
        .collect()
}

fn find_placeholders(s: &str) -> HashSet<String> {
    let raw_placeholders = parse::find_placeholders(s);
    let mut final_placeholders = HashSet::with_capacity(raw_placeholders.len());
    for entry in raw_placeholders {
        let mut entry = entry.clone();
        entry.push('_');
        final_placeholders.insert(entry);
    }
    final_placeholders
}

fn locale_name_from_translations_file_path(path: &PathBuf) -> LocaleName {
    let file_stem = path
        .file_stem()
        .expect("file stem")
        .to_str()
        .expect("file to string");
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
    fn test_find_locale_files() {
        let input = "tests/locales";

        let locale_files = find_locale_files(input);

        assert_eq!(locale_files.len(), 2);
        assert!(locale_files[0].to_str().unwrap().contains("en.json"));
        assert!(locale_files[1].to_str().unwrap().contains("da.json"));
    }

    #[test]
    fn test_reading_files() {
        let input = "tests/locales";
        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let locale_path = crate_root_path.join(input).join(PathBuf::from("en.json"));

        let map = read_translations_file(&locale_path);
        let mut keys = build_keys_from_json(map);
        keys.sort_by_key(|key| key.key.0.clone());

        assert_eq!(keys[0].key.0, "duplicate_placeholders");
        assert_eq!(keys[0].translation.0, "Hey {name}. Is your name {name}?");
        assert_eq!(to_vec(keys[0].placeholders.0.clone()), vec!["name_"]);
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

    #[test]
    fn test_parsing_placeholders_with_escaping() {
        assert_eq!(find_placeholders("Hello {{foo}}"), hashset![]);
        assert_eq!(
            find_placeholders("Hello {foo} {{bar}} {baz}"),
            hashset!["foo_".to_string(), "baz_".to_string()]
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

    fn to_vec<T: std::hash::Hash + Eq>(set: HashSet<T>) -> Vec<T> {
        set.into_iter().collect()
    }
}
