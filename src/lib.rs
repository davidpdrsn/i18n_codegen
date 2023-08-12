//! Internationalization library based on code generation.
//!
//! By leveraging code generation we are able to prevent common bugs like typos in i18n keys,
//! missing interpolations, or various mistakes between locales.
//!
//! It requires a directory with one JSON file per locale. Here is an example with English and
//! Danish translations:
//!
//! ```json
//! // tests/doc_locales/en.json
//! {
//!     "hello_world": "Hello, World!",
//!     "greeting": "Hello {name}"
//! }
//!
//! // tests/doc_locales/da.json
//! {
//!     "hello_world": "Hej, Verden!",
//!     "greeting": "Hej {name}"
//! }
//! ```
//!
//! And in Rust:
//!
//! ```
//! use i18n_codegen::i18n;
//!
//! i18n!("tests/doc_locales");
//!
//! fn main() {
//!     assert_eq!("Hello, World!", Locale::En.hello_world());
//!     assert_eq!("Hej, Verden!", Locale::Da.hello_world());
//!
//!     assert_eq!("Hello Bob", Locale::En.greeting(Name("Bob")));
//!     assert_eq!("Hej Bob", Locale::Da.greeting(Name("Bob")));
//! }
//! ```
//!
//! ## What gets generated?
//!
//! This is what gets generated for the example above:
//!
//! ```
//! // Locale enum with variant for each JSON file
//! #[derive(Copy, Clone, Debug)]
//! pub enum Locale {
//!     En,
//!     Da,
//! }
//!
//! impl Locale {
//!     // Each string in the locale files becomes a method on `Locale`
//!     pub fn hello_world(self) -> String {
//!         match self {
//!             Locale::Da => format!("Hej, Verden!"),
//!             Locale::En => format!("Hello, World!"),
//!         }
//!     }
//!
//!     // Placeholders in strings become arguments to the methods.
//!     // For strings with multiple placeholders they must be provided in
//!     // alphabetical order.
//!     pub fn greeting(self, name_: Name<'_>) -> String {
//!         match self {
//!             Locale::Da => format!("Hej {name}", name = name_.0),
//!             Locale::En => format!("Hello {name}", name = name_.0),
//!         }
//!     }
//! }
//!
//! // A placeholder for strings such as `"Hello {name}"`.
//! pub struct Name<'a>(pub &'a str);
//!
//! fn main() {
//!     assert_eq!("Hello, World!", Locale::En.hello_world());
//!     assert_eq!("Hej, Verden!", Locale::Da.hello_world());
//!
//!     assert_eq!("Hello Bob", Locale::En.greeting(Name("Bob")));
//!     assert_eq!("Hej Bob", Locale::Da.greeting(Name("Bob")));
//! }
//! ```
//!
//! It expects all the JSON keys to be lowercase and will replace `-` and `.` with `_`.
//!
//! You can set the environment variable `I18N_CODE_GEN_DEBUG` to `1` to have the generated code
//! printed during compilation. For example: `I18N_CODE_GEN_DEBUG=1 cargo build`.
//!
//! ## Customizing placeholders
//!
//! By default it will assume you use `{` and `}` for placeholders such as `"Hello {name}"`. That can be
//! customized like so:
//!
//! ```
//! # use i18n_codegen::i18n;
//! #
//! i18n!("tests/locales_with_different_placeholders", open: "%{", close: "}");
//! #
//! # fn main() {
//! #     assert_eq!("Hello Bob", Locale::En.greeting_different_placeholder(PercentPlaceholder("Bob")));
//! #     assert_eq!("Hej Bob", Locale::Da.greeting_different_placeholder(PercentPlaceholder("Bob")));
//! # }
//! ```
//!
//! There is currently no support for escaping placeholders.

#![deny(
    unused_imports,
    dead_code,
    unused_variables,
    unknown_lints,
    missing_docs,
    unused_must_use
)]
#![doc(html_root_url = "https://docs.rs/i18n_codegen/0.1.1")]

extern crate proc_macro;
extern crate proc_macro2;

mod error;
mod placeholder_parsing;

use error::{Error, MissingKeysInLocale, Result};
use heck::ToUpperCamelCase;
use placeholder_parsing::find_placeholders;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    env,
    path::{Path, PathBuf},
};
use syn::{
    parse::{self, Parse, ParseStream},
    Token,
};

/// See root module for more info.
#[proc_macro]
pub fn i18n(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = match syn::parse::<Input>(input) {
        Ok(input) => input,
        Err(err) => return err.to_compile_error().into(),
    };

    match try_i18n(input) {
        Ok(tokens) => tokens,
        Err(err) => panic!("{}", err),
    }
}

fn try_i18n(input: Input) -> Result<proc_macro::TokenStream> {
    let file_paths = find_locale_files(&input.filename)?;

    let paths_and_contents = file_paths
        .iter()
        .map(|path| {
            let contents = std::fs::read_to_string(path)?;
            Ok((path, contents))
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let translations = build_translations_from_files(&paths_and_contents, &input.config)?;
    validate_translations(&translations)?;
    let locales = build_locale_names_from_files(&file_paths)?;

    let mut output = TokenStream::new();
    gen_code(locales, translations, &mut output);

    if env::var("I18N_CODE_GEN_DEBUG").ok() == Some("1".to_string()) {
        println!("{}", output);
    }

    Ok(output.into())
}

#[derive(Debug)]
struct Input {
    filename: String,
    config: Config,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let filename = input.parse::<syn::LitStr>()?.value();

        let config = if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;

            let open_ident = input.parse::<Ident>()?;
            if open_ident != "open" {
                return Err(syn::Error::new(open_ident.span(), "expected `open`"));
            }

            input.parse::<Token![:]>()?;
            let open = input.parse::<syn::LitStr>()?.value();
            input.parse::<Token![,]>()?;

            let close_ident = input.parse::<Ident>()?;
            if close_ident != "close" {
                return Err(syn::Error::new(close_ident.span(), "expected `close`"));
            }
            input.parse::<Token![:]>()?;
            let close = input.parse::<syn::LitStr>()?.value();

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }

            Config { open, close }
        } else {
            Config::default()
        };

        Ok(Input { filename, config })
    }
}

#[derive(Debug)]
struct Config {
    open: String,
    close: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            open: "{".to_string(),
            close: "}".to_string(),
        }
    }
}

type Translations = HashMap<Key, HashMap<LocaleName, (Translation, Placeholders)>>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct LocaleName(String);

impl LocaleName {
    #[cfg(test)]
    fn new<T: Into<String>>(t: T) -> LocaleName {
        LocaleName(t.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Key(String);

#[derive(Debug)]
struct Translation(String);

#[derive(Debug, Clone)]
struct Placeholders(HashSet<(String, Option<String>)>);

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
    let all_variants = locales.iter().map(|key| {
        let name = ident(&key.0);
        quote!{ Locale :: #name, }
    });
    let to_variants = locales.iter().enumerate().map(|(i, key)| {
        let name = ident(&key.0);
        quote!{ #i => & Locale :: #name, }
    });
    let from_variants = locales.iter().enumerate().map(|(i, key)| {
        let name = ident(&key.0);
        quote!{ Locale :: #name => #i, }
    });

    out.extend(quote! {
        /// Locale enum generated by "i18n_codegen"
        #[derive(Copy, Clone, Debug)]
        pub enum Locale {
            #(#variants),*
        }

        pub const fn all_locales() -> &'static [Locale] {
            &[
                #(#all_variants)*
            ]
        }

        /// Locale enum generated by "i18n_codegen" for use in static context
        pub struct AtomicLocale {
            v: std::sync::atomic::AtomicUsize,
        }
        fn to_locale(locale: &AtomicLocale) -> &Locale {
            match locale.v.load(std::sync::atomic::Ordering::Relaxed) {
                #(#to_variants)*
                _ => unsafe { std::hint::unreachable_unchecked() }
            }
        }
        const fn from_locale(locale: Locale) -> usize {
            match locale {
                #(#from_variants)*
            }
        }
        pub fn set_locale(locale: &AtomicLocale, new_locale: Locale) {
            locale.v.store(from_locale(new_locale), std::sync::atomic::Ordering::Relaxed);
        }
        const fn new_atomic_locale(locale: Locale) -> AtomicLocale {
            AtomicLocale{
                v: std::sync::atomic::AtomicUsize::new(from_locale(locale))
            }
        }
    });
}

fn gen_i18n_struct(translations: Translations, out: &mut TokenStream) {
    let mut all_unique_placeholders = HashSet::<(Ident, Option<Ident>)>::new();
    let mut atomic_methods = Vec::new();

    let methods = translations
        .iter()
        .map(|(key, translations)| {
            let name = ident(&key.0);

            let mut placeholders = translations
                .iter()
                .flat_map(|(_, (_, placeholders))|
                    placeholders.0.iter().map(|(i, t)| (ident(i), t.as_ref().map(|t|ident(&t)))))
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>();
            placeholders.sort();

            for placeholder in &placeholders {
                all_unique_placeholders.insert(placeholder.clone());
            }

            let args = placeholders.iter().map(|(placeholder, placeholder_type)| {
                if let Some(placeholder_type) = placeholder_type {
                    quote! { #placeholder: #placeholder_type }
                } else {
                    let type_name = ident(&placeholder.to_string().to_upper_camel_case());
                    quote! { #placeholder: #type_name<'_> }
                }
            }).collect::<Vec<_>>();

            let match_arms = translations.iter().map(|(locale_name, (translation, _))| {
                let locale_name = ident(&locale_name.0);
                let translation = translation.0.to_string();

                let body = if placeholders.is_empty() {
                    quote! { #translation }
                } else {
                    let fields = placeholders.iter().filter_map(|(placeholder, placeholder_type)| {
                        let mut format_key = placeholder.to_string();
                        format_key.truncate(format_key.len() - 1);

                        let placehoder_with_open_close = format!(
                            "{open}{placeholder}{close}",
                            open = "{",
                            placeholder = format_key,
                            close = "}",
                        );
                        let placehoder_with_open_format = format!(
                            "{open}{placeholder}{close}",
                            open = "{",
                            placeholder = format_key,
                            close = ":",
                        );
                        if translation.contains(&placehoder_with_open_close) || translation.contains(&placehoder_with_open_format) {
                            let format_key = ident(&format_key);
                            if placeholder_type.is_some() {
                                Some(quote! { #format_key = #placeholder })
                            } else {
                                Some(quote! { #format_key = #placeholder.0 })
                            }
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

            if placeholders.is_empty() {
                atomic_methods.push(
                    quote! {
                            #[allow(missing_docs)]
                            pub fn #name(&self) -> &'static str {
                                to_locale(self).#name()
                            }
                    });
                quote! {
                #[allow(missing_docs)]
                pub fn #name(self) -> &'static str {
                    match self {
                        #(#match_arms),*
                    }
                }
            }
            } else {
                atomic_methods.push(
                    quote! {
                            #[allow(missing_docs)]
                            pub fn #name(&self, #(#args),*) -> String {
                                to_locale(&self).#name(#(#args),*)
                            }
                    });
                quote! {
                #[allow(missing_docs)]
                pub fn #name(self, #(#args),*) -> String {
                    match self {
                        #(#match_arms),*
                    }
                }
            }
            }
        })
        .collect::<Vec<_>>();

    let placeholder_newtypes = all_unique_placeholders.into_iter().map(|(placeholder, placeholder_type)| {
        if placeholder_type.is_none() {
            let placeholder = ident(&placeholder.to_string().to_upper_camel_case());
            quote! {
                #[allow(missing_docs)]
                pub struct #placeholder<'a>(pub &'a str);
            }
        } else {
            TokenStream::new()
        }
    });

    out.extend(quote! {
        #(#placeholder_newtypes)*

        impl Locale {
            #(#methods)*
        }

        impl AtomicLocale {
            #(#atomic_methods)*
        }
    });
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

fn build_translations_from_files(
    paths_and_contents: &[(&PathBuf, String)],
    config: &Config,
) -> Result<Translations> {
    let keys_per_locale = paths_and_contents
        .iter()
        .map(|(path, contents)| {
            let locale_name = locale_name_from_translations_file_path(&path)?;

            let map = parse_translations_file(&contents)?;
            let keys_in_file = build_keys_from_json(map, config, &locale_name)?;

            let locale_and_keys = keys_in_file
                .into_iter()
                .map(|key| (locale_name.clone(), key))
                .collect::<Vec<(LocaleName, I18nKey)>>();
            Ok(locale_and_keys)
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let keys_per_locale: HashMap<(LocaleName, Key), (Translation, Placeholders)> = keys_per_locale
        .into_iter()
        .flatten()
        .map(|(locale, key)| ((locale, key.key), (key.translation, key.placeholders)))
        .collect();

    let number_of_keys_per_locale = keys_per_locale.len() / paths_and_contents.len();
    let mut acc: Translations = HashMap::with_capacity(number_of_keys_per_locale);

    for ((locale_name, key), (translation, placeholders)) in keys_per_locale {
        let entry = acc
            .entry(key)
            .or_insert_with(|| HashMap::with_capacity(paths_and_contents.len()));
        entry.insert(locale_name, (translation, placeholders));
    }

    Ok(acc)
}

fn build_locale_names_from_files(file_paths: &[PathBuf]) -> Result<Vec<LocaleName>> {
    file_paths
        .iter()
        .map(|file_path| locale_name_from_translations_file_path(&file_path))
        .collect()
}

fn validate_translations(translations: &Translations) -> Result<()> {
    let all_keys = all_keys(translations);
    let keys_per_locale = keys_per_locale(translations);

    let mut errors = Vec::new();
    for (locale_name, keys) in keys_per_locale {
        let keys_missing = all_keys.difference(&keys).collect::<HashSet<_>>();
        if !keys_missing.is_empty() {
            let keys = keys_missing.iter().map(|key| (**key).clone()).collect();

            errors.push(MissingKeysInLocale {
                locale_name: locale_name.clone(),
                keys,
            });
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(Error::MissingKeysInLocale(errors))
    }
}

fn all_keys(translations: &Translations) -> HashSet<&Key> {
    translations.keys().collect()
}

fn keys_per_locale(
    translations: &Translations,
) -> HashMap<&LocaleName, HashSet<&Key>> {
    let mut acc = HashMap::new();

    for (key, translations_for_key) in translations {
        for (locale_name, (_translation, _placeholders)) in translations_for_key {
            acc.entry(locale_name)
                .or_insert_with(HashSet::new)
                .insert(key);
        }
    }

    acc
}

const CARGO_MANIFEST_DIR: &str = "CARGO_MANIFEST_DIR";

fn find_locale_files<P: AsRef<Path>>(locales_path: P) -> Result<Vec<PathBuf>> {
    let cargo_dir =
        env::var(CARGO_MANIFEST_DIR).map_err(Error::missing_env_var(CARGO_MANIFEST_DIR))?;

    let pwd = PathBuf::from(cargo_dir);
    let full_locales_path = pwd.join(locales_path);

    let paths = std::fs::read_dir(full_locales_path)?
        .map(|entry| {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                Err(Error::DirectoryInLocalesFolder)
            } else {
                Ok(path)
            }
        })
        .filter(|path| match path {
            Ok(path) => path
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or_else(|| false),
            // don't throw errors away
            Err(_) => true,
        })
        .collect::<Result<_, Error>>()?;
    Ok(paths)
}

fn parse_translations_file(contents: &str) -> Result<HashMap<&str, String>> {
    serde_json::from_str(&contents).map_err(From::from)
}

fn build_keys_from_json(
    map: HashMap<&str, String>,
    config: &Config,
    locale_name: &LocaleName,
) -> Result<Vec<I18nKey>> {
    map.into_par_iter()
        .map(|(key, mut value)| {
            let placeholders = find_placeholders(&mut value, &config.open, &config.close, locale_name)?;
            let key = key.replace(".", "_").replace("-", "_");

            Ok(I18nKey {
                key: Key(key),
                translation: Translation(value),
                placeholders: Placeholders(placeholders),
            })
        })
        .collect()
}

fn locale_name_from_translations_file_path(path: &PathBuf) -> Result<LocaleName> {
    let file_stem = path
        .file_stem()
        .ok_or_else(|| Error::NoFileStem)?
        .to_str()
        .ok_or_else(|| Error::InvalidUtf8InFileName)?;
    let name = uppercase_first_letter(file_stem);
    Ok(LocaleName(name))
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

        let locale_files = find_locale_files(input).unwrap();
        assert_eq!(locale_files.len(), 2);
        assert!(locale_files.iter().find(|p| p.to_str().unwrap().contains("en.json")).is_some());
        assert!(locale_files.iter().find(|p| p.to_str().unwrap().contains("da.json")).is_some());
    }

    #[test]
    fn test_reading_files() {
        let input = "tests/locales";
        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let locale_path = crate_root_path.join(input).join(PathBuf::from("en.json"));

        let contents = std::fs::read_to_string(&locale_path).unwrap();
        let map = parse_translations_file(&contents).unwrap();
        let mut keys =
            build_keys_from_json(map, &Config::default(), &LocaleName::new("test")).unwrap();
        keys.sort_by_key(|key| key.key.0.clone());

        assert_eq!(keys[0].key.0, "duplicate_placeholders");
        assert_eq!(keys[0].translation.0, "Hey {name}. Is your name {name}?");
        assert_eq!(to_vec(keys[0].placeholders.0.clone()), vec![("name_".to_string(), None)]);
    }

    #[test]
    fn test_finding_locale_names() {
        let input = "tests/locales";
        let crate_root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let locale_path = crate_root_path.join(input).join(PathBuf::from("en.json"));

        let locale_name = locale_name_from_translations_file_path(&locale_path).unwrap();

        assert_eq!(locale_name.0, "En");
    }

    #[test]
    fn test_building_translations() {
        let input = "tests/locales";
        let locale_files = find_locale_files(input).unwrap();

        let paths_and_contents = locale_files
            .iter()
            .map(|path| {
                let contents = std::fs::read_to_string(path).expect("read file");
                (path, contents)
            })
            .collect::<Vec<_>>();

        let translations =
            build_translations_from_files(&paths_and_contents, &Config::default()).unwrap();

        assert_eq!(
            (translations[&Key("greeting".to_string())][&LocaleName("En".to_string())].0).0,
            "Hello {name}",
        );
    }

    //#[test] // does not work, because of paths not found (on windows)
    fn _ui() {
        let t = trybuild::TestCases::new();
        t.compile_fail("tests/compile_fail/*.rs");
    }

    #[test]
    fn test_html_root_url() {
        version_sync::assert_html_root_url_updated!("src/lib.rs");
    }

    fn to_vec<T: std::hash::Hash + Eq>(set: HashSet<T>) -> Vec<T> {
        set.into_iter().collect()
    }
}
