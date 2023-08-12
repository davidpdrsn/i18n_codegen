use crate::{Key, LocaleName};
use std::{collections::HashSet, fmt};

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) enum Error {
    JsonParsing(serde_json::error::Error),
    ProcMacroInput(syn::Error),
    Io(std::io::Error),
    MissingEnvVar {
        name: String,
        _inner_error: std::env::VarError,
    },
    DirectoryInLocalesFolder,
    NoFileStem,
    InvalidUtf8InFileName,
    UnbalancedPlaceholders {
        locale_name: LocaleName,
        string: String,
    },
    MissingKeysInLocale(Vec<MissingKeysInLocale>),
}

#[derive(Debug)]
pub(crate) struct MissingKeysInLocale {
    pub(crate) locale_name: LocaleName,
    pub(crate) keys: HashSet<Key>,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::JsonParsing(e) => write!(f, "{}", e),
            Error::ProcMacroInput(e) => write!(f, "{}", e),
            Error::Io(e) => write!(f, "{}", e),
            Error::MissingEnvVar { name, .. } => {
                write!(f, "Missing environment variable `{}`", name)
            }
            Error::DirectoryInLocalesFolder => {
                write!(f, "The locales directory should not contain other folders")
            }
            Error::NoFileStem => write!(f, "Failed to get file stem of locale file"),
            Error::InvalidUtf8InFileName => write!(f, "File name contained invalid UTF-8"),
            Error::UnbalancedPlaceholders {
                locale_name,
                string,
            } => {
                writeln!(f, "Unbalanced placeholders in string")?;
                writeln!(f, "Locale: {}", locale_name.0)?;
                write!(f, "String: {:?}", string)?;
                Ok(())
            }
            Error::MissingKeysInLocale(errors) => {
                let mut errors = errors.iter().collect::<Vec<_>>();
                errors.sort_by_key(|error| &error.locale_name.0);

                writeln!(f, "Some locales are missing keys")?;
                for error in errors {
                    writeln!(f, "`{}` is missing:", error.locale_name.0)?;
                    for key in &error.keys {
                        writeln!(f, "  `{}`", key.0)?;
                    }
                    writeln!(f)?;
                }
                Ok(())
            }
        }
    }
}

macro_rules! impl_from {
    ($from:ty => $to:ident :: $variant:ident) => {
        impl From<$from> for $to {
            fn from(from: $from) -> Self {
                $to::$variant(from)
            }
        }
    };
}

impl_from!(serde_json::error::Error => Error::JsonParsing);
impl_from!(syn::Error => Error::ProcMacroInput);
impl_from!(std::io::Error => Error::Io);

impl Error {
    pub fn missing_env_var<S: Into<String>>(name: S) -> impl FnOnce(std::env::VarError) -> Self {
        let name = name.into();
        move |var_error| Error::MissingEnvVar {
            name,
            _inner_error: var_error,
        }
    }
}
