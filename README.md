# [i18n_codegen](https://crates.io/crates/i18n_codegen)

Internationalization library for Rust based on code generation.

By leveraging code generation we are able to prevent common bugs like typos in i18n keys,
missing interpolations, or various mistakes between locales.

It requires a directory with one JSON file per locale. Here is an example with English and
Danish translations:

```javascript
// tests/doc_locales/en.json
{
    "hello_world": "Hello, World!",
    "greeting": "Hello {name}"
}

// tests/doc_locales/da.json
{
    "hello_world": "Hej, Verden!",
    "greeting": "Hej {name}"
}
```

And in Rust:

```rust
use i18n_codegen::i18n;

i18n!("tests/doc_locales");

fn main() {
    assert_eq!("Hello, World!", Locale::En.hello_world());
    assert_eq!("Hej, Verden!", Locale::Da.hello_world());

    assert_eq!("Hello Bob", Locale::En.greeting(Name("Bob")));
    assert_eq!("Hej Bob", Locale::Da.greeting(Name("Bob")));
}
```

You can find more details on <https://docs.rs/i18n_codegen>.
