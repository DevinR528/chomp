use std::ops::Range;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Span {
    rng: Range<usize>,
}

impl Span {
    pub fn new<R: Into<Range<usize>>>(r: R) -> Self {
        Self { rng: r.into() }
    }

    pub fn text<'a>(&self, input: &'a str) -> &'a str {
        &input[self.rng.start..self.rng.end]
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Ident {
    span: Span,
}

impl Ident {}

macro_rules! keywords {
    ($($tkn:ident: $rep:expr,)*) => {
        pub mod kw {
            $(
                pub struct $tkn;
                impl $tkn {
                    pub fn text(&self) -> &'static str {
                        $rep
                    }
                }
            )*
        }
    };
}
// After modifying this list adjust `is_special`, `is_used_keyword`/`is_unused_keyword`,
// this should be rarely necessary though if the keywords are kept in alphabetic order.
keywords! {
    // Special reserved identifiers used internally for elided lifetimes,
    // unnamed method parameters, crate root module, error recovery etc.
    Empty:              "",
    PathRoot:           "{{root}}",
    DollarCrate:        "$crate",
    Underscore:         "_",

    // Keywords that are used in stable Rust.
    As:                 "as",
    Break:              "break",
    Const:              "const",
    Continue:           "continue",
    Crate:              "crate",
    Else:               "else",
    Enum:               "enum",
    Extern:             "extern",
    False:              "false",
    Fn:                 "fn",
    For:                "for",
    If:                 "if",
    Impl:               "impl",
    In:                 "in",
    Let:                "let",
    Loop:               "loop",
    Match:              "match",
    Mod:                "mod",
    Move:               "move",
    Mut:                "mut",
    Pub:                "pub",
    Ref:                "ref",
    Return:             "return",
    SelfLower:          "self",
    SelfUpper:          "Self",
    Static:             "static",
    Struct:             "struct",
    Super:              "super",
    Trait:              "trait",
    True:               "true",
    Type:               "type",
    Unsafe:             "unsafe",
    Use:                "use",
    Where:              "where",
    While:              "while",

    // Keywords that are used in unstable Rust or reserved for future use.
    Abstract:           "abstract",
    Become:             "become",
    Box:                "box",
    Do:                 "do",
    Final:              "final",
    Macro:              "macro",
    Override:           "override",
    Priv:               "priv",
    Typeof:             "typeof",
    Unsized:            "unsized",
    Virtual:            "virtual",
    Yield:              "yield",

    // Edition-specific keywords that are used in stable Rust.
    Async:              "async", // >= 2018 Edition only
    Await:              "await", // >= 2018 Edition only
    Dyn:                "dyn", // >= 2018 Edition only

    // Edition-specific keywords that are used in unstable Rust or reserved for future use.
    Try:                "try", // >= 2018 Edition only

    // Special lifetime names
    UnderscoreLifetime: "'_",
    StaticLifetime:     "'static",

    // Weak keywords, have special meaning only in specific contexts.
    Auto:               "auto",
    Catch:              "catch",
    Default:            "default",
    MacroRules:         "macro_rules",
    Raw:                "raw",
    Union:              "union",
}
