//! [`ParserError`] used in rpmspec-rs.
//! Yes. You heard me. The only error is [`ParserError`] and everything else is
//! unfortunately String.
use std::{fmt::Display, num::ParseIntError, path::Path, sync::Arc};

use crate::{expr::Expression, util::Brace};
use chumsky::prelude::Simple;
use lazy_format::lazy_format as lzf;
use smartstring::alias::String;
use thiserror::Error;

pub type ParseResult<T> = Result<T, ParseErr>;
pub type ExprResult<T> = Result<T, ExprErr>;

/// Spec Syntax Errors and whatnot
#[derive(Debug, Error)]
pub enum SpecErr {
    #[error("Unclosed quotes: {quotes:?}")]
    UnclosedBraces { quotes: Vec<Brace> },
    #[error("Unmatched quote: expected `{expected}`, found `{found}`")]
    UnmatchedBrace { expected: Brace, found: Brace },
    #[error("Unexpected closing brace: {0}")]
    UnexpectedClosingBrace(Brace),
    #[error("Invalid package name: {name}")]
    InvalidPackageName { name: String, offending: Option<char> },
    #[error("Invalid package version: {version}")]
    InvalidPackageVersion { version: String, offending: Option<String> },
    #[error("Invalid package release: {release}")]
    InvalidPackageRelease { release: String, offending: Option<String> },
    #[error("Invalid package epoch: `{epoch}`; Epoch can only be positive integers")]
    InvalidPackageEpoch { epoch: String, err: ParseIntError },
    #[error("Invalid package architecture: {arch}")]
    InvalidPackageArch { arch: String, offending: Option<String> },

    #[error("Wrong number of arguments: expected one of {expected:?}, found {found}")]
    BadArgCount { expected: &'static [usize], found: usize },
    #[error("Bad mode: `{mode}`; expected integer")]
    BadMode { mode: String, err: ParseIntError },
    #[error("Bad modifier `{modifier}` for `{id}(...)`")]
    BadModifier { modifier: String, id: &'static str },
    #[error("Unknown %files directive: %{0}")]
    UnknownFilesDirective(String),
    #[error("Invalid changelog date: {0}")]
    InvalidChangelogDate(chrono::ParseError),

    #[error("Declared macro has empty/no definition in macro definition file")]
    EmptyMacroDefinition { name: String, file: std::path::PathBuf },
    #[error("Unexpected character in macro definition file")]
    BadChInMacroDef { ch: char, file: std::path::PathBuf },

    #[error("Unexpected conditional statement %{0}")]
    UnexpectedCond(&'static str),
    #[error("Bad arguments to parameterized macro or section %{0}: {1}")]
    BadCallToParamMacro(&'static str, Box<ParseErr>),
    #[error("Expected `%package {pkg}` to be declared before use of currently unknown subpackage in `%{section}`")]
    UseOfUnknownSubpkgInSection { pkg: String, section: &'static str },
    #[error("Unexpected flag `-{flag}` when calling `%{call}`")]
    UnexpectedFlagInStaticCall { flag: String, call: &'static str },
    #[error("Expected arguments to `-{flag}` when calling `%{call}`")]
    NoArgWithFlag { flag: &'static str, call: &'static str },
    #[error("Duplicated flags `-{flag}` to `%{call}`, previously `{prev}`, found `{found}`")]
    DupFlagsToStaticCall { flag: &'static str, call: &'static str, prev: String, found: String },
    #[error("Creating new sub-`%package {0}` without `-n` while main package `Name:` is undefined")]
    NoMainNameToInferSubpkgName(String),
    #[error("Declaring subpackage with taken name: `{0}`")]
    RedeclareSubpkg(String),

    #[error("Expected type bool, found expression `{value}` while parsing preamble `{preamble}:`")]
    PreambleNotBool { value: String, preamble: &'static str, err: std::str::ParseBoolError },
    #[error("Expected preamble, found non-empty line: `{0}`")]
    InvalidLineExpectedPreamble(String),

    #[error("Bad flag passed to parameterized macro because the flag starts with a non-ascii-alphabetic character `{0}`")]
    BadFlagToParamMacro(char),
    #[error("Bad flag in definition of parameterized macro: {0}")]
    BadFlagInCurlyInParamMacroDef(String),
}

/// Position information (char offset)
#[derive(Debug, Default)]
pub struct Span {
    filename: Option<Arc<Path>>,
    start: usize,
    end: usize,
    kind: SpanType,
}

impl Span {
    // let's implement fuctions to specify span data maybe
    /// Set the starting point (byte offset) of the span
    pub fn start(&mut self, start: usize) -> &mut Self {
        self.start = start;
        self
    }

    /// Set the ending  point (byte offset) of the span
    pub fn end(&mut self, end: usize) -> &mut Self {
        self.end = end;
        self
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { start, end, kind, filename } = self;
        let filename: Box<dyn Display> = filename.as_ref().map_or(Box::new("<unknown>"), |f| Box::new(f.display()));
        f.write_fmt(format_args!("{filename}:{start}-{end} as character positions ({kind})"))
    }
}

#[derive(Debug, Default)]
pub enum SpanType {
    #[default]
    Err,
    Warn,
    Note,
    Hint,
}

impl Display for SpanType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Err => "Error",
            Self::Warn => "Warn",
            Self::Note => "Note",
            Self::Hint => "Hint",
        })
    }
}

pub trait DisplayDebug: Display + std::fmt::Debug + Send + Sync {}

impl<T: Display + std::fmt::Debug + Send + Sync> DisplayDebug for T {}

/// Errors for some special parsing issues
#[derive(Debug, Error)]
#[allow(clippy::module_name_repetitions)]
pub enum ParseErr {
    /// Bad RPM Expression (`%[]`)
    #[error("Bad RPM Expression: {0:?}")]
    Expr(#[from] ExprErr),

    /// Invalid spec syntax
    #[error("Invalid RPM Spec Syntax: {err} (at {span})")]
    InvalidSyntax {
        span: Span, // probably todo?
        err: SpecErr,
        notes: Vec<(Option<Span>, Box<dyn DisplayDebug>)>,
    },

    /// The preamble specified is invalid.
    #[error("Preamble not supported")]
    UnknownPreamble(usize, String),
    /// A preamble that cannot be specified more than once was found duplicate.
    #[error("Preamble `{1}` specified more than once")]
    Duplicate(usize, String),
    /// A modifier (such as `pre` in `Requires(pre):`) is found to be invalid.
    #[error("Unknown preamble modifier `{1}`")]
    UnknownModifier(usize, String),
    /// Bad conditional operator used in package queries
    #[error("Invalid conditional operator `{0}` in package query")]
    BadPkgQCond(String),
    /// Macro not found (`_rp_macro()`)
    #[error("Macro not found: {0}")]
    MacroNotFound(String),
    /// Macro undefined (`_rp_macro()`)
    #[error("Macro not found: {0}")]
    MacroUndefined(String),
    /// Bad Expresssion `%[]`
    #[error("Bad RPM Expression: {0:#?}")]
    BadExpression(ExprErr),
    #[error("{0:?}")]
    #[deprecated = "Please implement a specific error type!!"]
    Others(String),

    #[error("Shell expansion %({0}) gives non-UTF-8 string: {1}")]
    ShellExpansionNonUtf8(String, std::str::Utf8Error),
    #[error("Shell expansion %({shellcmd}) failed (status code {})", status_code.map_or(Box::new("unknown") as Box<dyn Display>, |code| Box::new(lzf!("{code}"))))]
    ShellExpansionFail { shellcmd: String, status_code: Option<i32>, stdout: Vec<u8>, stderr: Vec<u8> },
    #[error("Shell expansion %({0}) failed to execute: {1}")]
    ShellExpansionFailToExecute(String, std::io::Error),

    /// A Lua error
    #[error("Lua error: {0}")]
    LuaError(#[from] mlua::Error),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Encounter non-fatal errors while parsing spec: {0:?}")]
    ParseFailErrs(Box<[Self]>),

    #[error("Invalid file format in %uncompress")]
    InternalMacroEmittedError(String),
}

impl From<&str> for ParseErr {
    fn from(value: &str) -> Self {
        #[allow(deprecated)]
        Self::Others(value.into())
    }
}

impl From<std::string::String> for ParseErr {
    fn from(value: std::string::String) -> Self {
        #[allow(deprecated)]
        Self::Others(value.into())
    }
}

impl From<Vec<chumsky::error::Simple<char>>> for ParseErr {
    fn from(value: Vec<chumsky::error::Simple<char>>) -> Self {
        Self::BadExpression(ExprErr::BadExprParse(value.into_boxed_slice()))
    }
}

impl Clone for ParseErr {
    fn clone(&self) -> Self {
        match self {
            Self::BadPkgQCond(cond) => Self::BadPkgQCond(cond.clone()),
            Self::UnknownPreamble(a, b) => Self::UnknownPreamble(*a, b.clone()),
            Self::Duplicate(a, b) => Self::Duplicate(*a, b.clone()),
            Self::UnknownModifier(a, b) => Self::UnknownModifier(*a, b.clone()),
            Self::MacroNotFound(m) => Self::MacroNotFound(m.clone()),
            Self::MacroUndefined(m) => Self::MacroUndefined(m.clone()),
            Self::BadExpression(e) => Self::BadExpression(e.clone()),
            /*
            cannot move out of a shared reference
            move occurs because value has type `std::io::Error`, which does not implement the `Copy` trait            */
            // Self::IoError(e) => Self::IoError(e),
            Self::LuaError(e) => Self::LuaError(e.clone()),
            #[allow(deprecated)]
            Self::Others(r) => {
                tracing::warn!("Cloning ParserError::Others(color_eyre::Report):\n{r:#}");
                // ignore deprecation
                #[allow(deprecated)]
                Self::Others(r.clone())
            },
            _ => unimplemented!(),
        }
    }
}

/// Errors during parsing expressions
#[derive(Debug, Clone, thiserror::Error)]
pub enum ExprErr {
    /// Cannot perform this operation with a number
    #[error("Cannot perform this operation on a number: {0:#}")]
    NotNum(Box<Expression>),
    /// Cannot perform addition with this kind of expression
    #[error("Cannot perform addition on this kind of expression: {0:#}")]
    NoAdd(Box<Expression>),
    /// Cannot perform multiplication and division this kind of expression
    #[error("Cannot perform multiplication and division on this kind of expression: {0:#}")]
    NoMulDiv(Box<Expression>),
    /// The types of the 2 expressions do not match
    #[error("The types of the 2 expressions do not match: `{0:#}` and `{1:#}`")]
    TypeMismatch(Box<Expression>, Box<Expression>),
    /// Error when parsing macros
    #[error("In `%[expr]`, found macro expansion error: {0:#}")]
    MacroErr(Box<ParseErr>),
    /// Error when parsing %[] from Chumsky
    #[error("Cannot parse expression: {0:?}")]
    BadExprParse(Box<[Simple<char>]>),

    #[error("Error: {0}")]
    Others(String),

    #[error("Cannot parse epoch: {0}")]
    EpochParse(ParseIntError),
}

impl From<ParseErr> for ExprErr {
    fn from(value: ParseErr) -> Self {
        Self::MacroErr(Box::new(value))
    }
}

impl From<&str> for ExprErr {
    fn from(value: &str) -> Self {
        Self::Others(value.into())
    }
}
// impl from any asref string

impl From<String> for ExprErr {
    fn from(value: String) -> Self {
        Self::Others(value)
    }
}

impl From<std::string::String> for ExprErr {
    fn from(value: std::string::String) -> Self {
        Self::Others(value.into())
    }
}

impl From<Vec<Simple<char>>> for ExprErr {
    fn from(value: Vec<Simple<char>>) -> Self {
        Self::BadExprParse(value.into_boxed_slice())
    }
}

#[macro_export]
macro_rules! syntaxerr {
    (#notes) => {vec![]};
    (#notes [$($note:expr),*$(,)?]) => {
        vec![$((None, Box::new($note))),*]
    };
    (~$kind:ident$tt:tt@$span:expr$(=>[ $($note:expr),*$(,)? ])?) => {
        $crate::error::ParseErr::InvalidSyntax {
            span: $span,
            err: $crate::error::SpecErr::$kind$tt,
            notes: syntaxerr!(#notes $([$($note),*])?),
        }
    };
    ($kind:ident$tt:tt@$span:expr$(=>[ $($note:expr),*$(,)? ])?) => {
        return Err(syntaxerr!(~$kind$tt@$span$(=>[$($note),*])?))
    };
}
