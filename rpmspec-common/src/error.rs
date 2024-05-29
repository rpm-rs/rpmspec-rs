//! [`ParserError`] used in rpmspec-rs.
//! Yes. You heard me. The only error is [`ParserError`] and everything else is
//! unfortunately String.
use crate::expr::Expression;
use chumsky::prelude::Simple;
use smartstring::alias::String;
use thiserror::Error;

/// Errors for some special parsing issues
#[derive(Debug, Error)]
#[allow(clippy::module_name_repetitions)]
pub enum ParseErr {
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
    /// A color_eyre::Report. Some sort of syntax error.
    #[error("{0:#}")]
    Others(color_eyre::Report),
}

impl From<color_eyre::Report> for ParseErr {
    fn from(value: color_eyre::Report) -> Self {
        Self::Others(value)
    }
}

impl From<mlua::Error> for ParseErr {
    fn from(value: mlua::Error) -> Self {
        Self::Others(color_eyre::eyre::eyre!(value))
    }
}

impl From<std::io::Error> for ParseErr {
    fn from(value: std::io::Error) -> Self {
        Self::Others(color_eyre::eyre::eyre!(value))
    }
}

impl From<ExprErr> for ParseErr {
    fn from(value: ExprErr) -> Self {
        Self::BadExpression(value)
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
            Self::Others(r) => {
                tracing::warn!("Cloning ParserError::Others(color_eyre::Report):\n{r:#}");
                Self::Others(color_eyre::eyre::eyre!(r.to_string()))
            },
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
}

impl From<ParseErr> for ExprErr {
    fn from(value: ParseErr) -> Self {
        Self::MacroErr(Box::new(value))
    }
}

impl From<Vec<Simple<char>>> for ExprErr {
    fn from(value: Vec<Simple<char>>) -> Self {
        Self::BadExprParse(value.into_boxed_slice())
    }
}

impl From<color_eyre::Report> for ExprErr {
    fn from(value: color_eyre::Report) -> Self {
        Self::from(ParseErr::from(value))
    }
}
