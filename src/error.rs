//! [`ParserError`] used in rpmspec-rs.
//! Yes. You heard me. The only error is [`ParserError`] and everything else is
//! unfortunately String.
use smartstring::alias::String;
use thiserror::Error;

/// Errors for some special parsing issues
#[derive(Debug, Error)]
#[allow(clippy::module_name_repetitions)]
pub enum ParserError {
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
	BadExpression(crate::tools::expr::ExprErr),
	/// A color_eyre::Report. Some sort of syntax error.
	#[error("{0:#}")]
	Others(color_eyre::Report),
}

impl From<color_eyre::Report> for ParserError {
	fn from(value: color_eyre::Report) -> Self {
		Self::Others(value)
	}
}

impl From<rlua::Error> for ParserError {
	fn from(value: rlua::Error) -> Self {
		Self::Others(color_eyre::eyre::eyre!(value))
	}
}

impl From<std::io::Error> for ParserError {
	fn from(value: std::io::Error) -> Self {
		Self::Others(color_eyre::eyre::eyre!(value))
	}
}

impl From<crate::tools::expr::ExprErr> for ParserError {
	fn from(value: crate::tools::expr::ExprErr) -> Self {
		Self::BadExpression(value)
	}
}

impl From<Vec<chumsky::error::Simple<char>>> for ParserError {
	fn from(value: Vec<chumsky::error::Simple<char>>) -> Self {
		Self::BadExpression(crate::tools::expr::ExprErr::BadExprParse(value.into_boxed_slice()))
	}
}

impl Clone for ParserError {
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
			}
		}
	}
}
