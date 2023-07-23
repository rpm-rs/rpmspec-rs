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
	/// A color_eyre::Report. Some sort of syntax error.
	#[error("{0:#}")]
	Others(color_eyre::Report),
}

impl Clone for ParserError {
	fn clone(&self) -> Self {
		match self {
			Self::BadPkgQCond(cond) => Self::BadPkgQCond(cond.clone()),
			Self::UnknownPreamble(a, b) => Self::UnknownPreamble(*a, b.clone()),
			Self::Duplicate(a, b) => Self::Duplicate(*a, b.clone()),
			Self::UnknownModifier(a, b) => Self::UnknownModifier(*a, b.clone()),
			Self::Others(r) => {
				tracing::warn!("Cloning ParserError::Others(color_eyre::Report):\n{r:#}");
				Self::Others(color_eyre::eyre::eyre!(r.to_string()))
			}
		}
	}
}
