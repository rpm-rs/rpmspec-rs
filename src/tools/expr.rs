//! RPM expression parser
//!
//! RPM expressions are expressions used in spec files with the `%[]` notation,
//! composed of different operators and 3 data types: strings, integers and RPM versions.
use crate::{error::Err as PE, parse::SpecParser, util::Consumer};
use chumsky::prelude::*;
use color_eyre::eyre::eyre;
use smartstring::alias::String;
use std::str::FromStr;
use std::sync::Arc;

/// Errors during parsing expressions
#[derive(Debug, Clone, thiserror::Error)]
pub enum Err {
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
	MacroErr(Box<crate::error::Err>),
	/// Error when parsing %[] from Chumsky
	#[error("Cannot parse expression: {0:?}")]
	BadExprParse(Box<[Simple<char>]>),
}

impl From<crate::error::Err> for Err {
	fn from(value: PE) -> Self {
		Self::MacroErr(Box::new(value))
	}
}

impl From<Vec<Simple<char>>> for Err {
	fn from(value: Vec<Simple<char>>) -> Self {
		Self::BadExprParse(value.into_boxed_slice())
	}
}

impl From<color_eyre::Report> for Err {
	fn from(value: color_eyre::Report) -> Self {
		Self::from(crate::error::Err::from(value))
	}
}

/// Version in RPM spec
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
	/// Epoch is just epoch
	pub epoch: u32,
	/// Version
	pub ver: String,
	/// Release
	pub rel: Option<String>,
}

impl std::fmt::Display for Version {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.epoch != 0 {
			f.write_fmt(format_args!("{}:", self.epoch))?;
		}
		f.write_str(&self.ver)?;
		if let Some(r) = &self.rel {
			f.write_fmt(format_args!("-{r}"))?;
		}
		Ok(())
	}
}

impl FromStr for Version {
	type Err = Err;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut evr = Self::default();
		let mut last = String::new();
		for ch in s.chars() {
			if ch == ':' {
				match last.parse() {
					Ok(e) => evr.epoch = e,
					Err(e) => return Err(eyre!("Cannot parse epoch: {e:#}").into()),
				}
				last.clear();
				continue;
			}
			if ch == '-' {
				if !evr.ver.is_empty() {
					return Err(eyre!("Unexpected double `-` in version: `{}-{last:?}-`", evr.ver).into());
				}
				evr.ver = std::mem::take(&mut last);
			}
			if !ch.is_ascii_alphanumeric() && !"^~.".contains(ch) {
				return Err(eyre!("Unexpected character `{ch}` in evr").into());
			}
			last.push(ch);
		}
		if evr.ver.is_empty() {
			evr.ver = last;
		} else {
			evr.rel = Some(last);
		}
		Ok(evr)
	}
}

/// Denotes the output of an expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
	/// Numbers (64-bit signed integer)
	Num(i64),
	/// Strings
	Text(String),
	/// RPM [`Version`]
	Ver(Version),
}

impl Expression {
	/// Returns `true` if the expression is [`Num`].
	///
	/// [`Num`]: Expression::Num
	#[must_use]
	pub const fn is_num(&self) -> bool {
		matches!(self, Self::Num(..))
	}

	/// Returns `true` if the expression is [`Text`].
	///
	/// [`Text`]: Expression::Text
	#[must_use]
	pub const fn is_text(&self) -> bool {
		matches!(self, Self::Text(..))
	}

	/// Returns `true` if the expression is [`Ver`].
	///
	/// [`Ver`]: Expression::Ver
	#[must_use]
	pub const fn is_ver(&self) -> bool {
		matches!(self, Self::Ver(..))
	}

	/// Returns the inner value of [`Num`].
	///
	/// # Errors
	/// The expression is not of item [`Num`].
	///
	/// [`Num`]: Expression::Num
	pub fn try_into_num(self) -> Result<i64, Self> {
		if let Self::Num(v) = self {
			Ok(v)
		} else {
			Err(self)
		}
	}

	/// Returns the inner value of [`Text`].
	///
	/// # Errors
	/// The expression is not of item [`Text`].
	///
	/// [`Text`]: Expression::Text
	pub fn try_into_text(self) -> Result<String, Self> {
		if let Self::Text(v) = self {
			Ok(v)
		} else {
			Err(self)
		}
	}

	/// Returns the inner value of [`Ver`].
	///
	/// # Errors
	/// The expression is not of item [`Ver`].
	///
	/// [`Ver`]: Expression::Ver
	pub fn try_into_ver(self) -> Result<Version, Self> {
		if let Self::Ver(v) = self {
			Ok(v)
		} else {
			Err(self)
		}
	}

	/// Returns the inner value of [`Num`] if it is one.
	///
	/// [`Num`]: Expression::Num
	#[must_use]
	pub const fn as_num(&self) -> Option<&i64> {
		if let Self::Num(v) = self {
			Some(v)
		} else {
			None
		}
	}

	/// Returns the inner value of [`Text`] if it is one.
	///
	/// [`Text`]: Expression::Text
	#[must_use]
	pub const fn as_text(&self) -> Option<&String> {
		if let Self::Text(v) = self {
			Some(v)
		} else {
			None
		}
	}

	/// Returns the inner value of [`Ver`] if it is one.
	///
	/// [`Ver`]: Expression::Ver
	#[must_use]
	pub const fn as_ver(&self) -> Option<&Version> {
		if let Self::Ver(v) = self {
			Some(v)
		} else {
			None
		}
	}
}

impl std::fmt::Display for Expression {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Num(n) => n.fmt(f),
			Self::Text(s) => s.fmt(f),
			Self::Ver(v) => v.fmt(f),
		}
	}
}

/// Represents an expression (`%[...]`)
/// See <https://rpm-software-management.github.io/rpm/manual/macros.html#expression-expansion>
#[derive(Clone)]
pub enum Expr {
	/// Literals
	Out(Expression),

	/// Represents an RPM macro
	Macro(String),

	/// `-...`
	Neg(Box<Expr>),
	/// `!...`
	Not(Box<Expr>),
	/// `... && ...`
	And(Box<Expr>, Box<Expr>),
	/// `... || ...`
	Or(Box<Expr>, Box<Expr>),

	/// `... != ...`
	Ne(Box<Expr>, Box<Expr>),
	/// `... == ...`
	Eq(Box<Expr>, Box<Expr>),
	/// `... < ...`
	Lt(Box<Expr>, Box<Expr>),
	/// `... > ...`
	Gt(Box<Expr>, Box<Expr>),
	/// `... <= ...`
	Le(Box<Expr>, Box<Expr>),
	/// `... >= ...`
	Ge(Box<Expr>, Box<Expr>),

	/// `... + ...`
	Add(Box<Expr>, Box<Expr>),
	/// `... - ...`
	Sub(Box<Expr>, Box<Expr>),
	/// `... / ...`
	Div(Box<Expr>, Box<Expr>),
	/// `... * ...`
	Mul(Box<Expr>, Box<Expr>),

	/// `... ? ... : ...`
	Ter(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[rustfmt::skip]
macro_rules! gen_chk {
	($dollar:tt, $sp:ident) => {
		macro_rules! typed_chk {
			($a:ident:$l:ident $b:ident:$r:ident => $e:expr) => {{
				let $a = $a.eval($sp)?;
				Ok(match $a {
					Expression::Ver($l) => {
						let $b = $b.eval($sp)?;
						let Expression::Ver($r) = $b else {
							return Err(Err::TypeMismatch(Box::new(Expression::Ver($l)), Box::new($b)));
						};
						Expression::Num(i64::from($e))
					}
					Expression::Num($l) => {
						let $b = $b.eval($sp)?;
						let Expression::Num($r) = $b else {
							return Err(Err::TypeMismatch(Box::new($a), Box::new($b)));
						};
						Expression::Num(i64::from($e))
					}
					Expression::Text($l) => {
						let $b = $b.eval($sp)?;
						let Expression::Text($r) = $b else {
							return Err(Err::TypeMismatch(Box::new(Expression::Text($l)), Box::new($b)));
						};
						Expression::Num(i64::from($e))
					}
				})
			}};
		}
		macro_rules! eval_type_chk {
			($a:ident, $b:ident) => {
				let $a = $a.eval($sp)?;
				let $b = $b.eval($sp)?;
				match $a {
					Expression::Num(_) => {
						let Expression::Num(_) = $b else {
							return Err(Err::TypeMismatch(Box::new($a), Box::new($b)));
						};
					}
					Expression::Text(_) => {
						let Expression::Text(_) = $b else {
							return Err(Err::TypeMismatch(Box::new($a), Box::new($b)));
						};
					}
					Expression::Ver(_) => {
						let Expression::Ver(_) = $b else {
							return Err(Err::TypeMismatch(Box::new($a), Box::new($b)));
						};
					}
				}
			};
		}
		macro_rules! give {
			// * Escaping `$` sign for nested macros
			// Apparently `macro_rules` parses `$($e:expr)` while evaluating `gen_chk` and thinks
			// that `$e` doesn't exist, so the code actually doesn't compile. What a design flaw!
			// This is honestly a design flaw but I guess the rust developers are just stubborn and
			// insist on calling this bug a feature. Guess that's understandable, have a great day!
			// Anyway, to fix this we need to use `$dollar:tt` and call `gen_chk` with `$` so it
			// expands correctly; also we can't use `$$` yet as the feature is still in nightly.
			// See: https://veykril.github.io/tlborm/decl-macros/minutiae/metavar-expr.html
			// * Why `@else` expands to `unreachable!()`
			// The 2 `@else ...` branches here are used for checking if the `$()?` metavar is
			// given. It goes to the second branch if provided, otherwise the first branch.
			(@else) => { unreachable!() };
			(@else $et:ident : $dollar($e:expr),+) => {
				return Err(Err::$et($dollar(Box::new($e)),+));
			};
			($t:ident($x:ident) = $y:ident $dollar(else $et:ident : $dollar($e:expr),+)?) => {
				//                         -------^^^^^^^^^^^^^^^^^^*******^^^^^^^^^^^^^
				// This is optional, and if omitted, it should expand to `unreachable!();`
				// when expanding `gen_chk` with `$dollar = $`:
				// $(else $et:ident : $($e:expr),+)?
				// -^^^^^^^^^^^^^^^^^^*^^^^^^^^^^^^^
				let Expression::$t($x) = $y else {
					give!(@else $dollar($et : $dollar($e),+)?);
				};
			};
		}
	};
}

impl Expr {
	/// Returns a parser than can take in a &str and returns [`Expr`].
	///
	/// # Examples
	/// ```
	/// use rpmspec_rs::tools::expr::Expr;
	///
	/// assert_eq!(Expr::parser().parse("1 + 1").and_then(|x| x.eval()), Ok(2));
	/// ```
	///
	/// # Panics
	/// - Cannot parse identified integers (0% chance of happening)
	#[must_use]
	pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
		recursive(|expr| {
			let n = text::int(10).map(|s: std::string::String| Self::Out(Expression::Num(s.parse().unwrap()))).padded();
			let atom = n.or(expr.delimited_by(just('('), just(')')));
			let op = |c| just(c).padded();

			let string = just('"')
				.ignore_then(none_of('"').repeated())
				.then_ignore(just('"'))
				.collect::<Vec<_>>()
				.map(|v| {
					let mut s = String::new();
					v.into_iter().for_each(|c| s.push(c));
					Self::Out(Expression::Text(s))
				})
				.padded();

			let ver = just("v\"")
				.ignore_then(none_of('"').repeated())
				.then_ignore(just('"'))
				.collect::<Vec<_>>()
				.try_map(|v, span| {
					Ok(Self::Out(Expression::Ver(Version::from_str(&v.into_iter().collect::<String>()).map_err(|e| Simple::custom(span, format!("Error when parsing version: {e:?}")))?)))
				})
				.padded();

			let atom = atom.or(string).or(ver);

			let macros = just('%')
				.ignore_then(
					text::ident().or(text::ident().delimited_by(just('{'), just('}'))).or(text::ident().delimited_by(just('['), just(']'))).or(text::ident().delimited_by(just('('), just(')'))),
				)
				.map(String::from)
				.map(Expr::Macro as fn(_) -> _);

			let atom = atom.or(macros);

			let unary = op('-').repeated().then(atom.clone()).foldr(|_, r| Self::Neg(Box::new(r))).or(op('!').repeated().then(atom).foldr(|_, r| Self::Not(Box::new(r))));
			let muldiv = unary.clone().then(op('*').to(Expr::Mul as fn(_, _) -> _).or(op('/').to(Expr::Div as fn(_, _) -> _)).then(unary).repeated()).foldl(|l, (op, r)| op(Box::new(l), Box::new(r)));
			let addsub =
				muldiv.clone().then(op('+').to(Expr::Add as fn(_, _) -> _).or(op('-').to(Expr::Sub as fn(_, _) -> _)).then(muldiv).repeated()).foldl(|l, (op, r)| op(Box::new(l), Box::new(r)));
			macro_rules! cmpop {
				($s:expr, $item:expr $(;$ss:expr, $ii:expr)*) => {{
					just($s).padded().to($item as fn(_, _) -> _)
					$(.or(cmpop!($ss, $ii)))*
				}};
			}
			let cmp = addsub
				.clone()
				.then(cmpop!("!=", Expr::Ne; "==", Expr::Eq; '<', Expr::Lt; '>', Expr::Gt; "<=", Expr::Le; ">=", Expr::Ge).then(addsub).repeated())
				.foldl(|l, (op, r)| op(Box::new(l), Box::new(r)));
			cmp.clone().then(cmpop!("&&", Expr::And; "||", Expr::Or).then(cmp).repeated()).foldl(|l, (op, r)| op(Box::new(l), Box::new(r)))
		})
		.then_ignore(end())
	}
	/// Evaluates an [`Expr`] to [`Expression`].
	///
	/// # Examples
	///
	/// ```
	/// use rpmspec_rs::tools::expr::{Expr, Expression};
	///
	/// let expr = Expr::Out(Expression::Text("hai"));
	/// assert_eq!(expr.eval(), Ok(Expression::Text("hai")));
	/// ```
	///
	/// # Errors
	/// Invalid expression
	#[allow(clippy::cognitive_complexity)] // weird to split it apart
	pub fn eval(self, sp: &mut SpecParser) -> Result<Expression, Err> {
		gen_chk!($, sp);
		match self {
			Self::Out(expr) => Ok(expr),
			Self::Macro(m) => {
				let mut out = String::new();
				// any type will do
				sp._use_raw_macro::<std::fs::File>(&mut out, &mut Consumer::new(Arc::new(parking_lot::RwLock::new(m)), None, Arc::from(std::path::Path::new("<expr>"))))?;
				match Self::parser().parse(&*out)? {
					Self::Out(x) => Ok(x),
					_ => Err(eyre!("Bad Expression: `{out}`").into()),
				}
			},
			Self::Neg(num) => {
				let num = num.eval(sp)?;
				give!(Num(n) = num else NotNum: num);
				Ok(Expression::Num(-n))
			},
			Self::Not(num) => Ok(Expression::Num(match num.eval(sp)? {
				Expression::Ver(_) => 1,
				Expression::Num(n) => i64::from(n == 0),
				Expression::Text(s) => i64::from(s.is_empty()),
			})),
			Self::And(a, b) => {
				eval_type_chk!(a, b);
				Ok(match a {
					Expression::Ver(_) => a,
					Expression::Num(l) => {
						give!(Num(r) = b);
						Expression::Num(if l != 0 && r != 0 { r } else { 0 })
					},
					Expression::Text(l) => {
						give!(Text(r) = b);
						Expression::Text(if !l.is_empty() && !r.is_empty() { r } else { "".into() })
					},
				})
			},
			Self::Or(a, b) => {
				eval_type_chk!(a, b);
				Ok(match a {
					Expression::Ver(_) => b,
					Expression::Num(l) => {
						give!(Num(r) = b);
						Expression::Num(if l != 0 {
							l
						} else if r != 0 {
							r
						} else {
							0
						})
					},
					Expression::Text(l) => {
						give!(Text(r) = b);
						Expression::Text(if !l.is_empty() {
							l
						} else if !r.is_empty() {
							r
						} else {
							"".into()
						})
					},
				})
			},
			Self::Ne(a, b) => typed_chk!(a:l b:r => l != r),
			Self::Eq(a, b) => typed_chk!(a:l b:r => l == r),
			Self::Lt(a, b) => typed_chk!(a:l b:r => l < r),
			Self::Gt(a, b) => typed_chk!(a:l b:r => l > r),
			Self::Le(a, b) => typed_chk!(a:l b:r => l <= r),
			Self::Ge(a, b) => typed_chk!(a:l b:r => l >= r),
			Self::Add(a, b) => {
				eval_type_chk!(a, b);
				if let Expression::Num(l) = a {
					give!(Num(r) = b);
					return Ok(Expression::Num(l + r));
				}
				let Expression::Text(l) = a else { return Err(Err::NoAdd(Box::new(a))) };
				give!(Text(r) = b);
				Ok(Expression::Text(format!("{l}{r}").into()))
			},
			Self::Sub(a, b) => Self::Add(a, Box::new(Self::Neg(b))).eval(sp),
			Self::Div(a, b) => {
				eval_type_chk!(a, b);
				give!(Num(l) = a else NoMulDiv: a);
				give!(Num(r) = b);
				Ok(Expression::Num(l / r))
			},
			Self::Mul(a, b) => {
				eval_type_chk!(a, b);
				give!(Num(l) = a else NoMulDiv: a);
				give!(Num(r) = b);
				Ok(Expression::Num(l * r))
			},
			Self::Ter(cond, yes, no) => {
				let cond = match cond.eval(sp)? {
					Expression::Num(n) => n != 0,
					Expression::Text(s) => !s.is_empty(),
					Expression::Ver(_) => false,
				};
				eval_type_chk!(yes, no);
				Ok(if cond { yes } else { no })
			},
		}
	}
}

