//! RPM expression parser
//!
//! RPM expressions are expressions used in spec files with the `%[]` notation,
//! composed of different operators and 3 data types: strings, integers and RPM versions.
use std::sync::Arc;

use chumsky::prelude::*;
use smartstring::alias::String;

use crate::{error::ParserError, parse::SpecParser, util::Consumer};

/// Errors during parsing expressions
#[derive(Debug, Clone)]
pub enum ExprErr {
	/// Cannot perform this operation with a number
	NotNum(Expression),
	/// Cannot perform addition with this kind of expression
	NoAdd(Expression),
	/// Cannot perform multiplication and division this kind of expression
	NoMulDiv(Expression),
	/// The types of the 2 expressions do not match
	TypeMismatch(Expression, Expression),
	/// Error when parsing macros
	MacroErr(Box<crate::error::ParserError>),
	/// Error when parsing %[] from Chumsky
	BadExprParse(Box<[Simple<char>]>),
}

impl From<ParserError> for ExprErr {
	fn from(value: ParserError) -> Self {
		Self::MacroErr(Box::new(value))
	}
}

impl From<Vec<Simple<char>>> for ExprErr {
	fn from(value: Vec<Simple<char>>) -> Self {
		Self::BadExprParse(value.into_boxed_slice())
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

impl Expr {
	/// Returns a parser than can take in a &str and returns [`Expr`].
	///
	///
	/// # Examples
	///
	/// ```
	/// use rpmspec_rs::tools::expr::Expr;
	///
	/// assert_eq!(Expr::parser().parse("1 + 1").and_then(|x| x.eval()), Ok(2));
	/// ```
	pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
		recursive(|expr| {
			let n = text::int(10).map(|s: std::string::String| Expr::Out(Expression::Num(s.parse().unwrap()))).padded();
			let atom = n.or(expr.delimited_by(just('('), just(')')));
			let op = |c| just(c).padded();

			let string = just('"')
				.ignore_then(none_of('"').repeated())
				.then_ignore(just('"'))
				.collect::<Vec<_>>()
				.map(|v| {
					let mut s = String::new();
					v.into_iter().for_each(|c| s.push(c));
					Expr::Out(Expression::Text(s))
				})
				.padded();

			let ver = just("v\"")
				.ignore_then(none_of('"').repeated())
				.then_ignore(just('"'))
				.collect::<Vec<_>>()
				.try_map(|v, span| {
					let mut evr = Version::default();
					let mut last = String::new();
					for ch in &v {
						let ch = *ch;
						if ch == ':' {
							match last.parse() {
								Ok(e) => evr.epoch = e,
								Err(e) => return Err(Simple::custom(span, format!("Cannot parse epoch: {e:#}"))),
							}
							last.clear();
							continue;
						}
						if ch == '-' {
							if !evr.ver.is_empty() {
								return Err(Simple::custom(span, format!("Unexpected double `-` in version: {v:?}")));
							}
							evr.ver = std::mem::take(&mut last);
						}
						if !ch.is_ascii_alphanumeric() && !"^~.".contains(ch) {
							return Err(Simple::custom(span, format!("Unexpected character `{ch}` in evr")));
						}
						last.push(ch);
					}
					if evr.ver.is_empty() {
						evr.ver = last;
					} else {
						evr.rel = Some(last);
					}
					Ok(Expr::Out(Expression::Ver(evr)))
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

			let unary = op('-').repeated().then(atom.clone()).foldr(|_, r| Expr::Neg(Box::new(r))).or(op('!').repeated().then(atom).foldr(|_, r| Expr::Not(Box::new(r))));
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
	pub fn eval(self, sp: &mut SpecParser) -> Result<Expression, ExprErr> {
		#[rustfmt::skip]
		macro_rules! typed_chk {
			($a:ident:$l:ident $b:ident:$r:ident => $e:expr) => {{
				let $a = $a.eval(sp)?;
				Ok(match $a {
					Expression::Ver($l) => {
						let $b = $b.eval(sp)?;
						let Expression::Ver($r) = $b else {
							return Err(ExprErr::TypeMismatch(Expression::Ver($l), $b));
						};
						Expression::Num(i64::from($e))
					}
					Expression::Num($l) => {
						let $b = $b.eval(sp)?;
						let Expression::Num($r) = $b else {
							return Err(ExprErr::TypeMismatch($a, $b));
						};
						Expression::Num(i64::from($e))
					}
					Expression::Text($l) => {
						let $b = $b.eval(sp)?;
						let Expression::Text($r) = $b else {
							return Err(ExprErr::TypeMismatch(Expression::Text($l), $b));
						};
						Expression::Num(i64::from($e))
					}
				})
			}};
		}
		#[rustfmt::skip]
		macro_rules! eval_type_chk {
			($a:ident, $b:ident) => {
				let $a = $a.eval(sp)?;
				let $b = $b.eval(sp)?;
				match $a {
					Expression::Num(_) => {
						let Expression::Num(_) = $b else {
							return Err(ExprErr::TypeMismatch($a, $b));
						};
					}
					Expression::Text(_) => {
						let Expression::Text(_) = $b else {
							return Err(ExprErr::TypeMismatch($a, $b));
						};
					}
					Expression::Ver(_) => {
						let Expression::Ver(_) = $b else {
							return Err(ExprErr::TypeMismatch($a, $b));
						};
					}
				}
			};
		}
		match self {
			Self::Out(expr) => Ok(expr),
			Self::Macro(m) => {
				let mut out = String::new();
				// any type will do
				let mut csm: Consumer<std::fs::File> = Consumer::new(Arc::new(parking_lot::Mutex::new(m)), None, Arc::from(std::path::Path::new("<expr>")));
				sp._use_raw_macro(&mut out, &mut csm)?;
				match Self::parser().parse(&*out)? {
					Self::Out(x) => Ok(x),
					_ => Err(ExprErr::MacroErr(Box::new(ParserError::Others(color_eyre::eyre::eyre!("Bad Expression: `{out}`"))))),
				}
			}
			Self::Neg(num) => {
				let num = num.eval(sp)?;
				let Expression::Num(n) = num else {
					return Err(ExprErr::NotNum(num));
				};
				return Ok(Expression::Num(-n));
			}
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
						let Expression::Num(r) = b else { unreachable!() };
						Expression::Num(if l != 0 && r != 0 { r } else { 0 })
					}
					Expression::Text(l) => {
						let Expression::Text(r) = b else { unreachable!() };
						Expression::Text(if !l.is_empty() && !r.is_empty() { r } else { "".into() })
					}
				})
			}
			Self::Or(a, b) => {
				eval_type_chk!(a, b);
				Ok(match a {
					Expression::Ver(_) => b,
					Expression::Num(l) => {
						let Expression::Num(r) = b else { unreachable!() };
						Expression::Num(if l != 0 {
							l
						} else if r != 0 {
							r
						} else {
							0
						})
					}
					Expression::Text(l) => {
						let Expression::Text(r) = b else { unreachable!() };
						Expression::Text(if !l.is_empty() {
							l
						} else if !r.is_empty() {
							r
						} else {
							"".into()
						})
					}
				})
			}
			Self::Ne(a, b) => typed_chk!(a:l b:r => l != r),
			Self::Eq(a, b) => typed_chk!(a:l b:r => l == r),
			Self::Lt(a, b) => typed_chk!(a:l b:r => l < r),
			Self::Gt(a, b) => typed_chk!(a:l b:r => l > r),
			Self::Le(a, b) => typed_chk!(a:l b:r => l <= r),
			Self::Ge(a, b) => typed_chk!(a:l b:r => l >= r),
			Self::Add(a, b) => {
				let a = a.eval(sp)?;
				let b = b.eval(sp)?;
				if let Expression::Num(l) = a {
					let Expression::Num(r) = b else {
						return Err(ExprErr::TypeMismatch(a, b));
					};
					Ok(Expression::Num(l + r))
				} else if let Expression::Text(l) = a {
					let Expression::Text(r) = b else {
						return Err(ExprErr::TypeMismatch(Expression::Text(l), b));
					};
					Ok(Expression::Text(format!("{l}{r}").into()))
				} else {
					Err(ExprErr::NoAdd(a))
				}
			}
			Self::Sub(a, b) => Expr::Add(a, Box::new(Expr::Neg(b))).eval(sp),
			Self::Div(a, b) => {
				let a = a.eval(sp)?;
				let b = b.eval(sp)?;
				if let Expression::Num(l) = a {
					let Expression::Num(r) = b else {
						return Err(ExprErr::TypeMismatch(a, b));
					};
					Ok(Expression::Num(l / r))
				} else {
					Err(ExprErr::NoMulDiv(a))
				}
			}
			Self::Mul(a, b) => {
				let a = a.eval(sp)?;
				let b = b.eval(sp)?;
				if let Expression::Num(l) = a {
					let Expression::Num(r) = b else {
						return Err(ExprErr::TypeMismatch(a, b));
					};
					Ok(Expression::Num(l * r))
				} else {
					Err(ExprErr::NoMulDiv(a))
				}
			}
			Self::Ter(cond, yes, no) => {
				let cond = match cond.eval(sp)? {
					Expression::Num(n) => n != 0,
					Expression::Text(s) => !s.is_empty(),
					Expression::Ver(_) => false,
				};
				let yes = yes.eval(sp)?;
				let no = no.eval(sp)?;
				match yes {
					Expression::Num(_) => {
						if !matches!(no, Expression::Num(_)) {
							return Err(ExprErr::TypeMismatch(yes, no));
						}
					}
					Expression::Text(_) => {
						if !matches!(no, Expression::Text(_)) {
							return Err(ExprErr::TypeMismatch(yes, no));
						}
					}
					Expression::Ver(_) => {
						if !matches!(no, Expression::Ver(_)) {
							return Err(ExprErr::TypeMismatch(yes, no));
						}
					}
				}
				Ok(if cond { yes } else { no })
			}
		}
	}
}
