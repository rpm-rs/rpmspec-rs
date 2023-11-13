//! RPM expression parser
//!
//! RPM expressions are expressions used in spec files with the `%[]` notation,
//! composed of different operators and 3 data types: strings, integers and RPM versions.
use chumsky::prelude::*;
use color_eyre::eyre::eyre;
use rpmspec_common::expr::{Expression, Version};
use rpmspec_common::{ExprErr as Err, PErr as PE};
use smartstring::alias::String;
use std::str::FromStr;

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
    pub fn eval(self, sp: &mut impl FnMut(&mut String, String) -> Result<(), PE>) -> Result<Expression, Err> {
        gen_chk!($, sp);
        match self {
            Self::Out(expr) => Ok(expr),
            Self::Macro(m) => {
                let mut out = String::new();
                sp(&mut out, m)?;
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
