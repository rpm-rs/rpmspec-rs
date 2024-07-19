pub mod error;
pub mod expr;
pub mod util;

pub use error::ExprErr;
pub use error::ParseErr as PErr;

// pub type ParseResult<T> = Result<T, PErr>;
// pub type ExprResult<T> = Result<T, ExprErr>;
pub use error::{ExprResult, ParseResult};
