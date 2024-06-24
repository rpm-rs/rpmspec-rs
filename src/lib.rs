//! # rpmspec-rs
//! RPM Spec parser in Rust
//!
//! RPMs are built from sources using a spec file. The spec file
//! contains information on how to build the package, what files to include,
//! and what dependencies are required.
//!
//! RPMs make use of macros, which are evaluated at build time. Macros are
//! defined in the spec files and various other files in the macros directory.
//! They are also picked up from ~/.rpmrc and /etc/rpmrc.
//!
#![warn(clippy::disallowed_types)]
#![warn(missing_docs)]
#![warn(clippy::complexity)]
#![warn(clippy::correctness)]
#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![warn(clippy::perf)]
#![warn(clippy::style)]
#![warn(clippy::suspicious)]
#![allow(clippy::equatable_if_let)]
// followings are from clippy::restriction
#![warn(clippy::missing_errors_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::missing_safety_doc)]
#![warn(clippy::unwrap_used)]
#![warn(clippy::expect_used)]
#![warn(clippy::format_push_string)]
#![warn(clippy::get_unwrap)]
#![warn(clippy::format_push_string)]
#![allow(clippy::missing_inline_in_public_items)]
#![allow(clippy::implicit_return)]
#![allow(clippy::blanket_clippy_restriction_lints)]
#![allow(clippy::pattern_type_mismatch)]

pub mod parse;
#[macro_use]
pub mod lua;
pub mod macros;

pub use parse::RPMSpec;
pub use rpmspec_common::{error, util};

pub(crate) mod macrohelpers;
pub(crate) use macrohelpers::preamble_maker;
