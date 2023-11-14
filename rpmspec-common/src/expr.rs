use crate::error::ExprErr;
use color_eyre::eyre::eyre;
use smartstring::alias::String;
use std::str::FromStr;

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
    type Err = ExprErr;
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
