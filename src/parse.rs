//! Parser for rpmspec. See [`SpecParser`].
use crate::macros::MacroType;
use crate::util::{textproc, Consumer};
use color_eyre::{eyre::eyre, Help, Result, SectionExt};
use itertools::Itertools;
use lazy_format::lazy_format as lzf;
use parking_lot::RwLock;
use regex::Regex;
use rpmspec_common::{gen_read_helper, PErr as Err};
use smartstring::alias::String;
use std::{
    collections::HashMap,
    io::{BufReader, Read},
    mem::take,
    num::ParseIntError,
    path::Path,
    process::Command,
    str::FromStr,
    sync::Arc,
};
use tracing::{debug, error, trace, warn};

const PKGNAMECHARSET: &str = "_-().";
lazy_static::lazy_static! {
    static ref RE_PQC:	Regex = Regex::new(r"(>=?|<=?|=)\s+(\d+:)?([\w\d.^]+)(-([\w\d.^]+))?(.*)").unwrap();
    static ref RE_REQ:	Regex = Regex::new(r"(?m)^Requires(?:\(([\w,\s]+)\))?:\s*(.+)$").unwrap();
    static ref RE_FILE:	Regex = Regex::new(r"(?m)^(%\w+(\(.+\))?\s+)?(.+)$").unwrap();
    static ref RE_CLOG:	Regex = Regex::new(r"(?m)^\*[ \t]*((\w{3})[ \t]+(\w{3})[ \t]+(\d+)[ \t]+(\d+))[ \t]+(\S+)([ \t]+<([\w@.+]+)>)?([ \t]+-[ \t]+([\d.-^~_\w]+))?$((\n^[^*\n]*)+)").unwrap();
    static ref RE_PMB:	Regex = Regex::new(r"(\w+\d*):\s*(.+)").unwrap();
    static ref RE_DNL:	Regex = Regex::new(r"%dnl\b").unwrap();
}

/// Conditional operators used in specifying dependencies.
/// ## Symbols
/// - [`PkgQCond::Eq`] : `=`
/// - [`PkgQCond::Le`] : `<=`
/// - [`PkgQCond::Lt`] : `<`
/// - [`PkgQCond::Ge`] : `>=`
/// - [`PkgQCond::Gt`] : `>`
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub enum PkgQCond {
    /// =
    #[default]
    Eq,
    /// <=
    Le,
    /// <
    Lt,
    /// >=
    Ge,
    /// >
    Gt,
}

impl FromStr for PkgQCond {
    type Err = Err;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(match s {
            "=" => Self::Eq,
            ">=" => Self::Ge,
            ">" => Self::Gt,
            "<=" => Self::Le,
            "<" => Self::Lt,
            _ => return Err(Err::BadPkgQCond(s.into())),
        })
    }
}

/// Denotes a package dependency.
///
/// This is used to represent a package specified in `Requires:` or `BuildRequires:`.
///
/// # Examples
/// ```
/// use rpmspec::parse::Package;
///
/// let pkg = Package::new("anda".into());
///
/// let mut recommends = vec![];
/// Package::add_simple_query(&mut recommends, "subatomic terra-release, mock")?;
/// # Ok::<(), color_eyre::Report>(())
/// ```
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Package {
    /// Name of the package dependency
    pub name: String,
    /// Version (right hand side of the dependency query)
    pub version: Option<String>,
    /// Release (right hand side of the dependency query)
    pub release: Option<String>,
    /// Epoch (right hand side of the dependency query)
    pub epoch: Option<u32>,
    /// Conditional operator (middle of the dependency query)
    pub condition: PkgQCond,
}

impl std::borrow::Borrow<str> for Package {
    fn borrow(&self) -> &str {
        &self.name
    }
}

impl Package {
    /// Creates a new Dependency representation with the package name.
    #[must_use]
    pub fn new(name: String) -> Self {
        Self { name, ..Self::default() }
    }
    /// Parses a simple query, i.e. packages specified without conditionals and versions.
    ///
    /// The names of the packages should be separated either by spaces or commas, just like in
    /// `Recommends:`.
    ///
    /// # Examples
    /// ```
    /// use rpmspec::parse::Package;
    ///
    /// let mut pkgs = vec![];
    /// Package::add_simple_query(&mut pkgs, "anda, subatomic rpm_macro(fdupes) pkgconfig(gio-2.0)".into())?;
    /// assert_eq!(
    ///   pkgs,
    ///   vec![
    ///     Package::new("anda".into()),
    ///     Package::new("subatomic".into()),
    ///     Package::new("rpm_macro(fdupes)".into()),
    ///     Package::new("pkgconfig(gio-2.0)".into()),
    ///   ]
    /// );
    /// # Ok::<(), color_eyre::Report>(())
    /// ```
    ///
    /// # Errors
    /// An error is returned if and only if there exists an invalid character that is
    /// not ascii-alphanumeric and not in [`PKGNAMECHARSET`].
    pub fn add_simple_query(pkgs: &mut Vec<Self>, query: &str) -> Result<()> {
        let mut last = String::new();
        let mut quotes = String::new();
        let chrs = query.trim().chars();
        for ch in chrs {
            if ch == ' ' || ch == ',' {
                if !quotes.is_empty() {
                    return Err(eyre!("Unclosed quotes: `{quotes}`").note(format!("Reading query `{query}`")).note(lzf!("Parsing `{last}`")).note(lzf!("Deliminator `{ch}`")));
                }
                if !last.is_empty() {
                    pkgs.push(Self::new(std::mem::take(&mut last)));
                }
                continue;
            }
            if !ch.is_ascii_alphanumeric() && !PKGNAMECHARSET.contains(ch) {
                return Err(eyre!("Invalid character `{ch}` in package name")
                    .note(format!("Reading query `{query}`"))
                    .note(lzf!("Parsing `{last}`"))
                    .suggestion(lzf!("Only ascii alphanumeric characters and `{PKGNAMECHARSET}` are valid")));
            }
            textproc::chk_ps(&mut quotes, ch)?;
            last.push(ch);
        }
        if !quotes.is_empty() {
            return Err(eyre!("Unclosed quotes: `{quotes}`").note(format!("Reading query `{query}`")).note(lzf!("Parsing `{last}`")).warning("Unexpected EOL"));
        }
        if !last.is_empty() {
            pkgs.push(Self::new(last));
        }
        Ok(())
    }
    /// Parses a query.
    ///
    /// Each package query that may contains a [condition](PkgQCond) and a version ('Dependency')
    /// should be separated by spaces or commas. There should also be spaces around the
    /// conditional operators, including `=`.
    ///
    /// # Errors
    /// An error is returned if and only if
    /// - there exists an invalid character in package names that is not ascii alphanumeric, a space, a
    ///   comma, and nott in [`PKGNAMECHARSET`]; or
    /// - an epoch specified cannot be parsed by `core::str::parse::<u32>()`.
    ///
    /// # Panics
    /// - [`Regex`] is used to parse the conditions.
    ///   A panic might occurs if a capture group is not found via `caps[n]`.
    ///   However, This is unlikely since the groups either exist in the regex
    ///   or the optional group is accessed using `caps.get(n)`.
    /// - A panic might occurs if it fails to strip the `:` suffix from the regex capture group for
    ///   the epoch, but again this is unlikely.
    ///
    /// # Caveats
    /// This function is recursive, but it should be safe.
    ///
    /// # Examples
    ///
    /// ```
    /// use rpmspec::parse::{Package, PkgQCond};
    ///
    /// let mut pkgs = vec![];
    /// let query = "hai, bai >= 3.0 another-pkg".into();
    /// Package::add_query(&mut pkgs, query)?;
    ///
    /// let hai = Package::new("hai".into());
    /// let bai = Package { name: "bai".into(), version: Some("3.0".into()), condition: PkgQCond::Ge, ..Default::default() };
    /// let another = Package::new("another-pkg".into());
    /// assert_eq!(pkgs, vec![hai, bai, another]);
    /// # Ok::<(), color_eyre::Report>(())
    /// ```
    pub fn add_query(pkgs: &mut Vec<Self>, query: &str) -> Result<()> {
        let query = query.trim(); // just in case
        if query.is_empty() {
            return Ok(());
        }
        let mut spaced = false;
        let mut comma = false;
        let mut name = String::new();
        let mut rest = String::new();
        let mut chrs = query.chars();
        while let Some(ch) = chrs.next() {
            if ch == ' ' {
                spaced = true;
                continue;
            }
            if ch == ',' {
                comma = true;
                rest = chrs.collect();
                break;
            }
            if spaced {
                rest = format!("{ch}{}", chrs.collect::<String>()).into();
                break;
            }
            if !ch.is_ascii_alphanumeric() && !PKGNAMECHARSET.contains(ch) {
                return Err(eyre!("Invalid character `{ch}` in package name")
                    .note(format!("Reading query `{query}`"))
                    .note(lzf!("Parsing `{name}`"))
                    .suggestion(lzf!("Only ascii alphanumeric characters and `{PKGNAMECHARSET}` are valid")));
            }
            name.push(ch);
        }
        if comma {
            pkgs.push(Self::new(name));
            let rest = rest.trim();
            if rest.is_empty() {
                warn!(query, "Trailing comma in package query");
                return Ok(());
            }
            return Self::add_query(pkgs, rest);
        }
        if name.is_empty() {
            // either a bug or trailing commas
            warn!(query, "Parsed package query and found empty name. Check if there are trailing commas.");
            return Ok(());
        }
        // the part that matches the good name is `name`. Check the rest.
        let mut pkg = Self::new(name);
        let Some(caps) = RE_PQC.captures(&rest) else {
            pkgs.push(pkg);
            if rest.is_empty() {
                return Ok(());
            }
            return Self::add_query(pkgs, &rest);
        };
        pkg.condition = PkgQCond::from_str(&caps[1])?;
        if let Some(epoch) = caps.get(2) {
            let epoch = epoch.as_str().strip_suffix(':').expect("epoch no `:` by RE_PKGQCOND");
            pkg.epoch = Some(epoch.parse().map_err(|e| eyre!("Cannot parse epoch to u32: `{epoch}`").error(e).suggestion("Epoch can only be positive integers"))?);
        }
        pkg.version = Some(caps[3].into());
        if let Some(rel) = caps.get(5) {
            pkg.release = Some(rel.as_str().into());
        }
        pkgs.push(pkg);
        if let Some(rest) = caps.get(6) {
            let mut chrs = rest.as_str().chars();
            while let Some(ch) = chrs.next() {
                if ch == ' ' {
                    continue;
                }
                if ch == ',' {
                    return Self::add_query(pkgs, chrs.collect::<String>().trim_start());
                }
                return Self::add_query(pkgs, &format!("{ch}{}", chrs.collect::<String>()));
            }
        }
        Ok(())
    }
}

/// Represents the `Requires:` preamble.
///
/// Each attribute/property in [`Self`] represents the `Requires(...):` syntax.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct RPMRequires {
    /// Dependencies listed in `Requires:`.
    pub none: Vec<Package>,
    /// Dependencies listed in `Requires(pre):`.
    pub pre: Vec<Package>,
    /// Dependencies listed in `Requires(post):`.
    pub post: Vec<Package>,
    /// Dependencies listed in `Requires(preun):`.
    pub preun: Vec<Package>,
    /// Dependencies listed in `Requires(postun):`.
    pub postun: Vec<Package>,
    /// Dependencies listed in `Requires(pretrans):`.
    pub pretrans: Vec<Package>,
    /// Dependencies listed in `Requires(posttrans):`.
    pub posttrans: Vec<Package>,
    /// Dependencies listed in `Requires(verify):`.
    pub verify: Vec<Package>,
    /// Dependencies listed in `Requires(interp):`.
    pub interp: Vec<Package>,
    /// Dependencies listed in `Requires(meta):`.
    pub meta: Vec<Package>,
}

impl RPMRequires {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.none.is_empty()
            && self.pre.is_empty()
            && self.post.is_empty()
            && self.preun.is_empty()
            && self.postun.is_empty()
            && self.pretrans.is_empty()
            && self.posttrans.is_empty()
            && self.verify.is_empty()
            && self.interp.is_empty()
            && self.meta.is_empty()
    }
}

impl std::fmt::Display for RPMRequires {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! w {
			($($attr:ident)*) => {
				$({
					let name = stringify!($attr);
					if name == "none" {
						f.write_str("Requires:       ")?;
						f.write_str(&self.none.join(" "))?;
						f.write_str("\n")?;
					} else {
						f.write_str("Requires(")?;
						f.write_str(name)?;
						f.write_str("):")?;
						let mut padding = 5 - name.len();
						while padding <= 0 {
							padding += 4;
						}
						f.write_str(&" ".repeat(padding))?;
						f.write_str(&self.$attr.join(" "))?;
						f.write_str("\n")?;
					}
				})*
			}
		}
        w!(none pre post preun postun pretrans posttrans verify interp meta);
        Ok(())
    }
}

// todo https://docs.fedoraproject.org/en-US/packaging-guidelines/Scriptlets/
/// Scriptlets like `%pre`, `%posttrans`, etc.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Scriptlets {
    /// `%pre` scriptlet.
    pub pre: Option<String>,
    /// `%post` scriptlet.
    pub post: Option<String>,
    /// `%preun` scriplets.
    pub preun: Option<String>,
    /// `%postun` scriplets.
    pub postun: Option<String>,
    /// `%pretrans` scriplets.
    pub pretrans: Option<String>,
    /// `%posttrans` scriplets.
    pub posttrans: Option<String>,
    /// `%verify` scriplets.
    pub verify: Option<String>,

    /// `%triggerprein` scriptlets.
    pub triggerprein: Option<String>,
    /// `%triggerin` scriptlets.
    pub triggerin: Option<String>,
    /// `%triggerun` scriptlets.
    pub triggerun: Option<String>,
    /// `%triggerpostun` scriptlets.
    pub triggerpostun: Option<String>,

    /// `%filetriggerin` scriptlets.
    pub filetriggerin: Option<String>,
    /// `%filetriggerun` scriptlets.
    pub filetriggerun: Option<String>,
    /// `%filetriggerpostun` scriptlets.
    pub filetriggerpostun: Option<String>,
    /// `%transfiletriggerin` scriptlets.
    pub transfiletriggerin: Option<String>,
    /// `%transfiletriggerun` scriptlets.
    pub transfiletriggerun: Option<String>,
    /// `%transfiletriggerpostun` scriptlets.
    pub transfiletriggerpostun: Option<String>,
}

/// Settings for `%config(...)` in `%files`.
///
/// - [`ConfigFileMod::MissingOK`] : `%config(missingok)`
/// - [`ConfigFileMod::NoReplace`] : `%config(noreplace)`
/// - [`ConfigFileMod::None`]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum ConfigFileMod {
    /// Represents `%config`
    #[default]
    None,
    /// Represents `%config(missingok)`
    MissingOK,
    /// Represents `%config(noreplace)`
    NoReplace,
}

/// Settings for `%verify(...)` in `%files`.
///
/// - [`VerifyFileMod::Owner`] : `%verify(user owner)`
/// - [`VerifyFileMod::Group`] : `%verify(group)`
/// - [`VerifyFileMod::Mode`] : `%verify(mode)`
/// - [`VerifyFileMod::Md5`] : `%verify(filedigest md5)`
/// - [`VerifyFileMod::Size`] : `%verify(size)`
/// - [`VerifyFileMod::Maj`] : `%verify(maj)`
/// - [`VerifyFileMod::Min`] : `%verify(min)`
/// - [`VerifyFileMod::Symlink`] : `%verify(link symlink)`
/// - [`VerifyFileMod::Rdev`] : `%verify(rdev)`
/// - [`VerifyFileMod::Mtime`] : `%verify(mtime)`
/// - [`VerifyFileMod::Not`] :`%verify(not ...)`
///
/// For [`VerifyFileMod::None(String)`], the `String` is the input into `VerifyFileMod::from()`.
/// This means the input is not recognised as a valid `%verify()` setting.
///
/// # See also
/// - <https://rpm-software-management.github.io/rpm/manual/spec.html#virtual-file-attributes>
/// - <http://ftp.rpm.org/max-rpm/s1-rpm-inside-files-list-directives.html>
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifyFileMod {
    /// Represents `%verify(user owner)`
    Owner,
    /// Represents `%verify(group)`
    Group,
    /// Represents `%verify(mode)`
    Mode,
    /// Represents `%verify(filedigest md5)`
    Md5,
    /// Represents `%verify(size)`
    Size,
    /// Represents `%verify(maj)`
    Maj,
    /// Represents `%verify(min)`
    Min,
    /// Represents `%verify(link symlink)`
    Symlink,
    /// Represents `%verify(mtime)`
    Mtime,
    /// Represents `%verify(rdev)`
    Rdev,
    /// Represents `%verify(...)` where `...` is invalid
    None(String),
    /// Represents `%verify(not ...)`
    Not,
}

impl VerifyFileMod {
    /// Returns all the possible arguments to `%verify`
    #[must_use]
    pub fn all() -> Vec<Self> {
        use VerifyFileMod::{Group, Maj, Md5, Min, Mode, Mtime, Owner, Size, Symlink};
        vec![Owner, Group, Mode, Md5, Size, Maj, Min, Symlink, Mtime]
    }
}

impl From<&str> for VerifyFileMod {
    fn from(value: &str) -> Self {
        use VerifyFileMod::{Group, Maj, Md5, Min, Mode, Mtime, None, Not, Owner, Rdev, Size, Symlink};
        match value {
            "user" | "owner" => Owner,
            "group" => Group,
            "mode" => Mode,
            "filedigest" | "md5" => Md5,
            "size" => Size,
            "maj" => Maj,
            "min" => Min,
            "link" | "symlink" => Symlink,
            "rdev" => Rdev,
            "mtime" => Mtime,
            "not" => Not,
            _ => None(value.into()),
        }
    }
}

/// File derivatives used in `%files`.
///
/// - `RPMFileAttr::Artifact`
/// - `RPMFileAttr::Ghost`
/// - `RPMFileAttr::Config(ConfigFileMod)` (See [`ConfigFileMod`])
/// - `RPMFileAttr::Dir`
/// - `RPMFileAttr::Doc`
/// - `RPMFileAttr::License`
/// - `RPMFileAttr::Verify(Box<[VerifyFileMod]>)` (See [`VerifyFileMod`])
/// - `RPMFileAttr::Docdir`
/// - `RPMFileAttr::Normal` (files without derivatives use this)
///
/// # See also
/// - <https://rpm-software-management.github.io/rpm/manual/spec.html#virtual-file-attributes>
/// - <http://ftp.rpm.org/max-rpm/s1-rpm-inside-files-list-directives.html>
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub enum RPMFileAttr {
    /// Represents `%artifact`
    Artifact,
    /// Represents `%ghost`
    Ghost,
    /// Represents `%config(...)`, see [`ConfigFileMod`]
    Config(ConfigFileMod),
    /// Represents `%dir`
    Dir,
    /// Represents `%doc`
    Doc,
    /// Represents `%license`
    License,
    /// Represents `%verify(...)`, see [`VerifyFileMod`]
    Verify(Box<[VerifyFileMod]>),
    /// Represents `%docdir`
    Docdir,
    /// Represents files without file derivatives
    #[default]
    Normal,
}

/// Represents a file in `%files`.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct RPMFile {
    /// File derivative
    pub attr: RPMFileAttr,

    /// the file / dir path
    pub path: String,
    /// permission / mode
    pub mode: u16,
    /// user / owner that owns the file
    pub user: String,
    /// group that owns the file
    pub group: String,
    /// directory permission / mode
    pub dmode: u16,
}

/// Represents a `%files` section.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct RPMFiles {
    /// Represents `%files -f ...`, the argument for `-f` is the file with a list of files to
    /// include in the `%files` section. It is NOT processed here; you should use an RPM builder.
    pub incl: String,
    /// Files listed in `%files`
    pub files: Box<[RPMFile]>,
    /// The raw `%files` sections
    pub raw: String,
}

impl RPMFiles {
    /// Parses a `%files` section using `self.raw`.
    fn parse(&mut self) -> Result<()> {
        //? http://ftp.rpm.org/max-rpm/s1-rpm-inside-files-list-directives.html
        let mut defattr = (0, "".into(), "".into(), 0);
        self.files = RE_FILE
            .captures_iter(&self.raw)
            .map(|cap| {
                if let Some(remain) = &cap[0].strip_prefix("%defattr(") {
                    let Some(remain) = remain.trim_end().strip_suffix(')') else {
                        return Err(eyre!("Closing `)` not found for `%defattr(`"));
                    };
                    let ss: Box<[&str]> = remain.split(',').map(str::trim).collect();
                    let [filemode, user, group, dirmode] = match *ss {
                        [filemode, user, group] => [filemode, user, group, ""],
                        [filemode, user, group, dmode] => [filemode, user, group, dmode],
                        _ => return Err(eyre!("Expected 3/4 arguments for %defattr(), found {}", ss.len())),
                    };
                    defattr = (
                        if filemode == "-" { 0 } else { filemode.parse().map_err(|e: ParseIntError| eyre!(e).wrap_err("Cannot parse file mode"))? },
                        (if user == "-" { "" } else { user }).into(),
                        (if group == "-" { "" } else { group }).into(),
                        if dirmode == "-" { 0 } else { dirmode.parse().map_err(|e: ParseIntError| eyre!(e).wrap_err("Cannot parse dir mode"))? },
                    );
                    return Ok(RPMFile::default());
                }
                let mut f = RPMFile::default();
                if let Some(name) = cap.get(1) {
                    let name = name.as_str();
                    if let Some(m) = cap.get(2) {
                        let x = m.as_str().strip_prefix('(').expect("RE_FILE not matching parens `(...)` but found capture group 2");
                        let x = x.strip_suffix(')').expect("RE_FILE not matching parens `(...)` but found capture group 2");
                        if name.starts_with("%attr(") {
                            let ss: Vec<&str> = x.split(',').map(str::trim).collect();
                            let Some([fmode, user, group]) = ss.get(0..=2) else {
                                return Err(eyre!("Expected 3 arguments in `%attr(...)`"));
                            };
                            let (fmode, user, group) = (*fmode, *user, *group);
                            if fmode != "-" {
                                f.mode = fmode.parse().map_err(|e: ParseIntError| eyre!(e).wrap_err("Cannot parse file mode"))?;
                            }
                            if user != "-" {
                                f.user = user.into();
                            }
                            if group != "-" {
                                f.group = group.into();
                            }
                            f.path = cap.get(3).expect("No RE grp 3 in %files?").as_str().into();
                            return Ok(f);
                        }
                        if name.starts_with("%verify(") {
                            let mut vs = x.split(' ').map_into().collect_vec();
                            if let Some(VerifyFileMod::None(s)) = vs.iter().find(|s| matches!(s, VerifyFileMod::None(_))) {
                                return Err(eyre!("`%verify({s})` is unknown"));
                            }
                            if vs.contains(&VerifyFileMod::Not) {
                                let mut ll = VerifyFileMod::all();
                                ll.retain(|x| !vs.contains(x));
                                vs = ll;
                            }
                            f.attr = RPMFileAttr::Verify(vs.into_boxed_slice());
                            f.path = cap.get(3).expect("No RE grp 3 in %files?").as_str().into();
                            (f.mode, f.user, f.group, f.dmode) = defattr.clone();

                            return Ok(f);
                        }
                        if name.starts_with("%config(") {
                            f.attr = RPMFileAttr::Config(match x {
                                "missingok" => ConfigFileMod::MissingOK,
                                "noreplace" => ConfigFileMod::NoReplace,
                                _ => return Err(eyre!("`%config({x})` is unknown")),
                            });
                            f.path = cap.get(3).expect("No RE grp 3 in %files?").as_str().into();
                            (f.mode, f.user, f.group, f.dmode) = defattr.clone();
                        }
                        return Err(eyre!("Unknown %files directive: %{name}"));
                    }
                    f.attr = match name {
                        "%artifact " => RPMFileAttr::Artifact,
                        "%ghost " => RPMFileAttr::Ghost,
                        "%config " => RPMFileAttr::Config(ConfigFileMod::None),
                        "%dir " => RPMFileAttr::Dir,
                        "%doc " | "%readme " => RPMFileAttr::Doc,
                        "%license " => RPMFileAttr::License,
                        "%docdir " => RPMFileAttr::Docdir,
                        _ => return Err(eyre!("Unknown %files directive: %{name}")),
                    }
                }
                f.path = cap.get(3).expect("No RE grp 3 in %files?").as_str().into();
                Ok(f)
            })
            .filter(|x| x.as_ref().map_or(false, |x| x.path.is_empty()))
            .collect::<Result<Box<[RPMFile]>>>()?;
        Ok(())
    }
}

/// Represents 1 changelog entry in `%changelog`.
///
/// # Example
/// ```
/// use rpmspec::parse::Changelog;
///
/// let mut changelog = Changelog {
///   date: chrono::NaiveDate::from_ymd_opt(2006, 1, 11).ok_or_else(|| color_eyre::eyre::eyre!("Cannot turn 2006-01-11 into NaiveDate"))?,
///   version: Some("1.11.0-6".into()),
///   maintainer: "madomado".into(),
///   email: Some("madonuko@outlook.com".into()),
///   message: "- messages here\n- *markdown magic* here\n- version and email is optional".into(),
/// };
/// # Ok::<(), color_eyre::Report>(())
/// ```
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Changelog {
    /// Date of changelog
    pub date: chrono::NaiveDate,
    /// Version corresponding to the changelog entry
    pub version: Option<String>,
    /// The person who created the changelog
    pub maintainer: String,
    /// Email of the maintainer
    pub email: Option<String>,
    /// Message of the changelog
    pub message: String,
}

/// Represents a `%changelog` section.
///
/// # Example
/// Let's look at this changelog:
/// ```rpmspec
/// * Wed Jan 11 2006 madomado <madonuko@outlook.com> - 1.11.0-6
/// - messages here
/// - *markdown magic* here
/// - version and email is optional
/// ```
/// in rust:
/// ```
/// let mut cl = rpmspec::parse::Changelogs::default();
/// cl.raw = r#"
/// * Wed Jan 11 2006 madomado <madonuko@outlook.com> - 1.11.0-6
/// - messages here
/// - *markdown magic* here
/// - version and email is optional
/// "#.into();
/// cl.parse()?;
/// // everything is now in `cl.changelogs`!
/// # Ok::<(), color_eyre::Report>(())
/// ```
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Changelogs {
    /// an immutable boxed vector of [`Changelog`]
    pub changelogs: Box<[Changelog]>,
    /// changelogs that are not (yet) parsed
    pub raw: String,
}

impl Changelogs {
    /// Parses a `%changelog` section.
    ///
    /// # Example
    /// Let's look at this changelog:
    /// ```rpmspec
    /// * Wed Jan 11 2006 madomado <madonuko@outlook.com> - 1.11.0-6
    /// - messages here
    /// - *markdown magic* here
    /// - version and email is optional
    /// ```
    /// in rust:
    /// ```
    /// let mut cl = rpmspec::parse::Changelogs::default();
    /// cl.raw = r#"
    /// * Wed Jan 11 2006 madomado <madonuko@outlook.com> - 1.11.0-6
    /// - messages here
    /// - *markdown magic* here
    /// - version and email is optional
    /// "#.into();
    /// cl.parse()?;
    /// // everything is now in `cl.changelogs`!
    /// # Ok::<(), color_eyre::Report>(())
    /// ```
    ///
    /// # Errors
    /// - [`chrono::ParseError`] if any dates cannot be parsed.
    pub fn parse(&mut self) -> Result<()> {
        self.changelogs = RE_CLOG
            .captures_iter(&self.raw)
            .map(|cap| {
                Ok(Changelog {
                    date: chrono::NaiveDate::parse_from_str(&cap[1], "%a %b %d %Y").map_err(|e| eyre!(e).wrap_err("Cannot parse date in %changelog"))?,
                    version: cap.get(10).map(|v| v.as_str().into()),
                    maintainer: cap[6].into(),
                    email: cap.get(8).map(|email| email.as_str().into()),
                    message: cap[11].trim().into(),
                })
            })
            .collect::<Result<Box<[Changelog]>>>()?;
        Ok(())
    }
}

/// Represents different sections in an RPM spec file.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub enum RPMSection {
    /// The global preamble section.
    #[default]
    Global,
    /// A subpackage (`%package ...`)
    Package(String),
    /// Description (`%description [...]`)
    Description(String),
    /// %prep
    Prep,
    /// %build
    Build,
    /// %install
    Install,
    /// %files [...] [-f ...]
    Files(String, Option<String>),
    /// %changelog
    Changelog,
}

/// Represents a subpackage (`%package ...`).
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct RPMSpecPkg {
    /// Name of subpackage (`%package [-n] ...`)
    ///
    /// If `-n` is used, the argument following `-n` is the name.
    /// Otherwise, the prefix `%{name}-` will be added.
    pub name: Option<String>,
    /// Summary of subpackage (`Summary:`)
    pub summary: String,
    /// License of subpackage (`License:`)
    pub license: Option<String>,
    /// Dependencies of subpackage listed in `Requires:`
    pub requires: RPMRequires,
    /// Description of subpackage (`%description [-n] ...`)
    pub description: String,
    /// Group of subpackage (`Group:`)
    pub group: Option<String>,
    /// What the subpackage `Provides:`
    pub provides: Vec<Package>,
    /// Represents `Conflicts:`
    pub conflicts: Vec<Package>,
    /// Represents `Obsoletes:`
    pub obsoletes: Vec<Package>,
    /// Represents `Recommends:`
    pub recommends: Vec<Package>,
    /// Represents `Suggests:`
    pub suggests: Vec<Package>,
    /// Represents `Supplements:`
    pub supplements: Vec<Package>,
    /// Represents `Enhances:`
    pub enhances: Vec<Package>,
    /// Files in subpackage listed in `%files [-n] ...`
    pub files: RPMFiles,
    /// Scriptlets present in the final RPM package, such as `%post [-n] ...` and `%pretrans [-n] ...`
    pub scriptlets: Scriptlets, // todo

                                // todo: BuildArch and stuff
}

/// Represents the entire spec file.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct RPMSpec {
    /// List of subpackages (`%package ...`).
    pub packages: HashMap<String, RPMSpecPkg>,

    /// Represents `%description`
    pub description: String,
    /// Represents `%prep`
    pub prep: String,
    /// Represents `%generate_buildrequires`
    pub generate_buildrequires: Option<String>,
    /// Represents `%conf`
    pub conf: Option<String>,
    /// Represents `%build`
    pub build: String,
    /// Represents `%install`
    pub install: String,
    /// Represents `%check`
    pub check: String,

    /// Scriptlets present in the final RPM package, such as `%post` and `%pretrans`
    pub scriptlets: Scriptlets,
    /// Files present in the final RPM package listed in `%files [-f ...]`
    pub files: RPMFiles,
    /// Represents `%changelog`
    pub changelog: Changelogs,

    /// Represents `Name:`;
    /// The base name of the package, which should match the SPEC filename.
    pub name: Option<String>,
    /// Represents `Version:`;
    /// This usually is the upstream version number of the software.
    pub version: Option<String>,
    /// Represents `Release:`
    pub release: Option<String>,
    /// Represents `Epoch:`
    pub epoch: u32,
    /// Repreesnts `License:`
    pub license: Option<String>,
    /// Repreesnts `SourceLicense:`
    pub sourcelicense: Option<String>,
    /// Repreesnts `Group:`
    pub group: Option<String>,
    /// Repreesnts `Summary:`
    pub summary: Option<String>,
    /// Repreesnts `Source0:`, `Source1:`, ...
    pub sources: HashMap<u32, String>,
    /// Repreesnts `Patch0:`, `Patch1:`, ...
    pub patches: HashMap<u32, String>,
    /// Represents `Copyright:`
    pub copyright: Option<String>,
    // TODO: icon
    // TODO: nosource nopatch
    /// Represents `URL:`
    pub url: Option<String>,
    /// Represents `BugURL:`
    pub bugurl: Option<String>,
    /// Represents `ModularityLabel:`
    pub modularitylabel: Option<String>,
    /// Represents `DistTag:`
    pub disttag: Option<String>,
    /// Represents `VCS:`
    pub vcs: Option<String>,
    /// Represents `Distribution:`
    pub distribution: Option<String>,
    /// Represents `Vendor:`
    pub vendor: Option<String>,
    /// Represents `Packager:`
    pub packager: Option<String>,
    // TODO: buildroot
    /// Represents `AutoReqProv:`
    pub autoreqprov: bool,
    /// Represents `AutoReq:`
    pub autoreq: bool,
    /// Represents `AutoProv:`
    pub autoprov: bool,
    /// Represents `Requires:` and `Requires(...):`
    pub requires: RPMRequires,
    /// Represents `Provides:`
    pub provides: Vec<Package>,
    /// Represents `Conflicts:`
    pub conflicts: Vec<Package>,
    /// Represents `Obsoletes:`
    pub obsoletes: Vec<Package>,
    /// Represents `Recommends:`
    pub recommends: Vec<Package>,
    /// Represents `Suggests:`
    pub suggests: Vec<Package>,
    /// Represents `Supplements:`
    pub supplements: Vec<Package>,
    /// Represents `Enhances:`
    pub enhances: Vec<Package>,
    /// Represents `OrderWithRequires:`
    pub orderwithrequires: Vec<Package>,
    /// Represents `BuildRequires:`
    pub buildrequires: Vec<Package>,
    /// Represents `BuildConflicts:`
    pub buildconflicts: Vec<Package>,
    /// Represents `ExcludeArch:`
    pub excludearch: Vec<String>,
    /// Represents `ExclusiveArch:`
    pub exclusivearch: Vec<String>,
    /// Represents `ExcludeOS:`
    pub excludeos: Vec<String>,
    /// Represents `ExclusiveOS:`
    pub exclusiveos: Vec<String>,
    /// Represents `BuildArch:`, `BuildArchitectures:`
    pub buildarch: Vec<String>,
    /// Represents `Prefix:`, `Prefixes:`
    pub prefix: Option<String>,
    /// Represents `Docdir:`
    pub docdir: Option<String>,
    /// Represents `RemovePathPostFixes:`
    pub removepathpostfixes: Vec<String>,
}

impl RPMSpec {
    /// Creates a new RPM spec object with good defaults:
    /// - `autoreqprov: true`
    /// - `autoreq: true`
    /// - `autoprov: true`
    ///
    /// # Examples
    ///
    /// ```
    /// use rpmspec::parse::RPMSpec;
    ///
    /// assert_eq!(RPMSpec::new(), RPMSpec {
    ///   autoreqprov: true,
    ///   autoreq: true,
    ///   autoprov: true,
    ///   ..Default::default()
    /// });
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self {
            // buildroot
            autoreqprov: true,
            autoreq: true,
            autoprov: true,
            ..Self::default()
        }
    }

    /// Write the content of [`RPMSpec`] into a file specified by the path.
    pub fn save_to<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        std::fs::write(path, self.render())
    }

    /// Renders the content of [`RPMSpec`] into a String.
    #[must_use]
    pub fn render(&self) -> String {
        let mut spec = String::new();

        macro_rules! pop {
			(@self) => {self};
			(@self $a:ident) => {$a};
			($preamble:expr, $val:expr) => {{
				let preamble = $preamble;
				let padding = 14 - preamble.len();
				spec.push_str(preamble);
				spec.push(':');
				spec.push_str(&" ".repeat(padding));
				spec.push_str($val);
				spec.push('\n');
			}};
			($preamble:expr => $(~$cur:ident.)?$attr:ident) => {{
				if let Some(val) = &pop!(@self $($cur)?).$attr {
					pop!($preamble, val);
				}
			}};
			($preamble:expr => $(~$cur:ident.)?$attr:ident or $default:expr) => {{
				pop!($preamble, pop!(@self $($cur)?).$attr.as_ref().map_or($default, |s| s));
			}};
			($preamble:expr => ..$(~$cur:ident.)?$attr:ident) => {{
				if !pop!(@self $($cur)?).$attr.is_empty() {
					pop!($preamble, &pop!(@self $($cur)?).$attr.join(" "));
				}
			}};
			($preamble:ident: $($x:tt)*) => {
				pop!(stringify!($preamble) => $($x)*);
			};
			($preamble:expr => $b:block) => {{
				pop!($preamble, $b);
			}};
			(@use) => { "" };
			(@use $subpackage:expr, $header:expr) => { $header:expr };
			(@in $scriptlets:ident $(for $header:expr)?) => {
				pop!(%(pre post preun postun pretrans posttrans verify triggerprein triggerin triggerun triggerpostun filetriggerin filetriggerun filetriggerpostun transfiletriggerin transfiletriggerun transfiletriggerpostun) in $scriptlets $(for $header)?);
			};
			(@header) => { "" };
			(@header $header:expr) => { format!(" {}", $header) };
			(%($($section:ident)*) in $scriptlets:ident $(for $header:expr)?) => {
				// we need this because rust doesn't support macro nesting with $()? inside $()*
				let header = pop!(@header $($header)?);
				$(
					if let Some(s) = &$scriptlets.$section {
						spec.push_str("\n\n%");
						spec.push_str(stringify!($section));
						spec.push_str(&header);
						spec.push('\n');
						spec.push_str(s);
					}
				)*
			};
		}

        pop!(Name: name or "pkgname");
        pop!(Version: version or "1.0.0");
        pop!(Release: release or "1%?dist");
        pop!(Summary: summary or "Missing summary");
        pop!(Epoch: { &self.epoch.to_string() });
        pop!(Vendor: vendor);
        pop!(URL: url);
        pop!(Copyright: copyright);
        pop!(Packager: packager);
        pop!(Group: group);
        // Icon:
        pop!(License: license);
        pop!(BuildArch: ..buildarch);
        pop!(ExclusiveArch: ..exclusivearch);
        pop!(ExclusiveOS: ..exclusiveos);
        pop!(BuildRequires: ..buildrequires);
        pop!(Obsoletes: ..obsoletes);
        pop!(Conflicts: ..conflicts);
        pop!(Provides: ..provides);
        self.patches.iter().for_each(|(i, p)| pop!(&format!("Patch{i}") => { p }));
        self.sources.iter().for_each(|(i, p)| pop!(&format!("Source{i}") => { p }));
        spec.push_str(&format!("{}", self.requires));

        spec.push_str("\n\n%description\n");
        spec.push_str(if self.description.is_empty() { "%{summary}." } else { &self.description });

        let headers = self.packages.iter().map(|(name, current)| {
            spec.push_str("\n\n%package ");
            let mut header = String::new();
            if let Some(suffix) = name.strip_prefix(&*format!("{}-", self.name.as_ref().map_or("pkgname", |s| s))) {
                header.push_str(suffix);
            } else {
                header.push_str("-n ");
                header.push_str(name);
            }
            spec.push_str(&header);
            spec.push('\n');

            if !current.summary.is_empty() {
                pop!(Summary: { &current.summary });
            }
            pop!(Group: ~current.group);
            pop!(Provides: ..~current.provides);
            pop!(Conflicts: ..~current.conflicts);
            pop!(Obsoletes: ..~current.obsoletes);
            pop!(Recommends: ..~current.recommends);
            pop!(Suggests: ..~current.suggests);
            pop!(Supplements: ..~current.supplements);
            pop!(Enhances: ..~current.enhances);
            spec.push_str(&format!("{}", current.requires));

            spec.push_str("\n\n%description ");
            spec.push_str(&header);
            spec.push('\n');
            spec.push_str(if current.description.is_empty() { "%{summary}." } else { &current.description });

            header
        });
        let headers: Box<[_]> = headers.collect();

        // %prep
        spec.push_str("\n\n%prep\n");
        spec.push_str(&self.prep);

        spec.push_str("\n\n%build\n");
        spec.push_str(&self.build);

        spec.push_str("\n\n%install\n");
        spec.push_str(&self.install);

        if !self.check.is_empty() {
            spec.push_str("\n\n%check\n");
            spec.push_str(&self.check);
        }

        let scriptlets = &self.scriptlets;
        pop!(@in scriptlets);

        self.packages.iter().zip(headers.iter()).for_each(|((_, current), header)| {
            let scriptlets = &current.scriptlets;
            pop!(@in scriptlets for header);
        });

        // todo: macros

        spec.push_str("\n\n%files");
        if !self.files.incl.is_empty() {
            spec.push_str(" -f ");
            spec.push_str(&self.files.incl);
        }
        spec.push('\n');
        if !self.files.raw.is_empty() {
            spec.push_str(&self.files.raw);
        } else {
            todo!()
        }

        if !self.changelog.raw.is_empty() {
            spec.push_str(&self.changelog.raw);
        } else {
            todo!()
        }

        spec
    }
}

/// An RPM spec parser.
///
/// # Examples
/// ```
/// use std::{sync::Arc, path::Path};
/// let mut parser = rpmspec::parse::SpecParser::new();
/// parser.parse::<&[u8]>(std::io::BufReader::new(Box::from(b"%define hai bai\nName: %hai" as &[u8])), &Arc::from(Path::new("world.spec")))?;
/// assert_eq!(parser.rpm.name, Some("bai".into()));
/// # Ok::<(), color_eyre::Report>(())
/// ```
#[derive(Default, Clone, Debug)]
pub struct SpecParser {
    /// The parsed RPM package
    pub rpm: RPMSpec,
    errors: Vec<Err>,
    /// Macros present in the spec file union the system macros
    pub macros: HashMap<String, Vec<MacroType>>,
    section: RPMSection,
    cond: Vec<(bool, bool)>, // current, before
}

impl SpecParser {
    /// Parse all macros (including shell expansions) in `reader`.
    ///
    /// # Errors
    /// - Fail to parse macros (probably invalid syntax)
    pub fn parse_macro<R: Read>(&mut self, out: &mut String, reader: &mut Consumer<R>) -> Result<(), Err> {
        while let Some(ch) = reader.next() {
            if ch != '%' {
                out.push(ch);
                continue;
            }
            self._start_parse_raw_macro(out, reader)?;
        }
        Ok(())
    }

    /// Parses an `%[expression]`.
    ///
    /// # Errors
    /// - Invalid syntax in expression
    /// - Cannot evaluate expression
    pub fn parse_expr(&mut self, out: &mut String, reader: &mut Consumer<impl Read>) -> Result<(), Err> {
        use chumsky::Parser;

        // FIXME: it is not an exact replica of rpm: they do not parse the macros in the expression
        // it's a bit different than that, but I think for now it'd be fine to do so instead of
        // wasting time on a much difficult implementation
        let mut inner = String::new();
        self.parse_macro(&mut inner, reader)?;
        trace!(?inner, "Before dealing with zeros");

        // FIXME: somehow rpmexpr accepts `00` and `01` as valid integers for whatever reason
        // too lazy to fix our own parser directly because that rpmexpr crate takes 45 seconds to
        // compile on my machine *every* *single* *time*. I'M SORRY!!!
        // To mitigate that, we're compressing zeros into just one zero character (or none) right here:
        let mut expr = String::new();
        let mut zeronum = false;
        let mut num = false;
        for ch in inner.chars() {
            if num && !zeronum {
                expr.push(ch);
                if !ch.is_numeric() {
                    num = false;
                }
                continue;
            }
            if num {
                // zeronum
                if ch == '0' {
                    continue;
                }
                zeronum = false;
                if !ch.is_numeric() {
                    num = false;
                    expr.push('0');
                }
                expr.push(ch);
                continue;
            }
            num = ch.is_numeric();
            zeronum = ch == '0';
            if zeronum {
                continue;
            }
            expr.push(ch);
        }
        if zeronum {
            expr.push('0');
        }

        debug!(?expr, "Parsing RPM Expression");
        let parser = rpmexpr::Expr::parser();
        let expr = parser.parse(&*expr)?;
        out.push_str(&expr.eval()?.to_string());
        Ok(())
    }

    /// Define a new macro.
    pub fn define_macro(&mut self, name: String, csm: &Consumer<dyn Read + '_>, param: bool, len: usize) {
        let m = MacroType::Runtime { file: Arc::clone(&csm.file), offset: csm.pos, s: Arc::clone(&csm.s), param, len };
        if let Some(v) = self.macros.get_mut(&name) {
            v.push(m);
            return;
        }
        self.macros.insert(name, vec![m]);
    }

    /// Parse the `Requires:` or `Requires(...):` preambles.
    ///
    /// # Errors
    /// - only if the dependency specified is invalid ([`Package::add_query`])
    ///
    /// # Panics
    /// - [`RPMSection::Package`] specified in `parser.section` doesn't exists in `rpm.packages`
    pub fn parse_requires(&mut self, sline: &str) -> Result<bool> {
        let Some(caps) = RE_REQ.captures(sline) else {
            return Ok(false);
        };
        let mut pkgs = vec![];
        Package::add_query(&mut pkgs, caps[2].trim())?;
        let modifiers = if caps.len() == 2 { &caps[2] } else { "none" };
        for modifier in modifiers.split(',') {
            let modifier = modifier.trim();
            let pkgs = pkgs.clone();
            let r = if let RPMSection::Package(ref p) = self.section { &mut self.rpm.packages.get_mut(p).expect("No subpackage when parsing Requires").requires } else { &mut self.rpm.requires };
            match modifier {
                "none" => r.none.extend(pkgs),
                "pre" => r.pre.extend(pkgs),
                "post" => r.post.extend(pkgs),
                "preun" => r.preun.extend(pkgs),
                "postun" => r.postun.extend(pkgs),
                "pretrans" => r.pretrans.extend(pkgs),
                "posttrans" => r.posttrans.extend(pkgs),
                "verify" => r.verify.extend(pkgs),
                "interp" => r.interp.extend(pkgs),
                "meta" => r.meta.extend(pkgs),
                _ => self.errors.push(Err::UnknownModifier(0, modifier.into())), // FIXME: what's the line number?
            }
        }
        Ok(true)
    }

    /// Returns the architecture of the system using `uname -m`
    ///
    /// # Errors
    /// - [`std::io::Error`] if command fails to execute
    /// - [`std::io::Utf8Error`] if command output cannot be parsed
    pub fn arch() -> Result<String> {
        let binding = Command::new("uname").arg("-m").output()?;
        let s = core::str::from_utf8(&binding.stdout)?;
        Ok(s[..s.len() - 1].into()) // remove new line
    }

    /// Loads all macros defined in a file.
    ///
    /// # Errors
    /// - [`io::Error`] when it fails open/read the file
    /// - [`core::str::Utf8Error`] when the file content cannot be converted into `&str`
    #[tracing::instrument(skip(self))]
    pub fn load_macro_from_file(&mut self, path: &Path) -> Result<()> {
        debug!(path=?path.display(), "Loading macros from file");
        let mut csm = Consumer::default();
        csm.r = Some(Arc::new(RwLock::new(BufReader::new(Box::new(std::fs::File::open(path)?)))));
        csm.file = Arc::from(path);
        while let Some(ch) = csm.next() {
            if ch.is_whitespace() {
                continue;
            }
            if ch == '#' {
                csm.until(|ch| ch == '\n');
                csm.next();
                continue;
            }
            if ch == '%' {
                let mut param = false;
                let mut name = String::new();
                csm.read_before(&mut name, |ch| ch == '(' || ch.is_whitespace());
                csm.until(|ch| ch == '(' || ch.is_whitespace());
                let Some(mut x) = csm.next() else {
                    return Err(eyre!("Unexpected EOF"));
                };
                if x == '(' {
                    csm.skip_til_endbrace(rpmspec_common::util::Brace::Round)?;
                    param = true;
                    x = csm.next().ok_or_else(|| eyre!("Unexpected EOF"))?;
                }
                if !x.is_whitespace() && x != '\\' {
                    return Err(eyre!("Unexpected character {x:?} at {}", csm.pos));
                }
                csm.until(|ch| !ch.is_whitespace());
                let offset = csm.pos; // start of definition
                trace!(pos = csm.pos, "parsing macro definition");
                csm.skip_til_eot()?; // eot is end of definition
                trace!(pos = csm.pos, "finished parsing macro definition");
                trace!(?name, "Insert macro");
                let m = MacroType::Runtime { file: Arc::clone(&csm.file), s: Arc::clone(&csm.s), param, offset, len: csm.pos - offset };
                if let Some(v) = self.macros.get_mut(&name) {
                    v.push(m);
                    continue;
                }
                self.macros.insert(name, vec![m]);
                continue;
            }
            warn!("Ignoring position {} which is a line that starts with `{ch}`", csm.pos);
            csm.until(|ch| ch == '\n');
        }
        Ok(())
    }

    /// Loads all system macros via the `Macro path` entry in `rpm --showrc`.
    ///
    /// # Errors
    /// - [`io::Error`] when `sh -c "rpm --showrc | grep '^Macro path' | sed 's/Macro path: //'"` fails to run
    /// - [`core::str::Utf8Error`] when the output of the above command cannot be parsed into `&str`
    /// - [`io::Error`] and [`core::str::Utf8Error`] from `uname -m` ([`SpecParser::arch()`])
    /// - [`glob::PatternError`] if the macro paths from the `rpm` command output are invalid
    /// - [`io::Error`] when [`SpecParser::load_macro_from_file()`] fails to open/read the file
    /// - [`core::str::Utf8Error`] when the file content cannot be converted into `&str`
    ///
    /// # Caveats
    /// Not sure where I've seen the docs, but there was one lying around saying you can define multiple
    /// macros with the same name, and when you undefine it the old one recovers (stack?). I don't think
    /// it is a good idea to do it like that (it is simply ridiculous and inefficient) but you can try.
    pub fn load_macros(&mut self) -> Result<()> {
        // run rpm --showrc | grep "^Macro path"
        let binding = Command::new("sh").args(["-c", "rpm --showrc|grep '^Macro path'|sed 's/Macro path: //'"]).output()?;
        let binding = core::str::from_utf8(&binding.stdout)?;
        let paths = binding.trim().split(':');

        // TODO: use Consumer::read_til_EOL() instead
        for path in paths {
            let path = path.replace("%{_target}", Self::arch()?.as_str());
            debug!(": {path}");
            for path in glob::glob(path.as_str())? {
                let p = path?;
                let metadata = p.metadata()?;
                if metadata.is_dir() {
                    self._load_macros_in_dir(p.as_path())?;
                } else if metadata.is_file() {
                    self.load_macro_from_file(&p)?;
                } else {
                    return Err(eyre!("Unknown file type in load_macros for {p:?}"));
                }
            }
        }
        Ok(())
    }

    fn _load_macros_in_dir(&mut self, dir: &Path) -> Result<()> {
        for path in std::fs::read_dir(dir)? {
            let path = path?;
            if path.metadata()?.is_dir() {
                self._load_macros_in_dir(&path.path())?;
            } else if path.metadata()?.is_file() {
                self.load_macro_from_file(&path.path())?;
            } else {
                return Err(eyre!("Unknown file type in _load_macros_in_dir for {path:?}"));
            }
        }
        Ok(())
    }

    /// Handles conditions as if they are sections, like `%if` and `%elifarch`, etc.
    ///
    /// # Errors
    /// - [`std::io::Error`] or [`std::io::Utf8Error`] when cannot detect arch via [`SpecParser::arch()`]
    pub fn _handle_conditions(&mut self, start: &str, remain: &str) -> Result<bool> {
        // TODO: parse using RPM expressions
        match start {
            "if" => {
                let c = remain.parse().map_or(true, |n: isize| n != 0);
                self.cond.push((c, c));
            },
            "ifarch" => {
                let c = remain == Self::arch()?;
                self.cond.push((c, c));
            },
            "ifnarch" => {
                let c = remain != Self::arch()?;
                self.cond.push((c, c));
            },
            "elifarch" => {
                let Some((a, b)) = self.cond.last_mut() else {
                    return Err(eyre!("%elifarch found without %if/%ifarch"));
                };
                if *b {
                    *a = false;
                } else {
                    *a = remain == Self::arch()?;
                    *b = *a;
                }
            },
            "elifnarch" => {
                let Some((a, b)) = self.cond.last_mut() else {
                    return Err(eyre!("%elifarch found without %if/%ifarch"));
                };
                if *b {
                    *a = false;
                } else {
                    *a = remain != Self::arch()?;
                    *b = *a;
                }
            },
            "elif" => {
                let Some((a, b)) = self.cond.last_mut() else {
                    return Err(eyre!("%elif found without %if"));
                };
                if *b {
                    *a = false;
                } else {
                    *a = remain.parse().map_or(true, |n: isize| n != 0);
                    *b = *a;
                }
            },
            "else" => {
                let Some((a, b)) = self.cond.last_mut() else {
                    return Err(eyre!("%else found without %if"));
                };
                if *b {
                    *a = false;
                } else {
                    *a = !(*a);
                    // *b = *a; (doesn't matter)
                }
            },
            "endif" => return if self.cond.pop().is_none() { Err(eyre!("%endif found without %if")) } else { Ok(true) },
            _ => return Ok(false),
        }
        Ok(true)
    }

    /// Detect a section in a spec file and returns `Ok(true)` if the line is processed.
    ///
    /// # Errors
    /// - Invalid syntax. See the error message. (of type [`color_eyre::Report`])
    /// - Fail to get arch ([`Self::arch()`]) via `uname -m`
    ///
    /// # Panics
    /// - Cannot unwind Consumer (cannot read something that has been read)
    pub fn _handle_section(&mut self, l: &mut String, consumer: &mut Consumer<impl Read>, oldpos: usize) -> Result<bool> {
        // FIXME: optimizations?
        let (start, _) = l.split_once(|ch: char| ch.is_whitespace()).unwrap_or((l.trim(), ""));
        if !(start.starts_with('%') && start.chars().nth(1) != Some('%')) {
            if let Some((false, _)) = self.cond.last() {
                return Ok(true); // false condition, skip parsing
            }
            return Ok(false);
        }
        // FIXME: temporary hack
        // please refactor this to be inside match?
        if !["description", "package", "prep", "build", "install", "files", "changelog"].contains(&&start[1..]) {
            return Ok(false);
        };
        let mut parsed_remain = String::new();
        consumer.after(|ch| ch.is_whitespace());
        let remainpos = consumer.pos;
        self.parse_macro(&mut parsed_remain, consumer)?;
        let mut remain = parsed_remain;
        if self._handle_conditions(&start[1..], &remain)? {
            return Ok(true);
        }
        if let Some((false, _)) = self.cond.last() {
            return Ok(true); // false condition, skip parsing
        }
        let start = start.to_string();
        if let Some((left, right)) = remain.split_once('\n') {
            *l = right.into();
            remain = left.into();
        }
        self.section = match &start[1..] {
            "description" if remain.is_empty() => RPMSection::Description("".into()),
            "description" => RPMSection::Description({
                let mut remaincsm = Consumer::<std::fs::File>::from(&*remain);
                let (_, mut args, flags) = self._param_macro_args(&mut remaincsm).map_err(|e| e.wrap_err("Cannot parse arguments to %description"))?;
                if let Some(x) = flags.iter().find(|x| **x != "n") {
                    return Err(eyre!("Unexpected %description flag `-{x}`"));
                }
                let [arg] = args.as_mut_slice() else {
                    return Err(eyre!("Expected 1, found {} arguments (excluding flags) to %description: {args:?}", args.len()));
                };
                if flags.is_empty() {
                    format!("{}-{arg}", self.rpm.name.as_ref().ok_or(eyre!("Expected package name before subpackage `{arg}`"))?).into()
                } else {
                    take(arg)
                }
            }),
            "package" if remain.is_empty() => return Err(eyre!("Expected arguments to %package: {start:?} / {remain:?}")),
            "package" => {
                let mut remaincsm = consumer.range(remainpos..consumer.pos).expect("Cannot unwind Consumer");
                let (_, mut args, flags) = self._param_macro_args(&mut remaincsm).map_err(|e| e.wrap_err("Cannot parse arguments to %package"))?;
                if let Some(x) = flags.iter().find(|x| **x != "n") {
                    return Err(eyre!("Unexpected %package flag `-{x}`"));
                }
                let [arg] = args.as_mut_slice() else {
                    return Err(eyre!("Expected 1, found {} arguments (excluding flags) to %package", args.len()));
                };
                let name = if flags.is_empty() { format!("{}-{arg}", self.rpm.name.as_ref().ok_or(eyre!("Expected package name before subpackage `{arg}`"))?).into() } else { take(arg) };
                if self.rpm.packages.contains_key(&name) {
                    return Err(eyre!("The subpackage {name} has already been declared"));
                }
                self.rpm.packages.insert(name.clone(), RPMSpecPkg::default());
                RPMSection::Package(name)
            },
            "prep" => RPMSection::Prep,
            "build" => RPMSection::Build,
            "install" => RPMSection::Install,
            "files" => {
                let (mut f, mut name, mut remains) = (None, String::new(), remain.split(' '));
                while let Some(remain) = remains.next() {
                    if remain.is_empty() {
                        break; // idk why this happens?
                    }
                    if let Some(flag) = remain.strip_prefix('-') {
                        match flag {
                            "f" => {
                                let Some(next) = remains.next() else {
                                    return Err(eyre!("Expected argument for %files after `-f`"));
                                };
                                if next.starts_with('-') {
                                    return Err(eyre!("Expected argument for %files after `-f`, found flag `{next}`"));
                                }
                                if let Some(old) = f {
                                    return Err(eyre!("Unexpected duplicated `-f`").note(format!("Old: {old}")).note(format!("New: {next}")));
                                }
                                f = Some(next.into());
                            },
                            "n" => {
                                let Some(next) = remains.next() else {
                                    return Err(eyre!("Expected argument for %files after `-n`"));
                                };
                                if next.starts_with('-') {
                                    return Err(eyre!("Expected argument for %files after `-n`, found flag `{next}`"));
                                }
                                if !name.is_empty() {
                                    return Err(eyre!("The name of the subpackage is already set.").note(format!("Old: {name}")).note(format!("New: {next}")));
                                }
                                name = next.into();
                            },
                            _ => return Err(eyre!("Unexpected flag `-{flag}` for %files")),
                        }
                    } else {
                        if !name.is_empty() {
                            return Err(eyre!("The name of the subpackage is already set.").note(format!("Old: {name}")).note(format!("New: {remain}")));
                        }
                        name = format!("{}-{remain}", self.rpm.name.as_ref().ok_or(eyre!("Expected package name before subpackage `{remain}`"))?).into();
                    }
                }
                RPMSection::Files(name, f)
            },
            "changelog" => RPMSection::Changelog,
            _ => return Ok(false),
        };
        Ok(true)
    }

    /// Parses the spec file given as a [`io::BufReader`].
    ///
    /// # Errors
    /// - Cannot expand macros ([`Self::_expand_macro()`])
    /// - Bad section syntax ([`Self::_handle_section()`])
    /// - Cannot detect arch ([`Self::arch()`])
    /// - Bad syntax in `Requires:` or other preambles
    /// - Other bad syntaxes
    ///
    /// # Panics
    /// - The function expects a subpackage to be previously defined and created in
    ///   `self.rpm.packages` and would panic if it was not found. This'd be a bug.
    pub fn parse<R: Read>(&mut self, bufread: BufReader<Box<R>>, path: &Arc<Path>) -> Result<()> {
        let mut consumer: Consumer<R> = Consumer::new(Arc::default(), Some(Arc::new(bufread.into())), Arc::clone(path));
        let mut old_pos = 0;
        loop {
            // FIXME: to_string() for now but best to not clone
            let rawlineguard = consumer.read_til_eot()?;
            if rawlineguard.is_empty() {
                break;
            }
            let mut rawline = String::from(&*rawlineguard);
            drop(rawlineguard);
            let older_pos = old_pos;
            old_pos = consumer.pos;
            trace!(?rawline, "Parsing line");
            if self._handle_section(&mut rawline, &mut consumer.range(older_pos..consumer.pos).expect("Cannot unwind Consumer"), older_pos)? {
                continue;
            }
            let mut line = String::new();
            self.parse_macro(&mut line, &mut consumer.range(older_pos..consumer.pos).expect("Cannot unwind Consumer"))?;
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || RE_DNL.is_match(line) {
                continue;
            }
            tracing::trace!(section=?self.section, ?line, "Handling section");
            match self.section {
                RPMSection::Global | RPMSection::Package(_) => {
                    // Check for Requires special preamble syntax first
                    if self.parse_requires(line)? {
                        continue;
                    }
                    let Some(cap) = RE_PMB.captures(line) else {
                        self.errors.push(Err::Others(eyre!("{}: Non-empty non-preamble line: {line}", 0))); // FIXME: what's the line number?
                        continue;
                    };
                    if let Some(digitpos) = cap[1].find(['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) {
                        let digit = cap[1][digitpos..].parse()?;
                        self.add_list_preamble(&cap[1][..digitpos], digit, &cap[2])?;
                    } else if ["Source", "Patch"].contains(&&cap[1]) {
                        self.add_list_preamble(&cap[1], 0, &cap[2])?;
                    } else {
                        let offset = consumer.pos - cap[2].len();
                        self.add_preamble(&cap[1], cap[2].into(), offset, &mut consumer)?;
                    }
                },
                RPMSection::Description(ref mut p) => {
                    if p.is_empty() {
                        self.rpm.description.push_str(line);
                        self.rpm.description.push('\n');
                        continue;
                    }
                    let p = self.rpm.packages.get_mut(p).expect("BUG: no subpackage at %description");
                    p.description.push_str(line);
                    p.description.push('\n');
                },
                RPMSection::Prep => {
                    self.rpm.prep.push_str(line);
                    self.rpm.prep.push('\n');
                },
                RPMSection::Build => {
                    self.rpm.build.push_str(line);
                    self.rpm.build.push('\n');
                },
                RPMSection::Install => {
                    self.rpm.install.push_str(line);
                    self.rpm.install.push('\n');
                },
                RPMSection::Files(ref mut p, ref mut f) => {
                    if let Some(f) = f {
                        if p.is_empty() && self.rpm.files.incl.is_empty() {
                            self.rpm.files.incl = take(f);
                        } else {
                            let p = self.rpm.packages.get_mut(p).expect("BUG: no subpackage at %files");
                            if p.files.incl.is_empty() {
                                p.files.incl = take(f);
                            }
                        }
                    }
                    if p.is_empty() {
                        self.rpm.files.raw.push_str(line);
                        self.rpm.files.raw.push('\n');
                        continue;
                    }
                    let p = self.rpm.packages.get_mut(p).expect("BUG: no subpackage at %files");
                    p.files.raw.push_str(line);
                    p.files.raw.push('\n');
                },
                RPMSection::Changelog => {
                    self.rpm.changelog.raw.push_str(line);
                    self.rpm.changelog.raw.push('\n');
                },
            }
        }
        if !self.errors.is_empty() {
            println!("{:#?}", self.errors);
            return take(&mut self.errors).into_iter().fold(Err(eyre!("Cannot parse spec file")), color_eyre::Help::error);
        }
        // NOTE: we are leaving changelog parsing for probably users of this lib
        // it's just markdown… and it doesn't really have to be markdown…

        // self.rpm.changelog.parse()?;
        self.rpm.files.parse()?;
        self.rpm.packages.values_mut().try_for_each(|p| p.files.parse())?;
        Ok(())
    }

    /// Process and add `Source0:` and `Patch0:` preambles into `self.rpm`.
    ///
    /// # Messages
    /// - If a preambled defined previously has been overridden, an error message will be given
    ///   but parsing will continue:
    /// ```rpmspec
    /// Source0: ...
    /// Source1: again??? # error message from here
    /// ```
    ///
    /// # Errors
    /// - The preamble is unknown / invalid
    pub fn add_list_preamble(&mut self, name: &str, digit: u32, value: &str) -> Result<()> {
        let value = value;
        let rpm = &mut self.rpm;
        macro_rules! no_override_ins {
            ($attr:ident) => {{
                if let Some(old) = rpm.$attr.insert(digit, value.into()) {
                    warn!("Overriding preamble `{name}{digit}` value `{old}` -> `{value}`");
                }
            }};
        }
        match name {
            "Source" => no_override_ins!(sources),
            "Patch" => no_override_ins!(patches),
            _ => return Err(eyre!("Failed to match preamble `{name}{digit}` (value `{value}`)")),
        }
        Ok(())
    }

    // ! this function is impractical to be split.
    #[allow(clippy::cognitive_complexity)]
    /// Process and add preambles into `self.rpm` or subpackages.
    ///
    /// List preambles which are defined in the format of `{preamble_name}{digit}` will NOT be
    /// processed here. See [`SpecParser::add_list_preamble`].
    ///
    /// # Errors
    /// - Invalid dependency query ([`Package::add_query`], [`Package::add_simple_query`])
    /// - Cannot `parse()` string into boolean
    ///
    /// # Panics
    /// ## Todo
    /// The following preambles are currently unimplemented and their implementations will be done later:
    /// - `OrderWithRequires`
    /// - `BuildConflicts`
    /// - `Prefixes`
    /// - `Prefix`
    /// - `DocDir`
    /// - `RemovePathPostfixes`
    #[tracing::instrument(skip(self, csm))]
    pub fn add_preamble(&mut self, name: &str, value: String, offset: usize, csm: &mut Consumer<impl Read>) -> Result<()> {
        tracing::debug!("Adding preamble");
        let rpm = &mut self.rpm;

        macro_rules! opt {
			($x:ident $y:ident) => {
				if name == stringify!($x) {
					if let Some(ref old) = rpm.$y {
						warn!(
							"overriding existing {} preamble value `{old}` to `{value}`",
							stringify!($x)
						);
						self.errors.push(Err::Duplicate(0, stringify!($x).into())); // FIXME: what's the line number?
					}
					let m = MacroType::Runtime { s: csm.s.clone(), file: csm.file.clone(), offset, param: false, len: value.len() };
					if let Some(v) = self.macros.get_mut(stringify!($y)) {
						v.push(m);
					} else {
						self.macros.insert(stringify!($y).into(), vec![m]);
					}
					rpm.$y = Some(value);
					return Ok(());
				}
			};
			(~$x:ident $y:ident) => {
				if name == stringify!($x) {
					rpm.$y = value.parse()?;
					return Ok(());
				}
			};
			(%$x:ident $y:ident) => {
				if name == stringify!($x) {
					rpm.$y.append(&mut value.split_whitespace().map(|s| s.into()).collect());
					return Ok(());
				}
			};
			($a:ident $b:ident | $($x:ident $y:ident)|+) => {
				opt!($a $b);
				opt!($($x $y)|+);
			}
		}

        if let RPMSection::Package(ref pkg) = self.section {
            let rpm = rpm.packages.get_mut(pkg).expect("BUG: no subpackage in rpm.packages");
            match name {
                "Group" => {
                    if let Some(ref old) = rpm.group {
                        warn!("overriding existing Group preamble value `{old}` to `{value}`");
                        self.errors.push(Err::Duplicate(0, "Group".into())); // FIXME: what's the line number?
                    }
                    rpm.name = Some(value);
                    return Ok(());
                },
                "Summary" => {
                    if !rpm.summary.is_empty() {
                        warn!("overriding existing Summary preamble value `{}` to `{value}`", rpm.summary);
                        self.errors.push(Err::Duplicate(0, "Summary".into())); // FIXME: what's the line number?
                    }
                    rpm.summary = value;
                    return Ok(());
                },
                "License" => {
                    if let Some(license) = &rpm.license {
                        warn!("overriding existing License preamble value `{license}` to `{value}`");
                        self.errors.push(Err::Duplicate(0, "License".into()));
                    }
                    rpm.license = Some(value);
                    return Ok(());
                },
                "Provides" => return Package::add_query(&mut rpm.provides, &value),
                "Obsoletes" => return Package::add_query(&mut rpm.obsoletes, &value),
                "Conflicts" => return Package::add_query(&mut rpm.conflicts, &value),
                "Suggests" => return Package::add_simple_query(&mut rpm.suggests, &value),
                "Recommends" => return Package::add_simple_query(&mut rpm.recommends, &value),
                "Enhances" => return Package::add_simple_query(&mut rpm.enhances, &value),
                "Supplements" => return Package::add_simple_query(&mut rpm.supplements, &value),
                _ => {}, // get to global below
            }
        }

        opt!(Name name|Version version|Release release|License license|SourceLicense sourcelicense|URL url|BugURL bugurl|ModularityLabel modularitylabel|DistTag disttag|VCS vcs|Distribution distribution|Copyright copyright|Vendor vendor|Packager packager|Group group|Summary summary);
        opt!(~AutoReqProv autoreqprov);
        opt!(~AutoReq autoreq);
        opt!(~AutoProv autoprov);
        opt!(%ExcludeArch excludearch);
        opt!(%ExclusiveArch exclusivearch);
        opt!(%ExcludeOS exclusiveos);
        opt!(%ExclusiveOS exclusiveos);
        opt!(%BuildArch buildarch);
        opt!(%BuildArchitectures buildarch);

        match name {
            "Epoch" => rpm.epoch = value.parse().map_err(|e: ParseIntError| eyre!(e).wrap_err("Failed to decode epoch to int"))?,
            "Provides" => Package::add_query(&mut rpm.provides, &value)?,
            "Conflicts" => Package::add_query(&mut rpm.conflicts, &value)?,
            "Obsoletes" => Package::add_query(&mut rpm.obsoletes, &value)?,
            "Recommends" => Package::add_simple_query(&mut rpm.recommends, &value)?,
            "Suggests" => Package::add_simple_query(&mut rpm.suggests, &value)?,
            "Supplements" => Package::add_simple_query(&mut rpm.supplements, &value)?,
            "Enhances" => Package::add_simple_query(&mut rpm.enhances, &value)?,
            "BuildRequires" => Package::add_query(&mut rpm.buildrequires, &value)?,
            "OrderWithRequires" => todo!(),
            "BuildConflicts" => todo!(),
            "Prefixes" => todo!(),
            "Prefix" => todo!(),
            "DocDir" => todo!(),
            "RemovePathPostfixes" => todo!(),
            _ => self.errors.push(Err::UnknownPreamble(0, name.into())), // FIXME: what's the line number?
        }
        Ok(())
    }

    // TODO: optimizations?
    #[tracing::instrument(skip(self, reader))]
    fn _param_macro_args(&mut self, reader: &mut Consumer<impl Read>) -> Result<(String, Vec<String>, Vec<String>)> {
        // we start AFTER %macro_name
        let (mut content, mut quotes, mut flags) = (String::new(), String::new(), vec![]);
        gen_read_helper!(reader quotes);
        macro_rules! exit {
            () => {
                exit_chk!();
                let args = content.split(' ').filter(|x| !x.starts_with('-')).map(|x| x.into()).collect();
                return Ok((content, args, flags));
            };
        }
        // if there's a space, it's `%macro_name ...`
        // otherwise it's most likely `%{macro_name:...}`
        // but yeah we'll have to trim it anyway
        reader.until(|ch| !ch.is_whitespace());
        let mut space = true;
        'main: while let Some(ch) = reader.next() {
            if ch == '%' {
                space = false;
                if exit_if_eof!(else peek) == '%' {
                    content.push('%');
                    continue;
                }
                self._start_parse_raw_macro(&mut content, reader)?;
                continue;
            }
            if ch == '-' && space {
                space = false;
                let ch = next!(~'-');
                if !ch.is_ascii_alphabetic() {
                    return Err(eyre!("Argument flag `-{ch}` in parameterized macro is not alphabetic"));
                }
                let mut flag = String::new();
                flag.push(ch);
                reader.read_before(&mut flag, |ch| ['\\', ' ', '\n'].contains(&ch));
                flags.push(flag);
                content.push('-');
                content.push(ch);
                continue;
            }
            if ch == '\\' {
                space = false;
                // if the line ends with `\` (excl whitespace) then also parse next line
                content = content.trim_end().into();
                content.push(' ');
                while let Some(ch) = reader.next() {
                    if ch == '\n' {
                        reader.until(|ch| !ch.is_whitespace());
                        continue 'main;
                    }
                    if !ch.is_whitespace() {
                        return Err(eyre!("Got `{ch}` after `\\` before new line"));
                    }
                }
                return Err(eyre!("Unexpected EOF after `\\`"));
            }
            if ch == '\n' && quotes.is_empty() {
                break;
            }
            textproc::chk_ps(&mut quotes, ch)?;
            // compress whitespace to ' '
            if ch.is_whitespace() {
                if !space {
                    space = true;
                    content.push(' ');
                }
                continue;
            }
            content.push(ch);
            space = false;
        }
        exit!();
    }

    #[inline]
    fn __paramm_percent_star(follow: char, res: &mut String, raw_args: &str, args: &[String], def: &mut Consumer<impl Read>) {
        if follow == '*' {
            res.push_str(raw_args); // %**
        } else {
            def.back();
            res.push_str(&args.join(" ")); // %*
        }
    }

    #[tracing::instrument(skip(self, def))]
    fn __paramm_inner(&mut self, def: &mut Consumer<impl Read>, raw_args: &str, flags: &[String], quotes: &mut String, res: &mut String) -> Result<()> {
        let req_ql = quotes.len() - 1;
        let mut content = String::new();
        for ch in def.by_ref() {
            // find `}`
            textproc::chk_ps(quotes, ch)?;
            if req_ql == quotes.len() {
                break;
            }
            content.push(ch);
        }
        if req_ql != quotes.len() {
            tracing::error!(req_ql, new = quotes.len(), ?content, "Expected orig. no. quotes (req_ql) == new (quotes.len()) after parsing");
            return Err(eyre!("Unexpected EOF while parsing `%{{...`"));
        }
        #[allow(clippy::option_if_let_else)] // WARN refactor fail count: 2
        let notflag = if let Some(x) = content.strip_prefix('!') {
            content = x.into();
            true
        } else {
            false
        };
        let expand = {
            // WARN refactor fail count: 1
            let binding = content.clone();
            if let Some((name, e)) = binding.split_once(':') {
                content = name.to_string().into();
                e.into()
            } else {
                binding
            }
        };
        if !content.starts_with('-') {
            // normal %macros
            self._start_parse_raw_macro(res, &mut def.range(def.pos - content.len() - 2..def.pos).expect("Cannot unwind consumer to `{...}`"))?;
            return Ok(());
        }
        if let Some(content) = content.strip_suffix('*') {
            if content.len() != 2 {
                return Err(eyre!("Invalid macro param flag `%{{{content}}}`"));
            }
            let mut args = raw_args.split(' ');
            if !notflag {
                if let Some(n) = args.clone().enumerate().find_map(|(n, x)| if x == content { Some(n) } else { None }) {
                    if let Some(arg) = args.nth(n + 1) {
                        res.push_str(arg);
                    }
                }
            }
            return Ok(()); // no args after -f, add nothing.
        }
        if content.len() != 2 {
            return Err(eyre!("Found `%-{content}` which is not a flag"));
        }
        let flag = content.chars().last().unwrap();
        if !flag.is_ascii_alphabetic() {
            return Err(eyre!("Invalid macro name `%-{flag}`"));
        }
        if flags.contains(&String::from(format!("{flag}"))) ^ notflag {
            res.push_str(&expand);
        }
        Ok(())
    }

    #[tracing::instrument(skip(self, def, reader))]
    fn _param_macro(&mut self, name: &str, def: &mut Consumer<impl Read>, reader: &mut Consumer<impl Read>, out: &mut String) -> Result<()> {
        let (raw_args, args, flags) = self._param_macro_args(reader)?;
        let mut quotes = String::new();
        gen_read_helper!(def quotes);
        macro_rules! exit {
            () => {
                exit_chk!();
                return Ok(());
            };
        }
        while let Some(ch) = def.next() {
            if ch != '%' {
                textproc::chk_ps(&mut quotes, ch)?;
                out.push(ch);
                continue;
            }
            let ch = next!(~'%'); // will chk_ps after `%` chk
            if ch == '%' {
                out.push('%');
                continue;
            }
            textproc::chk_ps(&mut quotes, ch)?;
            // ? https://rpm-software-management.github.io/rpm/manual/macros.html
            match ch {
                '*' => Self::__paramm_percent_star(next!(~'*'), out, &raw_args, &args, def),
                '#' => out.push_str(&args.len().to_string()),
                '0' => out.push_str(name),
                '{' => self.__paramm_inner(def, &raw_args, &flags, &mut quotes, out)?,
                _ if ch.is_numeric() => {
                    let mut macroname = String::new();
                    macroname.push(ch);
                    // no need chk_ps!(), must be numeric
                    def.read_before(&mut macroname, |ch| !ch.is_numeric());
                    out.push_str(
                        match macroname.parse::<usize>() {
                            Ok(n) => args.get(n - 1),
                            Err(e) => return Err(eyre!("Cannot parse macro param `%{macroname}`: {e}")),
                        }
                        .unwrap_or(&String::new()),
                    );
                },
                _ => {
                    def.back();
                    self._start_parse_raw_macro(out, def)?;
                },
            }
        }
        exit!();
    }

    pub(crate) fn _find_macro_and_expand<R: Read>(&mut self, name: &str, reader: &mut Consumer<R>, out: &mut String) -> Result<(), Err> {
        debug!("getting %{name}");
        let Some(def) = self.macros.get(name) else {
            return Err(Err::MacroNotFound(name.into()));
        };
        let Some(def) = def.last() else {
            return Err(Err::MacroUndefined(name.into()));
        };
        match def {
            MacroType::Runtime { file, offset, s, param, len } => {
                let mut csm: Consumer<R> = Consumer::new(Arc::clone(s), None, Arc::clone(file));
                csm.pos = *offset;
                csm.end = *offset + len;
                if *param {
                    return self._param_macro(name, &mut csm, reader, out).map_err(Into::into);
                }
                self.parse_macro(out, &mut csm).map_err(Into::into)
            },
            MacroType::Internal(f) => {
                // * What is this gigantic mess??
                // `f()` in enum item `MacroType::Internal` does not allow `impl`. But don't worry,
                // introducing `impl` at home: `dyn`. `newreader` is the upcasted polymorphic version.
                // The only problem is: `Read` does not impl `Clone` so we have to temporarily own `r`
                // using `std::mem::take()`, then unwrap/build `Arc<RwLock<BufReader<Box<R>>>>`.
                let mut newreader: Consumer<dyn Read + '_> = Consumer {
                    file: Arc::clone(&reader.file),
                    pos: reader.pos,
                    s: Arc::clone(&reader.s),
                    // * How are we sure about the safety of `Arc::try_unwrap()`?
                    // Not like `f()` will call another macro, so it's certain we won't get to
                    // `_rp_macro()` again if we reach `MacroType::Internal`.
                    r: take(&mut reader.r).map(Arc::try_unwrap).map(|r| {
                        let Ok(bufreader) = r.map(RwLock::into_inner) else { panic!("Cannot unwrap Arc for Consumer reader") };
                        // then we get the inner `R`, upcast it, then rebuild everything
                        Arc::new(RwLock::new(BufReader::new(bufreader.into_inner() as _)))
                    }),
                    end: reader.end,
                };
                f(self, out, &mut newreader)?;
                // Similarly here we just put `r` back into the original `reader`.
                reader.r = take(&mut newreader.r).map(Arc::try_unwrap).map(|r| {
                    let Ok(bufreader) = r.map(RwLock::into_inner) else { panic!("Cannot unwrap Arc for Consumer reader") };
                    // * What is this ugly downcasting code?
                    // SAFETY:
                    // The compiler doesn't know the actual type of `dyn Read` after upcasting...
                    // Except it does: `R`! We just need some raw pointer arithmetic to downcast it.
                    let r = unsafe { Box::<R>::from_raw(Box::into_raw(bufreader.into_inner()).cast::<R>()) };
                    Arc::new(RwLock::new(BufReader::new(r)))
                });
                reader.pos = newreader.pos;
                Ok(())
            },
        }
    }

    fn __rawm_shellexpand(strout: &mut String, chars: &mut Consumer<impl Read>, mut quotes: String) -> Result<()> {
        let mut shellcmd = String::new();
        for ch in chars.by_ref() {
            textproc::chk_ps(&mut quotes, ch)?;
            if !quotes.is_empty() {
                shellcmd.push(ch);
                continue;
            }
            return Err(match Command::new("sh").arg("-c").arg(&*shellcmd).output() {
                Ok(out) if out.status.success() => {
                    strout.push_str(core::str::from_utf8(&out.stdout)?.trim_end_matches('\n'));
                    return Ok(());
                },
                Ok(out) => eyre!("Shell expansion command did not succeed")
                    .note(out.status.code().map_or("No status code".into(), |c| format!("Status code: {c}")))
                    .section(std::string::String::from_utf8(out.stdout)?.header("Stdout:"))
                    .section(std::string::String::from_utf8(out.stderr)?.header("Stderr:")),
                Err(e) => eyre!(e).wrap_err("Shell expansion failed"),
            })
            .note(shellcmd);
        }
        Err(eyre!("Unexpected end of shell expansion command: `%({shellcmd}`"))
    }

    /// Expand macros depending on `notflag`.
    ///
    /// when %a is undefined, %{!a} expands to %{!a}, but %!a expands to %a.
    #[tracing::instrument(skip(self, reader))]
    fn _macro_expand_flagproc<R: std::io::Read>(&mut self, qus: bool, notflag: bool, reader: &mut Consumer<R>, name: &str, out: &mut String, curly: bool) -> Result<()> {
        let mut buf = String::new();
        let res = self._find_macro_and_expand(name, reader, &mut buf);
        // we still need to process the macro even if we know it expands to nothing
        // yes `%!?macro_name` is always nothing, same for curly
        if !matches!(res, Ok(()) | Err(Err::MacroNotFound(_) | Err::MacroUndefined(_))) {
            return res.map_err(Into::into);
        }
        if qus && (notflag || res.is_err()) {
            return Ok(());
        }
        out.push_str(&res.map_or_else(
            |e| {
                // NOTE: `_find_macro_and_expand()` was once called `_rp_macro()` (replace-process
                // macro?)
                debug!("_rp_macro: {e:#}");
                if curly {
                    if notflag {
                        format!("%{{!{name}}}")
                    } else {
                        format!("%{{{name}}}")
                    }
                } else {
                    format!("%{name}")
                }
                .into()
            },
            |()| buf,
        ));
        Ok(())
    }

    /// Parse the stuff after %, and determines `{[()]}`.
    /// FIXME: please REFACTOR me!!
    #[tracing::instrument(skip(self, chars))]
    pub fn _start_parse_raw_macro<R: Read>(&mut self, out: &mut String, chars: &mut Consumer<R>) -> Result<(), Err> {
        let (mut notflag, mut question, mut first) = (false, false, true);
        let (mut content, mut quotes) = (String::new(), String::new());
        gen_read_helper!(chars quotes);
        while let Some(ch) = chars.next() {
            match textproc::flag(&mut question, &mut notflag, &mut first, ch) {
                Some(true) => continue,
                Some(false) => {},
                None => {
                    // %abc?...
                    //  └┬┘└─── stop
                    //   └ macro name
                    chars.back();
                    break;
                },
            }
            textproc::chk_ps(&mut quotes, ch)?; // we read until we encounter '}' or ':' or the end
            match ch {
                '{' | '[' | '(' if notflag || question => {
                    error!("You are not supposed to follow `{{` or `[` or `(` after flags (`!` or `?`).");
                    chars.back();
                    break;
                },
                '{' | '[' | '(' if !content.is_empty() => {
                    textproc::back(chars, &mut quotes, ch)?;
                    break;
                },
                '{' => {
                    first = true;
                    let mut name = String::new();
                    while let Some(ch) = chars.next() {
                        textproc::chk_ps(&mut quotes, ch)?;
                        if quotes.is_empty() {
                            self._macro_expand_flagproc(question, notflag, chars, &name, out, true)?;
                            return Ok(());
                        }
                        if ch == ':' {
                            let after_colon_pos = chars.pos;
                            chars.skip_til_endbrace(rpmspec_common::util::Brace::Curly)?;
                            let mut chars = chars.range(after_colon_pos..chars.pos - 1).expect("Cannot unwind consumer to `{*:...}`");
                            if question {
                                if self.macros.contains_key(&name) ^ notflag {
                                    self.parse_macro(out, &mut chars)?;
                                }
                                return Ok(());
                            }
                            self._macro_expand_flagproc(false, notflag, &mut chars, &name, out, true)?;
                            return Ok(());
                        }
                        match textproc::flag(&mut question, &mut notflag, &mut first, ch) {
                            Some(true) => continue,
                            Some(false) => name.push(ch),
                            None => return Err(eyre!("Unexpected flag `{ch}` in %{{...?...}}").into()),
                        }
                    }
                    return Err(eyre!("EOF while parsing `%{{...`").into());
                },
                '[' => {
                    if notflag || question {
                        error!("flags (! and ?) are not supported for %[].");
                    }
                    let start = chars.pos;
                    chars.skip_til_endbrace(rpmspec_common::util::Brace::Square)?;
                    return self.parse_expr(out, &mut chars.range(start..chars.pos - 1).expect("Cannot unwind consumer to `%[...]`"));
                },
                '(' => {
                    if notflag || question {
                        error!("flags (! and ?) are not supported for %().");
                    }
                    Self::__rawm_shellexpand(out, chars, quotes)?;
                    return Ok(());
                },
                '%' if first => {
                    out.push('%');
                    return Ok(());
                },
                _ if !(ch.is_ascii_alphanumeric() || ch == '_') => {
                    textproc::back(chars, &mut quotes, ch)?;
                    break;
                },
                _ => {},
            }
            first = false;
            content.push(ch);
        }
        exit_chk!();
        // `%macro`, but if this is the start of the line, `_rp_macro()` might need the remaining line
        // FIXME:
        //       `%aaa.this_should_not_get_passed_into_it_but_it_does_get_passed_into_it`
        //            ^*****************************************************************
        // somehow we need to diff 1. curly; 2. line start; 3. others
        self._macro_expand_flagproc(question, notflag, chars, &content, out, false)?;
        Ok(())
    }

    /// Creates a new RPM spec parser.
    #[must_use]
    pub fn new() -> Self {
        Self { rpm: RPMSpec::new(), errors: vec![], macros: crate::macros::INTERNAL_MACROS.clone(), ..Self::default() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    type RR = std::fs::File;

    #[test]
    fn parse_spec() -> Result<()> {
        // tracing_subscriber::FmtSubscriber::builder().pretty().with_max_level(tracing::Level::TRACE).init();
        let f = File::open("./tests/test.spec")?;
        let f = BufReader::new(Box::new(f));

        let mut sp = SpecParser::new();
        sp.load_macros()?;
        sp.macros.insert("nil".into(), vec!["".into()]); // FIXME
        sp.parse(f, &Arc::from(Path::new("./tests/test.spec")))?;
        println!("Name: {}", sp.rpm.name.unwrap_or_default());
        println!("Summary: {}", sp.rpm.summary.unwrap_or_default());
        Ok(())
    }

    #[test]
    fn test_load_macros() -> Result<()> {
        // tracing_subscriber::FmtSubscriber::builder().pretty().with_max_level(tracing::Level::TRACE).init();
        println!("{}", SpecParser::arch()?);
        let mut sp = SpecParser::new();
        sp.load_macros()?;
        println!("{:#?}", sp.macros);
        Ok(())
    }

    #[test]
    fn simple_macro_expand() -> Result<()> {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("macrohai".into(), vec!["hai hai".into()]);
        let mut out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("macrohai".into()))?;
        assert_eq!(out, "hai hai");
        Ok(())
    }

    #[test]
    fn text_recursive_macro_expand() -> Result<()> {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("mhai".into(), vec!["hai hai".into()]);
        parser.macros.insert("quadhai".into(), vec!["%mhai %{mhai}".into()]);
        let mut out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("quadhai".into()))?;
        assert_eq!(out, "hai hai hai hai");
        Ok(())
    }

    #[test]
    fn text_quoting_recursive_macro_expand() -> Result<()> {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("mhai".into(), vec!["hai hai".into()]);
        parser.macros.insert("idk".into(), vec!["%!?mhai %?!mhai %{mhai}".into()]);
        parser.macros.insert("idk2".into(), vec!["%{?mhai} %{!mhai} %{!?mhai} %{?!mhai}".into()]);
        parser.macros.insert("aaa".into(), vec!["%idk %idk2".into()]);
        let mut out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("aaa".into()))?;
        assert_eq!(out, "  hai hai hai hai hai hai  ");
        Ok(())
    }

    #[test]
    fn shell_macro_expand() -> Result<()> {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("x".into(), vec!["%(echo haai | sed 's/a/aa/g')".into()]);
        let mut out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("x".into()))?;
        assert_eq!(out, "haaaai");
        Ok(())
    }

    #[test]
    fn bad_flags() {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("a".into(), vec!["1".into()]);
        let mut out = String::new();
        parser.parse_macro::<RR>(&mut out, &mut ("%a?a".into())).unwrap();
        assert_eq!(out, "1?a");
    }

    #[test]
    fn presence_macro_expand() -> Result<()> {
        let mut parser = super::SpecParser::new();
        parser.macros.insert("x".into(), vec!["%{?not_exist:hai}%{!?not_exist:bai}".into()]);
        let mut out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("x".into()))?;
        assert_eq!(out, "bai");
        parser.macros.insert("not_exist".into(), vec!["wha".into()]);
        out = String::new();
        parser._start_parse_raw_macro::<RR>(&mut out, &mut ("x".into()))?;
        assert_eq!(out, "hai");
        Ok(())
    }

    #[test]
    fn param_macro_args_parsing() -> Result<()> {
        let mut parser = super::SpecParser::new();
        assert_eq!(
            parser._param_macro_args(&mut Consumer::<RR>::from("-a hai -b asdfsdklj \\  \n abcd\ne"))?,
            ("-a hai -b asdfsdklj abcd".into(), vec!["hai".into(), "asdfsdklj".into(), "abcd".into()], vec!["a".into(), "b".into()])
        );
        Ok(())
    }

    #[test]
    fn param_macro_expand() {
        let mut p = super::SpecParser::new();
        p.macros.insert("hai".into(), vec![MacroType::Runtime { s: Arc::new(RwLock::new("hai, %1!".into())), file: Arc::from(Path::new("<?>")), offset: 0, param: true, len: 8 }]);
        let out = &mut String::new();
        p.parse_macro::<RR>(out, &mut "%hai madomado".into()).unwrap();
        assert_eq!(out, "hai, madomado!");
    }

    #[test]
    fn bad_macro() {
        let mut p = super::SpecParser::new();
        let out = &mut String::new();
        p.parse_macro::<RR>(out, &mut "%hai %{bai} %!?some %{!?what}".into()).unwrap();
        assert_eq!(out, "%hai %{bai}  ");
        out.clear();
        p.parse_macro::<RR>(out, &mut "%!a %{!b} %?c %{?d}".into()).unwrap();
        assert_eq!(out, "%a %{!b}  ");
    }

    #[test]
    fn simple_query() {
        let mut pkgs = vec![];
        Package::add_simple_query(&mut pkgs, "hai, bai some(stuff-1.0)").unwrap();
        assert_eq!(pkgs, vec![Package::new("hai".into()), Package::new("bai".into()), Package::new("some(stuff-1.0)".into())]);
        let _ = Package::add_simple_query(&mut pkgs, "bad!").unwrap_err();
        let _ = Package::add_simple_query(&mut pkgs, "also(bad").unwrap_err();
        let _ = Package::add_simple_query(&mut pkgs, "not-good >= 1.0").unwrap_err();
    }

    #[test]
    fn expression() {
        let mut p = super::SpecParser::new();
        let out = &mut String::new();
        p.macros.insert("hai".into(), vec![MacroType::Runtime { s: Arc::new(RwLock::new("0".into())), file: Arc::from(Path::new("<?>")), offset: 0, param: true, len: 1 }]);
        p.parse_macro::<RR>(out, &mut "%[1 + 2 * (3+4) - %hai]".into()).unwrap();
        assert_eq!(out, "15");
        out.clear();
        p.parse_macro::<RR>(out, &mut "%[(%hai + 1) ? 42 : 111]".into()).unwrap();
        assert_eq!(out, "42");
    }
}
