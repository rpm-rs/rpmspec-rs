//! Handlers for Lua \
//! FYI RPM spec has tight integration with Lua \
//!
//! BTW everything here is heavily rewritten to use `rlua`
//!
//! original C code creates a lib which is not really supported
//! in rlua so we kinda have to register a custom type instead
//! aka. `rlua::UserData` \
//! there are two things: `rpm` and `posix`. See more information:
//! <https://rpm-software-management.github.io/rpm/manual/lua.html>
use crate::parse::SpecParser;
use parking_lot::RwLock;
use rlua::{Lua, Result};
use std::sync::Arc;

// https://github.com/amethyst/rlua/blob/master/examples/repl.rs
fn repl() {
	Lua::new().context(|lua| {
		let mut editor = rustyline::DefaultEditor::new().expect("Can't make new rustyline::DefaultEditor");

		loop {
			let mut prompt = "> ";
			let mut line = String::new();

			loop {
				match editor.readline(prompt) {
					Ok(input) => line.push_str(&input),
					Err(_) => return,
				}

				match lua.load(&line).eval::<rlua::MultiValue>() {
					Ok(values) => {
						// editor.add_history_entry(line);
						println!("{}", values.iter().map(|value| format!("{value:?}")).collect::<Vec<_>>().join("\t"));
						break;
					}
					Err(rlua::Error::SyntaxError { incomplete_input: true, .. }) => {
						// continue reading input and append it to `line`
						line.push('\n'); // separate input lines
						prompt = ">> ";
					}
					Err(e) => {
						eprintln!("error: {e}");
						break;
					}
				}
			}
		}
	});
}

macro_rules! __lua {
	(@type $t:ty | $default:ty) => { $t };
	(@type | $default:ty) => { $default };
	($(mod $ext:ident{$(fn $name:ident($p:pat, $ctx:pat, $arg:pat$(=>$at:ty)?)$(: $res:ty)? $body:block)+})+) => {
		$(
			mod $ext {
				#[allow(unused_imports)]
				use crate::{macros::MacroType, parse::SpecParser};
				#[allow(unused_imports)]
				use base64::{engine::general_purpose::STANDARD, Engine};
				#[allow(unused_imports)]
				use rlua::{Context, ExternalError, Result};
				use parking_lot::RwLock;
				use std::sync::Arc;
				$(
					#[allow(clippy::unnecessary_wraps)]
					pub fn $name(
						$p: &Arc<RwLock<SpecParser>>,
						$ctx: Context,
						$arg: __lua!(@type $($at)? | String)
					) -> Result<__lua!(@type $($res)? | ())> $body
				)+
			}
		)+
		pub(crate) fn run(rpmparser: &Arc<RwLock<SpecParser>>, script: &str) -> Result<String> {
			let lua = Lua::new();
			let printout = Arc::new(RwLock::new(String::new()));
			lua.context(|ctx| -> rlua::Result<()> {
				let globals = ctx.globals();
				$(
					let $ext = ctx.create_table()?;
					$({
						let p = Arc::clone(rpmparser);
						$ext.set(stringify!($name), ctx.create_function(move |ctx, arg| $ext::$name(&p, ctx, arg))?)?;
					})+
					globals.set(stringify!($ext), $ext)?;
				)+
				let printout = Arc::clone(&printout);
				globals.set(
					"print",
					ctx.create_function(move |_, s: String| {
						printout.write().push_str(&s);
						Ok(())
					})?,
				)?;
				ctx.load(script).exec()?;
				Ok(())
			})?;
			Ok(Arc::try_unwrap(printout).expect("Cannot unwrap Arc for print() output in lua").into_inner())
		}
	};
}

__lua!(
	mod rpm {
		fn b64decode(_, _, arg): String {
			String::from_utf8(STANDARD.decode(arg).map_err(ExternalError::to_lua_err)?).map_err(ExternalError::to_lua_err)
		}
		fn b64encode(_, _, arg): String {
			Ok(STANDARD.encode(arg))
		}
		fn define(p, _, arg) {
			if let Some((name, def)) = arg.split_once(' ') {
				let mut def: String = def.into();
				let name: String = name.strip_suffix("()").map_or_else(
					|| name.into(),
					|name| {
						def.push(' ');
						name.into()
					},
				);
				p.write().macros.insert(name.into(), vec![(*def).into()]);
				Ok(())
			} else {
				Err("Invalid syntax: `%define {def}`".to_lua_err())
			}
		}
		fn execute(_, _, args=>Vec<String>): i32 {
			Ok(std::process::Command::new(&args[0]).args(&args[1..]).status().map_err(rlua::ExternalError::to_lua_err)?.code().unwrap_or(-1))
		}
		fn expand(p, _, arg): String {
			let mut out = smartstring::SmartString::new();
			p.write().parse_macro::<std::fs::File>(&mut out, &mut (&*arg).into()).map_err(rlua::ExternalError::to_lua_err)?;
			Ok(out.to_string())
		}
		// glob(_, _, arg=>(String, Option<String>))
		fn interactive(_, _, _=>()) {
			super::repl(); // lazy
			// todo mimic
			Ok(())
		}
		fn isdefined(p, _, name): (bool, bool) {
			if let Some(def) = p.read().macros.get(&*name) {
				if let Some(MacroType::Runtime { param, .. }) = def.last() {
					return Ok((true, *param));
				}
			}
			Ok((false, false))
		}
		fn load(p, _, arg) {
			p.write().load_macro_from_file(&std::path::PathBuf::from(arg)).map_err(rlua::ExternalError::to_lua_err)
		}
		// todo rpm.fd
		// * BEGIN: File operations
		fn open(_, _, _=>(String, String)) {
			todo!()
		}
		fn close(_, _, _=>()) {
			todo!()
		}
		fn flush(_, _, _=>()) {
			todo!()
		}
		fn read(_, _, _=>Option<usize>) {
			todo!()
		}
		fn seek(_, _, _=>(String, usize)) {
			todo!()
		}
		fn write(_, _, _=>(String, Option<usize>)) {
			todo!()
		}
		fn reopen(_, _, _) {
			todo!()
		}
		// ... END: File operations

		fn redirect2null(_, _, _) {
			todo!()
		}
		fn undefine(p, _, name) {
			p.write().macros.remove(&*name).ok_or_else(|| "error undefining macro".to_lua_err())?;
			Ok(())
		}
		fn vercmp(_, _, vers=>(String, String)): i8 {
			use rpmspec_common::expr::Version;
			use std::str::FromStr;
			let (v1, v2) = vers;
			let (v1, v2) = (Version::from_str(&v1).map_err(ExternalError::to_lua_err)?, Version::from_str(&v2).map_err(ExternalError::to_lua_err)?);
			Ok(v1.cmp(&v2) as i8)
		}
	}
	mod posix {
		fn access(_, _, args=>(String, Option<String>)): bool {
			let (path, mode) = args;
			let mode = mode.as_ref().map_or("f", std::string::String::as_str);
			let p = std::path::Path::new(&path);
			if mode.contains('f') && !p.exists(){
				return Ok(false);
			}
			use std::os::unix::fs::PermissionsExt;
			let perms = p.metadata().map_err(ExternalError::to_lua_err)?.permissions().mode();
			let x = perms & 0o7;
			let w = (perms >> 3) & 0o7;
			let r = (perms >> 6) & 0o7;
			if mode.contains('r') && r == 0 { return Ok(false); }
			if mode.contains('w') && w == 0 { return Ok(false); }
			if mode.contains('x') && x == 0 { return Ok(false); }
			Ok(true)
		}
		fn chdir(_, _, dir) {
			std::env::set_current_dir(dir).map_err(ExternalError::to_lua_err)
		}
		fn chmod(_, _, args=>(String, String)) {
			std::process::Command::new("chmod").arg(args.1).arg(args.0).status().map_err(ExternalError::to_lua_err)?;
			Ok(())
		}
		fn chown(_, _, args=>(String, String, String)) {
			let (path, user, group) = args;
			std::process::Command::new("chown").arg(format!("{user}:{group}")).arg(path).status().map_err(ExternalError::to_lua_err)?;
			Ok(())
		}
		fn ctermid(_, _, _=>()): String {
			// SAFETY: mado dunno??
			// well it's just pointer arithmetic + string manipulations
			let cstr = unsafe {
				// ctermid() can pass in null for current terminal
				let out = libc::ctermid(std::ptr::null_mut());
				core::ffi::CStr::from_ptr(out)
			};
			cstr.to_str().map_err(ExternalError::to_lua_err).map(ToOwned::to_owned)
		}
		fn dir(_, _, dir=>Option<String>): Vec<String> {
			let dir = match dir {
				Some(dir) => std::path::PathBuf::from(dir),
				None => std::env::current_dir().map_err(ExternalError::to_lua_err)?,
			};
			let rd = std::fs::read_dir(dir).map_err(ExternalError::to_lua_err)?;
			let mut res = vec![];
			for f in rd {
				let f = f.map_err(ExternalError::to_lua_err)?;
				res.push(f.path().to_string_lossy().to_string());
			}
			Ok(res)
		}
		fn errno(_, _, _=>()): (String, isize) {
			todo!()
		}
		fn exec(_, _, _=>Vec<String>) {
			todo!()
		}
		fn files(p, ctx, f=>Option<String>): Vec<String> {
			dir(p, ctx, f)
		}
		fn fork(_, _, _=>()): isize {
			todo!()
		}
		fn getcwd(_, _, _=>()): String {
			std::env::current_dir().map_err(ExternalError::to_lua_err).map(|p| p.to_string_lossy().to_string())
		}
		fn getenv(_, _, name=>String): String {
			std::env::var(name).map_err(ExternalError::to_lua_err)
		}
		// getgroup() return type???
		fn getlogin(_, _, _=>()): String {
			todo!()
		}
		fn getpasswd(_, _, _=>Vec<String>): String {
			todo!()
		}
		fn getprocessid(_, _, _): usize {
			todo!()
		}
		fn kill(_, _, _=>(usize, Option<usize>)) {
			todo!()
		}
		fn link(_, _, _=>(String, String)) {
			todo!()
		}
		fn mkdir(_, _, path) {
			std::fs::create_dir(path).map_err(ExternalError::to_lua_err)
		}
		fn mkfifo(_, _, _) {
			todo!()
		}
		// pathconf() return type???
		fn putenv(_, _, kv) {
			let (key, value) = kv.split_once('=').ok_or_else(|| "putenv(): Cannot find `=` in provided argument".to_lua_err())?;
			std::env::set_var(key, value);
			Ok(())
		}
		fn readlink(_, _, _): String {
			todo!()
		}
		fn rmdir(_, _, path) {
			std::fs::remove_dir(path).map_err(ExternalError::to_lua_err)
		}
		fn setgid(_, _, _) {
			todo!()
		}
		fn setuid(_, _, _) {
			todo!()
		}
		fn sleep(_, _, seconds=>u64) {
			std::thread::sleep(std::time::Duration::from_secs(seconds));
			Ok(())
		}
		// stat() return type???
		fn symlink(_, _, paths=>(String, String)) {
			let (path1, path2) = paths;
			let path1 = std::ffi::CString::new(path1).map_err(ExternalError::to_lua_err)?;
			let path2 = std::ffi::CString::new(path2).map_err(ExternalError::to_lua_err)?;
			if unsafe { libc::symlink(path1.as_ptr(), path2.as_ptr()) } != 0 {
				return Err("libc::symlink() returned -1".to_lua_err())
			}
			Ok(())
		}
		// sysconf() return type???
		// times() return type???
		fn ttyname(_, _, fd=>Option<i32>): String {
			Ok(unsafe { std::ffi::CStr::from_ptr(libc::ttyname(fd.unwrap_or(0))) }.to_string_lossy().to_string())
		}
		fn umask(_, _, _=>Option<String>): String {
			todo!()
		}
		fn uname(_, _, _): String {
			todo!()
		}
		fn utime(_, _, _=>(String, Option<usize>, Option<usize>)) {
			todo!()
		}
		fn wait(_, _, _=>usize) {
			todo!()
		}
		fn setenv(_, _, args=>(String, String, Option<bool>)) {
			let (name, value, should_override) = args;
			if should_override.unwrap_or(true) || matches!(std::env::var(&name), Err(std::env::VarError::NotPresent)) {
				std::env::set_var(name, value);
			}
			Ok(())
		}
		fn unsetenv(_, _, key) {
			std::env::remove_var(key);
			Ok(())
		}
	}
);
