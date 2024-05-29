//! Handlers for Lua \
//! FYI RPM spec has tight integration with Lua \
//!
//! BTW everything here is heavily rewritten to use `mlua`
//!
//! original C code creates a lib which is not really supported
//! in mlua so we kinda have to register a custom type instead
//! aka. `mlua::UserData` \
//! there are two things: `rpm` and `posix`. See more information:
//! <https://rpm-software-management.github.io/rpm/manual/lua.html>
use crate::parse::SpecParser;
use mlua::{ExternalResult, Lua, Result};
use parking_lot::RwLock;
use std::{
    io::{Read, Seek, Write},
    str::FromStr,
    sync::Arc,
};

#[derive(Default)]
enum RpmFileIOType {
    Bzdio(bzip2::Decompress),
    Fdio,
    Gzdio(flate2::Decompress),
    #[default]
    Ufdio,
    Xzdio,
    Zstdio,
}
impl RpmFileIOType {
    fn process(&mut self, bs: Vec<u8>) -> mlua::Result<Vec<u8>> {
        match self {
            Self::Bzdio(dc) => {
                let mut buf = vec![];
                while dc.decompress_vec(&bs, &mut buf).into_lua_err()? == bzip2::Status::MemNeeded {
                    buf.try_reserve(buf.capacity()).into_lua_err()?;
                }
                Ok(buf)
            },
            Self::Fdio => Ok(bs),
            Self::Gzdio(dc) => {
                let mut buf = vec![];
                dc.decompress_vec(&bs, &mut buf, flate2::FlushDecompress::None).into_lua_err()?;
                Ok(buf)
            },
            Self::Ufdio => Ok(bs),
            Self::Xzdio => {
                // let mut buf = vec![];
                // dc.read_to_end(&mut buf);
                // Ok(buf)
                todo!()
            },
            Self::Zstdio => todo!(),
        }
    }
}

struct RpmFileMode {
    append: bool,
    write: bool,
    read: bool,
    fail_if_exist: bool,
    thread: bool,
    iodebug: bool,
    iotype: RpmFileIOType,
}

impl std::str::FromStr for RpmFileMode {
    type Err = mlua::Error;

    fn from_str(s: &str) -> Result<Self> {
        let (iotype, left) = if let Some((left, right)) = s.split_once('.') {
            let iotype = match right {
                "bzdio" => RpmFileIOType::Bzdio(bzip2::Decompress::new(false)),
                "fdio" => RpmFileIOType::Fdio,
                "gzdio" => RpmFileIOType::Gzdio(flate2::Decompress::new(true)),
                "ufdio" => RpmFileIOType::Ufdio,
                "xzdio" => RpmFileIOType::Xzdio,
                "zstdio" => RpmFileIOType::Zstdio,
                _ => return Err("Invalid iotype for rpm:open()/reopen()").into_lua_err(),
            };
            (iotype, left)
        } else {
            (RpmFileIOType::default(), s)
        };
        let append = left.contains('a');
        let write = left.contains('w') || left.contains('+');
        let read = left.contains('r') || left.contains('+');
        let fail_if_exist = left.contains('x');
        let thread = left.contains('T');
        let iodebug = left.contains('?');
        Ok(Self { append, write, read, fail_if_exist, thread, iodebug, iotype })
    }
}

struct RpmFile {
    path: String,
    innerfile: std::fs::File,
    modes: RpmFileMode,
}

impl RpmFile {
    fn new(path: String, mode: &str) -> mlua::Result<Self> {
        let modes = RpmFileMode::from_str(mode).into_lua_err()?;
        let mut f = std::fs::File::options();
        f.read(modes.read).write(modes.write);
        if modes.fail_if_exist {
            f.create_new(true);
        } else {
            f.create(true);
        }
        let mut innerfile = f.open(&path).into_lua_err()?;
        if modes.append {
            innerfile.seek(std::io::SeekFrom::End(0)).into_lua_err()?;
        }
        Ok(Self { path, innerfile, modes })
    }
}

impl mlua::UserData for RpmFile {
    fn add_methods<'lua, T: mlua::prelude::LuaUserDataMethods<'lua, Self>>(methods: &mut T) {
        methods.add_method("close", |_, _, ()| Ok(())); // literally do nothing
        methods.add_method_mut("flush", |_, this, ()| this.innerfile.flush().into_lua_err());
        methods.add_method_mut("read", |_, this, len: Option<usize>| {
            let Some(len) = len else {
                let mut buf = vec![];
                let size = this.innerfile.read_to_end(&mut buf).into_lua_err()?;
                buf.truncate(size);
                return this.modes.iotype.process(buf);
            };
            let mut buf = Vec::with_capacity(len);
            this.innerfile.read_exact(&mut buf).into_lua_err()?;
            this.modes.iotype.process(buf)
        });
        methods.add_method_mut("seek", |_, this, (mode, offset): (String, isize)| {
            this.innerfile
                .seek(match &*mode {
                    "set" => std::io::SeekFrom::Start(offset.try_into().into_lua_err()?),
                    "cur" => std::io::SeekFrom::Current(offset.try_into().into_lua_err()?),
                    "end" => std::io::SeekFrom::End(offset.try_into().into_lua_err()?),
                    _ => return Err(format!("Invalid mode for seek(): `{mode}`")).into_lua_err()?,
                })
                .into_lua_err()
        });
        methods.add_method_mut("write", |_, this, (buf, len): (String, usize)| this.innerfile.write(buf[..len].as_bytes()).into_lua_err());
        methods.add_method_mut("reopen", |_, this, mode: String| {
            let f = Self::new(this.path.clone(), &mode).into_lua_err()?;
            drop(std::mem::replace(this, f));
            Ok(())
        });
    }
}

// https://github.com/amethyst/mlua/blob/master/examples/repl.rs
fn repl() {
    let lua = Lua::new();
    let mut editor = rustyline::DefaultEditor::new().expect("Failed to create editor");

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            match editor.readline(prompt) {
                Ok(input) => line.push_str(&input),
                Err(_) => return,
            }

            match lua.load(&line).eval::<mlua::MultiValue>() {
                Ok(values) => {
                    editor.add_history_entry(line).unwrap();
                    println!("{}", values.iter().map(|value| format!("{value:#?}")).collect::<Vec<_>>().join("\t"));
                    break;
                },
                Err(mlua::Error::SyntaxError { incomplete_input: true, .. }) => {
                    // continue reading input and append it to `line`
                    line.push('\n'); // separate input lines
                    prompt = ">> ";
                },
                Err(e) => {
                    eprintln!("error: {e}");
                    break;
                },
            }
        }
    }
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
                use mlua::{ExternalError, Result};
                use parking_lot::RwLock;
                use std::sync::Arc;
                $(
                    #[allow(clippy::unnecessary_wraps)]
                    pub fn $name(
                        $p: &Arc<RwLock<SpecParser>>,
                        $ctx: &mlua::Lua,
                        $arg: __lua!(@type $($at)? | String)
                    ) -> Result<__lua!(@type $($res)? | ())> $body
                )+
            }
        )+
        pub(crate) fn run(rpmparser: &Arc<RwLock<SpecParser>>, script: &str) -> Result<String> {
            let lua = Lua::new();
            let printout = Arc::new(RwLock::new(String::new()));
            {
                let globals = lua.globals();
                $(
                    let $ext = lua.create_table()?;
                    $({
                        let p = Arc::clone(rpmparser);
                        $ext.set(stringify!($name), lua.create_function(move |lua, arg| $ext::$name(&p, lua, arg))?)?;
                    })+
                    globals.set(stringify!($ext), $ext)?;
                )+
                let printout = Arc::clone(&printout);
                globals.set(
                    "print",
                    lua.create_function(move |_, s: String| {
                        printout.write().push_str(&s);
                        Ok(())
                    })?,
                )?;
                lua.load(script).exec()?;
            }
            Ok(Arc::try_unwrap(printout).expect("Cannot unwrap Arc for print() output in lua").into_inner())
        }
    };
}

__lua!(
    mod rpm {
        fn b64decode(_, _, arg): String {
            String::from_utf8(STANDARD.decode(arg).map_err(ExternalError::into_lua_err)?).map_err(ExternalError::into_lua_err)
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
                Err("Invalid syntax: `%define {def}`".into_lua_err())
            }
        }
        fn execute(_, _, args=>Vec<String>): i32 {
            Ok(std::process::Command::new(&args[0]).args(&args[1..]).status().map_err(mlua::ExternalError::into_lua_err)?.code().unwrap_or(-1))
        }
        fn expand(p, _, arg): String {
            let mut out = smartstring::SmartString::new();
            p.write().parse_macro::<std::fs::File>(&mut out, &mut (&*arg).into()).map_err(mlua::ExternalError::into_lua_err)?;
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
            p.write().load_macro_from_file(&std::path::PathBuf::from(arg)).map_err(mlua::ExternalError::into_lua_err)
        }
        fn open(_, _, args=>(String, Option<String>)): super::RpmFile {
            let (path, mode) = args;
            super::RpmFile::new(path, mode.as_ref().map_or("+", |s| s))
        }
        fn undefine(p, _, name) {
            p.write().macros.remove(&*name).ok_or_else(|| "error undefining macro".into_lua_err())?;
            Ok(())
        }
        fn vercmp(_, _, vers=>(String, String)): i8 {
            use rpmspec_common::expr::Version;
            use std::str::FromStr;
            let (v1, v2) = vers;
            let (v1, v2) = (Version::from_str(&v1).map_err(ExternalError::into_lua_err)?, Version::from_str(&v2).map_err(ExternalError::into_lua_err)?);
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
            let perms = p.metadata().map_err(ExternalError::into_lua_err)?.permissions().mode();
            let x = perms & 0o7;
            let w = (perms >> 3) & 0o7;
            let r = (perms >> 6) & 0o7;
            if mode.contains('r') && r == 0 { return Ok(false); }
            if mode.contains('w') && w == 0 { return Ok(false); }
            if mode.contains('x') && x == 0 { return Ok(false); }
            Ok(true)
        }
        fn chdir(_, _, dir) {
            std::env::set_current_dir(dir).map_err(ExternalError::into_lua_err)
        }
        fn chmod(_, _, args=>(String, String)) {
            std::process::Command::new("chmod").arg(args.1).arg(args.0).status().map_err(ExternalError::into_lua_err)?;
            Ok(())
        }
        fn chown(_, _, args=>(String, String, String)) {
            let (path, user, group) = args;
            std::process::Command::new("chown").arg(format!("{user}:{group}")).arg(path).status().map_err(ExternalError::into_lua_err)?;
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
            cstr.to_str().map_err(ExternalError::into_lua_err).map(ToOwned::to_owned)
        }
        fn dir(_, _, dir=>Option<String>): Vec<String> {
            let dir = match dir {
                Some(dir) => std::path::PathBuf::from(dir),
                None => std::env::current_dir().map_err(ExternalError::into_lua_err)?,
            };
            let rd = std::fs::read_dir(dir).map_err(ExternalError::into_lua_err)?;
            let mut res = vec![];
            for f in rd {
                let f = f.map_err(ExternalError::into_lua_err)?;
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
            std::env::current_dir().map_err(ExternalError::into_lua_err).map(|p| p.to_string_lossy().to_string())
        }
        fn getenv(_, _, name=>String): String {
            std::env::var(name).map_err(ExternalError::into_lua_err)
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
            std::fs::create_dir(path).map_err(ExternalError::into_lua_err)
        }
        fn mkfifo(_, _, _) {
            todo!()
        }
        // pathconf() return type???
        fn putenv(_, _, kv) {
            let (key, value) = kv.split_once('=').ok_or_else(|| "putenv(): Cannot find `=` in provided argument".into_lua_err())?;
            std::env::set_var(key, value);
            Ok(())
        }
        fn readlink(_, _, _): String {
            todo!()
        }
        fn rmdir(_, _, path) {
            std::fs::remove_dir(path).map_err(ExternalError::into_lua_err)
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
            let path1 = std::ffi::CString::new(path1).map_err(ExternalError::into_lua_err)?;
            let path2 = std::ffi::CString::new(path2).map_err(ExternalError::into_lua_err)?;
            if unsafe { libc::symlink(path1.as_ptr(), path2.as_ptr()) } != 0 {
                return Err("libc::symlink() returned -1".into_lua_err())
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
