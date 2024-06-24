//! Decompression
//!
//! <https://github.com/rpm-software-management/rpm/blob/master/tools/rpmuncompress.c>

use std::{io::Read, path::Path};

/// Compression formats identified
pub enum CmprxFmt {
    /// .bzip2
    BZIP2,
    /// .zip
    ZIP,
    /// .xz
    XZ,
    /// .zstd
    ZSTD,
    /// .lzip
    LZIP,
    /// .lrzip
    LRZIP,
    /// gzip, old gzip, pack, SCO lzh, compress
    Other,
    /// .7z
    SEVENZIP,
    // oh well
    /// .lzma
    LZMA,
    /// .gem
    GEM,
    /// not identified
    Nil,
}

impl TryFrom<&Path> for CmprxFmt {
    type Error = std::io::Error;
    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let fmt = file_format::FileFormat::from_file(value)?;
        
        
        // Actuaslly we might not even need enums
        // can just delete this entire file
        match fmt {
            file_format::FileFormat::Bzip2 => Ok(Self::BZIP2),
        }
        // suggestion: use https://crates.io/crates/file-format ?
        // While gem is not a compression format, it's literally just a tarball
        let mut f = std::fs::File::open(value)?;
        f.read_exact(&mut buf)?;
        magic!(BZIP2: b'B' b'Z' b'h');
        magic!(ZIP: b'P' b'K' 3 4);
        magic!(ZIP: b'P' b'K' 0 0); // pkzip
        magic!(XZ: 0xfd 0x37 0x7a 0x58 0x5a 0x00);
        // new style xz (lzma) with magic
        magic!(ZSTD: 0x28 0x85 0x2f);
        magic!(LZIP: b'L' b'Z' b'I' b'P');
        magic!(LRZIP: b'L' b'R' b'Z' b'I');
        magic!(Other: 37 213); // gzip
        magic!(Other: 37 236); // old gzip
        magic!(Other: 37 36); // pack
        magic!(Other: 37 240); // SCO lzh
        magic!(Other: 37 235); // compress
        magic!(SEVENZIP: b'7' b'z' 0xbc 0xaf 0x27 0x1c);
        if let Some(Some(ext)) = value.extension().map(std::ffi::OsStr::to_str) {
            if ext == "lzma" || ext == "LZMA" {
                return Ok(Self::LZMA);
            }
            if ext == "gem" || ext == "GEM" {
                return Ok(Self::GEM); // tarballs in disguise
            }
        }
        Ok(Self::Nil)
    }
}
