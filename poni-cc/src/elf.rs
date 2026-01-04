//! Support for ELF files.
//! 
//! This is going to be the main object file (and maybe executable) format that
//! we support internally.

use std::{io::Write, ops::Add};

pub struct ElfWriter<W: Write> {
    writer: W,
}



type Byte   = u8;
type Half   = u16;
type Sword  = i32;
type Word   = u32;
type Xword  = u64;
type Sxword = i64;
type Addr   = u64;
type Off    = u64;

// e_type values
const ET_REL: Half = 1;

// e_machine values
const EM_X86_64: Half = 62;

// e_version values
const EV_CURRENT: Word = 2;

#[repr(C)]
struct ElfHeader {
    e_ident: [u8; 16],
    e_type: Half,
    e_machine: Half,
    e_version: Word,
    e_entry: Addr,
    e_phoff: Off,
    e_shoff: Off,
    e_flags: Word,
    e_ehsize: Half,
    e_phentsize: Half,
    e_phnum: Half,
    e_shentsize: Half,
    e_shnum: Half,
    e_shstrndx: Half,
}

#[repr(C)]
struct SectionHeader {
    name     : Word ,
    typ      : Word ,
    flags    : Word ,
    addr     : Addr ,
    offset   : Off  ,
    size     : Xword,
    link     : Word ,
    info     : Word ,
    addralign: Xword,
    entsize  : Xword,
}

#[repr(C)]
struct ProgramHeader {
    p_type: Word,
    p_flags: Word,
    p_offset: Off,
    p_vaddr: Addr,
    p_paddr: Addr,
    p_filesz: Xword,
    p_memsz: Xword,
    p_align: Xword,
}

pub struct Symbol<'name> {
    pub name  : &'name str,
    pub offset: u64,
}

impl<W: Write> ElfWriter<W> {
    pub fn new(writer: W) -> Self {
        ElfWriter { writer }
    }

    pub fn write(
        &mut self,
        text_section: &[u8],
        symbols: &[Symbol]
    ) -> std::io::Result<()> {
        let w = &mut self.writer;

        let header = ElfHeader {
            e_ident: *b"\x7fELF\x02\x01\x01\0\0\0\0\0\0\0\0\0",
            e_type: ET_REL,
            e_machine: EM_X86_64,
            e_version: EV_CURRENT,
            e_entry: 0,
            e_phoff: 0,
            e_shoff: 0,
            e_flags: 0,
            e_ehsize: size_of::<ElfHeader>() as Half,
            e_phentsize: size_of::<ProgramHeader>() as Half,
            e_phnum: 0,
            e_shentsize: size_of::<SectionHeader>() as Half,
            e_shnum: 0,
            e_shstrndx: 0,
        };

        Ok(())
    }
}