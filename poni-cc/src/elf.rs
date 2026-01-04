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
type Section = u16;

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

impl ElfHeader {
    // TODO: Do we have to manually add any padding?
    fn write<W: Write>(&self, out: &mut W) -> std::io::Result<()> {
        out.write_all(&self.e_ident)?;
        out.write_all(&self.e_type.to_le_bytes())?;
        out.write_all(&self.e_machine.to_le_bytes())?;
        out.write_all(&self.e_version.to_le_bytes())?;
        out.write_all(&self.e_entry.to_le_bytes())?;
        out.write_all(&self.e_phoff.to_le_bytes())?;
        out.write_all(&self.e_shoff.to_le_bytes())?;
        out.write_all(&self.e_flags.to_le_bytes())?;
        out.write_all(&self.e_ehsize.to_le_bytes())?;
        out.write_all(&self.e_phentsize.to_le_bytes())?;
        out.write_all(&self.e_phnum.to_le_bytes())?;
        out.write_all(&self.e_shentsize.to_le_bytes())?;
        out.write_all(&self.e_shnum.to_le_bytes())?;
        out.write_all(&self.e_shstrndx.to_le_bytes())?;
        Ok(())
    }
}


#[repr(C)]
struct SectionHeader {
    name     : Word ,
    typ      : Word ,
    flags    : Xword,
    addr     : Addr ,
    offset   : Off  ,
    size     : Xword,
    link     : Word ,
    info     : Word ,
    addralign: Xword,
    entsize  : Xword,
}

impl SectionHeader {
    // TODO: Do we have to manually add any padding?
    fn write<W: Write>(&self, out: &mut W) -> std::io::Result<()> {
        out.write_all(&self.name.to_le_bytes())?;
        out.write_all(&self.typ.to_le_bytes())?;
        out.write_all(&self.flags.to_le_bytes())?;
        out.write_all(&self.addr.to_le_bytes())?;
        out.write_all(&self.offset.to_le_bytes())?;
        out.write_all(&self.size.to_le_bytes())?;
        out.write_all(&self.link.to_le_bytes())?;
        out.write_all(&self.info.to_le_bytes())?;
        out.write_all(&self.addralign.to_le_bytes())?;
        out.write_all(&self.entsize.to_le_bytes())?;
        Ok(())
    }
}


const SHT_NULL    : Word = 0;
const SHT_PROGBITS: Word = 1;
const SHT_SYMTAB  : Word = 2;
const SHT_STRTAB  : Word = 3;

const SHF_WRITE    : Xword = 1;
const SHF_ALLOC    : Xword = 2;
const SHF_EXECINSTR: Xword = 4;
const SHF_MERGE    : Xword = 16;
const SHF_STRINGS  : Xword = 32;
const SHF_INFO_LINK: Xword = 64;

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

#[repr(transparent)]
struct StInfo(u8);

impl StInfo {
    pub fn from(bind: u8, typ: u8) -> Self {
        StInfo(bind << 4 + (typ & 0xF))
    }
}

#[repr(C)]
struct Sym {
    st_name: Word,
    st_info: StInfo,
    st_other: u8,
    st_shndx: Section,
    st_value: Addr,
    st_size: Xword,
}

impl Sym {
    // TODO: Do we have to manually add any padding?
    fn write<W: Write>(&self, out: &mut W) -> std::io::Result<()> {
        out.write_all(&self.st_name.to_le_bytes())?;
        out.write_all(&self.st_info.0.to_le_bytes())?;
        out.write_all(&self.st_other.to_le_bytes())?;
        out.write_all(&self.st_shndx.to_le_bytes())?;
        out.write_all(&self.st_value.to_le_bytes())?;
        out.write_all(&self.st_size.to_le_bytes())?;
        Ok(())
    }
}

// st_bind
const STB_GLOBAL: u8 = 1;
// st_type
const STT_FUNC: u8 = 2;

pub struct Symbol<'name> {
    pub name  : &'name [u8],
    pub offset: u64,
}

struct Strtab {
    current: Vec<u8>,
}

impl Strtab {
    fn new() -> Self {
        Strtab {
            // Start with a 0 byte as required.
            current: vec![0]
        }
    }

    fn push(&mut self, str: &[u8]) -> Word {
        let idx = self.current.len();

        // TODO: Consider checking that all of the bytes are nonzero? Or maybe
        // make str a &[NonZeroU8]??
        self.current.extend_from_slice(str);

        // Null terminate
        self.current.push(0);
        idx.try_into()
            .expect("strtab index too large")
    }
}

fn build_symtab(shndx: Section, strtab: &mut Strtab, symbols: &[Symbol]) -> Vec<u8> {
    let mut result = Vec::new();

    for symbol in symbols {
        let sym = Sym {
            st_name: strtab.push(symbol.name),
            st_info: StInfo::from(STB_GLOBAL, STT_FUNC),
            st_other: 0,
            st_shndx: shndx,
            st_value: symbol.offset,
            // TODO: Track symbol size...
            st_size: 0,
        };
        
        // Infallible
        sym.write(&mut result).unwrap();
    }

    result
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

        // Decide our section indices ahead of time
        let shndx_null    : Section = 0;
        let shndx_text    : Section = 1;
        let shndx_strtab  : Section = 2;
        let shndx_symtab  : Section = 3;
        // TODO: Consider putting shstrtab first..?
        let shndx_shstrtab: Section = 4;

        let mut strtab   = Strtab::new();
        let mut shstrtab = Strtab::new();

        let symtab = build_symtab(shndx_strtab, &mut strtab, symbols);

        let str_text     = shstrtab.push(b".text");
        let str_strtab   = shstrtab.push(b".strtab");
        let str_symtab   = shstrtab.push(b".symtab");
        let str_shstrtab = shstrtab.push(b".shstrtab");

        let offset_text = size_of::<ElfHeader>();
        // TODO: Round up alignment??
        let offset_strtab = offset_text + text_section.len();
        let offset_symtab = offset_strtab + strtab.current.len();
        let offset_shstrtab = offset_symtab + symtab.len();
        let offset_sectionheaders = offset_shstrtab + shstrtab.current.len();

        eprintln!("offsets:\n\ttext = {}\n\tstrtab = {}\n\tsymtab = {}\n\tshstrtab = {}\n\tsection headers = {}",
            offset_text, offset_strtab, offset_symtab, offset_shstrtab, offset_sectionheaders);

        let sh_null = SectionHeader {
            name: 0,
            typ: SHT_NULL,
            flags: 0,
            addr: 0,
            offset: 0,
            size: 0,
            link: 0,
            info: 0,
            addralign: 0,
            entsize: 0,
        };

        let sh_text = SectionHeader {
            name: str_text,
            typ: SHT_PROGBITS,
            flags: SHF_ALLOC | SHF_EXECINSTR | SHF_MERGE,
            addr: 0,
            offset: offset_text.try_into().expect("offset too big"),
            size: text_section.len().try_into().expect("size too big"),
            link: 0,
            info: 0,
            addralign: 1,
            entsize: 0,
        };

        let sh_strtab = SectionHeader {
            name: str_strtab,
            typ: SHT_STRTAB,
            flags: SHF_STRINGS,
            addr: 0,
            offset: offset_strtab.try_into().expect("offset too big"),
            size: strtab.current.len().try_into().expect("size too big"),
            link: 0,
            info: 0,
            addralign: 1,
            entsize: 0,
        };

        let sh_symtab = SectionHeader {
            name: str_symtab,
            typ: SHT_SYMTAB,
            flags: 0,
            addr: 0,
            offset: offset_symtab.try_into().expect("offset too big"),
            size: symtab.len().try_into().expect("size too big"),
            link: shndx_strtab.into(),
            info: 0,
            addralign: 1,
            entsize: size_of::<Sym>().try_into().unwrap(),
        };

        let sh_shstrtab = SectionHeader {
            name: str_shstrtab,
            typ: SHT_STRTAB,
            flags: SHF_STRINGS,
            addr: 0,
            offset: offset_shstrtab.try_into().expect("offset too big"),
            size: shstrtab.current.len().try_into().expect("size too big"),
            link: 0,
            info: 0,
            addralign: 1,
            entsize: 0,
        };

        let header = ElfHeader {
            e_ident: *b"\x7fELF\x02\x01\x01\0\0\0\0\0\0\0\0\0",
            e_type: ET_REL,
            e_machine: EM_X86_64,
            e_version: EV_CURRENT,
            e_entry: 0,
            e_phoff: 0,
            e_shoff: offset_sectionheaders.try_into().expect("e_shoff too big"),
            e_flags: 0,
            e_ehsize: size_of::<ElfHeader>() as Half,
            e_phentsize: size_of::<ProgramHeader>() as Half,
            e_phnum: 0,
            e_shentsize: size_of::<SectionHeader>() as Half,
            e_shnum: 5, // 5 section headers
            e_shstrndx: shndx_shstrtab,
        };

        // Now, write everything out.
        header.write(w)?;
        w.write_all(text_section)?;
        w.write_all(&strtab.current)?;
        w.write_all(&symtab)?;
        w.write_all(&shstrtab.current)?;
    
        sh_null.write(w)?;
        sh_text.write(w)?;
        sh_strtab.write(w)?;
        sh_symtab.write(w)?;
        sh_shstrtab.write(w)?;

        Ok(())
    }
}