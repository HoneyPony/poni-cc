use std::num::NonZeroU32;

use poni_arena::{Arena, ArenaKey, define_arena_key};
use rustc_hash::FxHashMap;

use crate::{ir, lexer::StrKey};

define_arena_key!(StrId);

pub struct Ctx {
    strs: Arena<String, StrId>,
    str_side_map: FxHashMap<Vec<u8>, StrId>,
    label_idx: usize,

    constant_one: StrId,
    constant_zero: StrId,
}

impl Ctx {
    pub fn new() -> Self {
        let mut strs = Arena::new();
        let constant_one = strs.push("1".into());
        let constant_zero = strs.push("0".into());

        Ctx {
            strs,
            str_side_map: FxHashMap::default(),
            label_idx: 0,

            constant_one,
            constant_zero
        }
    }

    #[expect(unused)]
    pub fn put_str(&mut self, str: &[u8]) -> StrId {
        if let Some(existing) = self.str_side_map.get(str) {
            return *existing;
        }

        let key = self.strs.push(String::from_utf8_lossy(&str).into_owned());
        self.str_side_map.insert(str.to_vec(), key);

        key
    }

    pub fn put_and_clear_str(&mut self, str: &mut Vec<u8>) -> StrId {
        // Taking the str clears it.
        let take = std::mem::take(str);

        // Taking it also lets us use the entry API, which should (?) let us
        // only do one lookup instead of two.
        let key = self.str_side_map.entry(take)
            .or_insert_with_key(|key| {
                let key = self.strs.push(String::from_utf8_lossy(&key).into_owned());
                key
            });

        *key
    }

    pub fn get_id(&self, id: StrId) -> &str {
        self.strs.get(id)
    }

    pub fn get<'a>(&'a self, str: &'a StrKey, buf: &'a mut [u8; 8]) -> &'a str {
        let inner = str.0.get();
        let as_bytes = inner.to_le_bytes();
        if as_bytes[7] == 0 {
            let mut as_bytes_u32: [u8; 4] = [0; 4];
            as_bytes_u32.copy_from_slice(&as_bytes[0..4]);
            let as_strid = u32::from_le_bytes(as_bytes_u32);
            // SAFETY: This is the invariant of the StrKey type.
            let as_strid = unsafe { StrId::from_nonzero_u32(NonZeroU32::new_unchecked(as_strid)) };
            self.get_id(as_strid)
        }
        else {
            buf.copy_from_slice(&as_bytes);
            let len = as_bytes[7] as usize;
            let ret = &buf[0..len];
            unsafe { str::from_utf8_unchecked(ret) }
        }
        // match str {
        //     StrKey::Id(str_id) => self.strs.get(*str_id),
        //     // TODO: This is not really right. I guess instead I should somehow
        //     // store the bytes themselves as a str? Or maybe just check them for
        //     // utf8? Idk...
        //     StrKey::Bytes(len, bytes) => unsafe {
        //         str::from_utf8_unchecked(&bytes[0..len.get() as usize])
        //     }
        // }
    }

    /// Creates a new temporary variable, by giving it a StrId that does not
    /// map to any string.
    pub fn tmp(&mut self) -> ir::Tmp {
        // No actual string is necessary, and we don't add this to the side
        // map either.
        let id = self.strs.push(String::new());
        ir::Tmp(id)
    }

    /// If the existing ir::Val is a temporary, steals its variable; otherwise
    /// synthesizes a new temporary variable.
    pub fn steal_tmp(&mut self, existing: ir::Val) -> ir::Tmp {
        if let ir::Val::Tmp(tmp) = existing {
            return ir::Tmp(tmp);
        }
        return self.tmp()
    }

    /// Same thing as tmp(), more or less, but slightly distinguished by not
    /// using a wrapper type for now. It would be nice to maybe do this in a
    /// more type-safe way.
    pub fn var(&mut self) -> ir::Var {
        let id = self.strs.push(String::new());
        id
    }

    pub fn label(&mut self, prefix: &'static str) -> ir::Label {
        let id = self.strs.push(format!(".L{}{}", prefix, self.label_idx));
        self.label_idx += 1;
        id
    }

    #[inline(always)]
    pub fn one(&self) -> StrKey {
        // TODO: Consider using from_bytes or w/e
       //StrKey::Id(self.constant_one)
       StrKey::from_known_bytes(b"1")
    }

    #[inline(always)]
    pub fn zero(&self) -> StrKey {
        StrKey::from_known_bytes(b"0")
       //StrKey::Id(self.constant_zero)
    }
}
