use std::collections::HashMap;

use poni_arena::{Arena, define_arena_key};
use rustc_hash::FxHashMap;

use crate::ir;

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

    pub fn put_str(&mut self, str: &[u8]) -> StrId {
        if let Some(existing) = self.str_side_map.get(str) {
            return *existing;
        }

        let key = self.strs.push(String::from_utf8_lossy(&str).into_owned());
        self.str_side_map.insert(str.to_vec(), key);

        key
    }

    pub fn put_and_clear_str(&mut self, str: &mut Vec<u8>) -> StrId {
        if let Some(existing) = self.str_side_map.get(str) {
            str.clear();
            return *existing;
        }

        let key = self.strs.push(String::from_utf8_lossy(&str).into_owned());
        self.str_side_map.insert(std::mem::take(str), key);

        key
    }

    pub fn get(&self, str: StrId) -> &str {
        self.strs.get(str)
    }

    /// Creates a new temporary variable, by giving it a StrId that does not
    /// map to any string.
    pub fn tmp(&mut self) -> ir::Var {
        // No actual string is necessary, and we don't add this to the side
        // map either.
        let id = self.strs.push(String::new());
        id
    }

    pub fn label(&mut self, prefix: &'static str) -> ir::Label {
        let id = self.strs.push(format!(".L{}{}", prefix, self.label_idx));
        self.label_idx += 1;
        id
    }

    #[inline(always)]
    pub fn one(&self) -> StrId {
        self.constant_one
    }

    #[inline(always)]
    pub fn zero(&self) -> StrId {
        self.constant_zero
    }
}
