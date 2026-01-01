use std::collections::HashMap;

use poni_arena::{Arena, define_arena_key};

define_arena_key!(StrId);

pub struct Ctx {
    strs: Arena<Vec<u8>, StrId>,
    str_side_map: HashMap<Vec<u8>, StrId>,
}

impl Ctx {
    pub fn put_str(&mut self, str: Vec<u8>) -> StrId {
        if let Some(existing) = self.str_side_map.get(&str) {
            return *existing;
        }

        let key = self.strs.push(str.clone());
        self.str_side_map.insert(str, key);

        key
    }

    pub fn put_and_clear_str(&mut self, str: &mut Vec<u8>) -> StrId {
        if let Some(existing) = self.str_side_map.get(str) {
            str.clear();
            return *existing;
        }

        let key = self.strs.push(str.clone());
        self.str_side_map.insert(std::mem::take(str), key);

        key
    }
}
