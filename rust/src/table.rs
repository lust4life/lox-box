use std::rc::Rc;

use crate::{
    chunk::{Value, Vec},
    object::ObjString,
};

pub struct Table {
    entries: Vec<Option<Entry>>,
}

impl Default for Table {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

type KeyType = Rc<ObjString>;

impl Table {
    const MAX_LOAD: f64 = 0.75;

    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    fn with_capacity(capacity: usize) -> Self {
        let mut entries = Vec::with_capacity(capacity);

        // initialize with an empty None value,
        // cause we need this to see if a slot has been set already
        for i in 0..capacity {
            entries.write(i, None, false)
        }
        Self { entries: entries }
    }

    fn is_need_grow(&self) -> bool {
        let count = self.entries.count + 1;
        return count as f64 > self.entries.capacity as f64 * Self::MAX_LOAD;
    }

    fn find_key_idx(&self, key: &KeyType) -> (usize, bool) {
        let mut idx = key.hash % self.entries.capacity;
        let mut tombestone_idx: Option<usize> = None;
        loop {
            match self.entries.read(idx) {
                Some(entry) => match entry.key {
                    Some(ref found_key) => {
                        if found_key == key {
                            return (idx, false);
                        }
                    }
                    None => {
                        if tombestone_idx == None {
                            tombestone_idx = Some(idx)
                        }
                    }
                },
                None => return (tombestone_idx.unwrap_or(idx), tombestone_idx != None),
            }

            idx = (idx + 1) % self.entries.capacity
        }
    }

    pub fn set(&mut self, key: KeyType, value: Value, only_update: bool) -> bool {
        if self.is_need_grow() {
            self.grow_array();
        }

        let (idx, is_tombsome) = self.find_key_idx(&key);
        let exist;
        match self.entries.read(idx) {
            Some(entry) => {
                entry.value = value;
                exist = true;
            }
            None => {
                if !only_update {
                    let entry = Some(Entry {
                        key: Some(key),
                        value,
                    });
                    self.entries.write(idx, entry, !is_tombsome);
                    exist = true;
                } else {
                    exist = false;
                }
            }
        };

        return exist;
    }

    pub fn get(&self, key: &KeyType) -> Option<&Value> {
        if self.entries.count == 0 {
            return None;
        }

        let item = self
            .entries
            .read(self.find_key_idx(key).0)
            .as_ref()
            .map(|x| &x.value);
        return item;
    }

    fn delete(&mut self, key: &KeyType) {
        if self.entries.count == 0 {
            return;
        }

        if let Some(entry) = self.entries.read(self.find_key_idx(key).0) {
            entry.set_removed();
        }
    }

    fn grow_array(&mut self) {
        let capacity = self.entries.grow_capacity();
        let mut new_table = Self::with_capacity(capacity);

        for idx in 0..self.entries.capacity {
            if let Some(entry) = self.entries.read(idx) {
                match entry.key {
                    Some(ref key) => {
                        new_table.set(key.clone(), entry.value.clone(), false);
                    }
                    None => (),
                }
            }
        }

        *self = new_table;
    }

    pub fn find_key(&self, key: &str, hash: usize) -> Option<KeyType> {
        if self.entries.count == 0 {
            return None;
        }

        let mut idx = hash % self.entries.capacity;
        loop {
            match self.entries.read(idx) {
                Some(entry) => match entry.key {
                    Some(ref found_key) => {
                        if found_key.hash == hash && found_key.chars == key {
                            return Some(found_key.clone());
                        }
                    }
                    None => (),
                },
                None => return None,
            }

            idx = (idx + 1) % self.entries.capacity
        }
    }
}

#[derive(Debug)]
pub struct Entry {
    key: Option<KeyType>,
    value: Value,
}

impl Entry {
    fn set_removed(&mut self) {
        self.key = None;
        self.value = Value::NIL;
    }
}

#[cfg(test)]
mod tests {
    use super::Table;

    #[test]
    fn poc() {
        let t = Table::with_capacity(8).entries;
        for i in 0..8 {
            let x = t.read(i);
            dbg!(i, x);
        }
    }
}
