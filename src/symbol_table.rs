use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Type,
    pub stack_offset: i32,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    next_offset: i32,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            next_offset: 0,
        }
    }

    pub fn add_variable(&mut self, name: String, var_type: Type) -> Result<i32, String> {
        if self.symbols.contains_key(&name) {
            return Err(format!("Variable '{}' already declared in this scope", name));
        }

        let size = align_to_8(var_type.size());
        self.next_offset -= size;
        let offset = self.next_offset;
        self.symbols.insert(
            name.clone(),
            Symbol {
                name,
                symbol_type: var_type,
                stack_offset: offset,
            },
        );
        Ok(offset)
    }

    pub fn get_variable(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_stack_size(&self) -> i32 {
        -self.next_offset
    }
}

fn align_to_8(size: i32) -> i32 {
    if size % 8 == 0 {
        size
    } else {
        size + (8 - (size % 8))
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
