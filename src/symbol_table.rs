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
        let size = var_type.size();
        let align = align_for_size(size);
        self.add_variable_with_layout(name, var_type, size, align)
    }

    pub fn add_variable_with_layout(
        &mut self,
        name: String,
        var_type: Type,
        size: i32,
        align: i32,
    ) -> Result<i32, String> {
        if self.symbols.contains_key(&name) {
            return Err(format!(
                "Variable '{}' already declared in this scope",
                name
            ));
        }

        let size = size.max(0);
        let align = align.max(1);
        let pad = (align - ((-self.next_offset) % align)) % align;
        self.next_offset -= pad;
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

fn align_for_size(size: i32) -> i32 {
    if size >= 8 {
        8
    } else if size >= 4 {
        4
    } else if size >= 2 {
        2
    } else {
        1
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
