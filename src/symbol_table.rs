use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Type,
    pub stack_offset: i32,
}

#[derive(Default, Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    next_offset: i32,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable::default()
    }

    pub fn reserve_stack_space(&mut self, size: i32, align: i32) {
        let size = size.max(0);
        let align = align.max(1);

        self.next_offset -= size;
        let misalignment = (-self.next_offset) % align;
        if misalignment != 0 {
            self.next_offset -= align - misalignment;
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

        // Subtract size first, then align the result
        self.next_offset -= size;
        // Align down (toward more negative) to ensure the address is aligned
        let misalignment = (-self.next_offset) % align;
        if misalignment != 0 {
            self.next_offset -= align - misalignment;
        }
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

    /// Add or replace a variable, allocating a new stack offset even if it exists
    /// Used for statement expressions that may shadow variables
    pub fn add_or_replace_variable_with_layout(
        &mut self,
        name: String,
        var_type: Type,
        size: i32,
        align: i32,
    ) -> i32 {
        let size = size.max(0);
        let align = align.max(1);

        // Subtract size first, then align the result
        self.next_offset -= size;
        // Align down (toward more negative) to ensure the address is aligned
        let misalignment = (-self.next_offset) % align;
        if misalignment != 0 {
            self.next_offset -= align - misalignment;
        }
        let offset = self.next_offset;

        self.symbols.insert(
            name.clone(),
            Symbol {
                name,
                symbol_type: var_type,
                stack_offset: offset,
            },
        );
        offset
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
