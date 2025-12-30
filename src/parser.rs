use crate::ast::*;
use crate::lexer::{FloatSuffix, IntSuffix, LocatedToken, SourceLocation, Token};
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<LocatedToken>,
    position: usize,
    type_aliases: std::collections::HashMap<String, Type>,
    struct_defs: std::collections::HashMap<String, Vec<StructField>>,
    union_defs: std::collections::HashMap<String, Vec<StructField>>,
}

#[derive(Clone, Copy)]
struct StorageClassSpecifiers {
    is_extern: bool,
    is_static: bool,
    is_auto: bool,
    is_register: bool,
    is_const: bool,
    is_volatile: bool,
}

impl Parser {
    pub fn new(tokens: Vec<LocatedToken>) -> Self {
        Parser {
            tokens,
            position: 0,
            type_aliases: std::collections::HashMap::new(),
            struct_defs: std::collections::HashMap::new(),
            union_defs: std::collections::HashMap::new(),
        }
    }

    /// Get the current token
    fn current_token(&self) -> &Token {
        &self.tokens[self.position].token
    }

    /// Get the current location
    fn current_location(&self) -> SourceLocation {
        self.tokens[self.position].location
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            if self.current_token() == &Token::Eof {
                break;
            }
            if self.is_struct_definition() {
                items.push(self.parse_struct_definition()?);
            } else if self.is_union_definition() {
                items.push(self.parse_union_definition()?);
            } else if self.is_enum_definition() {
                items.push(self.parse_enum_definition()?);
            } else {
                // Could be a function or global variable
                items.push(self.parse_declaration()?);
            }
        }

        // Add anonymous struct/union definitions to the AST
        for (name, fields) in &self.struct_defs {
            items.insert(0, AstNode::StructDef {
                name: name.clone(),
                fields: fields.clone(),
            });
        }

        for (name, fields) in &self.union_defs {
            items.insert(0, AstNode::UnionDef {
                name: name.clone(),
                fields: fields.clone(),
            });
        }

        Ok(AstNode::Program(items))
    }

    fn parse_declaration(&mut self) -> Result<AstNode, String> {
        // Check for typedef first
        if matches!(self.current_token(), Token::Typedef) {
            return self.parse_typedef();
        }

        // Check for storage class specifiers and type qualifiers
        let mut is_extern = false;
        let mut is_static = false;
        let mut is_auto = false;
        let mut is_register = false;
        let mut is_const = false;
        let mut is_volatile = false;

        // Parse storage class and qualifiers (can appear in any order)
        loop {
            match self.current_token() {
                Token::Extern if !is_extern => {
                    self.advance();
                    is_extern = true;
                }
                Token::Static if !is_static => {
                    self.advance();
                    is_static = true;
                }
                Token::Auto if !is_auto => {
                    self.advance();
                    is_auto = true;
                }
                Token::Register if !is_register => {
                    self.advance();
                    is_register = true;
                }
                Token::Const if !is_const => {
                    self.advance();
                    is_const = true;
                }
                Token::Volatile if !is_volatile => {
                    self.advance();
                    is_volatile = true;
                }
                _ => break,
            }
        }

        let var_type = self.parse_type()?;

        // Parse _Alignas if present
        let alignment = if matches!(self.current_token(), Token::Alignas) {
            self.advance();
            self.expect(Token::OpenParen)?;
            // _Alignas can take either a type or a constant expression
            let align_value = if self.is_type_start(Some(self.current_token())) {
                let ty = self.parse_type()?;
                self.type_alignment_value(&ty)?
            } else {
                self.parse_constant_expr()?
            };
            self.expect(Token::CloseParen)?;
            Some(align_value)
        } else {
            None
        };

        let storage = StorageClassSpecifiers {
            is_extern,
            is_static,
            is_auto,
            is_register,
            is_const,
            is_volatile,
        };
        let (var_type, name) = self.parse_declarator(var_type)?;

        // Check what follows to determine if it's a function or variable
        match self.current_token() {
            Token::OpenParen => {
                // It's a function (extern/static don't affect function declarations in our simple compiler)
                self.parse_function_rest(var_type, name)
            }
            Token::Semicolon | Token::Equals | Token::OpenBracket => {
                // It's a global variable
                self.parse_global_variable_with_storage(var_type, name, storage, alignment)
            }
            _ => Err(format!(
                "Expected '(', ';', '=', or '[' after identifier, got {:?}",
                self.current_token()
            )),
        }
    }

    fn parse_function_rest(&mut self, return_type: Type, name: String) -> Result<AstNode, String> {
        self.expect(Token::OpenParen)?;
        let mut params = Vec::new();
        let mut is_variadic = false;
        let mut kr_param_names = None;

        if self.current_token() != &Token::CloseParen {
            let is_prototype = self.is_type_start(Some(self.current_token()))
                || matches!(
                    self.current_token(),
                    Token::Const | Token::Volatile | Token::Ellipsis
                );

            if is_prototype {
                let parsed = self.parse_parameters()?;
                params = parsed.0;
                is_variadic = parsed.1;
            } else {
                kr_param_names = Some(self.parse_kr_parameter_names()?);
            }
        }

        self.expect(Token::CloseParen)?;

        if let Some(names) = kr_param_names {
            let mut decls = HashMap::new();
            if self.current_token() != &Token::Semicolon
                && self.current_token() != &Token::OpenBrace
            {
                decls = self.parse_kr_parameter_decls(&names)?;
            }
            params = self.build_kr_parameters(&names, decls)?;
        }

        // Check if this is a forward declaration (semicolon) or definition (body)
        let body = if self.current_token() == &Token::Semicolon {
            // Forward declaration
            self.advance();
            None
        } else {
            // Function definition
            Some(Box::new(self.parse_block()?))
        };

        Ok(AstNode::Function {
            name,
            return_type,
            params,
            body,
            is_variadic,
        })
    }

    fn parse_kr_parameter_names(&mut self) -> Result<Vec<String>, String> {
        let mut names = Vec::new();
        loop {
            let name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => {
                    return Err(format!(
                        "Expected parameter name, got {:?}",
                        self.current_token()
                    ));
                }
            };
            if names.iter().any(|existing| existing == &name) {
                return Err(format!("Duplicate parameter name '{}'", name));
            }
            self.advance();
            names.push(name);

            if self.current_token() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(names)
    }

    fn parse_kr_parameter_decls(
        &mut self,
        param_names: &[String],
    ) -> Result<HashMap<String, Type>, String> {
        let mut decls = HashMap::new();

        while self.is_type_start(Some(self.current_token()))
            || matches!(self.current_token(), Token::Const | Token::Volatile)
        {
            while matches!(self.current_token(), Token::Const | Token::Volatile) {
                self.advance();
            }

            let base_type = self.parse_type()?;
            let (decl_type, name) = self.parse_declarator(base_type)?;

            if !param_names.iter().any(|param| param == &name) {
                return Err(format!(
                    "K&R parameter declaration for unknown name '{}'",
                    name
                ));
            }
            if decls.contains_key(&name) {
                return Err(format!("Duplicate K&R parameter declaration '{}'", name));
            }

            self.expect(Token::Semicolon)?;
            decls.insert(name, decl_type);
        }

        Ok(decls)
    }

    fn build_kr_parameters(
        &self,
        param_names: &[String],
        mut decls: HashMap<String, Type>,
    ) -> Result<Vec<Parameter>, String> {
        let mut params = Vec::new();

        for name in param_names {
            let param_type = decls.remove(name).unwrap_or(Type::Int);
            params.push(Parameter {
                name: name.clone(),
                param_type,
            });
        }

        if let Some((extra, _)) = decls.into_iter().next() {
            return Err(format!(
                "K&R declaration provided for extra parameter '{}'",
                extra
            ));
        }

        Ok(params)
    }

    fn parse_global_variable_with_storage(
        &mut self,
        mut var_type: Type,
        name: String,
        storage: StorageClassSpecifiers,
        alignment: Option<i64>,
    ) -> Result<AstNode, String> {
        let StorageClassSpecifiers {
            is_extern,
            is_static,
            is_auto,
            is_register,
            is_const,
            is_volatile,
        } = storage;

        // Handle array type suffix if present
        var_type = self.parse_array_type_suffix(var_type)?;

        // Parse initializer if present
        let init = if self.current_token() == &Token::Equals {
            self.advance();
            if self.current_token() == &Token::OpenBrace {
                if matches!(var_type, Type::Struct(_) | Type::Union(_)) {
                    Some(Box::new(self.parse_struct_initializer()?))
                } else {
                    Some(Box::new(self.parse_array_initializer()?))
                }
            } else {
                Some(Box::new(self.parse_expression()?))
            }
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(AstNode::VarDecl {
            name,
            var_type,
            init,
            is_extern,
            is_static,
            is_auto,
            is_register,
            is_const,
            is_volatile,
            alignment,
        })
    }

    fn parse_typedef(&mut self) -> Result<AstNode, String> {
        // Consume 'typedef' token
        self.expect(Token::Typedef)?;

        // Parse the target type and declarator
        let target_type = self.parse_type()?;
        let (target_type, name) = self.parse_declarator(target_type)?;

        // Expect semicolon
        self.expect(Token::Semicolon)?;

        // Store the type alias
        self.type_aliases.insert(name.clone(), target_type.clone());

        Ok(AstNode::TypedefDef { name, target_type })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let mut ty = match self.current_token() {
            Token::Unsigned => {
                self.advance();
                match self.current_token() {
                    Token::Char => {
                        self.advance();
                        Type::UChar
                    }
                    Token::Short => {
                        self.advance();
                        Type::UShort
                    }
                    Token::Long => {
                        self.advance();
                        if self.current_token() == &Token::Long {
                            self.advance();
                            if self.current_token() == &Token::Int {
                                self.advance();
                            }
                            Type::ULong // unsigned long long maps to Type::ULong (64-bit)
                        } else if self.current_token() == &Token::Int {
                            self.advance();
                            Type::ULong
                        } else {
                            Type::ULong
                        }
                    }
                    Token::Int => {
                        self.advance();
                        Type::UInt
                    }
                    _ => Type::UInt,
                }
            }
            Token::Short => {
                self.advance();
                if self.current_token() == &Token::Int {
                    self.advance();
                }
                Type::Short
            }
            Token::Long => {
                self.advance();
                if self.current_token() == &Token::Long {
                    self.advance();
                    if self.current_token() == &Token::Int {
                        self.advance();
                    }
                    Type::Long // long long maps to Type::Long (64-bit)
                } else if self.current_token() == &Token::Int {
                    self.advance();
                    Type::Long
                } else {
                    Type::Long
                }
            }
            Token::Int => {
                self.advance();
                Type::Int
            }
            Token::Char => {
                self.advance();
                Type::Char
            }
            Token::Void => {
                self.advance();
                Type::Void
            }
            Token::VaList => {
                self.advance();
                Type::VaList
            }
            Token::Float => {
                self.advance();
                Type::Float
            }
            Token::Double => {
                self.advance();
                Type::Double
            }
            Token::Struct => {
                self.advance();
                match self.current_token() {
                    Token::Identifier(s) => {
                        let name = s.clone();
                        self.advance();
                        Type::Struct(name)
                    }
                    Token::OpenBrace => {
                        // Anonymous struct - generate a unique name and parse definition
                        static ANON_COUNTER: std::sync::atomic::AtomicUsize =
                            std::sync::atomic::AtomicUsize::new(0);
                        let name = format!(
                            "__anon_struct_{}",
                            ANON_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                        );

                        // Parse struct definition
                        self.expect(Token::OpenBrace)?;
                        let mut members = Vec::new();
                        while self.current_token() != &Token::CloseBrace {
                            // Parse _Alignas if present
                            let alignment = if matches!(self.current_token(), Token::Alignas) {
                                self.advance();
                                self.expect(Token::OpenParen)?;
                                let align_value = if self.is_type_start(Some(self.current_token()))
                                {
                                    let ty = self.parse_type()?;
                                    self.type_alignment_value(&ty)?
                                } else {
                                    self.parse_constant_expr()?
                                };
                                self.expect(Token::CloseParen)?;
                                Some(align_value)
                            } else {
                                None
                            };

                            let base_type = self.parse_type()?;

                            // Parse all declarators (can have multiple like "char x, y;")
                            loop {
                                let (member_type, member_name) =
                                    self.parse_declarator(base_type.clone())?;
                                members.push(StructField {
                                    name: member_name,
                                    field_type: member_type,
                                    bit_width: None,
                                    alignment,
                                });

                                if self.current_token() == &Token::Comma {
                                    self.advance();
                                    continue;
                                } else {
                                    break;
                                }
                            }

                            self.expect(Token::Semicolon)?;
                        }
                        self.expect(Token::CloseBrace)?;

                        // Register the struct
                        self.struct_defs.insert(name.clone(), members);
                        Type::Struct(name)
                    }
                    _ => {
                        return Err(format!(
                            "Expected struct name or {{, got {:?}",
                            self.current_token()
                        ));
                    }
                }
            }
            Token::Union => {
                self.advance();
                match self.current_token() {
                    Token::Identifier(s) => {
                        let name = s.clone();
                        self.advance();
                        Type::Union(name)
                    }
                    Token::OpenBrace => {
                        // Anonymous union - generate a unique name and parse definition
                        static ANON_COUNTER: std::sync::atomic::AtomicUsize =
                            std::sync::atomic::AtomicUsize::new(0);
                        let name = format!(
                            "__anon_union_{}",
                            ANON_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                        );

                        // Parse union definition
                        self.expect(Token::OpenBrace)?;
                        let mut members = Vec::new();
                        while self.current_token() != &Token::CloseBrace {
                            // Parse _Alignas if present
                            let alignment = if matches!(self.current_token(), Token::Alignas) {
                                self.advance();
                                self.expect(Token::OpenParen)?;
                                let align_value = if self.is_type_start(Some(self.current_token()))
                                {
                                    let ty = self.parse_type()?;
                                    self.type_alignment_value(&ty)?
                                } else {
                                    self.parse_constant_expr()?
                                };
                                self.expect(Token::CloseParen)?;
                                Some(align_value)
                            } else {
                                None
                            };

                            let base_type = self.parse_type()?;

                            // Parse all declarators (can have multiple like "char x, y;")
                            loop {
                                let (member_type, member_name) =
                                    self.parse_declarator(base_type.clone())?;
                                members.push(StructField {
                                    name: member_name,
                                    field_type: member_type,
                                    bit_width: None,
                                    alignment,
                                });

                                if self.current_token() == &Token::Comma {
                                    self.advance();
                                    continue;
                                } else {
                                    break;
                                }
                            }

                            self.expect(Token::Semicolon)?;
                        }
                        self.expect(Token::CloseBrace)?;

                        // Register the union
                        self.union_defs.insert(name.clone(), members);
                        Type::Union(name)
                    }
                    _ => {
                        return Err(format!(
                            "Expected union name or {{, got {:?}",
                            self.current_token()
                        ));
                    }
                }
            }
            Token::Enum => {
                self.advance();
                let name = match self.current_token() {
                    Token::Identifier(s) => s.clone(),
                    _ => {
                        return Err(format!(
                            "Expected enum name, got {:?}",
                            self.current_token()
                        ));
                    }
                };
                self.advance();
                Type::Enum(name)
            }
            Token::Identifier(name) => {
                // Check if this is a typedef'd type
                if let Some(aliased_type) = self.type_aliases.get(name).cloned() {
                    self.advance();
                    aliased_type
                } else {
                    return Err(format!("Unknown type: {}", name));
                }
            }
            _ => return Err(format!("Expected type, got {:?}", self.current_token())),
        };

        while self.current_token() == &Token::Star {
            self.advance();
            ty = Type::Pointer(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_declarator(&mut self, base_type: Type) -> Result<(Type, String), String> {
        // Handle pointer prefix (e.g., *x or **x)
        let mut pointer_depth = 0;
        while self.current_token() == &Token::Star {
            self.advance();
            pointer_depth += 1;
        }

        if self.current_token() == &Token::OpenParen {
            let saved_pos = self.position;
            self.advance();
            if self.current_token() == &Token::Star {
                let mut inner_pointer_depth = 0;
                while self.current_token() == &Token::Star {
                    self.advance();
                    inner_pointer_depth += 1;
                }

                let name = match self.current_token() {
                    Token::Identifier(s) => s.clone(),
                    _ => {
                        return Err(
                            "Expected identifier in function pointer declarator".to_string()
                        );
                    }
                };
                self.advance();
                self.expect(Token::CloseParen)?;
                if self.current_token() != &Token::OpenParen {
                    return Err("Expected '(' after function pointer declarator".to_string());
                }
                self.advance();
                let param_types = self.parse_param_type_list()?;
                self.expect(Token::CloseParen)?;

                let mut decl_type = Type::FunctionPointer {
                    return_type: Box::new(base_type),
                    param_types,
                };
                for _ in 1..inner_pointer_depth {
                    decl_type = Type::Pointer(Box::new(decl_type));
                }
                for _ in 0..pointer_depth {
                    decl_type = Type::Pointer(Box::new(decl_type));
                }
                return Ok((decl_type, name));
            }
            self.position = saved_pos;
        }

        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
                    self.current_token(),
                    self.current_location()
                ));
            }
        };
        self.advance();
        let mut decl_type = self.parse_array_type_suffix(base_type)?;

        // Apply pointer prefix
        for _ in 0..pointer_depth {
            decl_type = Type::Pointer(Box::new(decl_type));
        }

        Ok((decl_type, name))
    }

    fn parse_param_declarator(
        &mut self,
        base_type: Type,
    ) -> Result<(Type, Option<String>), String> {
        if self.current_token() == &Token::OpenParen {
            let saved_pos = self.position;
            self.advance();
            if self.current_token() == &Token::Star {
                let mut pointer_depth = 0;
                while self.current_token() == &Token::Star {
                    self.advance();
                    pointer_depth += 1;
                }

                let name = match self.current_token() {
                    Token::Identifier(s) => {
                        let name = s.clone();
                        self.advance();
                        Some(name)
                    }
                    Token::CloseParen => None,
                    _ => {
                        return Err(
                            "Expected parameter name or ')' in function pointer declarator"
                                .to_string(),
                        );
                    }
                };
                self.expect(Token::CloseParen)?;
                if self.current_token() != &Token::OpenParen {
                    return Err("Expected '(' after function pointer declarator".to_string());
                }
                self.advance();
                let param_types = self.parse_param_type_list()?;
                self.expect(Token::CloseParen)?;

                let mut decl_type = Type::FunctionPointer {
                    return_type: Box::new(base_type),
                    param_types,
                };
                for _ in 1..pointer_depth {
                    decl_type = Type::Pointer(Box::new(decl_type));
                }
                return Ok((decl_type, name));
            }
            self.position = saved_pos;
        }

        if let Token::Identifier(s) = self.current_token() {
            let name = s.clone();
            self.advance();
            let decl_type = self.parse_array_type_suffix(base_type)?;
            return Ok((decl_type, Some(name)));
        }

        Ok((base_type, None))
    }

    fn parse_param_type_list(&mut self) -> Result<Vec<Type>, String> {
        let mut params = Vec::new();

        if self.current_token() == &Token::CloseParen {
            return Ok(params);
        }

        // Check for single 'void' parameter (means no parameters in C)
        if self.current_token() == &Token::Void {
            let saved_pos = self.position;
            self.advance();
            let next_is_close_paren = self.current_token() == &Token::CloseParen;
            self.position = saved_pos;

            if next_is_close_paren {
                self.advance(); // Skip 'void'
                return Ok(params);
            }
        }

        loop {
            if self.current_token() == &Token::Ellipsis {
                return Err("Variadic function pointer types are not supported".to_string());
            }

            while matches!(self.current_token(), Token::Const | Token::Volatile) {
                self.advance();
            }

            let param_type = self.parse_type()?;
            let (param_type, _name) = self.parse_param_declarator(param_type)?;
            params.push(param_type);

            if self.current_token() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_parameters(&mut self) -> Result<(Vec<Parameter>, bool), String> {
        let mut params = Vec::new();
        let mut is_variadic = false;

        if self.current_token() == &Token::CloseParen {
            return Ok((params, false));
        }

        // Check for single 'void' parameter (means no parameters in C)
        // But only if it's not part of a function pointer type (void (*func)(...))
        if self.current_token() == &Token::Void {
            // Peek ahead to see if this is followed by close paren
            let saved_pos = self.position;
            self.advance();
            let next_is_close_paren = self.current_token() == &Token::CloseParen;
            self.position = saved_pos;

            if next_is_close_paren {
                // This is a single void parameter, consume it and return empty list
                self.advance(); // Skip 'void'
                return Ok((params, false));
            }
            // Otherwise, fall through to normal parameter parsing
        }

        loop {
            // Check for ellipsis (...)
            if self.current_token() == &Token::Ellipsis {
                self.advance();
                is_variadic = true;
                break;
            }

            // Skip const and volatile qualifiers in function parameters
            while matches!(self.current_token(), Token::Const | Token::Volatile) {
                self.advance();
            }

            let param_type = self.parse_type()?;
            let (param_type, name) = self.parse_param_declarator(param_type)?;
            let name = name.unwrap_or_else(|| format!("_param{}", params.len()));

            params.push(Parameter { name, param_type });

            if self.current_token() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok((params, is_variadic))
    }

    fn parse_block(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;

        let mut statements = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            statements.push(self.parse_statement()?);
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.current_token() {
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_loop(),
            Token::Do => self.parse_do_while_loop(),
            Token::For => self.parse_for_loop(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Goto => self.parse_goto(),
            Token::Switch => self.parse_switch_statement(),
            Token::Case => self.parse_case(),
            Token::Default => self.parse_default(),
            Token::Asm => self.parse_inline_asm(),
            Token::Struct
                if matches!(self.peek(1), Some(&Token::Identifier(_)))
                    && matches!(self.peek(2), Some(&Token::OpenBrace)) =>
            {
                // struct T { ... }; - struct definition
                self.parse_struct_definition()
            }
            Token::Union
                if matches!(self.peek(1), Some(&Token::Identifier(_)))
                    && matches!(self.peek(2), Some(&Token::OpenBrace)) =>
            {
                // union U { ... }; - union definition
                self.parse_union_definition()
            }
            Token::Enum
                if matches!(self.peek(1), Some(&Token::Identifier(_)))
                    && matches!(self.peek(2), Some(&Token::OpenBrace)) =>
            {
                // enum E { ... }; - enum definition
                self.parse_enum_definition()
            }
            Token::Unsigned
            | Token::Int
            | Token::Char
            | Token::Short
            | Token::Long
            | Token::Float
            | Token::Double
            | Token::Struct
            | Token::Union
            | Token::Enum
            | Token::Void
            | Token::VaList
            | Token::Static
            | Token::Auto
            | Token::Register
            | Token::Const
            | Token::Volatile
            | Token::Alignas => self.parse_var_decl(),
            Token::Identifier(name) if self.type_aliases.contains_key(name) => {
                // This is a typedef'd type, parse as variable declaration
                self.parse_var_decl()
            }
            Token::Identifier(name) if matches!(self.peek(1), Some(&Token::Colon)) => {
                // This is a label
                let label_name = name.clone();
                self.advance(); // consume identifier
                self.advance(); // consume colon
                Ok(AstNode::Label(label_name))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(expr)
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<AstNode, String> {
        // Check for storage class and qualifiers
        let mut is_static = false;
        let mut is_auto = false;
        let mut is_register = false;
        let mut is_const = false;
        let mut is_volatile = false;

        // Parse storage class and qualifiers (can appear in any order)
        loop {
            match self.current_token() {
                Token::Static if !is_static => {
                    self.advance();
                    is_static = true;
                }
                Token::Auto if !is_auto => {
                    self.advance();
                    is_auto = true;
                }
                Token::Register if !is_register => {
                    self.advance();
                    is_register = true;
                }
                Token::Const if !is_const => {
                    self.advance();
                    is_const = true;
                }
                Token::Volatile if !is_volatile => {
                    self.advance();
                    is_volatile = true;
                }
                _ => break,
            }
        }

        // Parse _Alignas if present
        let alignment = if matches!(self.current_token(), Token::Alignas) {
            self.advance();
            self.expect(Token::OpenParen)?;
            // _Alignas can take either a type or a constant expression
            let align_value = if self.is_type_start(Some(self.current_token())) {
                let ty = self.parse_type()?;
                self.type_alignment_value(&ty)?
            } else {
                self.parse_constant_expr()?
            };
            self.expect(Token::CloseParen)?;
            Some(align_value)
        } else {
            None
        };

        let base_type = self.parse_type()?;

        // Parse first declarator
        let mut decls = Vec::new();
        loop {
            let (var_type, name) = self.parse_declarator(base_type.clone())?;

            let init = if self.current_token() == &Token::Equals {
                self.advance();
                if self.current_token() == &Token::OpenBrace {
                    if matches!(var_type, Type::Struct(_) | Type::Union(_)) {
                        Some(Box::new(self.parse_struct_initializer()?))
                    } else {
                        Some(Box::new(self.parse_array_initializer()?))
                    }
                } else {
                    Some(Box::new(self.parse_expression()?))
                }
            } else {
                None
            };

            decls.push(AstNode::VarDecl {
                name,
                var_type,
                init,
                is_extern: false,
                is_static,
                is_auto,
                is_register,
                is_const,
                is_volatile,
                alignment,
            });

            // Check for more declarators
            if self.current_token() == &Token::Comma {
                self.advance();
                continue;
            } else {
                break;
            }
        }

        self.expect(Token::Semicolon)?;

        // If multiple declarations, return a block; otherwise return the single declaration
        if decls.len() == 1 {
            Ok(decls.into_iter().next().unwrap())
        } else {
            // Reverse the order to match GCC's stack layout (allocate in reverse declaration order)
            decls.reverse();
            Ok(AstNode::Block(decls))
        }
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let then_branch = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        let else_branch = if self.current_token() == &Token::Else {
            self.advance();
            if self.current_token() == &Token::OpenBrace {
                Some(Box::new(self.parse_block()?))
            } else {
                Some(Box::new(self.parse_statement()?))
            }
        } else {
            None
        };

        Ok(AstNode::IfStatement {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_while_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::While)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let body = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        Ok(AstNode::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_do_while_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Do)?;

        let body = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        self.expect(Token::While)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::DoWhileLoop {
            body: Box::new(body),
            condition: Box::new(condition),
        })
    }

    fn parse_for_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::For)?;
        self.expect(Token::OpenParen)?;

        let init = if self.current_token() == &Token::Semicolon {
            self.advance();
            None
        } else if matches!(
            self.current_token(),
            Token::Int
                | Token::Char
                | Token::Unsigned
                | Token::Short
                | Token::Long
                | Token::Float
                | Token::Double
                | Token::Struct
                | Token::Union
                | Token::Enum
                | Token::Void
                | Token::VaList
                | Token::Static
                | Token::Auto
                | Token::Register
                | Token::Const
                | Token::Volatile
        ) {
            let decl = self.parse_var_decl()?;
            Some(Box::new(decl))
        } else {
            return Err("For loop init must be a variable declaration or empty".to_string());
        };

        let condition = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };
        self.expect(Token::Semicolon)?;

        let increment = if self.current_token() == &Token::CloseParen {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(Token::CloseParen)?;

        let body = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        Ok(AstNode::ForLoop {
            init,
            condition,
            increment,
            body: Box::new(body),
        })
    }

    fn parse_return(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Return)?;

        let expr = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(Token::Semicolon)?;
        Ok(AstNode::Return(expr))
    }

    fn parse_break(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Break)?;
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Break)
    }

    fn parse_continue(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Continue)?;
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Continue)
    }

    fn parse_goto(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Goto)?;
        let label = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected label name after goto, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Goto(label))
    }

    fn parse_switch_statement(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Switch)?;
        self.expect(Token::OpenParen)?;
        let expr = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::OpenBrace)?;

        let mut body = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            body.push(self.parse_statement()?);
        }
        self.expect(Token::CloseBrace)?;

        Ok(AstNode::SwitchStatement {
            expr: Box::new(expr),
            body,
        })
    }

    fn parse_case(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Case)?;
        let value = match self.current_token() {
            Token::IntLiteral(n, _) => *n,
            _ => {
                return Err(format!(
                    "Expected integer literal after case, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();
        self.expect(Token::Colon)?;
        Ok(AstNode::Case(value))
    }

    fn parse_default(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Default)?;
        self.expect(Token::Colon)?;
        Ok(AstNode::Default)
    }

    fn parse_inline_asm(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Asm)?;
        self.expect(Token::OpenParen)?;

        let asm_code = match self.current_token() {
            Token::StringLiteral(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected string literal in asm statement, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();

        self.expect(Token::CloseParen)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::InlineAsm(asm_code))
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_comma()
    }

    fn parse_comma(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_assignment()?;

        while self.current_token() == &Token::Comma {
            self.advance();
            let right = self.parse_assignment()?;
            left = AstNode::BinaryOp {
                op: BinOp::Comma,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_assignment(&mut self) -> Result<AstNode, String> {
        let left = self.parse_ternary()?;

        match self.current_token() {
            Token::Equals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::StarEquals
            | Token::SlashEquals
            | Token::PercentEquals
            | Token::AmpersandEquals
            | Token::PipeEquals
            | Token::CaretEquals
            | Token::LessLessEquals
            | Token::GreaterGreaterEquals => {
                // left is now the lvalue (could be Variable, MemberAccess, ArrayIndex, Dereference)
                let op_token = self.current_token().clone();
                self.advance();
                let right_value = self.parse_assignment()?;

                let value = match op_token {
                    Token::Equals => right_value,
                    Token::PlusEquals => AstNode::BinaryOp {
                        op: BinOp::Add,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::MinusEquals => AstNode::BinaryOp {
                        op: BinOp::Subtract,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::StarEquals => AstNode::BinaryOp {
                        op: BinOp::Multiply,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::SlashEquals => AstNode::BinaryOp {
                        op: BinOp::Divide,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::PercentEquals => AstNode::BinaryOp {
                        op: BinOp::Modulo,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::AmpersandEquals => AstNode::BinaryOp {
                        op: BinOp::BitAnd,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::PipeEquals => AstNode::BinaryOp {
                        op: BinOp::BitOr,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::CaretEquals => AstNode::BinaryOp {
                        op: BinOp::BitXor,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::LessLessEquals => AstNode::BinaryOp {
                        op: BinOp::ShiftLeft,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::GreaterGreaterEquals => AstNode::BinaryOp {
                        op: BinOp::ShiftRight,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    _ => unreachable!(),
                };

                Ok(AstNode::Assignment {
                    target: Box::new(left),
                    value: Box::new(value),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_ternary(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_logical_or()?;

        if self.current_token() == &Token::Question {
            self.advance();
            let true_expr = self.parse_expression()?;
            self.expect(Token::Colon)?;
            let false_expr = self.parse_ternary()?;

            Ok(AstNode::TernaryOp {
                condition: Box::new(condition),
                true_expr: Box::new(true_expr),
                false_expr: Box::new(false_expr),
            })
        } else {
            Ok(condition)
        }
    }

    fn parse_logical_or(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_logical_and()?;

        while self.current_token() == &Token::LogicalOr {
            self.advance();
            let right = self.parse_logical_and()?;
            left = AstNode::BinaryOp {
                op: BinOp::LogicalOr,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_bitwise_or()?;

        while self.current_token() == &Token::LogicalAnd {
            self.advance();
            let right = self.parse_bitwise_or()?;
            left = AstNode::BinaryOp {
                op: BinOp::LogicalAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_or(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_bitwise_xor()?;

        while self.current_token() == &Token::Pipe {
            self.advance();
            let right = self.parse_bitwise_xor()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitOr,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_xor(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_bitwise_and()?;

        while self.current_token() == &Token::Caret {
            self.advance();
            let right = self.parse_bitwise_and()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitXor,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_and(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_equality()?;

        while self.current_token() == &Token::Ampersand {
            self.advance();
            let right = self.parse_equality()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current_token(), Token::EqualEqual | Token::NotEqual) {
            let op = match self.current_token() {
                Token::EqualEqual => BinOp::EqualEqual,
                Token::NotEqual => BinOp::NotEqual,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_shift()?;

        while matches!(
            self.current_token(),
            Token::Less | Token::Greater | Token::LessEqual | Token::GreaterEqual
        ) {
            let op = match self.current_token() {
                Token::Less => BinOp::Less,
                Token::Greater => BinOp::Greater,
                Token::LessEqual => BinOp::LessEqual,
                Token::GreaterEqual => BinOp::GreaterEqual,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_shift()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_shift(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_additive()?;

        while matches!(
            self.current_token(),
            Token::LessLess | Token::GreaterGreater
        ) {
            let op = match self.current_token() {
                Token::LessLess => BinOp::ShiftLeft,
                Token::GreaterGreater => BinOp::ShiftRight,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_additive()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_multiplicative()?;

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let op = match self.current_token() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Subtract,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_unary()?;

        while matches!(
            self.current_token(),
            Token::Star | Token::Slash | Token::Percent
        ) {
            let op = match self.current_token() {
                Token::Star => BinOp::Multiply,
                Token::Slash => BinOp::Divide,
                Token::Percent => BinOp::Modulo,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_unary()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<AstNode, String> {
        match self.current_token() {
            Token::PlusPlus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::PrefixIncrement(Box::new(operand)))
            }
            Token::MinusMinus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::PrefixDecrement(Box::new(operand)))
            }
            Token::LogicalNot => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::LogicalNot,
                    operand: Box::new(operand),
                })
            }
            Token::Plus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::Plus,
                    operand: Box::new(operand),
                })
            }
            Token::Minus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::Negate,
                    operand: Box::new(operand),
                })
            }
            Token::Tilde => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::BitNot,
                    operand: Box::new(operand),
                })
            }
            Token::Star => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::Dereference(Box::new(operand)))
            }
            Token::Ampersand => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::AddressOf(Box::new(operand)))
            }
            Token::Sizeof => self.parse_sizeof(),
            Token::Alignof => self.parse_alignof(),
            Token::Offsetof => self.parse_offsetof(),
            Token::VaStart => self.parse_va_start(),
            Token::VaArg => self.parse_va_arg(),
            Token::VaEnd => self.parse_va_end(),
            Token::OpenParen => {
                // Check if this is a cast expression: (type)expr
                if self.is_type_start(self.peek(1)) {
                    self.advance(); // consume (
                    let target_type = self.parse_type()?;
                    self.expect(Token::CloseParen)?;
                    let expr = self.parse_unary()?;
                    Ok(AstNode::Cast {
                        target_type,
                        expr: Box::new(expr),
                    })
                } else {
                    self.parse_primary()
                }
            }
            _ => self.parse_primary(),
        }
    }

    fn int_literal_node(&self, value: i64, suffix: IntSuffix) -> AstNode {
        match suffix {
            IntSuffix::None => AstNode::IntLiteral(value),
            IntSuffix::Unsigned => AstNode::Cast {
                target_type: Type::UInt,
                expr: Box::new(AstNode::IntLiteral(value)),
            },
            IntSuffix::Long => AstNode::Cast {
                target_type: Type::Long,
                expr: Box::new(AstNode::IntLiteral(value)),
            },
            IntSuffix::UnsignedLong => AstNode::Cast {
                target_type: Type::ULong,
                expr: Box::new(AstNode::IntLiteral(value)),
            },
        }
    }

    fn float_literal_node(&self, value: f64, suffix: FloatSuffix) -> AstNode {
        match suffix {
            FloatSuffix::None | FloatSuffix::LongDouble => AstNode::FloatLiteral(value),
            FloatSuffix::Float => AstNode::Cast {
                target_type: Type::Float,
                expr: Box::new(AstNode::FloatLiteral(value)),
            },
        }
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        let mut expr = match self.current_token() {
            Token::IntLiteral(n, suffix) => {
                let val = *n;
                let suffix = *suffix;
                self.advance();
                self.int_literal_node(val, suffix)
            }
            Token::FloatLiteral(f, suffix) => {
                let val = *f;
                let suffix = *suffix;
                self.advance();
                self.float_literal_node(val, suffix)
            }
            Token::CharLiteral(c) => {
                let val = *c;
                self.advance();
                AstNode::CharLiteral(val)
            }
            Token::StringLiteral(s) => {
                // C89 string literal concatenation: adjacent string literals are concatenated
                let mut val = s.clone();
                self.advance();

                // Concatenate any adjacent string literals
                while matches!(self.current_token(), Token::StringLiteral(_)) {
                    if let Token::StringLiteral(next_s) = self.current_token() {
                        val.push_str(next_s);
                        self.advance();
                    }
                }

                AstNode::StringLiteral(val)
            }
            Token::WideStringLiteral(s) => {
                let mut val = s.clone();
                self.advance();

                while matches!(self.current_token(), Token::WideStringLiteral(_)) {
                    if let Token::WideStringLiteral(next_s) = self.current_token() {
                        val.push_str(next_s);
                        self.advance();
                    }
                }

                AstNode::WideStringLiteral(val)
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                AstNode::Variable(name)
            }
            Token::OpenParen => {
                self.advance();
                // Check for statement expression: ({ stmts; expr })
                if self.current_token() == &Token::OpenBrace {
                    self.advance(); // consume {
                    let mut stmts = Vec::new();
                    let mut last_expr = None;

                    // Parse statements until we find the last expression
                    while self.current_token() != &Token::CloseBrace {
                        // Try to parse as a statement
                        let stmt = self.parse_statement()?;
                        // Check if this might be the final expression (no semicolon after)
                        if self.current_token() == &Token::CloseBrace {
                            // This is the final expression
                            last_expr = Some(stmt);
                            break;
                        }
                        stmts.push(stmt);
                    }

                    self.expect(Token::CloseBrace)?;
                    self.expect(Token::CloseParen)?;

                    // Statement expressions must have a result expression
                    let result =
                        last_expr.ok_or("Statement expression must have a result expression")?;
                    AstNode::StmtExpr {
                        stmts,
                        result: Box::new(result),
                    }
                } else {
                    // Regular parenthesized expression
                    let expr = self.parse_expression()?;
                    self.expect(Token::CloseParen)?;
                    expr
                }
            }
            _ => {
                return Err(format!(
                    "Expected expression, got {:?}",
                    self.current_token()
                ));
            }
        };

        loop {
            match self.current_token() {
                Token::OpenParen => {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(Token::CloseParen)?;
                    if let AstNode::Variable(name) = expr {
                        // Direct function call
                        expr = AstNode::FunctionCall { name, args };
                    } else {
                        // Indirect function call (through function pointer)
                        expr = AstNode::IndirectCall {
                            target: Box::new(expr),
                            args,
                        };
                    }
                }
                Token::OpenBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(Token::CloseBracket)?;
                    expr = AstNode::ArrayIndex {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Token::Dot | Token::Arrow => {
                    let through_pointer = self.current_token() == &Token::Arrow;
                    self.advance();
                    let member = match self.current_token() {
                        Token::Identifier(s) => s.clone(),
                        _ => {
                            return Err(format!(
                                "Expected member name, got {:?}",
                                self.current_token()
                            ));
                        }
                    };
                    self.advance();
                    expr = AstNode::MemberAccess {
                        base: Box::new(expr),
                        member,
                        through_pointer,
                    };
                }
                Token::PlusPlus => {
                    self.advance();
                    expr = AstNode::PostfixIncrement(Box::new(expr));
                }
                Token::MinusMinus => {
                    self.advance();
                    expr = AstNode::PostfixDecrement(Box::new(expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<AstNode>, String> {
        let mut args = Vec::new();

        if self.current_token() == &Token::CloseParen {
            return Ok(args);
        }

        loop {
            // Use parse_assignment instead of parse_expression to avoid
            // parsing commas as comma operators (commas are argument separators here)
            args.push(self.parse_assignment()?);

            if self.current_token() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.position += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.current_token() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, got {:?} at {}",
                expected,
                self.current_token(),
                self.current_location()
            ))
        }
    }

    fn parse_array_type_suffix(&mut self, base: Type) -> Result<Type, String> {
        // Collect all array dimensions first
        let mut dimensions = Vec::new();
        while self.current_token() == &Token::OpenBracket {
            self.advance();
            // Allow empty brackets [] for arrays with initializers (size will be inferred)
            let len = if self.current_token() == &Token::CloseBracket {
                0 // Placeholder size, will be inferred from initializer
            } else {
                match self.current_token() {
                    Token::IntLiteral(n, _) if *n >= 0 => {
                        let len = *n as usize;
                        self.advance();
                        len
                    }
                    _ => {
                        return Err(
                            "Array length must be a non-negative integer literal".to_string()
                        );
                    }
                }
            };
            self.expect(Token::CloseBracket)?;
            dimensions.push(len);
        }

        // Build array type from innermost to outermost (right to left)
        // For int x[2][3], dimensions = [2, 3]
        // We want Array(Array(Int, 3), 2)
        let mut ty = base;
        for &len in dimensions.iter().rev() {
            ty = Type::Array(Box::new(ty), len);
        }
        Ok(ty)
    }

    fn parse_array_initializer(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;
        let mut values = Vec::new();

        if self.current_token() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::ArrayInit(values));
        }

        loop {
            // Check if this is a nested initializer (for multi-dimensional arrays)
            if self.current_token() == &Token::OpenBrace {
                values.push(self.parse_array_initializer()?);
            } else {
                // Use parse_assignment to avoid parsing commas as comma operators
                values.push(self.parse_assignment()?);
            }

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::ArrayInit(values))
    }

    fn parse_struct_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Struct)?;
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected struct name, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();

        // Check for forward declaration (struct Foo;) vs definition (struct Foo { ... };)
        if self.current_token() == &Token::Semicolon {
            // Forward declaration - incomplete type
            self.advance();
            return Ok(AstNode::StructDef {
                name,
                fields: Vec::new(), // Empty fields indicates incomplete type
            });
        }

        self.expect(Token::OpenBrace)?;

        let mut fields = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            // Parse _Alignas if present
            let alignment = if matches!(self.current_token(), Token::Alignas) {
                self.advance();
                self.expect(Token::OpenParen)?;
                let align_value = if self.is_type_start(Some(self.current_token())) {
                    let ty = self.parse_type()?;
                    self.type_alignment_value(&ty)?
                } else {
                    self.parse_constant_expr()?
                };
                self.expect(Token::CloseParen)?;
                Some(align_value)
            } else {
                None
            };

            let base_type = self.parse_type()?;

            // Parse all declarators for this field (e.g., "char x, y, z;")
            loop {
                let (field_type, field_name) = self.parse_declarator(base_type.clone())?;

                // Check for bit-field syntax (: width)
                let bit_width = if self.current_token() == &Token::Colon {
                    self.advance();
                    match self.current_token() {
                        Token::IntLiteral(width, _) if *width > 0 => {
                            let w = *width as u32;
                            self.advance();
                            Some(w)
                        }
                        _ => {
                            return Err(
                                "Bit-field width must be a positive integer literal".to_string()
                            );
                        }
                    }
                } else {
                    None
                };

                fields.push(StructField {
                    name: field_name,
                    field_type,
                    bit_width,
                    alignment,
                });

                // Check if there are more declarators (comma-separated)
                if self.current_token() == &Token::Comma {
                    self.advance();
                    // Continue loop to parse next declarator
                } else {
                    break;
                }
            }

            self.expect(Token::Semicolon)?;
        }

        self.expect(Token::CloseBrace)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::StructDef { name, fields })
    }

    fn parse_struct_initializer(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;
        let mut fields = Vec::new();

        if self.current_token() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::StructInit(fields));
        }

        loop {
            // Check if this is a designated initializer (.field = value)
            let field_name = if self.current_token() == &Token::Dot {
                self.advance(); // consume '.'
                match self.current_token() {
                    Token::Identifier(name) => {
                        let field = name.clone();
                        self.advance(); // consume field name
                        self.expect(Token::Equals)?;
                        Some(field)
                    }
                    _ => {
                        return Err(format!(
                            "Expected field name after '.', got {:?}",
                            self.current_token()
                        ));
                    }
                }
            } else {
                None // Positional initializer
            };

            // Parse the value - check if it's a nested struct/union initializer
            let value = if self.current_token() == &Token::OpenBrace {
                // Nested struct/union initialization
                self.parse_struct_initializer()?
            } else {
                // Use parse_assignment to avoid parsing commas as comma operators
                self.parse_assignment()?
            };
            fields.push(StructInitField { field_name, value });

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::StructInit(fields))
    }

    fn parse_union_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Union)?;
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected union name, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();

        // Check for forward declaration (union Foo;) vs definition (union Foo { ... };)
        if self.current_token() == &Token::Semicolon {
            // Forward declaration - incomplete type
            self.advance();
            return Ok(AstNode::UnionDef {
                name,
                fields: Vec::new(), // Empty fields indicates incomplete type
            });
        }

        self.expect(Token::OpenBrace)?;

        let mut fields = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            // Parse _Alignas if present
            let alignment = if matches!(self.current_token(), Token::Alignas) {
                self.advance();
                self.expect(Token::OpenParen)?;
                let align_value = if self.is_type_start(Some(self.current_token())) {
                    let ty = self.parse_type()?;
                    self.type_alignment_value(&ty)?
                } else {
                    self.parse_constant_expr()?
                };
                self.expect(Token::CloseParen)?;
                Some(align_value)
            } else {
                None
            };

            let base_type = self.parse_type()?;

            // Parse all declarators for this field (e.g., "char x, y, z;")
            loop {
                let (field_type, field_name) = self.parse_declarator(base_type.clone())?;

                // Check for bit-field syntax (: width)
                let bit_width = if self.current_token() == &Token::Colon {
                    self.advance();
                    match self.current_token() {
                        Token::IntLiteral(width, _) if *width > 0 => {
                            let w = *width as u32;
                            self.advance();
                            Some(w)
                        }
                        _ => {
                            return Err(
                                "Bit-field width must be a positive integer literal".to_string()
                            );
                        }
                    }
                } else {
                    None
                };

                fields.push(StructField {
                    name: field_name,
                    field_type,
                    bit_width,
                    alignment,
                });

                // Check if there are more declarators (comma-separated)
                if self.current_token() == &Token::Comma {
                    self.advance();
                    // Continue loop to parse next declarator
                } else {
                    break;
                }
            }

            self.expect(Token::Semicolon)?;
        }

        self.expect(Token::CloseBrace)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::UnionDef { name, fields })
    }

    fn parse_enum_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Enum)?;
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected enum name, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();
        self.expect(Token::OpenBrace)?;

        let mut enumerators = Vec::new();
        let mut next_value = 0i64;

        while self.current_token() != &Token::CloseBrace {
            let enumerator_name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => {
                    return Err(format!(
                        "Expected enumerator name, got {:?}",
                        self.current_token()
                    ));
                }
            };
            self.advance();

            let value = if self.current_token() == &Token::Equals {
                self.advance();
                match self.current_token() {
                    Token::IntLiteral(n, _) => {
                        let val = *n;
                        self.advance();
                        next_value = val + 1;
                        Some(val)
                    }
                    _ => return Err("Enumerator value must be an integer literal".to_string()),
                }
            } else {
                let val = next_value;
                next_value += 1;
                Some(val)
            };

            enumerators.push(Enumerator {
                name: enumerator_name,
                value,
            });

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::EnumDef { name, enumerators })
    }

    fn parse_sizeof(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Sizeof)?;

        if self.current_token() == &Token::OpenParen {
            if self.is_type_start(self.peek(1)) {
                self.advance();
                let ty = self.parse_type()?;
                let ty = self.parse_array_type_suffix(ty)?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::SizeOfType(ty))
            } else {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::SizeOfExpr(Box::new(expr)))
            }
        } else {
            let expr = self.parse_unary()?;
            Ok(AstNode::SizeOfExpr(Box::new(expr)))
        }
    }

    fn parse_alignof(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Alignof)?;

        if self.current_token() == &Token::OpenParen {
            if self.is_type_start(self.peek(1)) {
                self.advance();
                let ty = self.parse_type()?;
                let ty = self.parse_array_type_suffix(ty)?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::AlignOfType(ty))
            } else {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::AlignOfExpr(Box::new(expr)))
            }
        } else {
            let expr = self.parse_unary()?;
            Ok(AstNode::AlignOfExpr(Box::new(expr)))
        }
    }

    fn parse_offsetof(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Offsetof)?;
        self.expect(Token::OpenParen)?;

        // Parse the struct/union type
        let struct_type = self.parse_type()?;

        self.expect(Token::Comma)?;

        // Parse the member name
        let member = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected member name in offsetof, got {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();

        self.expect(Token::CloseParen)?;

        Ok(AstNode::OffsetOf {
            struct_type,
            member,
        })
    }

    fn parse_va_start(&mut self) -> Result<AstNode, String> {
        self.expect(Token::VaStart)?;
        self.expect(Token::OpenParen)?;

        // Parse the va_list argument
        let ap = self.parse_assignment()?;

        self.expect(Token::Comma)?;

        // Parse the last_param argument
        let last_param = self.parse_assignment()?;

        self.expect(Token::CloseParen)?;

        Ok(AstNode::VaStart {
            ap: Box::new(ap),
            last_param: Box::new(last_param),
        })
    }

    fn parse_va_arg(&mut self) -> Result<AstNode, String> {
        self.expect(Token::VaArg)?;
        self.expect(Token::OpenParen)?;

        // Parse the va_list argument
        let ap = self.parse_assignment()?;

        self.expect(Token::Comma)?;

        // Parse the type argument
        let arg_type = self.parse_type()?;

        self.expect(Token::CloseParen)?;

        Ok(AstNode::VaArg {
            ap: Box::new(ap),
            arg_type,
        })
    }

    fn parse_va_end(&mut self) -> Result<AstNode, String> {
        self.expect(Token::VaEnd)?;
        self.expect(Token::OpenParen)?;

        // Parse the va_list argument
        let ap = self.parse_assignment()?;

        self.expect(Token::CloseParen)?;

        Ok(AstNode::VaEnd(Box::new(ap)))
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len() || self.current_token() == &Token::Eof
    }

    fn is_struct_definition(&self) -> bool {
        // Accept both full definitions (struct Foo { ... };) and forward declarations (struct Foo;)
        matches!(self.current_token(), Token::Struct)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && (matches!(self.peek(2), Some(Token::OpenBrace))
                || matches!(self.peek(2), Some(Token::Semicolon)))
    }

    fn is_union_definition(&self) -> bool {
        // Accept both full definitions (union Foo { ... };) and forward declarations (union Foo;)
        matches!(self.current_token(), Token::Union)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && (matches!(self.peek(2), Some(Token::OpenBrace))
                || matches!(self.peek(2), Some(Token::Semicolon)))
    }

    fn is_enum_definition(&self) -> bool {
        matches!(self.current_token(), Token::Enum)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && matches!(self.peek(2), Some(Token::OpenBrace))
    }

    fn is_type_start(&self, token: Option<&Token>) -> bool {
        match token {
            Some(Token::Identifier(name)) => self.type_aliases.contains_key(name),
            Some(Token::Unsigned)
            | Some(Token::Int)
            | Some(Token::Char)
            | Some(Token::Struct)
            | Some(Token::Union)
            | Some(Token::Enum)
            | Some(Token::Short)
            | Some(Token::Long)
            | Some(Token::Float)
            | Some(Token::Double)
            | Some(Token::Void) => true,
            _ => false,
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset).map(|lt| &lt.token)
    }

    fn parse_constant_expr(&mut self) -> Result<i64, String> {
        // For now, just parse integer literals
        // TODO: Support full constant expressions
        match self.current_token() {
            Token::IntLiteral(value, _) => {
                let val = *value;
                self.advance();
                Ok(val)
            }
            _ => Err(format!(
                "Expected constant expression, got {:?}",
                self.current_token()
            )),
        }
    }

    fn type_alignment_value(&self, ty: &Type) -> Result<i64, String> {
        // Return the alignment of the given type
        match ty {
            Type::Char | Type::UChar => Ok(1),
            Type::Short | Type::UShort => Ok(2),
            Type::Int | Type::UInt | Type::Float => Ok(4),
            Type::Long | Type::ULong | Type::Double | Type::Pointer(_) => Ok(8),
            Type::Struct(_) | Type::Union(_) => {
                // For now, return a default alignment
                // TODO: Calculate actual struct/union alignment
                Ok(8)
            }
            Type::Array(base, _) => self.type_alignment_value(base),
            _ => Ok(4), // Default to 4-byte alignment
        }
    }
}
