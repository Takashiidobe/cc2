use crate::ast::*;
use crate::symbol_table::SymbolTable;
use std::collections::HashMap;

pub struct CodeGenerator {
    output: String,
    symbol_table: SymbolTable,
    label_counter: usize,
    current_function_end_label: Option<String>,
    struct_layouts: HashMap<String, StructLayout>,
}

#[derive(Debug, Clone)]
struct StructLayout {
    fields: HashMap<String, StructFieldInfo>,
    alignment: i32,
    size: i32,
}

#[derive(Debug, Clone)]
struct StructFieldInfo {
    field_type: Type,
    offset: i32,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            symbol_table: SymbolTable::new(),
            label_counter: 0,
            current_function_end_label: None,
            struct_layouts: HashMap::new(),
        }
    }

    pub fn generate(&mut self, ast: &AstNode) -> Result<String, String> {
        self.output.clear();
        self.symbol_table = SymbolTable::new();
        self.label_counter = 0;
        self.current_function_end_label = None;
        self.struct_layouts.clear();
        self.collect_struct_layouts(ast)?;
        self.generate_node(ast)?;
        Ok(self.output.clone())
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn generate_node(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Program(functions) => {
                for func in functions {
                    self.generate_node(func)?;
                }
                Ok(())
            }
            AstNode::StructDef { .. } => Ok(()),
            AstNode::Function { name, body, params, .. } => {
                self.symbol_table = SymbolTable::new();

                let param_regs_64 = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                let param_regs_32 = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
                for (i, param) in params.iter().enumerate() {
                    let param_size = self.type_size(&param.param_type)?;
                    let param_align = self.type_alignment(&param.param_type)?;
                    let _offset = self.symbol_table.add_variable_with_layout(
                        param.name.clone(),
                        param.param_type.clone(),
                        param_size,
                        param_align,
                    )?;
                    if i < param_regs_64.len() {
                        // Parameter will be stored on stack
                    }
                }

                // Create end label for this function
                let end_label = self.next_label();
                self.current_function_end_label = Some(end_label.clone());

                // Save current output and generate function body first to discover all variables
                let saved_output = self.output.clone();
                self.output.clear();

                for (i, param) in params.iter().enumerate() {
                    if i < param_regs_64.len() {
                        let symbol = self.symbol_table.get_variable(&param.name).unwrap();
                        if matches!(param.param_type, Type::Int) {
                            self.emit(&format!(
                                "    movl {}, {}(%rbp)",
                                param_regs_32[i],
                                symbol.stack_offset
                            ));
                        } else {
                            self.emit(&format!(
                                "    movq {}, {}(%rbp)",
                                param_regs_64[i],
                                symbol.stack_offset
                            ));
                        }
                    }
                }

                self.generate_node(body)?;

                let body_code = self.output.clone();
                self.output = saved_output;

                // Now emit function with correct stack allocation
                let stack_size = self.symbol_table.get_stack_size();
                self.emit(&format!("    .globl {}", name));
                self.emit(&format!("{}:", name));
                self.emit("    pushq %rbp");
                self.emit("    movq %rsp, %rbp");
                if stack_size > 0 {
                    self.emit(&format!("    subq ${}, %rsp", stack_size));
                }

                // Append the body code
                self.output.push_str(&body_code);

                // Emit end label and epilogue
                self.emit(&format!("{}:", end_label));
                if stack_size > 0 {
                    self.emit("    movq %rbp, %rsp");
                }
                self.emit("    popq %rbp");
                self.emit("    ret");

                self.current_function_end_label = None;
                Ok(())
            }
            AstNode::Block(statements) => {
                for stmt in statements {
                    self.generate_node(stmt)?;
                }
                Ok(())
            }
            AstNode::Return(expr) => {
                if let Some(e) = expr {
                    self.generate_node(e)?;
                }
                if let Some(end_label) = &self.current_function_end_label {
                    self.emit(&format!("    jmp {}", end_label));
                }
                Ok(())
            }
            AstNode::IfStatement { condition, then_branch, else_branch } => {
                let else_label = self.next_label();
                let end_label = self.next_label();

                self.generate_node(condition)?;
                self.emit("    cmpq $0, %rax");
                if else_branch.is_some() {
                    self.emit(&format!("    je {}", else_label));
                } else {
                    self.emit(&format!("    je {}", end_label));
                }

                self.generate_node(then_branch)?;
                if else_branch.is_some() {
                    self.emit(&format!("    jmp {}", end_label));
                }

                if let Some(else_br) = else_branch {
                    self.emit(&format!("{}:", else_label));
                    self.generate_node(else_br)?;
                }

                self.emit(&format!("{}:", end_label));
                Ok(())
            }
            AstNode::WhileLoop { condition, body } => {
                let start_label = self.next_label();
                let end_label = self.next_label();

                self.emit(&format!("{}:", start_label));
                self.generate_node(condition)?;
                self.emit("    cmpq $0, %rax");
                self.emit(&format!("    je {}", end_label));

                self.generate_node(body)?;
                self.emit(&format!("    jmp {}", start_label));

                self.emit(&format!("{}:", end_label));
                Ok(())
            }
            AstNode::ForLoop { init, condition, increment, body } => {
                let start_label = self.next_label();
                let end_label = self.next_label();

                if let Some(init_node) = init {
                    self.generate_node(init_node)?;
                }

                self.emit(&format!("{}:", start_label));

                if let Some(cond) = condition {
                    self.generate_node(cond)?;
                    self.emit("    cmpq $0, %rax");
                    self.emit(&format!("    je {}", end_label));
                }

                self.generate_node(body)?;

                if let Some(inc) = increment {
                    self.generate_node(inc)?;
                }

                self.emit(&format!("    jmp {}", start_label));
                self.emit(&format!("{}:", end_label));
                Ok(())
            }
            AstNode::VarDecl { name, var_type, init } => {
                let var_size = self.type_size(var_type)?;
                let var_align = self.type_alignment(var_type)?;
                let offset = self.symbol_table.add_variable_with_layout(
                    name.clone(),
                    var_type.clone(),
                    var_size,
                    var_align,
                )?;

                if let Some(init_expr) = init {
                    if var_type.is_array() {
                        match init_expr.as_ref() {
                            AstNode::ArrayInit(values) => {
                                let elem_type = self.array_element_type_from_type(var_type)?;
                                let elem_size = self.type_size(&elem_type)?;
                                let array_len = match var_type {
                                    Type::Array(_, len) => *len,
                                    _ => 0,
                                };
                                if values.len() > array_len {
                                    return Err(format!(
                                        "Array initializer has {} elements, but array length is {}",
                                        values.len(),
                                        array_len
                                    ));
                                }
                                for (i, value) in values.iter().enumerate() {
                                    self.generate_node(value)?;
                                    let elem_offset = offset + (i as i32) * elem_size;
                                    if matches!(elem_type, Type::Int) {
                                        self.emit(&format!("    movl %eax, {}(%rbp)", elem_offset));
                                    } else {
                                        self.emit(&format!("    movq %rax, {}(%rbp)", elem_offset));
                                    }
                                }
                            }
                            _ => {
                                return Err("Array initializer must be a brace-enclosed list".to_string());
                            }
                        }
                    } else {
                        match (var_type, init_expr.as_ref()) {
                            (Type::Struct(struct_name), AstNode::StructInit(values)) => {
                                self.emit_struct_init(struct_name, values, offset)?;
                            }
                            _ => {
                                self.generate_node(init_expr)?;
                                if matches!(var_type, Type::Int) {
                                    self.emit(&format!("    movl %eax, {}(%rbp)", offset));
                                } else {
                                    self.emit(&format!("    movq %rax, {}(%rbp)", offset));
                                }
                            }
                        }
                    }
                }
                Ok(())
            }
            AstNode::Assignment { name, value } => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                let symbol_type = symbol.symbol_type.clone();
                let stack_offset = symbol.stack_offset;

                self.generate_node(value)?;
                if matches!(symbol_type, Type::Int) {
                    self.emit(&format!("    movl %eax, {}(%rbp)", stack_offset));
                } else {
                    self.emit(&format!("    movq %rax, {}(%rbp)", stack_offset));
                }
                Ok(())
            }
            AstNode::Variable(name) => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;

                if symbol.symbol_type.is_array() || matches!(symbol.symbol_type, Type::Struct(_)) {
                    self.emit(&format!("    leaq {}(%rbp), %rax", symbol.stack_offset));
                } else if matches!(symbol.symbol_type, Type::Int) {
                    self.emit(&format!("    movslq {}(%rbp), %rax", symbol.stack_offset));
                } else {
                    self.emit(&format!("    movq {}(%rbp), %rax", symbol.stack_offset));
                }
                Ok(())
            }
            AstNode::FunctionCall { name, args } => {
                let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

                for arg in args.iter() {
                    self.generate_node(arg)?;
                    self.emit("    pushq %rax");
                }

                for (i, _) in args.iter().enumerate().rev() {
                    if i < arg_regs.len() {
                        self.emit(&format!("    popq {}", arg_regs[i]));
                    }
                }

                self.emit(&format!("    call {}", name));

                Ok(())
            }
            AstNode::BinaryOp { op, left, right } => {
                match op {
                    BinOp::LogicalAnd => {
                        let end_label = self.next_label();
                        let false_label = self.next_label();

                        self.generate_node(left)?;
                        self.emit("    cmpq $0, %rax");
                        self.emit(&format!("    je {}", false_label));

                        self.generate_node(right)?;
                        self.emit("    cmpq $0, %rax");
                        self.emit(&format!("    je {}", false_label));

                        self.emit("    movq $1, %rax");
                        self.emit(&format!("    jmp {}", end_label));

                        self.emit(&format!("{}:", false_label));
                        self.emit("    movq $0, %rax");

                        self.emit(&format!("{}:", end_label));
                    }
                    BinOp::LogicalOr => {
                        let end_label = self.next_label();
                        let true_label = self.next_label();

                        self.generate_node(left)?;
                        self.emit("    cmpq $0, %rax");
                        self.emit(&format!("    jne {}", true_label));

                        self.generate_node(right)?;
                        self.emit("    cmpq $0, %rax");
                        self.emit(&format!("    jne {}", true_label));

                        self.emit("    movq $0, %rax");
                        self.emit(&format!("    jmp {}", end_label));

                        self.emit(&format!("{}:", true_label));
                        self.emit("    movq $1, %rax");

                        self.emit(&format!("{}:", end_label));
                    }
                    BinOp::Add => {
                        self.generate_add(left, right)?;
                    }
                    BinOp::Subtract => {
                        self.generate_subtract(left, right)?;
                    }
                    _ => {
                        let int_operands = self.are_int_operands(left, right);
                        self.generate_node(left)?;
                        self.emit("    pushq %rax");
                        self.generate_node(right)?;
                        self.emit("    popq %rcx");

                        match op {
                            BinOp::Multiply => {
                                if int_operands {
                                    self.emit("    imull %ecx, %eax");
                                    self.emit("    cltq");
                                } else {
                                    self.emit("    imulq %rcx, %rax");
                                }
                            }
                            BinOp::Divide => {
                                if int_operands {
                                    self.emit("    movl %eax, %ebx");
                                    self.emit("    movl %ecx, %eax");
                                    self.emit("    cltd");
                                    self.emit("    idivl %ebx");
                                    self.emit("    cltq");
                                } else {
                                    self.emit("    movq %rax, %rbx");
                                    self.emit("    movq %rcx, %rax");
                                    self.emit("    cqto");
                                    self.emit("    idivq %rbx");
                                }
                            }
                            BinOp::Less => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    setl %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::Greater => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    setg %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::LessEqual => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    setle %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::GreaterEqual => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    setge %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::EqualEqual => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    sete %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::NotEqual => {
                                if int_operands {
                                    self.emit("    cmpl %eax, %ecx");
                                } else {
                                    self.emit("    cmpq %rax, %rcx");
                                }
                                self.emit("    setne %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::Add | BinOp::Subtract | BinOp::LogicalAnd | BinOp::LogicalOr => unreachable!(),
                        }
                    }
                }
                Ok(())
            }
            AstNode::UnaryOp { op, operand } => {
                self.generate_node(operand)?;
                match op {
                    UnaryOp::LogicalNot => {
                        self.emit("    cmpq $0, %rax");
                        self.emit("    sete %al");
                        self.emit("    movzbq %al, %rax");
                    }
                    UnaryOp::Negate => {
                        self.emit("    negq %rax");
                    }
                }
                Ok(())
            }
            AstNode::AddressOf(expr) => {
                self.generate_lvalue(expr)?;
                Ok(())
            }
            AstNode::Dereference(expr) => {
                self.generate_node(expr)?;
                let operand_type = self.expr_type(expr)?;
                let pointee_type = match operand_type {
                    Type::Pointer(pointee) => *pointee,
                    _ => return Err("Cannot dereference non-pointer type".to_string()),
                };
                if matches!(pointee_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::ArrayIndex { array, index } => {
                let elem_type = self.array_element_type(array)?;
                self.generate_array_index_address(array, index)?;
                if matches!(elem_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::ArrayInit(_) => Err("Array initializer codegen not implemented".to_string()),
            AstNode::StructInit(_) => Err("Struct initializer codegen not implemented".to_string()),
            AstNode::MemberAccess { base, member, through_pointer } => {
                let field_type = self.member_access_type(base, member, *through_pointer)?;
                self.generate_member_access_address(base, member, *through_pointer)?;
                if matches!(field_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::SizeOfType(ty) => {
                let size = self.type_size(ty)?;
                self.emit(&format!("    movq ${}, %rax", size));
                Ok(())
            }
            AstNode::SizeOfExpr(expr) => {
                let ty = self.expr_type(expr)?;
                let size = self.type_size(&ty)?;
                self.emit(&format!("    movq ${}, %rax", size));
                Ok(())
            }
            AstNode::IntLiteral(n) => {
                self.emit(&format!("    movl ${}, %eax", n));
                self.emit("    cltq");
                Ok(())
            }
        }
    }

    fn generate_lvalue(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Variable(name) => {
                let offset = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .stack_offset;
                self.emit(&format!("    leaq {}(%rbp), %rax", offset));
                Ok(())
            }
            AstNode::Dereference(inner) => {
                self.generate_node(inner)?;
                Ok(())
            }
            AstNode::ArrayIndex { array, index } => {
                self.generate_array_index_address(array, index)?;
                Ok(())
            }
            AstNode::MemberAccess { base, member, through_pointer } => {
                self.generate_member_access_address(base, member, *through_pointer)?;
                Ok(())
            }
            _ => Err("Expected an lvalue expression".to_string()),
        }
    }

    fn generate_array_index_address(
        &mut self,
        array: &AstNode,
        index: &AstNode,
    ) -> Result<(), String> {
        let elem_size = self.array_element_size(array)?;

        self.generate_node(array)?;
        self.emit("    pushq %rax");

        self.generate_node(index)?;
        if elem_size != 1 {
            self.emit(&format!("    imulq ${}, %rax", elem_size));
        }
        self.emit("    popq %rcx");
        self.emit("    addq %rax, %rcx");
        self.emit("    movq %rcx, %rax");
        Ok(())
    }

    fn generate_member_access_address(
        &mut self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
    ) -> Result<(), String> {
        let (struct_name, is_address) = self.resolve_struct_base(base, through_pointer)?;
        let layout = self.struct_layouts.get(&struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?;
        let field = layout.fields.get(member)
            .ok_or_else(|| format!("Unknown field '{}' on struct {}", member, struct_name))?;

        if is_address {
            self.emit(&format!("    addq ${}, %rax", field.offset));
        } else {
            self.emit(&format!("    leaq {}(%rbp), %rax", field.offset));
        }
        Ok(())
    }

    fn generate_add(&mut self, left: &AstNode, right: &AstNode) -> Result<(), String> {
        let left_elem = self.pointer_elem_size(left)?;
        let right_elem = self.pointer_elem_size(right)?;
        let int_operands = self.are_int_operands(left, right);
        if left_elem.is_some() && right_elem.is_some() {
            return Err("Cannot add two pointers".to_string());
        }

        self.generate_node(left)?;
        self.emit("    pushq %rax");
        self.generate_node(right)?;
        self.emit("    popq %rcx");

        if let Some(elem_size) = left_elem {
            self.emit_scale("%rax", elem_size);
            self.emit("    addq %rcx, %rax");
        } else if let Some(elem_size) = right_elem {
            self.emit_scale("%rcx", elem_size);
            self.emit("    addq %rcx, %rax");
        } else if int_operands {
            self.emit("    addl %ecx, %eax");
            self.emit("    cltq");
        } else {
            self.emit("    addq %rcx, %rax");
        }

        Ok(())
    }

    fn generate_subtract(&mut self, left: &AstNode, right: &AstNode) -> Result<(), String> {
        let left_elem = self.pointer_elem_size(left)?;
        let right_elem = self.pointer_elem_size(right)?;
        let int_operands = self.are_int_operands(left, right);

        if let (Some(left_size), Some(right_size)) = (left_elem, right_elem) {
            if left_size != right_size {
                return Err("Pointer subtraction requires matching element sizes".to_string());
            }
            self.generate_node(left)?;
            self.emit("    pushq %rax");
            self.generate_node(right)?;
            self.emit("    popq %rcx");
            self.emit("    subq %rax, %rcx");
            self.emit("    movq %rcx, %rax");
            if left_size != 1 {
                self.emit(&format!("    movq ${}, %rbx", left_size));
                self.emit("    cqto");
                self.emit("    idivq %rbx");
            }
            return Ok(());
        }

        if right_elem.is_some() {
            return Err("Cannot subtract pointer from integer".to_string());
        }

        self.generate_node(left)?;
        self.emit("    pushq %rax");
        self.generate_node(right)?;
        self.emit("    popq %rcx");

        if let Some(elem_size) = left_elem {
            self.emit_scale("%rax", elem_size);
            self.emit("    subq %rax, %rcx");
            self.emit("    movq %rcx, %rax");
        } else if int_operands {
            self.emit("    subl %eax, %ecx");
            self.emit("    movl %ecx, %eax");
            self.emit("    cltq");
        } else {
            self.emit("    subq %rax, %rcx");
            self.emit("    movq %rcx, %rax");
        }

        Ok(())
    }

    fn pointer_elem_size(&self, node: &AstNode) -> Result<Option<i32>, String> {
        match node {
            AstNode::Variable(name) => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                self.element_size_from_type(&symbol.symbol_type)
            }
            _ => Ok(None),
        }
    }

    fn are_int_operands(&self, left: &AstNode, right: &AstNode) -> bool {
        match (self.expr_type(left), self.expr_type(right)) {
            (Ok(Type::Int), Ok(Type::Int)) => true,
            _ => false,
        }
    }

    fn emit_scale(&mut self, reg: &str, elem_size: i32) {
        if elem_size != 1 {
            self.emit(&format!("    imulq ${}, {}", elem_size, reg));
        }
    }

    fn array_element_size(&self, array: &AstNode) -> Result<i32, String> {
        match array {
            AstNode::Variable(name) => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                self.element_size_from_type(&symbol.symbol_type)?
                    .ok_or_else(|| "Array indexing requires array or pointer element type".to_string())
            }
            _ => Err("Array indexing only supports array or pointer variables".to_string()),
        }
    }

    fn element_size_from_type(&self, ty: &Type) -> Result<Option<i32>, String> {
        match ty {
            Type::Pointer(pointee) => Ok(Some(self.type_size(pointee)?)),
            Type::Array(elem, _) => Ok(Some(self.type_size(elem)?)),
            _ => Ok(None),
        }
    }

    fn array_element_type(&self, array: &AstNode) -> Result<Type, String> {
        match array {
            AstNode::Variable(name) => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                self.array_element_type_from_type(&symbol.symbol_type)
            }
            _ => Err("Array indexing only supports array or pointer variables".to_string()),
        }
    }

    fn array_element_type_from_type(&self, ty: &Type) -> Result<Type, String> {
        match ty {
            Type::Array(elem, _) => Ok(*elem.clone()),
            Type::Pointer(pointee) => Ok(*pointee.clone()),
            _ => Err("Array element type unavailable".to_string()),
        }
    }

    fn type_size(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Int => Ok(4),
            Type::Pointer(_) => Ok(8),
            Type::Void => Ok(0),
            Type::Array(elem, len) => Ok(self.type_size(elem)? * (*len as i32)),
            Type::Struct(name) => {
                let layout = self.struct_layouts.get(name)
                    .ok_or_else(|| format!("Unknown struct type: {}", name))?;
                Ok(layout.size)
            }
        }
    }

    fn type_alignment(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Int => Ok(4),
            Type::Pointer(_) => Ok(8),
            Type::Void => Ok(1),
            Type::Array(elem, _) => self.type_alignment(elem),
            Type::Struct(name) => {
                let layout = self.struct_layouts.get(name)
                    .ok_or_else(|| format!("Unknown struct type: {}", name))?;
                Ok(layout.alignment)
            }
        }
    }

    fn register_struct_layout(
        &mut self,
        name: &str,
        fields: &[StructField],
    ) -> Result<(), String> {
        if self.struct_layouts.contains_key(name) {
            return Err(format!("Struct '{}' already defined", name));
        }

        let mut offset = 0;
        let mut max_align = 1;
        let mut field_map = HashMap::new();
        for field in fields {
            let field_size = self.type_size(&field.field_type)?;
            let field_align = self.type_alignment(&field.field_type)?;
            if field_size == 0 {
                return Err(format!("Field '{}' has invalid size", field.name));
            }
            offset = align_to(offset, field_align);
            max_align = max_align.max(field_align);
            field_map.insert(
                field.name.clone(),
                StructFieldInfo {
                    field_type: field.field_type.clone(),
                    offset,
                },
            );
            offset += field_size;
        }
        let size = align_to(offset, max_align);

        self.struct_layouts.insert(
            name.to_string(),
            StructLayout {
                fields: field_map,
                alignment: max_align,
                size,
            },
        );

        Ok(())
    }

    fn emit_struct_init(
        &mut self,
        struct_name: &str,
        values: &[AstNode],
        base_offset: i32,
    ) -> Result<(), String> {
        let layout = self.struct_layouts.get(struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?;
        let field_count = layout.fields.len();
        if values.len() > field_count {
            return Err(format!(
                "Struct initializer has {} fields, but struct has {}",
                values.len(),
                field_count
            ));
        }

        let mut ordered_fields: Vec<StructFieldInfo> = layout.fields.values().cloned().collect();
        ordered_fields.sort_by_key(|info| info.offset);

        for (i, value) in values.iter().enumerate() {
            self.generate_node(value)?;
            let field = &ordered_fields[i];
            let dest_offset = base_offset + field.offset;
            if matches!(field.field_type, Type::Int) {
                self.emit(&format!("    movl %eax, {}(%rbp)", dest_offset));
            } else {
                self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
            }
        }

        Ok(())
    }

    fn collect_struct_layouts(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::StructDef { name, fields } = item {
                    self.register_struct_layout(name, fields)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_struct_base(
        &mut self,
        base: &AstNode,
        through_pointer: bool,
    ) -> Result<(String, bool), String> {
        match base {
            AstNode::Variable(name) => {
                let symbol_type = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .symbol_type
                    .clone();
                match (symbol_type, through_pointer) {
                    (Type::Struct(struct_name), false) => {
                        self.generate_lvalue(base)?;
                        Ok((struct_name, true))
                    }
                    (Type::Pointer(pointee), true) => match pointee.as_ref() {
                        Type::Struct(struct_name) => {
                            self.generate_node(base)?;
                            Ok((struct_name.clone(), true))
                        }
                        _ => Err("Pointer does not target a struct".to_string()),
                    },
                    _ => Err("Member access base is not a struct".to_string()),
                }
            }
            _ => Err("Unsupported member access base".to_string()),
        }
    }

    fn member_access_type(
        &self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
    ) -> Result<Type, String> {
        let base_type = self.expr_type(base)?;
        let struct_name = match (base_type, through_pointer) {
            (Type::Struct(name), false) => name,
            (Type::Pointer(pointee), true) => match *pointee {
                Type::Struct(name) => name,
                _ => return Err("Pointer does not target a struct".to_string()),
            },
            _ => return Err("Member access base is not a struct".to_string()),
        };
        let layout = self.struct_layouts.get(&struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?;
        let field = layout.fields.get(member)
            .ok_or_else(|| format!("Unknown field '{}' on struct {}", member, struct_name))?;
        Ok(field.field_type.clone())
    }

    fn expr_type(&self, expr: &AstNode) -> Result<Type, String> {
        match expr {
            AstNode::IntLiteral(_) => Ok(Type::Int),
            AstNode::Variable(name) => {
                let symbol = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                Ok(symbol.symbol_type.clone())
            }
            AstNode::AddressOf(inner) => {
                let inner_type = self.expr_type(inner)?;
                Ok(Type::Pointer(Box::new(inner_type)))
            }
            AstNode::Dereference(inner) => {
                let inner_type = self.expr_type(inner)?;
                match inner_type {
                    Type::Pointer(pointee) => Ok(*pointee),
                    _ => Err("Cannot dereference non-pointer type".to_string()),
                }
            }
            AstNode::ArrayIndex { array, .. } => {
                let array_type = self.expr_type(array)?;
                match array_type {
                    Type::Array(elem, _) => Ok(*elem),
                    Type::Pointer(pointee) => Ok(*pointee),
                    _ => Err("Cannot index non-array type".to_string()),
                }
            }
            AstNode::MemberAccess { base, member, through_pointer } => {
                self.member_access_type(base, member, *through_pointer)
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_type = self.expr_type(left)?;
                let right_type = self.expr_type(right)?;
                match op {
                    BinOp::Add => match (left_type, right_type) {
                        (Type::Pointer(pointee), Type::Int) => Ok(Type::Pointer(pointee)),
                        (Type::Int, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        _ => Err("Invalid operands for pointer addition".to_string()),
                    },
                    BinOp::Subtract => match (left_type, right_type) {
                        (Type::Pointer(pointee), Type::Int) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(_), Type::Pointer(_)) => Ok(Type::Int),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        _ => Err("Invalid operands for pointer subtraction".to_string()),
                    },
                    _ => Ok(Type::Int),
                }
            }
            AstNode::UnaryOp { .. } | AstNode::FunctionCall { .. } => Ok(Type::Int),
            _ => Err("Unsupported expression in sizeof".to_string()),
        }
    }

    fn emit(&mut self, line: &str) {
        self.output.push_str(line);
        self.output.push('\n');
    }
}

fn align_to(size: i32, alignment: i32) -> i32 {
    if alignment <= 1 {
        return size;
    }
    let rem = size % alignment;
    if rem == 0 {
        size
    } else {
        size + (alignment - rem)
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
