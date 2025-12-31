use crate::ast::*;
use crate::symbol_table::SymbolTable;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct LoopContext {
    break_label: String,
    continue_label: String,
}

#[derive(Debug, Clone)]
struct GlobalVariable {
    var_type: Type,
    init: Option<AstNode>,
    is_extern: bool,
    is_static: bool,
    alignment: Option<i64>,
}

pub struct CodeGenerator {
    output: String,
    symbol_table: SymbolTable,
    label_counter: usize,
    current_function_end_label: Option<String>,
    loop_stack: Vec<LoopContext>,
    struct_layouts: HashMap<String, StructLayout>,
    union_layouts: HashMap<String, StructLayout>,
    enum_constants: HashMap<String, i64>,
    string_literals: Vec<(String, String)>, // (label, string_content)
    wide_string_literals: Vec<(String, String)>, // (label, string_content)
    float_literals: Vec<(String, f64)>,     // (label, value)
    global_variables: HashMap<String, GlobalVariable>,
    function_return_types: HashMap<String, Type>,
    function_param_types: HashMap<String, Vec<Type>>, // Function parameter types
    stack_depth: i32, // Number of 8-byte slots currently allocated below %rsp
    alloca_bottom_offset: Option<i32>, // Offset of __alloca_bottom__ variable from %rbp
}

#[derive(Debug, Clone)]
struct StructLayout {
    fields: Vec<StructFieldInfo>,
    fields_by_name: HashMap<String, StructFieldInfo>,
    alignment: i32,
    size: i32,
}

#[derive(Debug, Clone)]
struct StructFieldInfo {
    field_type: Type,
    offset: i32,
    bit_width: Option<u32>,
    bit_offset: u32,
    bit_unit_size: i32,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            symbol_table: SymbolTable::new(),
            label_counter: 0,
            current_function_end_label: None,
            loop_stack: Vec::new(),
            struct_layouts: HashMap::new(),
            union_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            string_literals: Vec::new(),
            wide_string_literals: Vec::new(),
            float_literals: Vec::new(),
            global_variables: HashMap::new(),
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            stack_depth: 0,
            alloca_bottom_offset: None,
        }
    }

    pub fn generate(&mut self, ast: &AstNode) -> Result<String, String> {
        self.output.clear();
        self.symbol_table = SymbolTable::new();
        self.label_counter = 0;
        self.current_function_end_label = None;
        self.loop_stack.clear();
        self.struct_layouts.clear();
        self.union_layouts.clear();
        self.enum_constants.clear();
        self.string_literals.clear();
        self.stack_depth = 0;
        self.wide_string_literals.clear();
        self.float_literals.clear();
        self.global_variables.clear();
        self.function_return_types.clear();
        self.function_param_types.clear();
        self.collect_struct_layouts(ast)?;
        self.collect_union_layouts(ast)?;
        self.collect_enum_constants(ast)?;
        self.collect_global_variables(ast)?;
        self.collect_static_locals(ast)?;
        self.collect_global_compound_literals()?;
        self.collect_function_signatures(ast);

        // Emit global variables first
        self.emit_global_variables()?;

        // Switch to .text section for functions
        if !self.global_variables.is_empty() {
            self.emit("    .text");
        }

        // Then generate code for functions
        self.generate_node(ast)?;

        // Emit .rodata section with string and float literals at the end
        self.emit_string_literals();
        self.emit_wide_string_literals();
        self.emit_float_literals();

        Ok(self.output.clone())
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn generate_node(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Program(items) => {
                for item in items {
                    // Skip global variables (already emitted) and type definitions
                    if !matches!(item, AstNode::VarDecl { .. }) {
                        self.generate_node(item)?;
                    }
                }
                Ok(())
            }
            AstNode::StructDef {
                name,
                fields,
                attributes,
            } => {
                let needs_register = self
                    .struct_layouts
                    .get(name)
                    .map(|layout| layout.fields.is_empty())
                    .unwrap_or(true);
                if needs_register {
                    self.register_struct_layout(name, fields, attributes)?;
                }
                Ok(())
            }
            AstNode::UnionDef {
                name,
                fields,
                attributes,
            } => {
                let needs_register = self
                    .union_layouts
                    .get(name)
                    .map(|layout| layout.fields.is_empty())
                    .unwrap_or(true);
                if needs_register {
                    self.register_union_layout(name, fields, attributes)?;
                }
                Ok(())
            }
            AstNode::TypedefDef { .. } => Ok(()),
            AstNode::EnumDef { .. } => Ok(()),
            AstNode::Function {
                name,
                body,
                params,
                is_variadic,
                ..
            } => {
                // Skip forward declarations (no body)
                let body = match body {
                    Some(b) => b,
                    None => return Ok(()), // Forward declaration, nothing to generate
                };

                self.symbol_table = SymbolTable::new();

                let param_regs_64 = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                let param_regs_32 = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
                let param_regs_16 = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
                let param_regs_8 = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
                let param_regs_xmm = [
                    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
                ];

                let mut int_param_count = 0;
                let mut float_param_count = 0;
                for param in params.iter() {
                    let param_size = self.type_size(&param.param_type)?;
                    let param_align = self.type_alignment(&param.param_type)?;
                    let _offset = self.symbol_table.add_variable_with_layout(
                        param.name.clone(),
                        param.param_type.clone(),
                        param_size,
                        param_align,
                    )?;
                    if self.is_float_type(&param.param_type) {
                        float_param_count += 1;
                    } else {
                        int_param_count += 1;
                    }
                }

                // Create end label for this function
                let end_label = self.next_label();
                self.current_function_end_label = Some(end_label.clone());

                // Allocate __alloca_bottom__ to track dynamic stack bottom for alloca support
                let alloca_bottom_offset = self
                    .symbol_table
                    .add_variable_with_layout(
                        "__alloca_bottom__".to_string(),
                        Type::Pointer(Box::new(Type::Void)),
                        8,
                        8,
                    )
                    .unwrap();
                self.alloca_bottom_offset = Some(alloca_bottom_offset);

                // For variadic functions, allocate register save area and __va_area__
                // Layout matches stdarg.h helpers: 6 GP regs (48 bytes) + 8 XMM regs (64 bytes) = 112 bytes
                let (reg_save_area_offset, va_area_offset) = if *is_variadic {
                    let reg_save_area_offset = self
                        .symbol_table
                        .add_variable_with_layout(
                            "__reg_save_area__".to_string(),
                            Type::Array(Box::new(Type::Char), 112),
                            112,
                            16,
                        )
                        .unwrap();
                    let va_area_offset = self
                        .symbol_table
                        .add_variable_with_layout(
                            "__va_area__".to_string(),
                            Type::Array(Box::new(Type::Char), 24),
                            24,
                            8,
                        )
                        .unwrap();
                    (Some(reg_save_area_offset), Some(va_area_offset))
                } else {
                    (None, None)
                };

                // Save current output and generate function body first to discover all variables
                let saved_output = self.output.clone();
                self.output.clear();

                let mut int_idx = 0;
                let mut float_idx = 0;
                let mut stack_param_offset = 16; // Parameters 7+ start at 16(%rbp)
                for param in params.iter() {
                    let local_offset = self
                        .symbol_table
                        .get_variable(&param.name)
                        .unwrap()
                        .stack_offset;

                    if self.is_float_type(&param.param_type) {
                        // Float parameter - use xmm registers or stack
                        if float_idx < param_regs_xmm.len() {
                            if matches!(param.param_type, Type::Float) {
                                self.emit(&format!(
                                    "    movss {}, {}(%rbp)",
                                    param_regs_xmm[float_idx], local_offset
                                ));
                            } else {
                                self.emit(&format!(
                                    "    movsd {}, {}(%rbp)",
                                    param_regs_xmm[float_idx], local_offset
                                ));
                            }
                        } else {
                            // Parameter is on stack - copy from incoming position to local slot
                            if matches!(param.param_type, Type::Float) {
                                self.emit(&format!(
                                    "    movss {}(%rbp), %xmm0",
                                    stack_param_offset
                                ));
                                self.emit(&format!("    movss %xmm0, {}(%rbp)", local_offset));
                            } else {
                                self.emit(&format!(
                                    "    movsd {}(%rbp), %xmm0",
                                    stack_param_offset
                                ));
                                self.emit(&format!("    movsd %xmm0, {}(%rbp)", local_offset));
                            }
                            stack_param_offset += 8;
                        }
                        float_idx += 1;
                    } else {
                        // Integer parameter - use general purpose registers or stack
                        if int_idx < param_regs_64.len() {
                            if matches!(param.param_type, Type::Int | Type::UInt) {
                                self.emit(&format!(
                                    "    movl {}, {}(%rbp)",
                                    param_regs_32[int_idx], local_offset
                                ));
                            } else if matches!(param.param_type, Type::UShort | Type::Short) {
                                self.emit(&format!(
                                    "    movw {}, {}(%rbp)",
                                    param_regs_16[int_idx], local_offset
                                ));
                            } else if matches!(
                                param.param_type,
                                Type::Char | Type::UChar | Type::Bool
                            ) {
                                self.emit(&format!(
                                    "    movb {}, {}(%rbp)",
                                    param_regs_8[int_idx], local_offset
                                ));
                            } else {
                                self.emit(&format!(
                                    "    movq {}, {}(%rbp)",
                                    param_regs_64[int_idx], local_offset
                                ));
                            }
                        } else {
                            // Parameter is on stack - copy from incoming position to local slot
                            self.emit(&format!("    movq {}(%rbp), %rax", stack_param_offset));
                            self.emit(&format!("    movq %rax, {}(%rbp)", local_offset));
                            stack_param_offset += 8;
                        }
                        int_idx += 1;
                    }
                }

                let overflow_arg_area_offset = stack_param_offset;

                // For variadic functions, save all argument registers to register save area
                if let Some(offset) = reg_save_area_offset {
                    self.emit("    # Save register arguments for variadic function");
                    // Save 6 general purpose argument registers
                    let gp_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                    for (i, reg) in gp_regs.iter().enumerate() {
                        let reg_offset = offset + (i as i32 * 8);
                        self.emit(&format!("    movq {}, {}(%rbp)", reg, reg_offset));
                    }
                    // Save 8 XMM argument registers (as doubles, 8 bytes each)
                    let xmm_regs = [
                        "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
                    ];
                    for (i, reg) in xmm_regs.iter().enumerate() {
                        let reg_offset = offset + 48 + (i as i32 * 8);
                        self.emit(&format!("    movsd {}, {}(%rbp)", reg, reg_offset));
                    }
                }

                if let (Some(va_offset), Some(reg_offset)) = (va_area_offset, reg_save_area_offset)
                {
                    let gp_offset = int_param_count.min(6) * 8;
                    let fp_offset = 48 + float_param_count.min(8) * 8;

                    self.emit(&format!("    leaq {}(%rbp), %rax", va_offset));
                    self.emit(&format!("    movl ${}, 0(%rax)", gp_offset));
                    self.emit(&format!("    movl ${}, 4(%rax)", fp_offset));
                    self.emit(&format!(
                        "    leaq {}(%rbp), %rcx",
                        overflow_arg_area_offset
                    ));
                    self.emit("    movq %rcx, 8(%rax)");
                    self.emit(&format!("    leaq {}(%rbp), %rcx", reg_offset));
                    self.emit("    movq %rcx, 16(%rax)");
                }

                self.generate_node(body)?;

                let body_code = self.output.clone();
                self.output = saved_output;

                // Now emit function with correct stack allocation
                let stack_size = align_to(self.symbol_table.get_stack_size(), 16);
                self.emit(&format!("    .globl {}", name));
                self.emit(&format!("{}:", name));
                self.emit("    pushq %rbp");
                self.emit("    movq %rsp, %rbp");
                if stack_size > 0 {
                    self.emit(&format!("    subq ${}, %rsp", stack_size));
                }

                // Initialize alloca_bottom to current stack position
                self.emit(&format!("    movq %rsp, {}(%rbp)", alloca_bottom_offset));

                // Append the body code
                self.output.push_str(&body_code);

                // Emit end label and epilogue
                self.emit(&format!("{}:", end_label));
                self.emit("    movq %rbp, %rsp");
                self.emit("    popq %rbp");
                self.emit("    ret");

                // Reset function-specific state
                self.current_function_end_label = None;
                self.alloca_bottom_offset = None;
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
            AstNode::Label(name) => {
                self.emit(&format!("{}:", name));
                Ok(())
            }
            AstNode::Goto(label) => {
                self.emit(&format!("    jmp {}", label));
                Ok(())
            }
            AstNode::InlineAsm(asm_code) => {
                // Emit the inline assembly code directly
                self.emit(&format!("    {}", asm_code));
                Ok(())
            }
            AstNode::SwitchStatement { expr, body } => {
                // Evaluate switch expression
                self.generate_node(expr)?;
                self.emit("    pushq %rax");

                // Create end label for break statements
                let end_label = self.next_label();

                // Push switch context for break handling
                let switch_ctx = LoopContext {
                    break_label: end_label.clone(),
                    continue_label: String::new(), // switch doesn't have continue
                };
                self.loop_stack.push(switch_ctx);

                // First pass: collect case values and create labels
                let mut case_labels = Vec::new();
                let mut default_label = None;
                for (i, node) in body.iter().enumerate() {
                    match node {
                        AstNode::Case(value) => {
                            let label = format!(".L_case_{}_{}", self.label_counter, i);
                            case_labels.push((*value, label));
                        }
                        AstNode::Default => {
                            default_label = Some(format!(".L_default_{}", self.label_counter));
                        }
                        _ => {}
                    }
                }

                // Generate comparison code for each case
                for (value, label) in &case_labels {
                    self.emit("    movq (%rsp), %rax");
                    self.emit(&format!("    cmpq ${}, %rax", value));
                    self.emit(&format!("    je {}", label));
                }

                // Jump to default or end
                if let Some(ref default_lbl) = default_label {
                    self.emit(&format!("    jmp {}", default_lbl));
                } else {
                    self.emit(&format!("    jmp {}", end_label));
                }

                // Generate code for each statement in body
                for node in body.iter() {
                    match node {
                        AstNode::Case(value) => {
                            // Find and emit the case label
                            for (case_val, label) in &case_labels {
                                if case_val == value {
                                    self.emit(&format!("{}:", label));
                                    break;
                                }
                            }
                        }
                        AstNode::Default => {
                            if let Some(ref lbl) = default_label {
                                self.emit(&format!("{}:", lbl));
                            }
                        }
                        _ => {
                            self.generate_node(node)?;
                        }
                    }
                }

                // Emit end label
                self.emit(&format!("{}:", end_label));

                // Clean up switch expression from stack
                self.emit("    addq $8, %rsp");

                // Pop switch context
                self.loop_stack.pop();

                Ok(())
            }
            AstNode::Case(_) => {
                // Case labels are handled by SwitchStatement
                Ok(())
            }
            AstNode::Default => {
                // Default label is handled by SwitchStatement
                Ok(())
            }
            AstNode::Break => {
                if let Some(ctx) = self.loop_stack.last() {
                    self.emit(&format!("    jmp {}", ctx.break_label));
                    Ok(())
                } else {
                    Err("break statement not within a loop".to_string())
                }
            }
            AstNode::Continue => {
                if let Some(ctx) = self.loop_stack.last() {
                    self.emit(&format!("    jmp {}", ctx.continue_label));
                    Ok(())
                } else {
                    Err("continue statement not within a loop".to_string())
                }
            }
            AstNode::IfStatement {
                condition,
                then_branch,
                else_branch,
            } => {
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

                // Push loop context for break/continue
                self.loop_stack.push(LoopContext {
                    break_label: end_label.clone(),
                    continue_label: start_label.clone(),
                });

                self.emit(&format!("{}:", start_label));
                self.generate_node(condition)?;
                self.emit("    cmpq $0, %rax");
                self.emit(&format!("    je {}", end_label));

                self.generate_node(body)?;
                self.emit(&format!("    jmp {}", start_label));

                self.emit(&format!("{}:", end_label));

                // Pop loop context
                self.loop_stack.pop();
                Ok(())
            }
            AstNode::DoWhileLoop { body, condition } => {
                let start_label = self.next_label();
                let end_label = self.next_label();
                let continue_label = self.next_label();

                // Push loop context for break/continue
                self.loop_stack.push(LoopContext {
                    break_label: end_label.clone(),
                    continue_label: continue_label.clone(),
                });

                self.emit(&format!("{}:", start_label));
                self.generate_node(body)?;

                self.emit(&format!("{}:", continue_label));
                self.generate_node(condition)?;
                self.emit("    cmpq $0, %rax");
                self.emit(&format!("    jne {}", start_label));

                self.emit(&format!("{}:", end_label));

                // Pop loop context
                self.loop_stack.pop();
                Ok(())
            }
            AstNode::ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                let start_label = self.next_label();
                let end_label = self.next_label();
                let continue_label = self.next_label();

                if let Some(init_node) = init {
                    self.generate_node(init_node)?;
                }

                self.emit(&format!("{}:", start_label));

                if let Some(cond) = condition {
                    self.generate_node(cond)?;
                    self.emit("    cmpq $0, %rax");
                    self.emit(&format!("    je {}", end_label));
                }

                // Push loop context for break/continue
                // Continue jumps to increment part (or start if no increment)
                self.loop_stack.push(LoopContext {
                    break_label: end_label.clone(),
                    continue_label: continue_label.clone(),
                });

                self.generate_node(body)?;

                self.emit(&format!("{}:", continue_label));
                if let Some(inc) = increment {
                    self.generate_node(inc)?;
                }

                self.emit(&format!("    jmp {}", start_label));
                self.emit(&format!("{}:", end_label));

                // Pop loop context
                self.loop_stack.pop();
                Ok(())
            }
            AstNode::VarDecl {
                name,
                var_type,
                init,
                is_extern: _,
                is_static,
                alignment,
                ..
            } => {
                // Handle static local variables differently - allocate in .data/.bss, not on stack
                if *is_static {
                    // Simply register as a global variable with static linkage
                    // It will be emitted in the .data/.bss section
                    self.global_variables.insert(
                        name.clone(),
                        GlobalVariable {
                            var_type: var_type.clone(),
                            init: init.clone().map(|n| (*n).clone()),
                            is_extern: false,
                            is_static: true,
                            alignment: *alignment,
                        },
                    );

                    return Ok(());
                }

                // If array has size 0 (from [] syntax) and string literal initializer,
                // infer the actual size before allocating stack space
                let actual_var_type = if let Type::Array(elem_type, 0) = var_type {
                    if let Some(init_expr) = init {
                        match init_expr.as_ref() {
                            AstNode::StringLiteral(s) => {
                                Type::Array(elem_type.clone(), s.len() + 1)
                            }
                            AstNode::WideStringLiteral(s) => {
                                Type::Array(elem_type.clone(), s.chars().count() + 1)
                            }
                            _ => var_type.clone(),
                        }
                    } else {
                        var_type.clone()
                    }
                } else {
                    var_type.clone()
                };

                let var_size = self.type_size(&actual_var_type)?;
                // Use explicit alignment from _Alignas if specified, otherwise use type's natural alignment
                let var_align = alignment
                    .map(|a| a as i32)
                    .unwrap_or_else(|| self.stack_alignment(&actual_var_type).unwrap_or(1));

                // Check if variable already exists (e.g., from statement expression pre-registration)
                let offset = if let Some(existing) = self.symbol_table.get_variable(name) {
                    existing.stack_offset
                } else {
                    self.symbol_table.add_variable_with_layout(
                        name.clone(),
                        actual_var_type.clone(),
                        var_size,
                        var_align,
                    )?
                };

                if let Some(init_expr) = init {
                    if var_type.is_array() {
                        match init_expr.as_ref() {
                            AstNode::ArrayInit(values) => {
                                self.generate_array_init(var_type, values, offset)?;
                            }
                            AstNode::StringLiteral(s) => {
                                // String literal initialization for char arrays
                                self.generate_string_array_init(var_type, s, offset)?;
                            }
                            AstNode::WideStringLiteral(s) => {
                                self.generate_wide_string_array_init(var_type, s, offset)?;
                            }
                            _ => {
                                return Err(
                                    "Array initializer must be a brace-enclosed list or string literal".to_string()
                                );
                            }
                        }
                    } else {
                        match (var_type, init_expr.as_ref()) {
                            (Type::Struct(struct_name), AstNode::StructInit(values)) => {
                                self.emit_struct_init(struct_name, values, offset)?;
                            }
                            (Type::Union(union_name), AstNode::StructInit(values)) => {
                                self.emit_union_init(union_name, values, offset)?;
                            }
                            _ => {
                                // For statement expressions, generate code first to populate symbol table
                                let init_type = if let AstNode::StmtExpr { stmts, result } =
                                    init_expr.as_ref()
                                {
                                    // Pre-register struct/union definitions for type inference
                                    for stmt in stmts {
                                        if let AstNode::StructDef {
                                            name,
                                            fields,
                                            attributes,
                                        } = stmt
                                        {
                                            self.register_struct_layout(name, fields, attributes)?;
                                        } else if let AstNode::UnionDef {
                                            name,
                                            fields,
                                            attributes,
                                        } = stmt
                                        {
                                            self.register_union_layout(name, fields, attributes)?;
                                        }
                                    }
                                    // Generate statements which adds variables to symbol table
                                    for stmt in stmts {
                                        self.generate_node(stmt)?;
                                    }
                                    // Now we can infer the type of the result
                                    let result_type = self.expr_type(result)?;
                                    // Generate the result expression
                                    self.generate_node(result)?;
                                    result_type
                                } else {
                                    let init_type = self.expr_type(init_expr)?;
                                    self.generate_node(init_expr)?;
                                    init_type
                                };
                                self.convert_type(&init_type, var_type)?;
                                if matches!(var_type, Type::Float) {
                                    self.emit("    cvtsd2ss %xmm0, %xmm0");
                                    self.emit(&format!("    movss %xmm0, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::Double) {
                                    self.emit(&format!("    movsd %xmm0, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::Int | Type::UInt | Type::Enum(_))
                                {
                                    self.emit(&format!("    movl %eax, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::UShort | Type::Short) {
                                    self.emit(&format!("    movw %ax, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::Char | Type::UChar | Type::Bool)
                                {
                                    self.emit(&format!("    movb %al, {}(%rbp)", offset));
                                } else {
                                    self.emit(&format!("    movq %rax, {}(%rbp)", offset));
                                }
                            }
                        }
                    }
                }
                Ok(())
            }
            AstNode::Assignment { target, value } => {
                if let AstNode::MemberAccess {
                    base,
                    member,
                    through_pointer,
                } = target.as_ref()
                {
                    let field_info =
                        self.member_access_field_info(base, member, *through_pointer)?;
                    if field_info.bit_width.is_some() {
                        return self.generate_bitfield_assignment(
                            base,
                            member,
                            *through_pointer,
                            &field_info,
                            value,
                        );
                    }
                }

                let target_type = self.expr_type(target)?;
                let value_type = self.expr_type(value)?;

                if matches!(target_type, Type::Struct(_) | Type::Union(_)) {
                    let size = self.type_size(&target_type)?;
                    self.generate_lvalue(target)?;
                    self.emit("    movq %rax, %rdi");
                    self.generate_lvalue(value)?;
                    self.emit("    movq %rax, %rsi");
                    self.emit("    movq %rdi, %rax");
                    self.emit(&format!("    movq ${}, %rcx", size));
                    self.emit("    rep movsb");
                    return Ok(());
                }

                // Generate the value first, then convert to the target type.
                self.generate_node(value)?;
                self.convert_type(&value_type, &target_type)?;

                if self.is_float_type(&target_type) {
                    self.emit("    subq $8, %rsp");
                    self.emit("    movsd %xmm0, (%rsp)");
                } else {
                    self.emit("    pushq %rax");
                    self.stack_depth += 1;
                }

                // Generate lvalue address
                self.generate_lvalue(target)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address

                if self.is_float_type(&target_type) {
                    self.emit("    movsd (%rsp), %xmm0");
                    self.emit("    addq $8, %rsp");
                } else {
                    self.emit_pop("%rax");
                }

                // Store value at address in %rcx
                if matches!(target_type, Type::Float) {
                    self.emit("    cvtsd2ss %xmm0, %xmm0");
                    self.emit("    movss %xmm0, (%rcx)");
                } else if matches!(target_type, Type::Double) {
                    self.emit("    movsd %xmm0, (%rcx)");
                } else if matches!(target_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl %eax, (%rcx)");
                } else if matches!(target_type, Type::UShort | Type::Short) {
                    self.emit("    movw %ax, (%rcx)");
                } else if matches!(target_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit("    movb %al, (%rcx)");
                } else {
                    self.emit("    movq %rax, (%rcx)");
                }
                if !self.is_float_type(&target_type) {
                    self.coerce_rax_to_type(&target_type);
                }
                Ok(())
            }
            AstNode::Variable(name) => {
                // Check if this is an enum constant first
                if let Some(&value) = self.enum_constants.get(name) {
                    self.emit(&format!("    movq ${}, %rax", value));
                    return Ok(());
                }

                // Check if this is a global variable
                if let Some(global) = self.global_variables.get(name) {
                    let var_type = &global.var_type;

                    // For arrays and structs, load the address instead of the value
                    if var_type.is_array() || matches!(var_type, Type::Struct(_) | Type::Union(_)) {
                        self.emit(&format!("    leaq {}(%rip), %rax", name));
                        return Ok(());
                    }

                    // Load from global variable using label
                    match var_type {
                        Type::Char => {
                            self.emit(&format!("    movsbl {}(%rip), %eax", name));
                            self.emit("    cltq");
                        }
                        Type::UChar | Type::Bool => {
                            self.emit(&format!("    movzbl {}(%rip), %eax", name));
                            self.emit("    cltq");
                        }
                        Type::Short => {
                            self.emit(&format!("    movswl {}(%rip), %eax", name));
                            self.emit("    cltq");
                        }
                        Type::UShort => {
                            self.emit(&format!("    movzwl {}(%rip), %eax", name));
                            self.emit("    cltq");
                        }
                        Type::Int | Type::Enum(_) => {
                            self.emit(&format!("    movslq {}(%rip), %rax", name));
                        }
                        Type::UInt => {
                            self.emit(&format!("    movl {}(%rip), %eax", name));
                        }
                        Type::Long | Type::ULong => {
                            self.emit(&format!("    movq {}(%rip), %rax", name));
                        }
                        Type::Float => {
                            self.emit(&format!("    movss {}(%rip), %xmm0", name));
                            self.emit("    cvtss2sd %xmm0, %xmm0");
                        }
                        Type::Double => {
                            self.emit(&format!("    movsd {}(%rip), %xmm0", name));
                        }
                        Type::Pointer(_) => {
                            self.emit(&format!("    movq {}(%rip), %rax", name));
                        }
                        _ => {
                            self.emit(&format!("    movq {}(%rip), %rax", name));
                        }
                    }
                    return Ok(());
                }

                if let Some(symbol) = self.symbol_table.get_variable(name) {
                    if symbol.symbol_type.is_array()
                        || matches!(symbol.symbol_type, Type::Struct(_) | Type::Union(_))
                    {
                        self.emit(&format!("    leaq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::Int | Type::Enum(_)) {
                        self.emit(&format!("    movslq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::UInt) {
                        self.emit(&format!("    movl {}(%rbp), %eax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::UShort) {
                        self.emit(&format!("    movzwq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::Short) {
                        self.emit(&format!("    movswq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::Char) {
                        self.emit(&format!("    movsbq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::UChar | Type::Bool) {
                        self.emit(&format!("    movzbq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::Long | Type::ULong) {
                        self.emit(&format!("    movq {}(%rbp), %rax", symbol.stack_offset));
                    } else if matches!(symbol.symbol_type, Type::Float) {
                        self.emit(&format!("    movss {}(%rbp), %xmm0", symbol.stack_offset));
                        self.emit("    cvtss2sd %xmm0, %xmm0");
                    } else if matches!(symbol.symbol_type, Type::Double) {
                        self.emit(&format!("    movsd {}(%rbp), %xmm0", symbol.stack_offset));
                    } else {
                        self.emit(&format!("    movq {}(%rbp), %rax", symbol.stack_offset));
                    }
                    return Ok(());
                }

                // Treat unknown identifiers as function names (function designator decay).
                self.emit(&format!("    leaq {}(%rip), %rax", name));
                Ok(())
            }
            AstNode::FunctionCall { name, args } => {
                // Handle builtin functions
                if name == "alloca" {
                    // alloca(size) allocates memory on the stack and returns a pointer
                    if args.len() != 1 {
                        return Err(format!("alloca expects 1 argument, got {}", args.len()));
                    }

                    // Evaluate the size argument
                    self.generate_node(&args[0])?;

                    // Align size to 16 bytes (x86-64 ABI requirement)
                    self.emit("    addq $15, %rax"); // Round up
                    self.emit("    andq $-16, %rax"); // Align to 16 bytes

                    // Save aligned size in %rdi
                    self.emit("    movq %rax, %rdi");

                    if let Some(alloca_bottom) = self.alloca_bottom_offset {
                        // Calculate size of temporary area to preserve: %rcx = alloca_bottom - %rsp
                        self.emit(&format!("    movq {}(%rbp), %rcx", alloca_bottom));
                        self.emit("    subq %rsp, %rcx");

                        // Allocate new space: %rsp -= %rdi
                        self.emit("    movq %rsp, %rax");
                        self.emit("    subq %rdi, %rsp");
                        self.emit("    movq %rsp, %rdx");

                        // Copy temporary area byte-by-byte from old position (%rax) to new (%rdx)
                        let copy_loop = self.next_label();
                        let copy_done = self.next_label();
                        self.emit(&format!("{}:", copy_loop));
                        self.emit("    cmpq $0, %rcx");
                        self.emit(&format!("    je {}", copy_done));
                        self.emit("    movb (%rax), %r8b");
                        self.emit("    movb %r8b, (%rdx)");
                        self.emit("    incq %rdx");
                        self.emit("    incq %rax");
                        self.emit("    decq %rcx");
                        self.emit(&format!("    jmp {}", copy_loop));
                        self.emit(&format!("{}:", copy_done));

                        // Update alloca_bottom: alloca_bottom -= %rdi
                        self.emit(&format!("    movq {}(%rbp), %rax", alloca_bottom));
                        self.emit("    subq %rdi, %rax");
                        self.emit(&format!("    movq %rax, {}(%rbp)", alloca_bottom));

                        // Return the updated alloca_bottom
                        // (which now points to the newly allocated space)
                    } else {
                        // No alloca_bottom (shouldn't happen in well-formed code)
                        // Fall back to simple allocation
                        self.emit("    subq %rdi, %rsp");
                        self.emit("    movq %rsp, %rax");
                    }

                    return Ok(());
                }

                if let Some(symbol) = self.symbol_table.get_variable(name) {
                    if matches!(symbol.symbol_type, Type::FunctionPointer { .. }) {
                        let target = AstNode::Variable(name.clone());
                        return self.generate_node(&AstNode::IndirectCall {
                            target: Box::new(target),
                            args: args.clone(),
                        });
                    }
                    return Err(format!("Cannot call non-function pointer '{}'", name));
                }

                if let Some(global) = self.global_variables.get(name) {
                    if matches!(global.var_type, Type::FunctionPointer { .. }) {
                        let target = AstNode::Variable(name.clone());
                        return self.generate_node(&AstNode::IndirectCall {
                            target: Box::new(target),
                            args: args.clone(),
                        });
                    }
                    return Err(format!("Cannot call non-function pointer '{}'", name));
                }

                let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                let arg_regs_xmm = [
                    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
                ];

                // Track which arguments are floats and their position within their type class
                let mut int_counter = 0;
                let mut float_counter = 0;
                let mut arg_info: Vec<(bool, usize)> = Vec::new(); // (is_float, index_within_type)

                for arg in args.iter() {
                    // For statement expressions, pre-register types and variables for type inference
                    if let AstNode::StmtExpr { stmts, .. } = arg {
                        for stmt in stmts {
                            // Register struct/union definitions
                            if let AstNode::StructDef {
                                name,
                                fields,
                                attributes,
                            } = stmt
                            {
                                let _ = self.register_struct_layout(name, fields, attributes);
                            } else if let AstNode::UnionDef {
                                name,
                                fields,
                                attributes,
                            } = stmt
                            {
                                let _ = self.register_union_layout(name, fields, attributes);
                            }
                            // Register variables with their alignment
                            // Use add_or_replace to allow statement expressions to shadow variables
                            else if let AstNode::VarDecl {
                                name,
                                var_type,
                                alignment,
                                ..
                            } = stmt
                            {
                                let size = self.type_size(var_type).unwrap_or(0);
                                let align = alignment
                                    .map(|a| a as i32)
                                    .unwrap_or_else(|| self.stack_alignment(var_type).unwrap_or(1));
                                let _ = self.symbol_table.add_or_replace_variable_with_layout(
                                    name.clone(),
                                    var_type.clone(),
                                    size,
                                    align,
                                );
                            } else if let AstNode::Block(decls) = stmt {
                                for decl in decls {
                                    if let AstNode::VarDecl {
                                        name,
                                        var_type,
                                        alignment,
                                        ..
                                    } = decl
                                    {
                                        let size = self.type_size(var_type).unwrap_or(0);
                                        let align =
                                            alignment.map(|a| a as i32).unwrap_or_else(|| {
                                                self.stack_alignment(var_type).unwrap_or(1)
                                            });
                                        let _ =
                                            self.symbol_table.add_or_replace_variable_with_layout(
                                                name.clone(),
                                                var_type.clone(),
                                                size,
                                                align,
                                            );
                                    }
                                }
                            }
                        }
                    }

                    let arg_type = self.expr_type(arg)?;

                    // Check if we have parameter type information for this function
                    let arg_index = args.iter().position(|a| std::ptr::eq(a, arg)).unwrap();
                    let expected_param_type = self
                        .function_param_types
                        .get(name)
                        .and_then(|params| params.get(arg_index));

                    // Use expected parameter type if available, otherwise use argument type
                    let effective_type = expected_param_type.unwrap_or(&arg_type);

                    if self.is_float_type(effective_type) {
                        arg_info.push((true, float_counter));
                        float_counter += 1;
                    } else {
                        arg_info.push((false, int_counter));
                        int_counter += 1;
                    }
                }

                let expected_types: Vec<Option<Type>> = (0..args.len())
                    .map(|i| {
                        self.function_param_types
                            .get(name)
                            .and_then(|params| params.get(i))
                            .cloned()
                    })
                    .collect();

                let mut reg_temp_offsets = vec![None; args.len()];
                let mut reg_temp_count = 0usize;
                let mut stack_args_count = 0usize;
                for (i, (is_float, idx)) in arg_info.iter().enumerate() {
                    let in_reg = if *is_float {
                        *idx < arg_regs_xmm.len()
                    } else {
                        *idx < arg_regs.len()
                    };
                    if in_reg {
                        reg_temp_offsets[i] = Some((reg_temp_count * 8) as i32);
                        reg_temp_count += 1;
                    } else {
                        stack_args_count += 1;
                    }
                }

                let reg_temp_size = reg_temp_count * 8;
                let stack_args_size = stack_args_count * 8;
                let current_align = (self.stack_depth & 1) as usize * 8;
                let align_pad =
                    if (current_align + reg_temp_size + stack_args_size).is_multiple_of(16) {
                        0
                    } else {
                        8
                    };

                if reg_temp_size > 0 {
                    self.emit(&format!("    subq ${}, %rsp", reg_temp_size));
                    self.stack_depth += (reg_temp_size / 8) as i32;
                }

                // Evaluate register arguments into temporary stack slots
                for (i, (arg, (is_float, _))) in args.iter().zip(arg_info.iter()).enumerate().rev()
                {
                    if let Some(temp_offset) = reg_temp_offsets[i] {
                        if let AstNode::StmtExpr { stmts, result } = arg {
                            for stmt in stmts {
                                self.generate_node(stmt)?;
                            }
                            self.generate_node(result)?;
                        } else {
                            self.generate_node(arg)?;
                        }

                        let arg_type = self.expr_type(arg)?;
                        if let Some(ref expected) = expected_types[i] {
                            self.convert_type(&arg_type, expected)?;
                        }

                        if *is_float {
                            self.emit(&format!("    movsd %xmm0, {}(%rsp)", temp_offset));
                        } else {
                            self.emit(&format!("    movq %rax, {}(%rsp)", temp_offset));
                        }
                    }
                }

                if align_pad > 0 {
                    self.emit(&format!("    subq ${}, %rsp", align_pad));
                    self.stack_depth += (align_pad / 8) as i32;
                }

                // Evaluate and push stack arguments (right-to-left)
                for (i, (arg, (is_float, _))) in args.iter().zip(arg_info.iter()).enumerate().rev()
                {
                    if reg_temp_offsets[i].is_none() {
                        if let AstNode::StmtExpr { stmts, result } = arg {
                            for stmt in stmts {
                                self.generate_node(stmt)?;
                            }
                            self.generate_node(result)?;
                        } else {
                            self.generate_node(arg)?;
                        }

                        let arg_type = self.expr_type(arg)?;
                        if let Some(ref expected) = expected_types[i] {
                            self.convert_type(&arg_type, expected)?;
                        }

                        if *is_float {
                            self.emit("    subq $8, %rsp");
                            self.emit("    movsd %xmm0, (%rsp)");
                        } else {
                            self.emit("    pushq %rax");
                        }
                        self.stack_depth += 1;
                    }
                }

                let reg_temp_base = (align_pad + stack_args_size) as i32;

                // Load register arguments from temp slots
                for (i, (is_float, idx)) in arg_info.iter().enumerate() {
                    let in_reg = if *is_float {
                        *idx < arg_regs_xmm.len()
                    } else {
                        *idx < arg_regs.len()
                    };
                    if in_reg {
                        let temp_offset = reg_temp_offsets[i].unwrap();
                        let offset = reg_temp_base + temp_offset;
                        if *is_float {
                            self.emit(&format!(
                                "    movsd {}(%rsp), {}",
                                offset, arg_regs_xmm[*idx]
                            ));
                        } else {
                            self.emit(&format!("    movq {}(%rsp), {}", offset, arg_regs[*idx]));
                        }
                    }
                }

                self.emit(&format!("    call {}", name));

                if stack_args_count > 0 || reg_temp_count > 0 || align_pad > 0 {
                    self.emit(&format!(
                        "    addq ${}, %rsp",
                        stack_args_size + reg_temp_size + align_pad
                    ));
                    self.stack_depth -= ((stack_args_size + reg_temp_size + align_pad) / 8) as i32;
                }

                Ok(())
            }
            AstNode::IndirectCall { target, args } => {
                let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                let arg_regs_xmm = [
                    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
                ];

                // Evaluate the function pointer target and save it in r11
                self.generate_node(target)?;
                self.emit("    movq %rax, %r11");

                // Track which arguments are floats and their position within their type class
                let mut int_counter = 0;
                let mut float_counter = 0;
                let mut arg_info: Vec<(bool, usize)> = Vec::new(); // (is_float, index_within_type)

                for arg in args.iter() {
                    // For statement expressions, pre-register types and variables for type inference
                    if let AstNode::StmtExpr { stmts, .. } = arg {
                        for stmt in stmts {
                            // Register struct/union definitions
                            if let AstNode::StructDef {
                                name,
                                fields,
                                attributes,
                            } = stmt
                            {
                                let _ = self.register_struct_layout(name, fields, attributes);
                            } else if let AstNode::UnionDef {
                                name,
                                fields,
                                attributes,
                            } = stmt
                            {
                                let _ = self.register_union_layout(name, fields, attributes);
                            }
                            // Register variables with their alignment
                            // Use add_or_replace to allow statement expressions to shadow variables
                            else if let AstNode::VarDecl {
                                name,
                                var_type,
                                alignment,
                                ..
                            } = stmt
                            {
                                let size = self.type_size(var_type).unwrap_or(0);
                                let align = alignment
                                    .map(|a| a as i32)
                                    .unwrap_or_else(|| self.stack_alignment(var_type).unwrap_or(1));
                                let _ = self.symbol_table.add_or_replace_variable_with_layout(
                                    name.clone(),
                                    var_type.clone(),
                                    size,
                                    align,
                                );
                            } else if let AstNode::Block(decls) = stmt {
                                for decl in decls {
                                    if let AstNode::VarDecl {
                                        name,
                                        var_type,
                                        alignment,
                                        ..
                                    } = decl
                                    {
                                        let size = self.type_size(var_type).unwrap_or(0);
                                        let align =
                                            alignment.map(|a| a as i32).unwrap_or_else(|| {
                                                self.stack_alignment(var_type).unwrap_or(1)
                                            });
                                        let _ =
                                            self.symbol_table.add_or_replace_variable_with_layout(
                                                name.clone(),
                                                var_type.clone(),
                                                size,
                                                align,
                                            );
                                    }
                                }
                            }
                        }
                    }

                    let arg_type = self.expr_type(arg)?;
                    if self.is_float_type(&arg_type) {
                        arg_info.push((true, float_counter));
                        float_counter += 1;
                    } else {
                        arg_info.push((false, int_counter));
                        int_counter += 1;
                    }
                }

                let mut reg_temp_offsets = vec![None; args.len()];
                let mut reg_temp_count = 0usize;
                let mut stack_args_count = 0usize;
                for (i, (is_float, idx)) in arg_info.iter().enumerate() {
                    let in_reg = if *is_float {
                        *idx < arg_regs_xmm.len()
                    } else {
                        *idx < arg_regs.len()
                    };
                    if in_reg {
                        reg_temp_offsets[i] = Some((reg_temp_count * 8) as i32);
                        reg_temp_count += 1;
                    } else {
                        stack_args_count += 1;
                    }
                }

                let reg_temp_size = reg_temp_count * 8;
                let stack_args_size = stack_args_count * 8;
                let current_align = (self.stack_depth & 1) as usize * 8;
                let align_pad =
                    if (current_align + reg_temp_size + stack_args_size).is_multiple_of(16) {
                        0
                    } else {
                        8
                    };

                if reg_temp_size > 0 {
                    self.emit(&format!("    subq ${}, %rsp", reg_temp_size));
                    self.stack_depth += (reg_temp_size / 8) as i32;
                }

                // Evaluate register arguments into temporary stack slots
                for (i, (arg, (is_float, _))) in args.iter().zip(arg_info.iter()).enumerate().rev()
                {
                    if let Some(temp_offset) = reg_temp_offsets[i] {
                        if let AstNode::StmtExpr { stmts, result } = arg {
                            for stmt in stmts {
                                self.generate_node(stmt)?;
                            }
                            self.generate_node(result)?;
                        } else {
                            self.generate_node(arg)?;
                        }

                        if *is_float {
                            self.emit(&format!("    movsd %xmm0, {}(%rsp)", temp_offset));
                        } else {
                            self.emit(&format!("    movq %rax, {}(%rsp)", temp_offset));
                        }
                    }
                }

                if align_pad > 0 {
                    self.emit(&format!("    subq ${}, %rsp", align_pad));
                    self.stack_depth += (align_pad / 8) as i32;
                }

                // Evaluate and push stack arguments (right-to-left)
                for (i, (arg, (is_float, _))) in args.iter().zip(arg_info.iter()).enumerate().rev()
                {
                    if reg_temp_offsets[i].is_none() {
                        if let AstNode::StmtExpr { stmts, result } = arg {
                            for stmt in stmts {
                                self.generate_node(stmt)?;
                            }
                            self.generate_node(result)?;
                        } else {
                            self.generate_node(arg)?;
                        }
                        if *is_float {
                            self.emit("    subq $8, %rsp");
                            self.emit("    movsd %xmm0, (%rsp)");
                        } else {
                            self.emit("    pushq %rax");
                        }
                        self.stack_depth += 1;
                    }
                }

                let reg_temp_base = (align_pad + stack_args_size) as i32;

                // Load register arguments from temp slots
                for (i, (is_float, idx)) in arg_info.iter().enumerate() {
                    let in_reg = if *is_float {
                        *idx < arg_regs_xmm.len()
                    } else {
                        *idx < arg_regs.len()
                    };
                    if in_reg {
                        let temp_offset = reg_temp_offsets[i].unwrap();
                        let offset = reg_temp_base + temp_offset;
                        if *is_float {
                            self.emit(&format!(
                                "    movsd {}(%rsp), {}",
                                offset, arg_regs_xmm[*idx]
                            ));
                        } else {
                            self.emit(&format!("    movq {}(%rsp), {}", offset, arg_regs[*idx]));
                        }
                    }
                }

                // Call through the function pointer
                self.emit("    call *%r11");

                if stack_args_count > 0 || reg_temp_count > 0 || align_pad > 0 {
                    self.emit(&format!(
                        "    addq ${}, %rsp",
                        stack_args_size + reg_temp_size + align_pad
                    ));
                    self.stack_depth -= ((stack_args_size + reg_temp_size + align_pad) / 8) as i32;
                }

                Ok(())
            }
            AstNode::BinaryOp { op, left, right } => {
                match op {
                    BinOp::Comma => {
                        // Evaluate left, discard result
                        self.generate_node(left)?;
                        // Evaluate right, keep result
                        self.generate_node(right)?;
                    }
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
                        // Check for float operations
                        if self.are_float_operands(left, right) {
                            match op {
                                BinOp::Multiply => {
                                    return self.generate_float_binary_op(left, right, "mulsd");
                                }
                                BinOp::Divide => {
                                    return self.generate_float_binary_op(left, right, "divsd");
                                }
                                _ => {}
                            }
                        }

                        let int_operands = self.are_int_operands(left, right);
                        let unsigned_ops = self.uses_unsigned_ops(left, right);
                        self.generate_node(left)?;
                        self.emit("    pushq %rax");
                        self.stack_depth += 1;
                        self.generate_node(right)?;
                        self.emit_pop("%rcx");

                        match op {
                            BinOp::Multiply => {
                                if int_operands {
                                    if unsigned_ops {
                                        self.emit("    mull %ecx");
                                    } else {
                                        self.emit("    imull %ecx, %eax");
                                        self.emit("    cltq");
                                    }
                                } else if unsigned_ops {
                                    self.emit("    mulq %rcx");
                                } else {
                                    self.emit("    imulq %rcx, %rax");
                                }
                            }
                            BinOp::Modulo => {
                                if int_operands {
                                    self.emit("    movl %eax, %ebx");
                                    self.emit("    movl %ecx, %eax");
                                    if unsigned_ops {
                                        self.emit("    xorl %edx, %edx");
                                        self.emit("    divl %ebx");
                                        self.emit("    movl %edx, %eax");
                                    } else {
                                        self.emit("    cltd");
                                        self.emit("    idivl %ebx");
                                        self.emit("    movl %edx, %eax");
                                        self.emit("    cltq");
                                    }
                                } else {
                                    self.emit("    movq %rax, %rbx");
                                    self.emit("    movq %rcx, %rax");
                                    if unsigned_ops {
                                        self.emit("    xorq %rdx, %rdx");
                                        self.emit("    divq %rbx");
                                    } else {
                                        self.emit("    cqto");
                                        self.emit("    idivq %rbx");
                                    }
                                    self.emit("    movq %rdx, %rax");
                                }
                            }
                            BinOp::Divide => {
                                if int_operands {
                                    self.emit("    movl %eax, %ebx");
                                    self.emit("    movl %ecx, %eax");
                                    if unsigned_ops {
                                        self.emit("    xorl %edx, %edx");
                                        self.emit("    divl %ebx");
                                    } else {
                                        self.emit("    cltd");
                                        self.emit("    idivl %ebx");
                                        self.emit("    cltq");
                                    }
                                } else {
                                    self.emit("    movq %rax, %rbx");
                                    self.emit("    movq %rcx, %rax");
                                    if unsigned_ops {
                                        self.emit("    xorq %rdx, %rdx");
                                        self.emit("    divq %rbx");
                                    } else {
                                        self.emit("    cqto");
                                        self.emit("    idivq %rbx");
                                    }
                                }
                            }
                            BinOp::BitAnd => {
                                if int_operands {
                                    self.emit("    andl %ecx, %eax");
                                    if !unsigned_ops {
                                        self.emit("    cltq");
                                    }
                                } else {
                                    self.emit("    andq %rcx, %rax");
                                }
                            }
                            BinOp::BitOr => {
                                if int_operands {
                                    self.emit("    orl %ecx, %eax");
                                    if !unsigned_ops {
                                        self.emit("    cltq");
                                    }
                                } else {
                                    self.emit("    orq %rcx, %rax");
                                }
                            }
                            BinOp::BitXor => {
                                if int_operands {
                                    self.emit("    xorl %ecx, %eax");
                                    if !unsigned_ops {
                                        self.emit("    cltq");
                                    }
                                } else {
                                    self.emit("    xorq %rcx, %rax");
                                }
                            }
                            BinOp::ShiftLeft | BinOp::ShiftRight => {
                                let shift_type = self.shift_result_type(left)?;
                                let shift_is_int = matches!(shift_type, Type::Int | Type::UInt);
                                let shift_unsigned = matches!(shift_type, Type::UInt | Type::ULong);
                                self.emit("    movq %rax, %rbx");
                                self.emit("    movq %rcx, %rax");
                                self.emit("    movb %bl, %cl");
                                match op {
                                    BinOp::ShiftLeft => {
                                        if shift_is_int {
                                            self.emit("    shll %cl, %eax");
                                            if !shift_unsigned {
                                                self.emit("    cltq");
                                            }
                                        } else {
                                            self.emit("    shlq %cl, %rax");
                                        }
                                    }
                                    BinOp::ShiftRight => {
                                        if shift_is_int {
                                            if shift_unsigned {
                                                self.emit("    shrl %cl, %eax");
                                            } else {
                                                self.emit("    sarl %cl, %eax");
                                                self.emit("    cltq");
                                            }
                                        } else if shift_unsigned {
                                            self.emit("    shrq %cl, %rax");
                                        } else {
                                            self.emit("    sarq %cl, %rax");
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            BinOp::Less => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "setb");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    if unsigned_ops {
                                        self.emit("    setb %al");
                                    } else {
                                        self.emit("    setl %al");
                                    }
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::Greater => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "seta");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    if unsigned_ops {
                                        self.emit("    seta %al");
                                    } else {
                                        self.emit("    setg %al");
                                    }
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::LessEqual => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "setbe");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    if unsigned_ops {
                                        self.emit("    setbe %al");
                                    } else {
                                        self.emit("    setle %al");
                                    }
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::GreaterEqual => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "setae");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    if unsigned_ops {
                                        self.emit("    setae %al");
                                    } else {
                                        self.emit("    setge %al");
                                    }
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::EqualEqual => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "sete");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    self.emit("    sete %al");
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::NotEqual => {
                                if self.are_float_operands(left, right) {
                                    self.generate_float_comparison(left, right, "setne");
                                } else {
                                    if int_operands {
                                        self.emit("    cmpl %eax, %ecx");
                                    } else {
                                        self.emit("    cmpq %rax, %rcx");
                                    }
                                    self.emit("    setne %al");
                                    self.emit("    movzbq %al, %rax");
                                }
                            }
                            BinOp::Add
                            | BinOp::Subtract
                            | BinOp::LogicalAnd
                            | BinOp::LogicalOr
                            | BinOp::Comma => {
                                unreachable!()
                            }
                        }
                    }
                }
                Ok(())
            }
            AstNode::TernaryOp {
                condition,
                true_expr,
                false_expr,
            } => {
                let false_label = self.next_label();
                let end_label = self.next_label();

                // Evaluate condition
                self.generate_node(condition)?;
                self.emit("    cmpq $0, %rax");
                self.emit(&format!("    je {}", false_label));

                // True branch
                self.generate_node(true_expr)?;
                self.emit(&format!("    jmp {}", end_label));

                // False branch
                self.emit(&format!("{}:", false_label));
                self.generate_node(false_expr)?;

                // End
                self.emit(&format!("{}:", end_label));
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
                    UnaryOp::Plus => {}
                    UnaryOp::Negate => {
                        self.emit("    negq %rax");
                    }
                    UnaryOp::BitNot => {
                        let operand_type = self.expr_type(operand)?;
                        let promoted = self.promote_integer_type(&operand_type);
                        let unsigned_ops = matches!(promoted, Type::UInt | Type::ULong);
                        let int_operands = matches!(promoted, Type::Int | Type::UInt);
                        if int_operands {
                            self.emit("    notl %eax");
                            if !unsigned_ops {
                                self.emit("    cltq");
                            }
                        } else {
                            self.emit("    notq %rax");
                        }
                    }
                }
                Ok(())
            }
            AstNode::PrefixIncrement(operand) => {
                if let AstNode::MemberAccess {
                    base,
                    member,
                    through_pointer,
                } = operand.as_ref()
                {
                    let field_info =
                        self.member_access_field_info(base, member, *through_pointer)?;
                    if field_info.bit_width.is_some() {
                        return self.generate_bitfield_incdec(
                            base,
                            member,
                            *through_pointer,
                            &field_info,
                            1,
                            false,
                        );
                    }
                }

                // Get address of operand
                self.generate_lvalue(operand)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address

                // Determine type and load, increment, store
                let operand_type = self.expr_type(operand)?;
                if matches!(operand_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl (%rcx), %eax");
                    self.emit("    addl $1, %eax");
                    self.emit("    movl %eax, (%rcx)");
                } else if matches!(operand_type, Type::UShort | Type::Short) {
                    self.emit("    movw (%rcx), %ax");
                    self.emit("    addw $1, %ax");
                    self.emit("    movw %ax, (%rcx)");
                } else if matches!(operand_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit("    movb (%rcx), %al");
                    self.emit("    addb $1, %al");
                    self.emit("    movb %al, (%rcx)");
                } else if matches!(operand_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    addq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                } else if matches!(operand_type, Type::Pointer(_)) {
                    let pointee_size = operand_type.element_size().unwrap_or(1);
                    self.emit("    movq (%rcx), %rax");
                    self.emit(&format!("    addq ${}, %rax", pointee_size));
                    self.emit("    movq %rax, (%rcx)");
                } else {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    addq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                }
                // Result is in %rax (the new value)
                self.coerce_rax_to_type(&operand_type);
                Ok(())
            }
            AstNode::PrefixDecrement(operand) => {
                if let AstNode::MemberAccess {
                    base,
                    member,
                    through_pointer,
                } = operand.as_ref()
                {
                    let field_info =
                        self.member_access_field_info(base, member, *through_pointer)?;
                    if field_info.bit_width.is_some() {
                        return self.generate_bitfield_incdec(
                            base,
                            member,
                            *through_pointer,
                            &field_info,
                            -1,
                            false,
                        );
                    }
                }

                // Get address of operand
                self.generate_lvalue(operand)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address

                // Determine type and load, decrement, store
                let operand_type = self.expr_type(operand)?;
                if matches!(operand_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl (%rcx), %eax");
                    self.emit("    subl $1, %eax");
                    self.emit("    movl %eax, (%rcx)");
                } else if matches!(operand_type, Type::UShort | Type::Short) {
                    self.emit("    movw (%rcx), %ax");
                    self.emit("    subw $1, %ax");
                    self.emit("    movw %ax, (%rcx)");
                } else if matches!(operand_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit("    movb (%rcx), %al");
                    self.emit("    subb $1, %al");
                    self.emit("    movb %al, (%rcx)");
                } else if matches!(operand_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    subq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                } else if matches!(operand_type, Type::Pointer(_)) {
                    let pointee_size = operand_type.element_size().unwrap_or(1);
                    self.emit("    movq (%rcx), %rax");
                    self.emit(&format!("    subq ${}, %rax", pointee_size));
                    self.emit("    movq %rax, (%rcx)");
                } else {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    subq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                }
                // Result is in %rax (the new value)
                self.coerce_rax_to_type(&operand_type);
                Ok(())
            }
            AstNode::PostfixIncrement(operand) => {
                if let AstNode::MemberAccess {
                    base,
                    member,
                    through_pointer,
                } = operand.as_ref()
                {
                    let field_info =
                        self.member_access_field_info(base, member, *through_pointer)?;
                    if field_info.bit_width.is_some() {
                        return self.generate_bitfield_incdec(
                            base,
                            member,
                            *through_pointer,
                            &field_info,
                            1,
                            true,
                        );
                    }
                }

                // Get address of operand
                self.generate_lvalue(operand)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address

                // Determine type and load, save old value, increment, store
                let operand_type = self.expr_type(operand)?;
                if matches!(operand_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl (%rcx), %eax");
                    self.emit("    movl %eax, %edx"); // Save old value in %edx
                    self.emit("    addl $1, %eax");
                    self.emit("    movl %eax, (%rcx)");
                    self.emit("    movl %edx, %eax"); // Return old value
                } else if matches!(operand_type, Type::UShort | Type::Short) {
                    self.emit("    movw (%rcx), %ax");
                    self.emit("    movw %ax, %dx"); // Save old value in %dx
                    self.emit("    addw $1, %ax");
                    self.emit("    movw %ax, (%rcx)");
                    self.emit("    movw %dx, %ax"); // Return old value
                } else if matches!(operand_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit("    movb (%rcx), %al");
                    self.emit("    movb %al, %dl"); // Save old value in %dl
                    self.emit("    addb $1, %al");
                    self.emit("    movb %al, (%rcx)");
                    self.emit("    movb %dl, %al"); // Return old value
                } else if matches!(operand_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit("    addq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                } else if matches!(operand_type, Type::Pointer(_)) {
                    let pointee_size = operand_type.element_size().unwrap_or(1);
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit(&format!("    addq ${}, %rax", pointee_size));
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                } else {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit("    addq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                }
                // Result is in %rax (the old value)
                self.coerce_rax_to_type(&operand_type);
                Ok(())
            }
            AstNode::PostfixDecrement(operand) => {
                if let AstNode::MemberAccess {
                    base,
                    member,
                    through_pointer,
                } = operand.as_ref()
                {
                    let field_info =
                        self.member_access_field_info(base, member, *through_pointer)?;
                    if field_info.bit_width.is_some() {
                        return self.generate_bitfield_incdec(
                            base,
                            member,
                            *through_pointer,
                            &field_info,
                            -1,
                            true,
                        );
                    }
                }

                // Get address of operand
                self.generate_lvalue(operand)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address

                // Determine type and load, save old value, decrement, store
                let operand_type = self.expr_type(operand)?;
                if matches!(operand_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl (%rcx), %eax");
                    self.emit("    movl %eax, %edx"); // Save old value in %edx
                    self.emit("    subl $1, %eax");
                    self.emit("    movl %eax, (%rcx)");
                    self.emit("    movl %edx, %eax"); // Return old value
                } else if matches!(operand_type, Type::UShort | Type::Short) {
                    self.emit("    movw (%rcx), %ax");
                    self.emit("    movw %ax, %dx"); // Save old value in %dx
                    self.emit("    subw $1, %ax");
                    self.emit("    movw %ax, (%rcx)");
                    self.emit("    movw %dx, %ax"); // Return old value
                } else if matches!(operand_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit("    movb (%rcx), %al");
                    self.emit("    movb %al, %dl"); // Save old value in %dl
                    self.emit("    subb $1, %al");
                    self.emit("    movb %al, (%rcx)");
                    self.emit("    movb %dl, %al"); // Return old value
                } else if matches!(operand_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit("    subq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                } else if matches!(operand_type, Type::Pointer(_)) {
                    let pointee_size = operand_type.element_size().unwrap_or(1);
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit(&format!("    subq ${}, %rax", pointee_size));
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                } else {
                    self.emit("    movq (%rcx), %rax");
                    self.emit("    movq %rax, %rdx"); // Save old value in %rdx
                    self.emit("    subq $1, %rax");
                    self.emit("    movq %rax, (%rcx)");
                    self.emit("    movq %rdx, %rax"); // Return old value
                }
                // Result is in %rax (the old value)
                self.coerce_rax_to_type(&operand_type);
                Ok(())
            }
            AstNode::AddressOf(expr) => {
                // Special case: taking address of a function
                if let AstNode::Variable(name) = &**expr {
                    // Check if this is a local/global variable or a function
                    let is_variable = self.symbol_table.get_variable(name).is_some()
                        || self.global_variables.contains_key(name);

                    if is_variable {
                        // Regular variable - use lvalue
                        self.generate_lvalue(expr)?;
                    } else {
                        // Assume it's a function name - load its address
                        self.emit(&format!("    leaq {}(%rip), %rax", name));
                    }
                    Ok(())
                } else {
                    self.generate_lvalue(expr)?;
                    Ok(())
                }
            }
            AstNode::Dereference(expr) => {
                self.generate_node(expr)?;
                let operand_type = self.expr_type(expr)?;
                let pointee_type = match operand_type {
                    Type::Pointer(pointee) => *pointee,
                    Type::Array(elem, _) => *elem, // Arrays decay to pointers
                    Type::FunctionPointer { .. } => {
                        // Function pointers are already addresses - dereferencing is a no-op.
                        return Ok(());
                    }
                    _ => return Err("Cannot dereference non-pointer type".to_string()),
                };

                if matches!(pointee_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else if matches!(pointee_type, Type::UInt) {
                    self.emit("    movl (%rax), %eax");
                } else if matches!(pointee_type, Type::UShort) {
                    self.emit("    movzwq (%rax), %rax");
                } else if matches!(pointee_type, Type::Short) {
                    self.emit("    movswq (%rax), %rax");
                } else if matches!(pointee_type, Type::Char) {
                    self.emit("    movsbq (%rax), %rax");
                } else if matches!(pointee_type, Type::UChar | Type::Bool) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(pointee_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
                } else if matches!(pointee_type, Type::Float) {
                    self.emit("    movss (%rax), %xmm0");
                    self.emit("    cvtss2sd %xmm0, %xmm0");
                } else if matches!(pointee_type, Type::Double) {
                    self.emit("    movsd (%rax), %xmm0");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::ArrayIndex { array, index } => {
                let elem_type = self.array_element_type(array)?;
                self.generate_array_index_address(array, index)?;

                // Arrays and structs decay to pointers, so keep the address
                if elem_type.is_array() || matches!(elem_type, Type::Struct(_) | Type::Union(_)) {
                    // Address is already in %rax, no need to load
                } else if matches!(elem_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else if matches!(elem_type, Type::UInt) {
                    self.emit("    movl (%rax), %eax");
                } else if matches!(elem_type, Type::UShort) {
                    self.emit("    movzwq (%rax), %rax");
                } else if matches!(elem_type, Type::Short) {
                    self.emit("    movswq (%rax), %rax");
                } else if matches!(elem_type, Type::Char) {
                    self.emit("    movsbq (%rax), %rax");
                } else if matches!(elem_type, Type::UChar | Type::Bool) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(elem_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
                } else if matches!(elem_type, Type::Float) {
                    self.emit("    movss (%rax), %xmm0");
                    self.emit("    cvtss2sd %xmm0, %xmm0");
                } else if matches!(elem_type, Type::Double) {
                    self.emit("    movsd (%rax), %xmm0");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::ArrayInit(_) => Err("Array initializer codegen not implemented".to_string()),
            AstNode::StructInit(_) => Err("Struct initializer codegen not implemented".to_string()),
            AstNode::CompoundLiteral { literal_type, .. } => {
                self.generate_lvalue(node)?;
                if literal_type.is_array()
                    || matches!(literal_type, Type::Struct(_) | Type::Union(_))
                {
                    return Ok(());
                }

                if matches!(literal_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else if matches!(literal_type, Type::UInt) {
                    self.emit("    movl (%rax), %eax");
                } else if matches!(literal_type, Type::UShort) {
                    self.emit("    movzwq (%rax), %rax");
                } else if matches!(literal_type, Type::Short) {
                    self.emit("    movswq (%rax), %rax");
                } else if matches!(literal_type, Type::Char) {
                    self.emit("    movsbq (%rax), %rax");
                } else if matches!(literal_type, Type::UChar | Type::Bool) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(literal_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
                } else if matches!(literal_type, Type::Float) {
                    self.emit("    movss (%rax), %xmm0");
                    self.emit("    cvtss2sd %xmm0, %xmm0");
                } else if matches!(literal_type, Type::Double) {
                    self.emit("    movsd (%rax), %xmm0");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::MemberAccess {
                base,
                member,
                through_pointer,
            } => {
                let field_info = self.member_access_field_info(base, member, *through_pointer)?;
                self.generate_member_access_address(base, member, *through_pointer)?;
                if field_info.bit_width.is_some() {
                    self.emit_bitfield_load("%rax", &field_info)?;
                    return Ok(());
                }
                let field_type = field_info.field_type.clone();
                if matches!(field_type, Type::Int) {
                    self.emit("    movslq (%rax), %rax");
                } else if matches!(field_type, Type::UInt) {
                    self.emit("    movl (%rax), %eax");
                } else if matches!(field_type, Type::UShort) {
                    self.emit("    movzwq (%rax), %rax");
                } else if matches!(field_type, Type::Short) {
                    self.emit("    movswq (%rax), %rax");
                } else if matches!(field_type, Type::Char) {
                    self.emit("    movsbq (%rax), %rax");
                } else if matches!(field_type, Type::UChar | Type::Bool) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(field_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
                } else if matches!(field_type, Type::Float) {
                    self.emit("    movss (%rax), %xmm0");
                    self.emit("    cvtss2sd %xmm0, %xmm0");
                } else if matches!(field_type, Type::Double) {
                    self.emit("    movsd (%rax), %xmm0");
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
            AstNode::AlignOfType(ty) => {
                let alignment = self.type_alignment(ty)?;
                self.emit(&format!("    movq ${}, %rax", alignment));
                Ok(())
            }
            AstNode::AlignOfExpr(expr) => {
                let ty = self.expr_type(expr)?;
                let alignment = self.type_alignment(&ty)?;
                self.emit(&format!("    movq ${}, %rax", alignment));
                Ok(())
            }
            AstNode::OffsetOf {
                struct_type,
                member,
            } => {
                // Get the struct/union layout
                let layout = match struct_type {
                    Type::Struct(name) => self
                        .struct_layouts
                        .get(name)
                        .ok_or_else(|| format!("Unknown struct: {}", name))?,
                    Type::Union(name) => self
                        .union_layouts
                        .get(name)
                        .ok_or_else(|| format!("Unknown union: {}", name))?,
                    _ => {
                        return Err(format!(
                            "offsetof requires struct or union type, got {:?}",
                            struct_type
                        ));
                    }
                };

                // Find the member and get its offset
                let field_info = layout
                    .fields_by_name
                    .get(member)
                    .ok_or_else(|| format!("Unknown member '{}' in struct/union", member))?;

                self.emit(&format!("    movq ${}, %rax", field_info.offset));
                Ok(())
            }
            AstNode::BuiltinTypesCompatible { left, right } => {
                let compatible = self.types_compatible(left, right)?;
                let value = if compatible { 1 } else { 0 };
                self.emit(&format!("    movl ${}, %eax", value));
                self.emit("    cltq");
                Ok(())
            }
            AstNode::Cast { target_type, expr } => {
                // Generate code for the expression
                self.generate_node(expr)?;

                // Get the source type
                let source_type = self.expr_type(expr)?;

                // Perform type conversion
                self.convert_type(&source_type, target_type)?;
                Ok(())
            }
            AstNode::StmtExpr { stmts, result } => {
                // Generate code for all statements
                for stmt in stmts.iter() {
                    self.generate_node(stmt)?;
                }
                // Generate code for the result expression
                self.generate_node(result)?;
                Ok(())
            }
            AstNode::IntLiteral(n) => {
                self.emit(&format!("    movl ${}, %eax", n));
                self.emit("    cltq");
                Ok(())
            }
            AstNode::CharLiteral(c) => {
                self.emit(&format!("    movl ${}, %eax", c));
                self.emit("    cltq");
                Ok(())
            }
            AstNode::FloatLiteral(f) => {
                // Get or create a label for this float literal
                let label = self.add_float_literal(*f);
                // Load the float value into xmm0
                self.emit(&format!("    movsd {}(%rip), %xmm0", label));
                Ok(())
            }
            AstNode::StringLiteral(s) => {
                // Get or create a label for this string literal
                let label = self.add_string_literal(s.clone());
                // Load the address of the string literal into %rax
                self.emit(&format!("    leaq {}(%rip), %rax", label));
                Ok(())
            }
            AstNode::WideStringLiteral(s) => {
                let label = self.add_wide_string_literal(s.clone());
                self.emit(&format!("    leaq {}(%rip), %rax", label));
                Ok(())
            }
        }
    }

    fn generate_lvalue(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::CompoundLiteral {
                name,
                literal_type,
                init,
                is_global,
            } => {
                if *is_global {
                    self.emit(&format!("    leaq {}(%rip), %rax", name));
                    return Ok(());
                }
                let offset = self.materialize_compound_literal(name, literal_type, init)?;
                self.emit(&format!("    leaq {}(%rbp), %rax", offset));
                Ok(())
            }
            AstNode::Variable(name) => {
                // Check if this is a global variable
                if self.global_variables.contains_key(name) {
                    // Load address of global variable using label
                    self.emit(&format!("    leaq {}(%rip), %rax", name));
                    return Ok(());
                }

                let offset = self
                    .symbol_table
                    .get_variable(name)
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
            AstNode::MemberAccess {
                base,
                member,
                through_pointer,
            } => {
                let field_info = self.member_access_field_info(base, member, *through_pointer)?;
                if field_info.bit_width.is_some() {
                    return Err("Cannot take address of bit-field".to_string());
                }
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
        // Check if we need to swap for reverse indexing (e.g., 2[x])
        let array_type = self.expr_type(array)?;
        let (actual_array, actual_index) =
            if matches!(array_type, Type::Array(_, _) | Type::Pointer(_)) {
                (array, index)
            } else {
                // Reverse indexing: swap array and index
                (index, array)
            };

        let elem_size = self.array_element_size(actual_array)?;

        self.generate_node(actual_array)?;
        self.emit("    pushq %rax");
        self.stack_depth += 1;

        self.generate_node(actual_index)?;
        if elem_size != 1 {
            self.emit(&format!("    imulq ${}, %rax", elem_size));
        }
        self.emit_pop("%rcx");
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
        let (struct_or_union_name, is_address) =
            self.resolve_struct_or_union_base(base, through_pointer)?;

        // Try struct first, then union
        let layout = self
            .struct_layouts
            .get(&struct_or_union_name)
            .or_else(|| self.union_layouts.get(&struct_or_union_name))
            .ok_or_else(|| format!("Unknown struct/union type: {}", struct_or_union_name))?;

        let field = layout.fields_by_name.get(member).ok_or_else(|| {
            format!(
                "Unknown field '{}' on struct/union {}",
                member, struct_or_union_name
            )
        })?;

        if is_address {
            self.emit(&format!("    addq ${}, %rax", field.offset));
        } else {
            self.emit(&format!("    leaq {}(%rbp), %rax", field.offset));
        }
        Ok(())
    }

    fn emit_bitfield_load(
        &mut self,
        addr_reg: &str,
        field: &StructFieldInfo,
    ) -> Result<(), String> {
        let width = field
            .bit_width
            .ok_or_else(|| "Expected bit-field metadata".to_string())?;
        let unit_size = field.bit_unit_size;
        if unit_size == 0 {
            return Err("Bit-field has no storage unit".to_string());
        }
        let unit_bits = (unit_size as u32) * 8;
        match unit_size {
            1 => self.emit(&format!("    movzbq ({}), %rax", addr_reg)),
            2 => self.emit(&format!("    movzwq ({}), %rax", addr_reg)),
            4 => self.emit(&format!("    movl ({}), %eax", addr_reg)),
            8 => self.emit(&format!("    movq ({}), %rax", addr_reg)),
            _ => return Err(format!("Unsupported bit-field unit size: {}", unit_size)),
        }
        if field.bit_offset > 0 {
            self.emit(&format!("    shrq ${}, %rax", field.bit_offset));
        }
        if width < unit_bits {
            let mask = (1u64 << width) - 1;
            if mask <= i32::MAX as u64 {
                self.emit(&format!("    andq ${}, %rax", mask));
            } else {
                self.emit(&format!("    movabsq ${}, %r8", mask as i64));
                self.emit("    andq %r8, %rax");
            }
        }
        if self.is_signed_integer_type(&field.field_type) && width < 64 {
            let shift = 64 - width;
            if shift > 0 {
                self.emit(&format!("    shlq ${}, %rax", shift));
                self.emit(&format!("    sarq ${}, %rax", shift));
            }
        }
        Ok(())
    }

    fn emit_bitfield_store_from_rdx(
        &mut self,
        addr_reg: &str,
        field: &StructFieldInfo,
    ) -> Result<(), String> {
        let width = field
            .bit_width
            .ok_or_else(|| "Expected bit-field metadata".to_string())?;
        let unit_size = field.bit_unit_size;
        if unit_size == 0 {
            return Err("Bit-field has no storage unit".to_string());
        }
        let unit_bits = (unit_size as u32) * 8;
        if width == 0 {
            return Ok(());
        }
        if width > unit_bits {
            return Err(format!("Bit-field width {} exceeds storage unit", width));
        }

        if width < 64 {
            let mask = (1u64 << width) - 1;
            if mask <= i32::MAX as u64 {
                self.emit(&format!("    andq ${}, %rdx", mask));
            } else {
                self.emit(&format!("    movabsq ${}, %r8", mask as i64));
                self.emit("    andq %r8, %rdx");
            }
        }

        match unit_size {
            1 => self.emit(&format!("    movzbq ({}), %rax", addr_reg)),
            2 => self.emit(&format!("    movzwq ({}), %rax", addr_reg)),
            4 => self.emit(&format!("    movl ({}), %eax", addr_reg)),
            8 => self.emit(&format!("    movq ({}), %rax", addr_reg)),
            _ => return Err(format!("Unsupported bit-field unit size: {}", unit_size)),
        }

        if width < 64 {
            let mask = (1u64 << width) - 1;
            let shifted_mask = mask << field.bit_offset;
            let clear_mask = !shifted_mask;
            self.emit(&format!("    movabsq ${}, %r8", clear_mask as i64));
            self.emit("    andq %r8, %rax");
            self.emit("    movq %rdx, %r9");
            if field.bit_offset > 0 {
                self.emit(&format!("    shlq ${}, %r9", field.bit_offset));
            }
            self.emit("    orq %r9, %rax");
        } else {
            self.emit("    movq %rdx, %rax");
        }

        match unit_size {
            1 => self.emit(&format!("    movb %al, ({})", addr_reg)),
            2 => self.emit(&format!("    movw %ax, ({})", addr_reg)),
            4 => self.emit(&format!("    movl %eax, ({})", addr_reg)),
            8 => self.emit(&format!("    movq %rax, ({})", addr_reg)),
            _ => return Err(format!("Unsupported bit-field unit size: {}", unit_size)),
        }

        if self.is_signed_integer_type(&field.field_type) && width < 64 {
            let shift = 64 - width;
            if shift > 0 {
                self.emit(&format!("    shlq ${}, %rdx", shift));
                self.emit(&format!("    sarq ${}, %rdx", shift));
            }
        }

        Ok(())
    }

    fn generate_bitfield_assignment(
        &mut self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
        field: &StructFieldInfo,
        value: &AstNode,
    ) -> Result<(), String> {
        let value_type = self.expr_type(value)?;
        self.generate_node(value)?;
        self.convert_type(&value_type, &field.field_type)?;
        self.emit("    pushq %rax");
        self.stack_depth += 1;
        self.generate_member_access_address(base, member, through_pointer)?;
        self.emit("    movq %rax, %rcx");
        self.emit_pop("%rdx");
        self.emit_bitfield_store_from_rdx("%rcx", field)?;

        self.emit("    movq %rdx, %rax");
        self.coerce_rax_to_type(&field.field_type);
        Ok(())
    }

    fn generate_bitfield_incdec(
        &mut self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
        field: &StructFieldInfo,
        delta: i64,
        postfix: bool,
    ) -> Result<(), String> {
        self.generate_member_access_address(base, member, through_pointer)?;
        self.emit("    movq %rax, %rcx");
        self.emit_bitfield_load("%rcx", field)?;

        if postfix {
            self.emit("    movq %rax, %r10");
        }

        if delta >= 0 {
            self.emit(&format!("    addq ${}, %rax", delta));
        } else {
            self.emit(&format!("    subq ${}, %rax", -delta));
        }
        self.emit("    movq %rax, %rdx");
        self.emit_bitfield_store_from_rdx("%rcx", field)?;

        if postfix {
            self.emit("    movq %r10, %rax");
        } else {
            self.emit("    movq %rdx, %rax");
        }
        self.coerce_rax_to_type(&field.field_type);
        Ok(())
    }

    fn generate_add(&mut self, left: &AstNode, right: &AstNode) -> Result<(), String> {
        // Check for float operands first
        if self.are_float_operands(left, right) {
            return self.generate_float_binary_op(left, right, "addsd");
        }

        let left_elem = self.pointer_elem_size(left)?;
        let right_elem = self.pointer_elem_size(right)?;
        let int_operands = self.are_int_operands(left, right);
        let unsigned_ops = self.uses_unsigned_ops(left, right);
        if left_elem.is_some() && right_elem.is_some() {
            return Err("Cannot add two pointers".to_string());
        }

        self.generate_node(left)?;
        self.emit("    pushq %rax");
        self.stack_depth += 1;
        self.generate_node(right)?;
        self.emit_pop("%rcx");

        if let Some(elem_size) = left_elem {
            self.emit_scale("%rax", elem_size);
            self.emit("    addq %rcx, %rax");
        } else if let Some(elem_size) = right_elem {
            self.emit_scale("%rcx", elem_size);
            self.emit("    addq %rcx, %rax");
        } else if int_operands {
            self.emit("    addl %ecx, %eax");
            if !unsigned_ops {
                self.emit("    cltq");
            }
        } else {
            self.emit("    addq %rcx, %rax");
        }

        Ok(())
    }

    fn generate_subtract(&mut self, left: &AstNode, right: &AstNode) -> Result<(), String> {
        // Check for float operands first
        if self.are_float_operands(left, right) {
            return self.generate_float_binary_op(left, right, "subsd");
        }

        let left_elem = self.pointer_elem_size(left)?;
        let right_elem = self.pointer_elem_size(right)?;
        let int_operands = self.are_int_operands(left, right);
        let unsigned_ops = self.uses_unsigned_ops(left, right);

        if let (Some(left_size), Some(right_size)) = (left_elem, right_elem) {
            if left_size != right_size {
                return Err("Pointer subtraction requires matching element sizes".to_string());
            }
            self.generate_node(left)?;
            self.emit("    pushq %rax");
            self.stack_depth += 1;
            self.generate_node(right)?;
            self.emit_pop("%rcx");
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
        self.stack_depth += 1;
        self.generate_node(right)?;
        self.emit_pop("%rcx");

        if let Some(elem_size) = left_elem {
            self.emit_scale("%rax", elem_size);
            self.emit("    subq %rax, %rcx");
            self.emit("    movq %rcx, %rax");
        } else if int_operands {
            self.emit("    subl %eax, %ecx");
            self.emit("    movl %ecx, %eax");
            if !unsigned_ops {
                self.emit("    cltq");
            }
        } else {
            self.emit("    subq %rax, %rcx");
            self.emit("    movq %rcx, %rax");
        }

        Ok(())
    }

    fn pointer_elem_size(&self, node: &AstNode) -> Result<Option<i32>, String> {
        match node {
            AstNode::Variable(name) => {
                // Check if it's a global variable first
                if let Some(global) = self.global_variables.get(name) {
                    return self.element_size_from_type(&global.var_type);
                }

                let symbol = self
                    .symbol_table
                    .get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                self.element_size_from_type(&symbol.symbol_type)
            }
            AstNode::AddressOf(inner) => {
                // &x has pointer type, so get the element size of what it points to
                let ty = self.expr_type(inner)?;
                let ptr_ty = Type::Pointer(Box::new(ty));
                self.element_size_from_type(&ptr_ty)
            }
            _ => {
                // For other expressions, try to get the type and check if it's a pointer
                match self.expr_type(node) {
                    Ok(ty) => self.element_size_from_type(&ty),
                    Err(_) => Ok(None),
                }
            }
        }
    }

    fn are_int_operands(&self, left: &AstNode, right: &AstNode) -> bool {
        match (self.expr_type(left), self.expr_type(right)) {
            (Ok(left_ty), Ok(right_ty))
                if self.is_integer_type(&left_ty) && self.is_integer_type(&right_ty) =>
            {
                matches!(
                    self.binary_integer_type(&left_ty, &right_ty),
                    Ok(Type::Int | Type::UInt)
                )
            }
            _ => false,
        }
    }

    fn uses_unsigned_ops(&self, left: &AstNode, right: &AstNode) -> bool {
        match (self.expr_type(left), self.expr_type(right)) {
            (Ok(left_ty), Ok(right_ty)) => {
                // Pointers are always compared using unsigned operations
                if matches!(left_ty, Type::Pointer(_)) || matches!(right_ty, Type::Pointer(_)) {
                    return true;
                }

                // Integer types use unsigned ops if the result type is unsigned
                if self.is_integer_type(&left_ty) && self.is_integer_type(&right_ty) {
                    matches!(
                        self.binary_integer_type(&left_ty, &right_ty),
                        Ok(Type::UInt | Type::ULong)
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn shift_result_type(&self, left: &AstNode) -> Result<Type, String> {
        let left_ty = self.expr_type(left)?;
        if !self.is_integer_type(&left_ty) {
            return Err("Shift operands must be integers".to_string());
        }
        Ok(self.promote_integer_type(&left_ty))
    }

    fn is_float_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Float | Type::Double | Type::LongDouble)
    }

    fn are_float_operands(&self, left: &AstNode, right: &AstNode) -> bool {
        match (self.expr_type(left), self.expr_type(right)) {
            (Ok(left_ty), Ok(right_ty)) => {
                self.is_float_type(&left_ty) || self.is_float_type(&right_ty)
            }
            _ => false,
        }
    }

    fn generate_float_binary_op(
        &mut self,
        left: &AstNode,
        right: &AstNode,
        instruction: &str,
    ) -> Result<(), String> {
        // Generate left operand (result in xmm0 or %rax)
        self.generate_node(left)?;

        // Convert to double if needed
        let left_type = self.expr_type(left)?;
        if !self.is_float_type(&left_type) {
            // Integer in %rax, convert to double in xmm0
            self.convert_type(&left_type, &Type::Double)?;
        }

        // Save xmm0 to stack
        self.emit("    subq $8, %rsp");
        self.emit("    movsd %xmm0, (%rsp)");

        // Generate right operand (result in xmm0 or %rax)
        self.generate_node(right)?;

        // Convert to double if needed
        let right_type = self.expr_type(right)?;
        if !self.is_float_type(&right_type) {
            // Integer in %rax, convert to double in xmm0
            self.convert_type(&right_type, &Type::Double)?;
        }

        // Move right to xmm1
        self.emit("    movsd %xmm0, %xmm1");

        // Restore left to xmm0
        self.emit("    movsd (%rsp), %xmm0");
        self.emit("    addq $8, %rsp");

        // Perform operation: instruction xmm1, xmm0
        self.emit(&format!("    {} %xmm1, %xmm0", instruction));

        Ok(())
    }

    fn generate_float_comparison(
        &mut self,
        left: &AstNode,
        right: &AstNode,
        set_instruction: &str,
    ) {
        self.generate_node(left).unwrap();
        self.emit("    subq $8, %rsp");
        self.emit("    movsd %xmm0, (%rsp)");
        self.generate_node(right).unwrap();
        self.emit("    movsd %xmm0, %xmm1");
        self.emit("    movsd (%rsp), %xmm0");
        self.emit("    addq $8, %rsp");
        self.emit("    ucomisd %xmm1, %xmm0");
        self.emit(&format!("    {} %al", set_instruction));
        self.emit("    movzbq %al, %rax");
    }

    fn is_integer_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Int
                | Type::UInt
                | Type::Char
                | Type::Bool
                | Type::UChar
                | Type::UShort
                | Type::Short
                | Type::Long
                | Type::ULong
        )
    }

    fn is_signed_integer_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Char | Type::Short | Type::Int | Type::Long | Type::Enum(_)
        )
    }

    fn promote_integer_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Char | Type::Bool | Type::UChar | Type::UShort | Type::Short => Type::Int,
            Type::Int => Type::Int,
            Type::UInt => Type::UInt,
            Type::Long => Type::Long,
            Type::ULong => Type::ULong,
            _ => ty.clone(),
        }
    }

    fn binary_integer_type(&self, left: &Type, right: &Type) -> Result<Type, String> {
        if !self.is_integer_type(left) || !self.is_integer_type(right) {
            return Err("Expected integer types".to_string());
        }
        let left = self.promote_integer_type(left);
        let right = self.promote_integer_type(right);
        if matches!(left, Type::ULong) || matches!(right, Type::ULong) {
            Ok(Type::ULong)
        } else if matches!(left, Type::Long) || matches!(right, Type::Long) {
            Ok(Type::Long)
        } else if matches!(left, Type::UInt) || matches!(right, Type::UInt) {
            Ok(Type::UInt)
        } else {
            Ok(Type::Int)
        }
    }

    fn emit_scale(&mut self, reg: &str, elem_size: i32) {
        if elem_size != 1 {
            self.emit(&format!("    imulq ${}, {}", elem_size, reg));
        }
    }

    fn array_element_size(&self, array: &AstNode) -> Result<i32, String> {
        let array_type = self.expr_type(array)?;
        self.element_size_from_type(&array_type)?
            .ok_or_else(|| "Array indexing requires array or pointer element type".to_string())
    }

    fn element_size_from_type(&self, ty: &Type) -> Result<Option<i32>, String> {
        match ty {
            Type::Pointer(pointee) => {
                if matches!(pointee.as_ref(), Type::Void) {
                    Ok(Some(1))
                } else {
                    Ok(Some(self.type_size(pointee)?))
                }
            }
            Type::Array(elem, _) => Ok(Some(self.type_size(elem)?)),
            _ => Ok(None),
        }
    }

    fn array_element_type(&self, array: &AstNode) -> Result<Type, String> {
        let array_type = self.expr_type(array)?;
        self.array_element_type_from_type(&array_type)
    }

    fn array_element_type_from_type(&self, ty: &Type) -> Result<Type, String> {
        match ty {
            Type::Array(elem, _) => Ok(*elem.clone()),
            Type::Pointer(pointee) => Ok(*pointee.clone()),
            Type::TypeofExpr(expr) => {
                let resolved = self.typeof_expr_type(expr)?;
                self.array_element_type_from_type(&resolved)
            }
            _ => Err("Array element type unavailable".to_string()),
        }
    }

    fn typeof_expr_type(&self, expr: &AstNode) -> Result<Type, String> {
        match expr {
            AstNode::StringLiteral(s) => Ok(Type::Array(Box::new(Type::Char), s.len() + 1)),
            AstNode::WideStringLiteral(s) => {
                Ok(Type::Array(Box::new(Type::Int), s.chars().count() + 1))
            }
            _ => self.expr_type(expr),
        }
    }

    fn resolve_type(&self, ty: &Type) -> Result<Type, String> {
        match ty {
            Type::TypeofExpr(expr) => self.typeof_expr_type(expr),
            Type::Pointer(pointee) => Ok(Type::Pointer(Box::new(self.resolve_type(pointee)?))),
            Type::Array(elem, len) => Ok(Type::Array(Box::new(self.resolve_type(elem)?), *len)),
            Type::FunctionPointer {
                return_type,
                param_types,
                is_variadic,
            } => {
                let mut resolved_params = Vec::with_capacity(param_types.len());
                for param in param_types {
                    resolved_params.push(self.resolve_type(param)?);
                }
                Ok(Type::FunctionPointer {
                    return_type: Box::new(self.resolve_type(return_type)?),
                    param_types: resolved_params,
                    is_variadic: *is_variadic,
                })
            }
            _ => Ok(ty.clone()),
        }
    }

    fn types_compatible(&self, left: &Type, right: &Type) -> Result<bool, String> {
        let left = self.resolve_type(left)?;
        let right = self.resolve_type(right)?;
        Ok(left == right)
    }

    fn type_size(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Int => Ok(4),
            Type::UInt => Ok(4),
            Type::Char => Ok(1),
            Type::Bool => Ok(1),
            Type::UChar => Ok(1),
            Type::UShort => Ok(2),
            Type::Short => Ok(2),
            Type::Long => Ok(8),
            Type::ULong => Ok(8),
            Type::Float => Ok(4),
            Type::Double => Ok(8),
            Type::LongDouble => Ok(16),
            Type::TypeofExpr(expr) => {
                let resolved = self.typeof_expr_type(expr)?;
                self.type_size(&resolved)
            }
            Type::Pointer(_) | Type::FunctionPointer { .. } => Ok(8),
            Type::Void => Ok(0),
            Type::Array(elem, len) => Ok(self.type_size(elem)? * (*len as i32)),
            Type::Struct(name) => {
                let layout = self
                    .struct_layouts
                    .get(name)
                    .ok_or_else(|| format!("Unknown struct type: {}", name))?;
                Ok(layout.size)
            }
            Type::Union(name) => {
                let layout = self
                    .union_layouts
                    .get(name)
                    .ok_or_else(|| format!("Unknown union type: {}", name))?;
                Ok(layout.size)
            }
            Type::Enum(_) => Ok(4),
        }
    }

    fn type_alignment(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Int => Ok(4),
            Type::UInt => Ok(4),
            Type::Char => Ok(1),
            Type::Bool => Ok(1),
            Type::UChar => Ok(1),
            Type::UShort => Ok(2),
            Type::Short => Ok(2),
            Type::Long => Ok(8),
            Type::ULong => Ok(8),
            Type::Float => Ok(4),
            Type::Double => Ok(8),
            Type::LongDouble => Ok(16),
            Type::TypeofExpr(expr) => {
                let resolved = self.typeof_expr_type(expr)?;
                self.type_alignment(&resolved)
            }
            Type::Pointer(_) | Type::FunctionPointer { .. } => Ok(8),
            Type::Void => Ok(1),
            Type::Array(elem, _) => {
                // _Alignof for arrays returns the element alignment
                self.type_alignment(elem)
            }
            Type::Struct(name) => {
                let layout = self
                    .struct_layouts
                    .get(name)
                    .ok_or_else(|| format!("Unknown struct type: {}", name))?;
                Ok(layout.alignment)
            }
            Type::Union(name) => {
                let layout = self
                    .union_layouts
                    .get(name)
                    .ok_or_else(|| format!("Unknown union type: {}", name))?;
                Ok(layout.alignment)
            }
            Type::Enum(_) => Ok(4),
        }
    }

    /// Get the preferred stack allocation alignment for a type
    /// This may differ from type_alignment for performance reasons
    fn stack_alignment(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Array(_elem, _) => {
                // Arrays on stack are aligned to min(array_size, 16) for better performance
                let array_size = self.type_size(ty)?;
                let max_align = 16;
                Ok(array_size.min(max_align))
            }
            _ => self.type_alignment(ty),
        }
    }

    fn convert_type(&mut self, from: &Type, to: &Type) -> Result<(), String> {
        let from = self.resolve_type(from)?;
        let to = self.resolve_type(to)?;

        // If types are the same, no conversion needed
        if from == to {
            return Ok(());
        }

        // Helper to determine if a type is signed
        let is_signed = |ty: &Type| matches!(ty, Type::Char | Type::Short | Type::Int | Type::Long);

        match (&from, &to) {
            // Array to pointer decay - exact match
            (Type::Array(elem, _), Type::Pointer(to_elem)) if elem.as_ref() == to_elem.as_ref() => {
                Ok(())
            }
            // Array to pointer decay - nested arrays to simple pointer (e.g., int[2][3] to int*)
            // This is technically a type mismatch but commonly allowed in C
            (Type::Array(_, _), Type::Pointer(_)) => Ok(()),
            // Pointer to pointer - no conversion needed
            (Type::Pointer(_), Type::Pointer(_)) => Ok(()),

            // Convert to _Bool
            (Type::Float | Type::Double | Type::LongDouble, Type::Bool) => {
                // Compare against 0.0 and materialize 0/1 in %rax.
                self.emit("    xorpd %xmm1, %xmm1");
                self.emit("    ucomisd %xmm1, %xmm0");
                self.emit("    setne %al");
                self.emit("    setp %dl");
                self.emit("    orb %dl, %al");
                self.emit("    movzbq %al, %rax");
                Ok(())
            }
            (_, Type::Bool) => {
                self.emit("    testq %rax, %rax");
                self.emit("    setne %al");
                self.emit("    movzbq %al, %rax");
                Ok(())
            }

            // Integer to pointer or pointer to integer - just treat as 64-bit value
            (Type::Pointer(_), Type::Long | Type::ULong) => Ok(()),
            (Type::Long | Type::ULong, Type::Pointer(_)) => Ok(()),
            (Type::Pointer(_), Type::Int | Type::UInt) => {
                // Truncate to 32 bits
                self.emit("    movl %eax, %eax");
                Ok(())
            }
            (Type::Int | Type::UInt, Type::Pointer(_)) => {
                // Sign/zero extend to 64 bits
                if is_signed(&from) {
                    self.emit("    movslq %eax, %rax");
                } else {
                    self.emit("    movl %eax, %eax");
                }
                Ok(())
            }

            // Float to integer conversions
            (
                Type::Float | Type::Double | Type::LongDouble,
                Type::Int
                | Type::UInt
                | Type::Long
                | Type::ULong
                | Type::Char
                | Type::UChar
                | Type::Short
                | Type::UShort,
            ) => {
                // Convert float (in xmm0) to int (in rax)
                self.emit("    cvttsd2si %xmm0, %rax");
                // Now convert based on target integer size
                self.convert_type(&Type::Long, &to)?;
                Ok(())
            }

            // Integer to float conversions
            (
                Type::Int
                | Type::UInt
                | Type::Long
                | Type::ULong
                | Type::Char
                | Type::Bool
                | Type::UChar
                | Type::Short
                | Type::UShort,
                Type::Float | Type::Double | Type::LongDouble,
            ) => {
                // First convert to 64-bit integer in rax if needed
                let from_size = self.type_size(&from)?;
                if from_size < 8 {
                    self.convert_type(&from, &Type::Long)?;
                }
                // Convert int (in rax) to double (in xmm0)
                self.emit("    cvtsi2sd %rax, %xmm0");
                Ok(())
            }

            // Float to float (no conversion needed, both use xmm0)
            (Type::Float, Type::Double)
            | (Type::Double, Type::Float)
            | (Type::Float, Type::LongDouble)
            | (Type::LongDouble, Type::Float)
            | (Type::Double, Type::LongDouble)
            | (Type::LongDouble, Type::Double) => Ok(()),

            // Cast to void (discard result)
            (_, Type::Void) => Ok(()),

            // Integer conversions
            _ => {
                let from_size = self.type_size(&from)?;
                let to_size = self.type_size(&to)?;

                match (from_size, to_size) {
                    // Same size - no conversion needed
                    (a, b) if a == b => Ok(()),

                    // Widening conversions
                    (1, 2) => {
                        if is_signed(&from) {
                            self.emit("    movsbw %al, %ax");
                        } else {
                            self.emit("    movzbw %al, %ax");
                        }
                        Ok(())
                    }
                    (1, 4) => {
                        if is_signed(&from) {
                            self.emit("    movsbl %al, %eax");
                        } else {
                            self.emit("    movzbl %al, %eax");
                        }
                        Ok(())
                    }
                    (1, 8) => {
                        if is_signed(&from) {
                            self.emit("    movsbq %al, %rax");
                        } else {
                            self.emit("    movzbq %al, %rax");
                        }
                        Ok(())
                    }
                    (2, 4) => {
                        if is_signed(&from) {
                            self.emit("    movswl %ax, %eax");
                        } else {
                            self.emit("    movzwl %ax, %eax");
                        }
                        Ok(())
                    }
                    (2, 8) => {
                        if is_signed(&from) {
                            self.emit("    movswq %ax, %rax");
                        } else {
                            self.emit("    movzwq %ax, %rax");
                        }
                        Ok(())
                    }
                    (4, 8) => {
                        if is_signed(&from) {
                            self.emit("    movslq %eax, %rax");
                        } else {
                            self.emit("    movl %eax, %eax");
                        }
                        Ok(())
                    }

                    // Narrowing conversions - just truncate, no instruction needed
                    (_, 1) => {
                        // Value is already in %al (lower 8 bits of %rax)
                        Ok(())
                    }
                    (_, 2) => {
                        // Value is already in %ax (lower 16 bits of %rax)
                        Ok(())
                    }
                    (_, 4) => {
                        // Value is already in %eax (lower 32 bits of %rax)
                        // But we need to zero the upper 32 bits
                        self.emit("    movl %eax, %eax");
                        Ok(())
                    }

                    _ => Err(format!(
                        "Unsupported type conversion from {:?} to {:?}",
                        from, to
                    )),
                }
            }
        }
    }

    fn register_struct_layout(
        &mut self,
        name: &str,
        fields: &[StructField],
        attributes: &TypeAttributes,
    ) -> Result<(), String> {
        // Check if struct already exists
        if let Some(existing_layout) = self.struct_layouts.get(name) {
            // If existing is complete (has fields), error
            if !existing_layout.fields.is_empty() {
                return Err(format!("Struct '{}' already defined", name));
            }
            // If existing is incomplete (forward declaration)
            if fields.is_empty() {
                // New is also incomplete - multiple forward declarations are OK
                return Ok(());
            }
            // New is complete - replace incomplete with complete
            // (fall through to register the complete definition)
        }

        // If this is a forward declaration (no fields), register with empty layout
        if fields.is_empty() {
            let mut alignment = 1;
            if let Some(explicit) = attributes.alignment {
                alignment = alignment.max(explicit as i32);
            }
            self.struct_layouts.insert(
                name.to_string(),
                StructLayout {
                    fields: Vec::new(),
                    fields_by_name: HashMap::new(),
                    size: 0,
                    alignment,
                },
            );
            return Ok(());
        }

        // This is a complete definition - calculate layout
        let mut offset = 0;
        let mut max_align = 1;
        let mut fields_by_name = HashMap::new();
        let mut ordered_fields = Vec::new();

        let mut bit_unit_size = 0;
        let mut bit_unit_align = 1;
        let mut bit_unit_start = 0;
        let mut bit_offset = 0u32;
        let mut in_bitfield = false;

        for field in fields {
            let field_size = self.type_size(&field.field_type)?;
            // Use explicit alignment from _Alignas if specified, otherwise use type's natural alignment
            let field_align = field.alignment.map(|a| a as i32).unwrap_or_else(|| {
                if attributes.packed {
                    1
                } else {
                    self.type_alignment(&field.field_type).unwrap_or(1)
                }
            });
            if field_size == 0 {
                return Err(format!("Field '{}' has invalid size", field.name));
            }

            let is_anonymous = field.name.starts_with("__anon_bitfield_");
            if let Some(bit_width) = field.bit_width {
                let unit_bits = (field_size as u32) * 8;
                if bit_width > unit_bits {
                    return Err(format!(
                        "Bit-field width {} exceeds type size for '{}'",
                        bit_width, field.name
                    ));
                }
                max_align = max_align.max(field_align);

                if bit_width == 0 {
                    if in_bitfield && bit_offset > 0 {
                        offset = offset.max(bit_unit_start + bit_unit_size);
                    }
                    offset = align_to(offset, field_align);
                    in_bitfield = false;
                    bit_offset = 0;
                    bit_unit_size = field_size;
                    bit_unit_align = field_align;
                    continue;
                }

                let start_new_unit =
                    !in_bitfield || bit_unit_size != field_size || bit_unit_align != field_align;
                if start_new_unit {
                    if in_bitfield && bit_offset > 0 {
                        offset = offset.max(bit_unit_start + bit_unit_size);
                    }
                    bit_unit_size = field_size;
                    bit_unit_align = field_align;

                    let mut unit_start = offset - (offset % bit_unit_size);
                    let mut start_bit = ((offset - unit_start) * 8) as u32;
                    if start_bit + bit_width > unit_bits {
                        unit_start += bit_unit_size;
                        start_bit = 0;
                    }
                    bit_unit_start = unit_start;
                    bit_offset = start_bit;
                    offset = offset.max(bit_unit_start + bit_unit_size);
                } else if bit_offset + bit_width > unit_bits {
                    bit_unit_start += bit_unit_size;
                    bit_offset = 0;
                    offset = offset.max(bit_unit_start + bit_unit_size);
                }

                if !is_anonymous {
                    let info = StructFieldInfo {
                        field_type: field.field_type.clone(),
                        offset: bit_unit_start,
                        bit_width: Some(bit_width),
                        bit_offset,
                        bit_unit_size,
                    };
                    ordered_fields.push(info.clone());
                    fields_by_name.insert(field.name.clone(), info);
                }

                bit_offset += bit_width;
                in_bitfield = true;
                continue;
            }

            if in_bitfield && bit_offset > 0 {
                offset = offset.max(bit_unit_start + bit_unit_size);
                in_bitfield = false;
                bit_offset = 0;
            }

            offset = align_to(offset, field_align);
            max_align = max_align.max(field_align);
            let info = StructFieldInfo {
                field_type: field.field_type.clone(),
                offset,
                bit_width: None,
                bit_offset: 0,
                bit_unit_size: 0,
            };
            ordered_fields.push(info.clone());
            fields_by_name.insert(field.name.clone(), info);
            offset += field_size;
        }
        if let Some(explicit) = attributes.alignment {
            max_align = max_align.max(explicit as i32);
        }
        let size = align_to(offset, max_align);

        self.struct_layouts.insert(
            name.to_string(),
            StructLayout {
                fields: ordered_fields,
                fields_by_name,
                alignment: max_align,
                size,
            },
        );

        Ok(())
    }

    fn register_union_layout(
        &mut self,
        name: &str,
        fields: &[StructField],
        attributes: &TypeAttributes,
    ) -> Result<(), String> {
        // Check if union already exists
        if let Some(existing_layout) = self.union_layouts.get(name) {
            // If existing is complete (has fields), error
            if !existing_layout.fields.is_empty() {
                return Err(format!("Union '{}' already defined", name));
            }
            // If existing is incomplete (forward declaration)
            if fields.is_empty() {
                // New is also incomplete - multiple forward declarations are OK
                return Ok(());
            }
            // New is complete - replace incomplete with complete
            // (fall through to register the complete definition)
        }

        // If this is a forward declaration (no fields), register with empty layout
        if fields.is_empty() {
            let mut alignment = 1;
            if let Some(explicit) = attributes.alignment {
                alignment = alignment.max(explicit as i32);
            }
            self.union_layouts.insert(
                name.to_string(),
                StructLayout {
                    fields: Vec::new(),
                    fields_by_name: HashMap::new(),
                    size: 0,
                    alignment,
                },
            );
            return Ok(());
        }

        // This is a complete definition - calculate layout
        let mut max_size = 0;
        let mut max_align = 1;
        let mut fields_by_name = HashMap::new();
        let mut ordered_fields = Vec::new();
        for field in fields {
            let field_size = self.type_size(&field.field_type)?;
            // Use explicit alignment from _Alignas if specified, otherwise use type's natural alignment
            let field_align = field.alignment.map(|a| a as i32).unwrap_or_else(|| {
                if attributes.packed {
                    1
                } else {
                    self.type_alignment(&field.field_type).unwrap_or(1)
                }
            });
            if field_size == 0 {
                return Err(format!("Field '{}' has invalid size", field.name));
            }
            if let Some(bit_width) = field.bit_width {
                let unit_bits = (field_size as u32) * 8;
                if bit_width > unit_bits {
                    return Err(format!(
                        "Bit-field width {} exceeds type size for '{}'",
                        bit_width, field.name
                    ));
                }
                if bit_width == 0 || field.name.starts_with("__anon_bitfield_") {
                    continue;
                }
            }
            max_size = max_size.max(field_size);
            max_align = max_align.max(field_align);
            // All fields in a union are at offset 0
            let (bit_width, bit_unit_size) = if let Some(width) = field.bit_width {
                (Some(width), field_size)
            } else {
                (None, 0)
            };
            let info = StructFieldInfo {
                field_type: field.field_type.clone(),
                offset: 0,
                bit_width,
                bit_offset: 0,
                bit_unit_size,
            };
            ordered_fields.push(info.clone());
            fields_by_name.insert(field.name.clone(), info);
        }
        if let Some(explicit) = attributes.alignment {
            max_align = max_align.max(explicit as i32);
        }
        let size = align_to(max_size, max_align);

        self.union_layouts.insert(
            name.to_string(),
            StructLayout {
                fields: ordered_fields,
                fields_by_name,
                alignment: max_align,
                size,
            },
        );

        Ok(())
    }

    fn generate_array_init(
        &mut self,
        array_type: &Type,
        values: &[AstNode],
        base_offset: i32,
    ) -> Result<(), String> {
        let elem_type = self.array_element_type_from_type(array_type)?;
        let elem_size = self.type_size(&elem_type)?;
        let array_len = match array_type {
            Type::Array(_, len) => *len,
            _ => return Err("Expected array type".to_string()),
        };

        if values.len() > array_len {
            return Err(format!(
                "Array initializer has {} elements, but array length is {}",
                values.len(),
                array_len
            ));
        }

        for (i, value) in values.iter().enumerate() {
            let elem_offset = base_offset + (i as i32) * elem_size;

            // Check if this is a nested array initializer
            if let AstNode::ArrayInit(nested_values) = value {
                // Recursively handle nested array initialization
                if elem_type.is_array() {
                    self.generate_array_init(&elem_type, nested_values, elem_offset)?;
                } else {
                    return Err("Nested initializer for non-array element".to_string());
                }
            } else {
                // Generate code for the value
                self.generate_node(value)?;

                // Store the value at the appropriate offset
                if matches!(elem_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit(&format!("    movl %eax, {}(%rbp)", elem_offset));
                } else if matches!(elem_type, Type::UShort | Type::Short) {
                    self.emit(&format!("    movw %ax, {}(%rbp)", elem_offset));
                } else if matches!(elem_type, Type::Char | Type::UChar | Type::Bool) {
                    self.emit(&format!("    movb %al, {}(%rbp)", elem_offset));
                } else {
                    self.emit(&format!("    movq %rax, {}(%rbp)", elem_offset));
                }
            }
        }

        // Zero-fill remaining elements
        for i in values.len()..array_len {
            let elem_offset = base_offset + (i as i32) * elem_size;
            self.zero_fill_element(&elem_type, elem_offset)?;
        }

        Ok(())
    }

    fn generate_string_array_init(
        &mut self,
        array_type: &Type,
        string: &str,
        base_offset: i32,
    ) -> Result<(), String> {
        let elem_type = self.array_element_type_from_type(array_type)?;
        let mut array_len = match array_type {
            Type::Array(_, len) => *len,
            _ => return Err("Expected array type".to_string()),
        };

        // If array length is 0 (from [] syntax), infer from string literal
        if array_len == 0 {
            array_len = string.len() + 1; // +1 for null terminator
        }

        // Ensure element type is char or uchar
        if !matches!(elem_type, Type::Char | Type::UChar) {
            return Err("String literals can only initialize char arrays".to_string());
        }

        // Initialize each character
        for (i, ch) in string.chars().enumerate() {
            if i >= array_len {
                return Err(format!(
                    "String literal has {} characters (plus null terminator), but array length is {}",
                    string.len(),
                    array_len
                ));
            }
            let char_offset = base_offset + (i as i32);
            self.emit(&format!("    movb ${}, {}(%rbp)", ch as u8, char_offset));
        }

        // Add null terminator if there's room
        if string.len() < array_len {
            let null_offset = base_offset + (string.len() as i32);
            self.emit(&format!("    movb $0, {}(%rbp)", null_offset));

            // Zero-fill any remaining elements
            for i in (string.len() + 1)..array_len {
                let char_offset = base_offset + (i as i32);
                self.emit(&format!("    movb $0, {}(%rbp)", char_offset));
            }
        }

        Ok(())
    }

    fn generate_wide_string_array_init(
        &mut self,
        array_type: &Type,
        string: &str,
        base_offset: i32,
    ) -> Result<(), String> {
        let elem_type = self.array_element_type_from_type(array_type)?;
        let mut array_len = match array_type {
            Type::Array(_, len) => *len,
            _ => return Err("Expected array type".to_string()),
        };

        let char_count = string.chars().count();
        if array_len == 0 {
            array_len = char_count + 1;
        }

        if !matches!(elem_type, Type::Int | Type::UInt | Type::Enum(_)) {
            return Err("Wide string literals can only initialize int arrays".to_string());
        }

        for (i, ch) in string.chars().enumerate() {
            if i >= array_len {
                return Err(format!(
                    "Wide string literal has {} characters (plus null terminator), but array length is {}",
                    char_count, array_len
                ));
            }
            let elem_offset = base_offset + (i as i32) * 4;
            self.emit(&format!("    movl ${}, {}(%rbp)", ch as u32, elem_offset));
        }

        if char_count < array_len {
            let null_offset = base_offset + (char_count as i32) * 4;
            self.emit(&format!("    movl $0, {}(%rbp)", null_offset));
            for i in (char_count + 1)..array_len {
                let elem_offset = base_offset + (i as i32) * 4;
                self.emit(&format!("    movl $0, {}(%rbp)", elem_offset));
            }
        }

        Ok(())
    }

    fn zero_fill_element(&mut self, elem_type: &Type, offset: i32) -> Result<(), String> {
        if elem_type.is_array() {
            // For nested arrays, recursively zero-fill
            let inner_elem_type = self.array_element_type_from_type(elem_type)?;
            let inner_elem_size = self.type_size(&inner_elem_type)?;
            let inner_array_len = match elem_type {
                Type::Array(_, len) => *len,
                _ => 0,
            };
            for i in 0..inner_array_len {
                let inner_offset = offset + (i as i32) * inner_elem_size;
                self.zero_fill_element(&inner_elem_type, inner_offset)?;
            }
        } else {
            // Zero-fill a single element
            if matches!(elem_type, Type::Int | Type::UInt | Type::Enum(_)) {
                self.emit(&format!("    movl $0, {}(%rbp)", offset));
            } else if matches!(elem_type, Type::UShort | Type::Short) {
                self.emit(&format!("    movw $0, {}(%rbp)", offset));
            } else if matches!(elem_type, Type::Char | Type::UChar | Type::Bool) {
                self.emit(&format!("    movb $0, {}(%rbp)", offset));
            } else {
                self.emit(&format!("    movq $0, {}(%rbp)", offset));
            }
        }
        Ok(())
    }

    fn emit_struct_init(
        &mut self,
        struct_name: &str,
        init_fields: &[StructInitField],
        base_offset: i32,
    ) -> Result<(), String> {
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?
            .clone(); // Clone to avoid borrow issues

        // Build ordered field list for positional initialization
        let ordered_fields = layout.fields.clone();

        let mut positional_index = 0;

        for init_field in init_fields {
            // Determine which field to initialize
            let field_info = if let Some(ref field_name) = init_field.field_name {
                // Designated initializer: look up by name
                layout.fields_by_name.get(field_name).ok_or_else(|| {
                    format!("Unknown field '{}' in struct '{}'", field_name, struct_name)
                })?
            } else {
                // Positional initializer: use next field in order
                if positional_index >= ordered_fields.len() {
                    return Err(format!(
                        "Too many initializers for struct '{}' (expected {} fields)",
                        struct_name,
                        ordered_fields.len()
                    ));
                }
                let field = &ordered_fields[positional_index];
                positional_index += 1;
                field
            };

            let dest_offset = base_offset + field_info.offset;

            if field_info.bit_width.is_some() {
                let value_type = self.expr_type(&init_field.value)?;
                self.generate_node(&init_field.value)?;
                self.convert_type(&value_type, &field_info.field_type)?;
                self.emit("    movq %rax, %rdx");
                self.emit(&format!("    leaq {}(%rbp), %rcx", dest_offset));
                self.emit_bitfield_store_from_rdx("%rcx", field_info)?;
                continue;
            }

            // Handle nested struct/union initialization
            match (&field_info.field_type, &init_field.value) {
                (Type::Struct(nested_name), AstNode::StructInit(nested_fields)) => {
                    self.emit_struct_init(nested_name, nested_fields, dest_offset)?;
                }
                (Type::Union(nested_name), AstNode::StructInit(nested_fields)) => {
                    self.emit_union_init(nested_name, nested_fields, dest_offset)?;
                }
                _ => {
                    // Generate code for the value
                    self.generate_node(&init_field.value)?;

                    // Store based on field type
                    match field_info.field_type {
                        Type::Char | Type::UChar | Type::Bool => {
                            self.emit(&format!("    movb %al, {}(%rbp)", dest_offset));
                        }
                        Type::UShort | Type::Short => {
                            self.emit(&format!("    movw %ax, {}(%rbp)", dest_offset));
                        }
                        Type::Int | Type::UInt | Type::Enum(_) => {
                            self.emit(&format!("    movl %eax, {}(%rbp)", dest_offset));
                        }
                        Type::Long | Type::ULong => {
                            self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
                        }
                        Type::Pointer(_) => {
                            self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
                        }
                        _ => {
                            self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn emit_union_init(
        &mut self,
        union_name: &str,
        init_fields: &[StructInitField],
        base_offset: i32,
    ) -> Result<(), String> {
        let layout = self
            .union_layouts
            .get(union_name)
            .ok_or_else(|| format!("Unknown union type: {}", union_name))?
            .clone(); // Clone to avoid borrow issues

        // For unions, we only initialize one field (typically the first one specified)
        // In C, initializers for unions initialize the first member unless designated
        if init_fields.is_empty() {
            return Ok(());
        }

        let init_field = &init_fields[0];

        // Determine which field to initialize
        let field_info = if let Some(ref field_name) = init_field.field_name {
            // Designated initializer: look up by name
            layout.fields_by_name.get(field_name).ok_or_else(|| {
                format!("Unknown field '{}' in union '{}'", field_name, union_name)
            })?
        } else {
            // Positional initializer: use first field in declaration order
            if layout.fields.is_empty() {
                return Err(format!("Union '{}' has no fields", union_name));
            }
            &layout.fields[0]
        };

        let dest_offset = base_offset; // All union fields start at offset 0
        if field_info.bit_width.is_some() {
            let value_type = self.expr_type(&init_field.value)?;
            self.generate_node(&init_field.value)?;
            self.convert_type(&value_type, &field_info.field_type)?;
            self.emit("    movq %rax, %rdx");
            self.emit(&format!("    leaq {}(%rbp), %rcx", dest_offset));
            self.emit_bitfield_store_from_rdx("%rcx", field_info)?;
            return Ok(());
        }

        // Generate code for the value
        self.generate_node(&init_field.value)?;

        // Store based on field type
        match field_info.field_type {
            Type::Char | Type::UChar | Type::Bool => {
                self.emit(&format!("    movb %al, {}(%rbp)", dest_offset));
            }
            Type::UShort | Type::Short => {
                self.emit(&format!("    movw %ax, {}(%rbp)", dest_offset));
            }
            Type::Int | Type::UInt | Type::Enum(_) => {
                self.emit(&format!("    movl %eax, {}(%rbp)", dest_offset));
            }
            Type::Long | Type::ULong => {
                self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
            }
            Type::Pointer(_) => {
                self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
            }
            _ => {
                self.emit(&format!("    movq %rax, {}(%rbp)", dest_offset));
            }
        }

        Ok(())
    }

    fn materialize_compound_literal(
        &mut self,
        name: &str,
        literal_type: &Type,
        init: &AstNode,
    ) -> Result<i32, String> {
        let offset = if let Some(existing) = self.symbol_table.get_variable(name) {
            existing.stack_offset
        } else {
            let size = self.type_size(literal_type)?;
            let align = self.stack_alignment(literal_type)?;
            self.symbol_table.add_variable_with_layout(
                name.to_string(),
                literal_type.clone(),
                size,
                align,
            )?
        };

        self.emit_compound_literal_init(literal_type, init, offset)?;
        Ok(offset)
    }

    fn emit_compound_literal_init(
        &mut self,
        literal_type: &Type,
        init: &AstNode,
        offset: i32,
    ) -> Result<(), String> {
        if literal_type.is_array() {
            match init {
                AstNode::ArrayInit(values) => {
                    self.generate_array_init(literal_type, values, offset)?;
                }
                AstNode::StringLiteral(s) => {
                    self.generate_string_array_init(literal_type, s, offset)?;
                }
                AstNode::WideStringLiteral(s) => {
                    self.generate_wide_string_array_init(literal_type, s, offset)?;
                }
                _ => {
                    return Err(
                        "Array compound literal requires a brace-enclosed initializer".to_string(),
                    );
                }
            }
            return Ok(());
        }

        match literal_type {
            Type::Struct(struct_name) => {
                if let AstNode::StructInit(values) = init {
                    self.emit_struct_init(struct_name, values, offset)?;
                    return Ok(());
                }
            }
            Type::Union(union_name) => {
                if let AstNode::StructInit(values) = init {
                    self.emit_union_init(union_name, values, offset)?;
                    return Ok(());
                }
            }
            _ => {}
        }

        let init_type = if let AstNode::StmtExpr { stmts, result } = init {
            for stmt in stmts {
                if let AstNode::StructDef {
                    name,
                    fields,
                    attributes,
                } = stmt
                {
                    self.register_struct_layout(name, fields, attributes)?;
                } else if let AstNode::UnionDef {
                    name,
                    fields,
                    attributes,
                } = stmt
                {
                    self.register_union_layout(name, fields, attributes)?;
                }
            }
            for stmt in stmts {
                self.generate_node(stmt)?;
            }
            let result_type = self.expr_type(result)?;
            self.generate_node(result)?;
            result_type
        } else {
            let init_type = self.expr_type(init)?;
            self.generate_node(init)?;
            init_type
        };

        self.convert_type(&init_type, literal_type)?;

        if matches!(literal_type, Type::Float) {
            self.emit("    cvtsd2ss %xmm0, %xmm0");
            self.emit(&format!("    movss %xmm0, {}(%rbp)", offset));
        } else if matches!(literal_type, Type::Double) {
            self.emit(&format!("    movsd %xmm0, {}(%rbp)", offset));
        } else if matches!(literal_type, Type::Int | Type::UInt | Type::Enum(_)) {
            self.emit(&format!("    movl %eax, {}(%rbp)", offset));
        } else if matches!(literal_type, Type::UShort | Type::Short) {
            self.emit(&format!("    movw %ax, {}(%rbp)", offset));
        } else if matches!(literal_type, Type::Char | Type::UChar | Type::Bool) {
            self.emit(&format!("    movb %al, {}(%rbp)", offset));
        } else {
            self.emit(&format!("    movq %rax, {}(%rbp)", offset));
        }

        Ok(())
    }

    fn collect_struct_layouts(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::StructDef {
                    name,
                    fields,
                    attributes,
                } = item
                {
                    self.register_struct_layout(name, fields, attributes)?;
                }
            }
        }
        Ok(())
    }

    fn collect_union_layouts(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::UnionDef {
                    name,
                    fields,
                    attributes,
                } = item
                {
                    self.register_union_layout(name, fields, attributes)?;
                }
            }
        }
        Ok(())
    }

    fn collect_global_variables(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::VarDecl {
                    name,
                    var_type,
                    init,
                    is_extern,
                    is_static,
                    alignment,
                    ..
                } = item
                {
                    // If variable already exists and new declaration is extern, skip it
                    // (the existing definition takes precedence)
                    if *is_extern && self.global_variables.contains_key(name) {
                        continue;
                    }

                    self.global_variables.insert(
                        name.clone(),
                        GlobalVariable {
                            var_type: var_type.clone(),
                            init: init.as_ref().map(|n| (**n).clone()),
                            is_extern: *is_extern,
                            is_static: *is_static,
                            alignment: *alignment,
                        },
                    );
                }
            }
        }
        Ok(())
    }

    fn collect_global_compound_literals(&mut self) -> Result<(), String> {
        let mut pending = Vec::new();
        let mut seen = HashSet::new();

        let globals: Vec<GlobalVariable> = self.global_variables.values().cloned().collect();
        for global in globals {
            if let Some(init) = &global.init {
                self.collect_compound_literals_from_expr(init, &mut pending, &mut seen)?;
            }
        }

        for (name, var_type, init) in pending {
            self.global_variables
                .entry(name.clone())
                .or_insert(GlobalVariable {
                    var_type,
                    init: Some(init),
                    is_extern: false,
                    is_static: true,
                    alignment: None,
                });
        }

        Ok(())
    }

    fn collect_compound_literals_from_expr(
        &self,
        node: &AstNode,
        pending: &mut Vec<(String, Type, AstNode)>,
        seen: &mut HashSet<String>,
    ) -> Result<(), String> {
        match node {
            AstNode::CompoundLiteral {
                name,
                literal_type,
                init,
                is_global,
            } => {
                if *is_global && seen.insert(name.clone()) {
                    pending.push((name.clone(), literal_type.clone(), (**init).clone()));
                }
                self.collect_compound_literals_from_expr(init, pending, seen)?;
            }
            AstNode::ArrayInit(exprs) => {
                for expr in exprs {
                    self.collect_compound_literals_from_expr(expr, pending, seen)?;
                }
            }
            AstNode::StructInit(fields) => {
                for field in fields {
                    self.collect_compound_literals_from_expr(&field.value, pending, seen)?;
                }
            }
            AstNode::Assignment { target, value } => {
                self.collect_compound_literals_from_expr(target, pending, seen)?;
                self.collect_compound_literals_from_expr(value, pending, seen)?;
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.collect_compound_literals_from_expr(left, pending, seen)?;
                self.collect_compound_literals_from_expr(right, pending, seen)?;
            }
            AstNode::TernaryOp {
                condition,
                true_expr,
                false_expr,
            } => {
                self.collect_compound_literals_from_expr(condition, pending, seen)?;
                self.collect_compound_literals_from_expr(true_expr, pending, seen)?;
                self.collect_compound_literals_from_expr(false_expr, pending, seen)?;
            }
            AstNode::UnaryOp { operand, .. }
            | AstNode::PrefixIncrement(operand)
            | AstNode::PrefixDecrement(operand)
            | AstNode::PostfixIncrement(operand)
            | AstNode::PostfixDecrement(operand)
            | AstNode::AddressOf(operand)
            | AstNode::Dereference(operand)
            | AstNode::Cast { expr: operand, .. } => {
                self.collect_compound_literals_from_expr(operand, pending, seen)?;
            }
            AstNode::ArrayIndex { array, index } => {
                self.collect_compound_literals_from_expr(array, pending, seen)?;
                self.collect_compound_literals_from_expr(index, pending, seen)?;
            }
            AstNode::MemberAccess { base, .. } => {
                self.collect_compound_literals_from_expr(base, pending, seen)?;
            }
            AstNode::FunctionCall { args, .. } | AstNode::IndirectCall { args, .. } => {
                for arg in args {
                    self.collect_compound_literals_from_expr(arg, pending, seen)?;
                }
            }
            AstNode::StmtExpr { stmts, result } => {
                for stmt in stmts {
                    self.collect_compound_literals_from_expr(stmt, pending, seen)?;
                }
                self.collect_compound_literals_from_expr(result, pending, seen)?;
            }
            AstNode::Return(Some(expr))
            | AstNode::SizeOfExpr(expr)
            | AstNode::AlignOfExpr(expr) => {
                self.collect_compound_literals_from_expr(expr, pending, seen)?;
            }
            AstNode::OffsetOf { .. } => {}
            AstNode::Block(stmts) => {
                for stmt in stmts {
                    self.collect_compound_literals_from_expr(stmt, pending, seen)?;
                }
            }
            AstNode::IfStatement {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_compound_literals_from_expr(condition, pending, seen)?;
                self.collect_compound_literals_from_expr(then_branch, pending, seen)?;
                if let Some(else_branch) = else_branch {
                    self.collect_compound_literals_from_expr(else_branch, pending, seen)?;
                }
            }
            AstNode::WhileLoop { condition, body } => {
                self.collect_compound_literals_from_expr(condition, pending, seen)?;
                self.collect_compound_literals_from_expr(body, pending, seen)?;
            }
            AstNode::DoWhileLoop { body, condition } => {
                self.collect_compound_literals_from_expr(body, pending, seen)?;
                self.collect_compound_literals_from_expr(condition, pending, seen)?;
            }
            AstNode::ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = init {
                    self.collect_compound_literals_from_expr(init, pending, seen)?;
                }
                if let Some(condition) = condition {
                    self.collect_compound_literals_from_expr(condition, pending, seen)?;
                }
                if let Some(increment) = increment {
                    self.collect_compound_literals_from_expr(increment, pending, seen)?;
                }
                self.collect_compound_literals_from_expr(body, pending, seen)?;
            }
            AstNode::SwitchStatement { expr, body } => {
                self.collect_compound_literals_from_expr(expr, pending, seen)?;
                for stmt in body {
                    self.collect_compound_literals_from_expr(stmt, pending, seen)?;
                }
            }
            AstNode::VarDecl {
                init: Some(init), ..
            } => {
                self.collect_compound_literals_from_expr(init, pending, seen)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn collect_function_signatures(&mut self, node: &AstNode) {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::Function {
                    name,
                    return_type,
                    params,
                    ..
                } = item
                {
                    self.function_return_types
                        .insert(name.clone(), return_type.clone());

                    // Collect parameter types
                    let param_types: Vec<Type> = params
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect();
                    self.function_param_types.insert(name.clone(), param_types);
                }
            }
        }
    }

    fn collect_static_locals(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::Function { body, .. } = item
                    && let Some(body_node) = body
                {
                    self.collect_static_locals_from_block(body_node)?;
                }
            }
        }
        Ok(())
    }

    fn collect_static_locals_from_block(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Block(stmts) => {
                for stmt in stmts {
                    self.collect_static_locals_from_block(stmt)?;
                }
            }
            AstNode::VarDecl {
                name,
                var_type,
                init,
                is_extern: _,
                is_static,
                alignment,
                ..
            } => {
                if *is_static {
                    self.global_variables.insert(
                        name.clone(),
                        GlobalVariable {
                            var_type: var_type.clone(),
                            init: init.as_ref().map(|n| (**n).clone()),
                            is_extern: false,
                            is_static: true,
                            alignment: *alignment,
                        },
                    );
                }
            }
            AstNode::IfStatement {
                then_branch,
                else_branch,
                ..
            } => {
                self.collect_static_locals_from_block(then_branch)?;
                if let Some(else_b) = else_branch {
                    self.collect_static_locals_from_block(else_b)?;
                }
            }
            AstNode::WhileLoop { body, .. } => {
                self.collect_static_locals_from_block(body)?;
            }
            AstNode::DoWhileLoop { body, .. } => {
                self.collect_static_locals_from_block(body)?;
            }
            AstNode::ForLoop { init, body, .. } => {
                if let Some(init_stmt) = init {
                    self.collect_static_locals_from_block(init_stmt)?;
                }
                self.collect_static_locals_from_block(body)?;
            }
            AstNode::SwitchStatement { body, .. } => {
                for stmt in body {
                    self.collect_static_locals_from_block(stmt)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn collect_enum_constants(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Program(nodes) => {
                for item in nodes {
                    self.collect_enum_constants(item)?;
                }
            }
            AstNode::EnumDef { enumerators, .. } => {
                for enumerator in enumerators {
                    if let Some(value) = enumerator.value {
                        self.enum_constants.insert(enumerator.name.clone(), value);
                    }
                }
            }
            AstNode::Function {
                body: Some(body_node),
                ..
            } => {
                self.collect_enum_constants(body_node)?;
            }
            AstNode::Block(stmts) => {
                for stmt in stmts {
                    self.collect_enum_constants(stmt)?;
                }
            }
            AstNode::StmtExpr { stmts, result } => {
                for stmt in stmts {
                    self.collect_enum_constants(stmt)?;
                }
                self.collect_enum_constants(result)?;
            }
            // Recursively walk all other node types that contain expressions
            AstNode::Return(Some(expr)) => self.collect_enum_constants(expr)?,
            AstNode::IfStatement {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_enum_constants(condition)?;
                self.collect_enum_constants(then_branch)?;
                if let Some(e) = else_branch {
                    self.collect_enum_constants(e)?;
                }
            }
            AstNode::WhileLoop { condition, body } => {
                self.collect_enum_constants(condition)?;
                self.collect_enum_constants(body)?;
            }
            AstNode::DoWhileLoop { body, condition } => {
                self.collect_enum_constants(body)?;
                self.collect_enum_constants(condition)?;
            }
            AstNode::ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(i) = init {
                    self.collect_enum_constants(i)?;
                }
                if let Some(c) = condition {
                    self.collect_enum_constants(c)?;
                }
                if let Some(inc) = increment {
                    self.collect_enum_constants(inc)?;
                }
                self.collect_enum_constants(body)?;
            }
            AstNode::VarDecl {
                init: Some(init), ..
            } => {
                self.collect_enum_constants(init)?;
            }
            AstNode::Assignment { target, value } => {
                self.collect_enum_constants(target)?;
                self.collect_enum_constants(value)?;
            }
            AstNode::FunctionCall { args, .. } | AstNode::IndirectCall { args, .. } => {
                for arg in args {
                    self.collect_enum_constants(arg)?;
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.collect_enum_constants(left)?;
                self.collect_enum_constants(right)?;
            }
            AstNode::TernaryOp {
                condition,
                true_expr,
                false_expr,
            } => {
                self.collect_enum_constants(condition)?;
                self.collect_enum_constants(true_expr)?;
                self.collect_enum_constants(false_expr)?;
            }
            AstNode::UnaryOp { operand, .. } => {
                self.collect_enum_constants(operand)?;
            }
            AstNode::PrefixIncrement(e)
            | AstNode::PrefixDecrement(e)
            | AstNode::PostfixIncrement(e)
            | AstNode::PostfixDecrement(e)
            | AstNode::AddressOf(e)
            | AstNode::Dereference(e) => {
                self.collect_enum_constants(e)?;
            }
            AstNode::ArrayIndex { array, index } => {
                self.collect_enum_constants(array)?;
                self.collect_enum_constants(index)?;
            }
            AstNode::ArrayInit(exprs) => {
                for expr in exprs {
                    self.collect_enum_constants(expr)?;
                }
            }
            AstNode::StructInit(fields) => {
                for field in fields {
                    self.collect_enum_constants(&field.value)?;
                }
            }
            AstNode::CompoundLiteral { init, .. } => {
                self.collect_enum_constants(init)?;
            }
            AstNode::MemberAccess { base, .. } => {
                self.collect_enum_constants(base)?;
            }
            AstNode::Cast { expr, .. } => {
                self.collect_enum_constants(expr)?;
            }
            AstNode::SwitchStatement { expr, body } => {
                self.collect_enum_constants(expr)?;
                for stmt in body {
                    self.collect_enum_constants(stmt)?;
                }
            }
            AstNode::SizeOfExpr(e) | AstNode::AlignOfExpr(e) => {
                self.collect_enum_constants(e)?;
            }
            _ => {
                // Literals, variables, labels, and other leaf nodes
            }
        }
        Ok(())
    }

    fn resolve_struct_or_union_base(
        &mut self,
        base: &AstNode,
        through_pointer: bool,
    ) -> Result<(String, bool), String> {
        let base_type = self.expr_type(base)?;
        match (base_type, through_pointer) {
            (Type::Struct(name), false) | (Type::Union(name), false) => {
                self.generate_lvalue(base)?;
                Ok((name, true))
            }
            (Type::Pointer(pointee), true) => match pointee.as_ref() {
                Type::Struct(name) | Type::Union(name) => {
                    self.generate_node(base)?;
                    Ok((name.clone(), true))
                }
                _ => Err("Pointer does not target a struct/union".to_string()),
            },
            _ => Err("Member access base is not a struct/union".to_string()),
        }
    }

    fn member_access_field_info(
        &self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
    ) -> Result<StructFieldInfo, String> {
        let base_type = self.expr_type(base)?;
        let struct_or_union_name = match (base_type, through_pointer) {
            (Type::Struct(name), false) | (Type::Union(name), false) => name,
            (Type::Pointer(pointee), true) => match *pointee {
                Type::Struct(name) | Type::Union(name) => name,
                _ => return Err("Pointer does not target a struct/union".to_string()),
            },
            _ => return Err("Member access base is not a struct/union".to_string()),
        };

        let layout = self
            .struct_layouts
            .get(&struct_or_union_name)
            .or_else(|| self.union_layouts.get(&struct_or_union_name))
            .ok_or_else(|| format!("Unknown struct/union type: {}", struct_or_union_name))?;

        let field = layout.fields_by_name.get(member).ok_or_else(|| {
            format!(
                "Unknown field '{}' on struct/union {}",
                member, struct_or_union_name
            )
        })?;
        Ok(field.clone())
    }

    fn member_access_type(
        &self,
        base: &AstNode,
        member: &str,
        through_pointer: bool,
    ) -> Result<Type, String> {
        let field_info = self.member_access_field_info(base, member, through_pointer)?;
        Ok(field_info.field_type.clone())
    }

    fn expr_type(&self, expr: &AstNode) -> Result<Type, String> {
        match expr {
            AstNode::IntLiteral(_) => Ok(Type::Int),
            AstNode::FloatLiteral(_) => Ok(Type::Double),
            AstNode::CharLiteral(_) => Ok(Type::Char),
            AstNode::Variable(name) => {
                // Check if it's a global variable first
                if let Some(global) = self.global_variables.get(name) {
                    return self.resolve_type(&global.var_type);
                }

                // Check if it's a local variable
                if let Some(symbol) = self.symbol_table.get_variable(name) {
                    return self.resolve_type(&symbol.symbol_type);
                }

                // Assume it's a function name - return a generic function pointer type
                // Use a known return type if we have a declaration.
                let return_type = self
                    .function_return_types
                    .get(name)
                    .cloned()
                    .unwrap_or(Type::Int);
                Ok(Type::FunctionPointer {
                    return_type: Box::new(return_type),
                    param_types: vec![],
                    is_variadic: false,
                })
            }
            AstNode::AddressOf(inner) => {
                if let AstNode::Variable(name) = &**inner {
                    if let Some(global) = self.global_variables.get(name) {
                        let resolved = self.resolve_type(&global.var_type)?;
                        return Ok(Type::Pointer(Box::new(resolved)));
                    }
                    if let Some(symbol) = self.symbol_table.get_variable(name) {
                        let resolved = self.resolve_type(&symbol.symbol_type)?;
                        return Ok(Type::Pointer(Box::new(resolved)));
                    }

                    let return_type = self
                        .function_return_types
                        .get(name)
                        .cloned()
                        .unwrap_or(Type::Int);
                    return Ok(Type::FunctionPointer {
                        return_type: Box::new(return_type),
                        param_types: vec![],
                        is_variadic: false,
                    });
                }

                let inner_type = self.expr_type(inner)?;
                if matches!(inner.as_ref(), AstNode::Dereference(_))
                    && matches!(inner_type, Type::FunctionPointer { .. })
                {
                    return Ok(inner_type);
                }

                Ok(Type::Pointer(Box::new(inner_type)))
            }
            AstNode::Dereference(inner) => {
                let inner_type = self.expr_type(inner)?;
                match inner_type {
                    Type::Pointer(pointee) => Ok(*pointee),
                    Type::Array(elem, _) => Ok(*elem), // Arrays decay to pointers
                    Type::FunctionPointer { .. } => Ok(inner_type),
                    _ => Err("Cannot dereference non-pointer type".to_string()),
                }
            }
            AstNode::ArrayIndex { array, index } => {
                let array_type = self.expr_type(array)?;
                match array_type {
                    Type::Array(elem, _) => Ok(*elem),
                    Type::Pointer(pointee) => Ok(*pointee),
                    _ => {
                        // Try reverse indexing: index[array] (e.g., 2[x])
                        let index_type = self.expr_type(index)?;
                        match index_type {
                            Type::Array(elem, _) => Ok(*elem),
                            Type::Pointer(pointee) => Ok(*pointee),
                            _ => Err("Cannot index non-array type".to_string()),
                        }
                    }
                }
            }
            AstNode::MemberAccess {
                base,
                member,
                through_pointer,
            } => self.member_access_type(base, member, *through_pointer),
            AstNode::BinaryOp { op, left, right } => {
                let mut left_type = self.expr_type(left)?;
                let mut right_type = self.expr_type(right)?;

                // Handle array to pointer decay
                if let Type::Array(elem, _) = left_type {
                    left_type = Type::Pointer(elem);
                }
                if let Type::Array(elem, _) = right_type {
                    right_type = Type::Pointer(elem);
                }

                let integer_type =
                    if self.is_integer_type(&left_type) && self.is_integer_type(&right_type) {
                        Some(self.binary_integer_type(&left_type, &right_type)?)
                    } else {
                        None
                    };
                match op {
                    BinOp::Add => {
                        if matches!(left_type, Type::Pointer(_))
                            || matches!(right_type, Type::Pointer(_))
                        {
                            match (left_type.clone(), right_type.clone()) {
                                (Type::Pointer(pointee), Type::Int) => Ok(Type::Pointer(pointee)),
                                (Type::Int, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::UInt) => Ok(Type::Pointer(pointee)),
                                (Type::UInt, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::UShort) => {
                                    Ok(Type::Pointer(pointee))
                                }
                                (Type::UShort, Type::Pointer(pointee)) => {
                                    Ok(Type::Pointer(pointee))
                                }
                                (Type::Pointer(pointee), Type::Short) => Ok(Type::Pointer(pointee)),
                                (Type::Short, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::UChar) => Ok(Type::Pointer(pointee)),
                                (Type::UChar, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::Bool) => Ok(Type::Pointer(pointee)),
                                (Type::Bool, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::Char) => Ok(Type::Pointer(pointee)),
                                (Type::Char, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::ULong) => Ok(Type::Pointer(pointee)),
                                (Type::ULong, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                (Type::Pointer(pointee), Type::Long) => Ok(Type::Pointer(pointee)),
                                (Type::Long, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                                _ => {
                                    eprintln!(
                                        "DEBUG: Invalid pointer addition - left: {:?}, right: {:?}",
                                        left_type, right_type
                                    );
                                    Err("Invalid operands for pointer addition".to_string())
                                }
                            }
                        } else if self.is_float_type(&left_type) || self.is_float_type(&right_type)
                        {
                            Ok(Type::Double)
                        } else {
                            if integer_type.is_none() {
                                eprintln!(
                                    "DEBUG: No integer type - left: {:?}, right: {:?}",
                                    left_type, right_type
                                );
                            }
                            integer_type
                                .clone()
                                .ok_or_else(|| "Invalid operands for pointer addition".to_string())
                        }
                    }
                    BinOp::Subtract => {
                        if matches!(left_type, Type::Pointer(_))
                            || matches!(right_type, Type::Pointer(_))
                        {
                            match (&left_type, &right_type) {
                                (Type::Pointer(pointee), Type::Int) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::UInt) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::UShort) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::UChar) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::Bool) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::Char) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::ULong) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::Short) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(pointee), Type::Long) => {
                                    Ok(Type::Pointer(pointee.clone()))
                                }
                                (Type::Pointer(_), Type::Pointer(_)) => Ok(Type::Int),
                                _ => Err(format!(
                                    "Invalid operands for pointer subtraction: {:?} - {:?}",
                                    left_type, right_type
                                )),
                            }
                        } else if self.is_float_type(&left_type) || self.is_float_type(&right_type)
                        {
                            Ok(Type::Double)
                        } else {
                            integer_type.ok_or_else(|| {
                                format!("Invalid operands for subtraction (not pointers or floats): {:?} - {:?}", left_type, right_type)
                            })
                        }
                    }
                    BinOp::ShiftLeft | BinOp::ShiftRight => {
                        if !self.is_integer_type(&left_type) || !self.is_integer_type(&right_type) {
                            return Err("Shift operands must be integers".to_string());
                        }
                        Ok(self.promote_integer_type(&left_type))
                    }
                    BinOp::Multiply | BinOp::Divide => {
                        if self.is_float_type(&left_type) || self.is_float_type(&right_type) {
                            Ok(Type::Double)
                        } else {
                            integer_type.ok_or_else(|| {
                                "Bitwise and arithmetic operators require integer operands"
                                    .to_string()
                            })
                        }
                    }
                    BinOp::Modulo | BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => integer_type
                        .ok_or_else(|| {
                            "Bitwise and arithmetic operators require integer operands".to_string()
                        }),
                    BinOp::Less
                    | BinOp::Greater
                    | BinOp::LessEqual
                    | BinOp::GreaterEqual
                    | BinOp::EqualEqual
                    | BinOp::NotEqual
                    | BinOp::LogicalAnd
                    | BinOp::LogicalOr => Ok(Type::Int),
                    BinOp::Comma => {
                        // Comma operator returns the type of the right expression
                        self.expr_type(right)
                    }
                }
            }
            AstNode::TernaryOp {
                true_expr,
                false_expr,
                ..
            } => {
                let true_type = self.expr_type(true_expr)?;
                let false_type = self.expr_type(false_expr)?;

                if true_type == false_type {
                    Ok(true_type)
                } else if self.is_integer_type(&true_type) && self.is_integer_type(&false_type) {
                    self.binary_integer_type(&true_type, &false_type)
                } else {
                    Ok(Type::Int)
                }
            }
            AstNode::Assignment { target, .. } => self.expr_type(target),
            AstNode::UnaryOp { op, operand } => match op {
                UnaryOp::LogicalNot => Ok(Type::Int),
                UnaryOp::Plus => {
                    let operand_type = self.expr_type(operand)?;
                    if self.is_integer_type(&operand_type) {
                        Ok(self.promote_integer_type(&operand_type))
                    } else {
                        Ok(operand_type)
                    }
                }
                UnaryOp::Negate | UnaryOp::BitNot => {
                    let operand_type = self.expr_type(operand)?;
                    Ok(self.promote_integer_type(&operand_type))
                }
            },
            AstNode::PrefixIncrement(operand)
            | AstNode::PrefixDecrement(operand)
            | AstNode::PostfixIncrement(operand)
            | AstNode::PostfixDecrement(operand) => self.expr_type(operand),
            AstNode::FunctionCall { name, .. } => {
                // Handle builtin functions
                if name == "alloca" {
                    return Ok(Type::Pointer(Box::new(Type::Void)));
                }

                if let Some(symbol) = self.symbol_table.get_variable(name)
                    && let Type::FunctionPointer { return_type, .. } = &symbol.symbol_type
                {
                    return Ok(*return_type.clone());
                }
                if let Some(global) = self.global_variables.get(name)
                    && let Type::FunctionPointer { return_type, .. } = &global.var_type
                {
                    return Ok(*return_type.clone());
                }
                if let Some(return_type) = self.function_return_types.get(name) {
                    Ok(return_type.clone())
                } else {
                    Ok(Type::Int)
                }
            }
            AstNode::IndirectCall { target, .. } => match self.expr_type(target) {
                Ok(Type::FunctionPointer { return_type, .. }) => Ok(*return_type),
                _ => Ok(Type::Int),
            },
            AstNode::StringLiteral(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            AstNode::WideStringLiteral(_) => Ok(Type::Pointer(Box::new(Type::Int))),
            AstNode::CompoundLiteral { literal_type, .. } => self.resolve_type(literal_type),
            AstNode::Cast { target_type, .. } => self.resolve_type(target_type),
            AstNode::BuiltinTypesCompatible { .. } => Ok(Type::Int),
            AstNode::StmtExpr { result, .. } => self.expr_type(result),
            AstNode::OffsetOf { .. } => Ok(Type::ULong), // offsetof returns size_t (unsigned long)
            AstNode::SizeOfType(_) | AstNode::SizeOfExpr(_) => Ok(Type::ULong),
            AstNode::AlignOfType(_) | AstNode::AlignOfExpr(_) => Ok(Type::ULong),
            _ => Err("Unsupported expression in sizeof".to_string()),
        }
    }

    fn coerce_rax_to_type(&mut self, ty: &Type) {
        match ty {
            Type::Char => self.emit("    movsbq %al, %rax"),
            Type::UChar | Type::Bool => self.emit("    movzbq %al, %rax"),
            Type::Short => self.emit("    movswq %ax, %rax"),
            Type::UShort => self.emit("    movzwq %ax, %rax"),
            Type::Int | Type::Enum(_) => self.emit("    cltq"),
            Type::UInt => self.emit("    movl %eax, %eax"),
            Type::Long | Type::ULong => {}
            _ => {}
        }
    }

    fn emit(&mut self, line: &str) {
        self.output.push_str(line);
        self.output.push('\n');
    }

    /// Pops into the specified register (e.g., "%rax" or "%rcx")
    fn emit_pop(&mut self, register: &str) {
        self.emit(&format!("    popq {}", register));
        self.stack_depth -= 1;
    }

    fn const_int_value(&self, expr: &AstNode) -> Result<i64, String> {
        match expr {
            AstNode::IntLiteral(n) => Ok(*n),
            AstNode::CharLiteral(n) => Ok(*n),
            AstNode::Variable(name) => {
                self.enum_constants.get(name).copied().ok_or_else(|| {
                    format!("Unsupported identifier in global initializer: {}", name)
                })
            }
            _ => Err("Unsupported global initializer expression".to_string()),
        }
    }

    fn write_int_bytes(
        &self,
        buffer: &mut [u8],
        offset: usize,
        value: i64,
        size: usize,
    ) -> Result<(), String> {
        if offset + size > buffer.len() {
            return Err("Initializer write out of bounds".to_string());
        }
        let raw = value as u64;
        for i in 0..size {
            buffer[offset + i] = (raw >> (i * 8)) as u8;
        }
        Ok(())
    }

    fn apply_bitfield_to_buffer(
        &self,
        buffer: &mut [u8],
        base_offset: usize,
        field: &StructFieldInfo,
        value: i64,
    ) -> Result<(), String> {
        let width = field
            .bit_width
            .ok_or_else(|| "Expected bit-field metadata".to_string())?;
        if width == 0 {
            return Ok(());
        }
        let unit_size = field.bit_unit_size as usize;
        let unit_offset = base_offset + field.offset as usize;
        if unit_offset + unit_size > buffer.len() {
            return Err("Bit-field initializer out of bounds".to_string());
        }
        let unit_bits = (unit_size as u32) * 8;
        if width > unit_bits {
            return Err("Bit-field width exceeds storage unit".to_string());
        }

        let mask = if width == 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };
        let truncated = (value as u64) & mask;

        let mut unit_value = 0u64;
        for i in 0..unit_size {
            unit_value |= (buffer[unit_offset + i] as u64) << (i * 8);
        }
        let shifted_mask = mask << field.bit_offset;
        unit_value = (unit_value & !shifted_mask) | (truncated << field.bit_offset);
        for i in 0..unit_size {
            buffer[unit_offset + i] = ((unit_value >> (i * 8)) & 0xff) as u8;
        }
        Ok(())
    }

    fn apply_struct_init_to_buffer(
        &self,
        struct_name: &str,
        init_fields: &[StructInitField],
        buffer: &mut [u8],
        base_offset: usize,
    ) -> Result<(), String> {
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?;

        let mut positional_index = 0;
        for init_field in init_fields {
            let field_info = if let Some(ref field_name) = init_field.field_name {
                layout.fields_by_name.get(field_name).ok_or_else(|| {
                    format!("Unknown field '{}' in struct '{}'", field_name, struct_name)
                })?
            } else {
                if positional_index >= layout.fields.len() {
                    return Err(format!(
                        "Too many initializers for struct '{}' (expected {} fields)",
                        struct_name,
                        layout.fields.len()
                    ));
                }
                let field = &layout.fields[positional_index];
                positional_index += 1;
                field
            };

            let field_offset = base_offset + field_info.offset as usize;
            if field_info.bit_width.is_some() {
                let value = self.const_int_value(&init_field.value)?;
                self.apply_bitfield_to_buffer(buffer, base_offset, field_info, value)?;
                continue;
            }

            match (&field_info.field_type, &init_field.value) {
                (Type::Struct(nested_name), AstNode::StructInit(nested_fields)) => {
                    self.apply_struct_init_to_buffer(
                        nested_name,
                        nested_fields,
                        buffer,
                        field_offset,
                    )?;
                }
                (Type::Union(nested_name), AstNode::StructInit(nested_fields)) => {
                    self.apply_union_init_to_buffer(
                        nested_name,
                        nested_fields,
                        buffer,
                        field_offset,
                    )?;
                }
                _ => {
                    let value = self.const_int_value(&init_field.value)?;
                    let size = self.type_size(&field_info.field_type)? as usize;
                    self.write_int_bytes(buffer, field_offset, value, size)?;
                }
            }
        }

        Ok(())
    }

    fn apply_union_init_to_buffer(
        &self,
        union_name: &str,
        init_fields: &[StructInitField],
        buffer: &mut [u8],
        base_offset: usize,
    ) -> Result<(), String> {
        let layout = self
            .union_layouts
            .get(union_name)
            .ok_or_else(|| format!("Unknown union type: {}", union_name))?;
        if init_fields.is_empty() {
            return Ok(());
        }

        let init_field = &init_fields[0];
        let field_info = if let Some(ref field_name) = init_field.field_name {
            layout.fields_by_name.get(field_name).ok_or_else(|| {
                format!("Unknown field '{}' in union '{}'", field_name, union_name)
            })?
        } else {
            if layout.fields.is_empty() {
                return Err(format!("Union '{}' has no fields", union_name));
            }
            &layout.fields[0]
        };

        if field_info.bit_width.is_some() {
            let value = self.const_int_value(&init_field.value)?;
            self.apply_bitfield_to_buffer(buffer, base_offset, field_info, value)?;
            return Ok(());
        }

        match (&field_info.field_type, &init_field.value) {
            (Type::Struct(nested_name), AstNode::StructInit(nested_fields)) => {
                self.apply_struct_init_to_buffer(nested_name, nested_fields, buffer, base_offset)?;
            }
            (Type::Union(nested_name), AstNode::StructInit(nested_fields)) => {
                self.apply_union_init_to_buffer(nested_name, nested_fields, buffer, base_offset)?;
            }
            _ => {
                let value = self.const_int_value(&init_field.value)?;
                let size = self.type_size(&field_info.field_type)? as usize;
                self.write_int_bytes(buffer, base_offset, value, size)?;
            }
        }

        Ok(())
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == 0 {
                let mut j = i + 1;
                while j < bytes.len() && bytes[j] == 0 {
                    j += 1;
                }
                self.emit(&format!("    .zero {}", j - i));
                i = j;
            } else {
                self.emit(&format!("    .byte {}", bytes[i]));
                i += 1;
            }
        }
    }

    fn emit_global_variables(&mut self) -> Result<(), String> {
        if self.global_variables.is_empty() {
            return Ok(());
        }

        // Clone global variables to avoid borrow checker issues
        let globals: Vec<(String, GlobalVariable)> = self
            .global_variables
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        // Separate globals into initialized and uninitialized
        let mut initialized = Vec::new();
        let mut uninitialized = Vec::new();

        for (name, global) in &globals {
            // Skip extern variables - they're declared elsewhere
            if global.is_extern {
                continue;
            }

            if global.init.is_some() {
                initialized.push((name.clone(), global.clone()));
            } else {
                uninitialized.push((name.clone(), global.clone()));
            }
        }

        // Emit .bss section for uninitialized globals
        if !uninitialized.is_empty() {
            self.emit("    .bss");
            for (name, global) in &uninitialized {
                let size = self.type_size(&global.var_type)?;
                // Use explicit alignment from _Alignas if specified, otherwise use type's natural alignment
                let align = global
                    .alignment
                    .map(|a| a as i32)
                    .unwrap_or_else(|| self.type_alignment(&global.var_type).unwrap_or(1));
                self.emit(&format!("    .align {}", align));
                // Only emit .globl for non-static variables (static has internal linkage)
                if !global.is_static {
                    self.emit(&format!("    .globl {}", name));
                }
                self.emit(&format!("{}:", name));
                self.emit(&format!("    .zero {}", size));
            }
        }

        // Emit .data section for initialized globals
        if !initialized.is_empty() {
            self.emit("    .data");
            for (name, global) in &initialized {
                // Use explicit alignment from _Alignas if specified, otherwise use type's natural alignment
                let align = global
                    .alignment
                    .map(|a| a as i32)
                    .unwrap_or_else(|| self.type_alignment(&global.var_type).unwrap_or(1));
                self.emit(&format!("    .align {}", align));
                // Only emit .globl for non-static variables (static has internal linkage)
                if !global.is_static {
                    self.emit(&format!("    .globl {}", name));
                }
                self.emit(&format!("{}:", name));

                // Emit the initialization value
                if let Some(init) = &global.init {
                    self.emit_global_initializer(&global.var_type, init)?;
                }
            }
        }

        Ok(())
    }

    fn emit_global_initializer(&mut self, var_type: &Type, init: &AstNode) -> Result<(), String> {
        match init {
            AstNode::IntLiteral(n) | AstNode::CharLiteral(n) => {
                // Emit the appropriate directive based on type size
                match var_type {
                    Type::Char | Type::UChar | Type::Bool => {
                        self.emit(&format!("    .byte {}", n));
                    }
                    Type::Short | Type::UShort => {
                        self.emit(&format!("    .short {}", n));
                    }
                    Type::Int | Type::UInt | Type::Enum(_) => {
                        self.emit(&format!("    .long {}", n));
                    }
                    Type::Long | Type::ULong | Type::Pointer(_) | Type::FunctionPointer { .. } => {
                        self.emit(&format!("    .quad {}", n));
                    }
                    _ => {
                        self.emit(&format!("    .long {}", n));
                    }
                }
                Ok(())
            }
            AstNode::AddressOf(inner) => match (var_type, inner.as_ref()) {
                (Type::Pointer(_) | Type::FunctionPointer { .. }, AstNode::Variable(name)) => {
                    self.emit(&format!("    .quad {}", name));
                    Ok(())
                }
                (
                    Type::Pointer(_) | Type::FunctionPointer { .. },
                    AstNode::CompoundLiteral { name, .. },
                ) => {
                    self.emit(&format!("    .quad {}", name));
                    Ok(())
                }
                _ => Err("Unsupported global address initializer".to_string()),
            },
            AstNode::StringLiteral(s) => {
                // For string literals, emit the label reference
                let label = self.add_string_literal(s.clone());
                self.emit(&format!("    .quad {}", label));
                Ok(())
            }
            AstNode::WideStringLiteral(s) => {
                let label = self.add_wide_string_literal(s.clone());
                self.emit(&format!("    .quad {}", label));
                Ok(())
            }
            AstNode::ArrayInit(values) => {
                // Emit array initializer
                if let Type::Array(elem_type, _) = var_type {
                    for value in values {
                        self.emit_global_initializer(elem_type, value)?;
                    }
                    Ok(())
                } else {
                    Err("Array initializer for non-array type".to_string())
                }
            }
            AstNode::StructInit(values) => match var_type {
                Type::Struct(struct_name) => self.emit_global_struct_init(struct_name, values),
                Type::Union(union_name) => self.emit_global_union_init(union_name, values),
                _ => Err("Struct initializer for non-struct type".to_string()),
            },
            _ => Err(format!("Unsupported global initializer: {:?}", init)),
        }
    }

    fn emit_global_struct_init(
        &mut self,
        struct_name: &str,
        init_fields: &[StructInitField],
    ) -> Result<(), String> {
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| format!("Unknown struct type: {}", struct_name))?
            .clone();

        if layout.fields.iter().any(|field| field.bit_width.is_some()) {
            let size = layout.size as usize;
            let mut buffer = vec![0u8; size];
            self.apply_struct_init_to_buffer(struct_name, init_fields, &mut buffer, 0)?;
            self.emit_bytes(&buffer);
            return Ok(());
        }

        let ordered_fields = layout.fields.clone();
        let mut positional_index = 0;
        let mut init_by_offset: HashMap<i32, &AstNode> = HashMap::new();

        for init_field in init_fields {
            let field_info = if let Some(ref field_name) = init_field.field_name {
                layout.fields_by_name.get(field_name).ok_or_else(|| {
                    format!("Unknown field '{}' in struct '{}'", field_name, struct_name)
                })?
            } else {
                if positional_index >= ordered_fields.len() {
                    return Err(format!(
                        "Too many initializers for struct '{}' (expected {} fields)",
                        struct_name,
                        ordered_fields.len()
                    ));
                }
                let field = &ordered_fields[positional_index];
                positional_index += 1;
                field
            };

            init_by_offset.insert(field_info.offset, &init_field.value);
        }

        let mut current_offset = 0usize;
        for field in &ordered_fields {
            let field_offset = field.offset as usize;
            if field_offset > current_offset {
                self.emit(&format!(
                    "    .zero {}",
                    field_offset.saturating_sub(current_offset)
                ));
            }

            if let Some(value) = init_by_offset.get(&field.offset) {
                self.emit_global_initializer(&field.field_type, value)?;
            } else {
                self.emit_zero_bytes(self.type_size(&field.field_type)? as usize);
            }

            current_offset = field_offset + self.type_size(&field.field_type)? as usize;
        }

        if current_offset < layout.size as usize {
            self.emit(&format!(
                "    .zero {}",
                (layout.size as usize).saturating_sub(current_offset)
            ));
        }

        Ok(())
    }

    fn emit_global_union_init(
        &mut self,
        union_name: &str,
        init_fields: &[StructInitField],
    ) -> Result<(), String> {
        let layout = self
            .union_layouts
            .get(union_name)
            .ok_or_else(|| format!("Unknown union type: {}", union_name))?
            .clone();

        if layout.fields.iter().any(|field| field.bit_width.is_some()) {
            let size = layout.size as usize;
            let mut buffer = vec![0u8; size];
            self.apply_union_init_to_buffer(union_name, init_fields, &mut buffer, 0)?;
            self.emit_bytes(&buffer);
            return Ok(());
        }

        if init_fields.is_empty() {
            self.emit_zero_bytes(layout.size as usize);
            return Ok(());
        }

        let init_field = &init_fields[0];
        let field_info = if let Some(ref field_name) = init_field.field_name {
            layout.fields_by_name.get(field_name).ok_or_else(|| {
                format!("Unknown field '{}' in union '{}'", field_name, union_name)
            })?
        } else {
            layout
                .fields
                .first()
                .ok_or_else(|| format!("Union '{}' has no fields", union_name))?
        };

        self.emit_global_initializer(&field_info.field_type, &init_field.value)?;

        let field_size = self.type_size(&field_info.field_type)? as usize;
        if field_size < layout.size as usize {
            self.emit(&format!(
                "    .zero {}",
                (layout.size as usize).saturating_sub(field_size)
            ));
        }

        Ok(())
    }

    fn emit_zero_bytes(&mut self, size: usize) {
        if size > 0 {
            self.emit(&format!("    .zero {}", size));
        }
    }

    fn add_float_literal(&mut self, value: f64) -> String {
        // Check if this exact value already exists
        for (label, v) in &self.float_literals {
            if *v == value {
                return label.clone();
            }
        }

        // Create a new label for this float
        let label = format!(".LF{}", self.float_literals.len());
        self.float_literals.push((label.clone(), value));
        label
    }

    fn add_string_literal(&mut self, s: String) -> String {
        // Check if this exact string already exists
        for (label, content) in &self.string_literals {
            if content == &s {
                return label.clone();
            }
        }

        // Create a new label for this string
        let label = format!(".LC{}", self.string_literals.len());
        self.string_literals.push((label.clone(), s));
        label
    }

    fn add_wide_string_literal(&mut self, s: String) -> String {
        for (label, content) in &self.wide_string_literals {
            if content == &s {
                return label.clone();
            }
        }

        let label = format!(".LCW{}", self.wide_string_literals.len());
        self.wide_string_literals.push((label.clone(), s));
        label
    }

    fn emit_string_literals(&mut self) {
        if self.string_literals.is_empty() {
            return;
        }

        // Clone the string literals to avoid borrow checker issues
        let literals = self.string_literals.clone();

        // Emit .rodata section with all string literals
        self.emit("    .section .rodata");
        for (label, content) in &literals {
            self.emit(&format!("{}:", label));
            // Emit the string as a .string directive
            // Escape special characters for assembly
            let escaped = self.escape_string_for_asm(content);
            self.emit(&format!("    .string \"{}\"", escaped));
        }
    }

    fn emit_wide_string_literals(&mut self) {
        if self.wide_string_literals.is_empty() {
            return;
        }

        let literals = self.wide_string_literals.clone();
        self.emit("    .section .rodata");
        for (label, content) in &literals {
            self.emit("    .align 4");
            self.emit(&format!("{}:", label));
            for ch in content.chars() {
                self.emit(&format!("    .long {}", ch as u32));
            }
            self.emit("    .long 0");
        }
    }

    fn emit_float_literals(&mut self) {
        if self.float_literals.is_empty() {
            return;
        }

        // Clone the float literals to avoid borrow checker issues
        let literals = self.float_literals.clone();

        // Emit .rodata section with all float literals
        if !literals.is_empty() {
            self.emit("    .section .rodata");
            for (label, value) in &literals {
                self.emit("    .align 8");
                self.emit(&format!("{}:", label));
                // Emit the exact IEEE-754 bits to avoid assembler parsing issues.
                self.emit(&format!("    .quad 0x{:016x}", value.to_bits()));
            }
        }
    }

    fn escape_string_for_asm(&self, s: &str) -> String {
        let mut result = String::new();
        for ch in s.chars() {
            match ch {
                '\n' => result.push_str("\\n"),
                '\t' => result.push_str("\\t"),
                '\r' => result.push_str("\\r"),
                '\\' => result.push_str("\\\\"),
                '"' => result.push_str("\\\""),
                '\0' => result.push_str("\\0"),
                _ => result.push(ch),
            }
        }
        result
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
