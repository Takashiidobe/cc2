use crate::ast::*;
use crate::symbol_table::SymbolTable;
use std::collections::HashMap;

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
    is_const: bool,
    is_volatile: bool,
}

pub struct CodeGenerator {
    output: String,
    symbol_table: SymbolTable,
    label_counter: usize,
    current_function_end_label: Option<String>,
    current_reg_save_area_offset: Option<i32>, // Offset of register save area for variadic functions
    current_gp_params: usize,                   // Number of GP parameters for current function
    current_fp_params: usize,                   // Number of FP parameters for current function
    loop_stack: Vec<LoopContext>,
    struct_layouts: HashMap<String, StructLayout>,
    union_layouts: HashMap<String, StructLayout>,
    enum_constants: HashMap<String, i64>,
    string_literals: Vec<(String, String)>, // (label, string_content)
    float_literals: Vec<(String, f64)>,     // (label, value)
    global_variables: HashMap<String, GlobalVariable>,
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
            current_reg_save_area_offset: None,
            current_gp_params: 0,
            current_fp_params: 0,
            loop_stack: Vec::new(),
            struct_layouts: HashMap::new(),
            union_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            string_literals: Vec::new(),
            float_literals: Vec::new(),
            global_variables: HashMap::new(),
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
        self.float_literals.clear();
        self.global_variables.clear();
        self.collect_struct_layouts(ast)?;
        self.collect_union_layouts(ast)?;
        self.collect_enum_constants(ast)?;
        self.collect_global_variables(ast)?;
        self.collect_static_locals(ast)?;

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
            AstNode::StructDef { .. } => Ok(()),
            AstNode::UnionDef { .. } => Ok(()),
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

                // For variadic functions, allocate register save area
                // System V AMD64 ABI: 6 GP regs (48 bytes) + 8 XMM regs (128 bytes) = 176 bytes
                let reg_save_area_offset = if *is_variadic {
                    // Allocate 176 bytes for register save area using a dummy variable
                    // This will be at a negative offset from %rbp
                    Some(
                        self.symbol_table
                            .add_variable_with_layout(
                                "__reg_save_area__".to_string(),
                                Type::Array(Box::new(Type::Char), 176),
                                176,
                                16,
                            )
                            .unwrap(),
                    )
                } else {
                    None
                };

                // Store variadic function info for va_start/va_arg
                self.current_reg_save_area_offset = reg_save_area_offset;
                self.current_gp_params = int_param_count;
                self.current_fp_params = float_param_count;

                // Save current output and generate function body first to discover all variables
                let saved_output = self.output.clone();
                self.output.clear();

                let mut int_idx = 0;
                let mut float_idx = 0;
                let mut stack_param_offset = 16; // Parameters 7+ start at 16(%rbp)
                for param in params.iter() {
                    let local_offset = self.symbol_table.get_variable(&param.name).unwrap().stack_offset;

                    if self.is_float_type(&param.param_type) {
                        // Float parameter - use xmm registers or stack
                        if float_idx < param_regs_xmm.len() {
                            self.emit(&format!(
                                "    movsd {}, {}(%rbp)",
                                param_regs_xmm[float_idx], local_offset
                            ));
                        } else {
                            // Parameter is on stack - copy from incoming position to local slot
                            self.emit(&format!("    movsd {}(%rbp), %xmm0", stack_param_offset));
                            self.emit(&format!("    movsd %xmm0, {}(%rbp)", local_offset));
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
                            } else if matches!(param.param_type, Type::Char | Type::UChar) {
                                self.emit(&format!(
                                    "    movb {}, {}(%rbp)",
                                    param_regs_8[int_idx], local_offset
                                ));
                            } else if matches!(param.param_type, Type::Long) {
                                self.emit(&format!(
                                    "    movq {}, {}(%rbp)",
                                    param_regs_64[int_idx], local_offset
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
                    let xmm_regs = ["%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7"];
                    for (i, reg) in xmm_regs.iter().enumerate() {
                        let reg_offset = offset + 48 + (i as i32 * 16); // XMM regs are 16 bytes apart in the save area
                        self.emit(&format!("    movsd {}, {}(%rbp)", reg, reg_offset));
                    }
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

                // Append the body code
                self.output.push_str(&body_code);

                // Emit end label and epilogue
                self.emit(&format!("{}:", end_label));
                if stack_size > 0 {
                    self.emit("    movq %rbp, %rsp");
                }
                self.emit("    popq %rbp");
                self.emit("    ret");

                // Reset function-specific state
                self.current_function_end_label = None;
                self.current_reg_save_area_offset = None;
                self.current_gp_params = 0;
                self.current_fp_params = 0;
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
                is_const,
                is_volatile,
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
                            is_const: *is_const,
                            is_volatile: *is_volatile,
                        },
                    );

                    return Ok(());
                }

                // If array has size 0 (from [] syntax) and string literal initializer,
                // infer the actual size before allocating stack space
                let actual_var_type = if let Type::Array(elem_type, 0) = var_type {
                    if let Some(init_expr) = init {
                        if let AstNode::StringLiteral(s) = init_expr.as_ref() {
                            Type::Array(elem_type.clone(), s.len() + 1) // +1 for null terminator
                        } else {
                            var_type.clone()
                        }
                    } else {
                        var_type.clone()
                    }
                } else {
                    var_type.clone()
                };

                let var_size = self.type_size(&actual_var_type)?;
                let var_align = self.type_alignment(&actual_var_type)?;
                let offset = self.symbol_table.add_variable_with_layout(
                    name.clone(),
                    actual_var_type.clone(),
                    var_size,
                    var_align,
                )?;

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
                                self.generate_node(init_expr)?;
                                if matches!(var_type, Type::Int | Type::UInt | Type::Enum(_)) {
                                    self.emit(&format!("    movl %eax, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::UShort | Type::Short) {
                                    self.emit(&format!("    movw %ax, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::Char | Type::UChar) {
                                    self.emit(&format!("    movb %al, {}(%rbp)", offset));
                                } else if matches!(var_type, Type::Long | Type::ULong) {
                                    self.emit(&format!("    movq %rax, {}(%rbp)", offset));
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
                // Generate the value first
                self.generate_node(value)?;
                self.emit("    pushq %rax");

                // Generate lvalue address
                self.generate_lvalue(target)?;
                self.emit("    movq %rax, %rcx"); // %rcx = address
                self.emit("    popq %rax"); // %rax = value

                // Determine the type to store
                let target_type = self.expr_type(target)?;

                // Store value at address in %rcx
                if matches!(target_type, Type::Int | Type::UInt | Type::Enum(_)) {
                    self.emit("    movl %eax, (%rcx)");
                } else if matches!(target_type, Type::UShort | Type::Short) {
                    self.emit("    movw %ax, (%rcx)");
                } else if matches!(target_type, Type::Char | Type::UChar) {
                    self.emit("    movb %al, (%rcx)");
                } else if matches!(target_type, Type::Long | Type::ULong) {
                    self.emit("    movq %rax, (%rcx)");
                } else {
                    self.emit("    movq %rax, (%rcx)");
                }
                self.coerce_rax_to_type(&target_type);
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
                        Type::UChar => {
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
                        Type::Pointer(_) => {
                            self.emit(&format!("    movq {}(%rip), %rax", name));
                        }
                        _ => {
                            self.emit(&format!("    movq {}(%rip), %rax", name));
                        }
                    }
                    return Ok(());
                }

                let symbol = self
                    .symbol_table
                    .get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;

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
                } else if matches!(symbol.symbol_type, Type::UChar) {
                    self.emit(&format!("    movzbq {}(%rbp), %rax", symbol.stack_offset));
                } else if matches!(symbol.symbol_type, Type::Long | Type::ULong) {
                    self.emit(&format!("    movq {}(%rbp), %rax", symbol.stack_offset));
                } else {
                    self.emit(&format!("    movq {}(%rbp), %rax", symbol.stack_offset));
                }
                Ok(())
            }
            AstNode::FunctionCall { name, args } => {
                let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                let arg_regs_xmm = [
                    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
                ];

                // Track which arguments are floats and their position within their type class
                let mut int_counter = 0;
                let mut float_counter = 0;
                let mut arg_info: Vec<(bool, usize)> = Vec::new(); // (is_float, index_within_type)

                for arg in args.iter() {
                    let arg_type = self.expr_type(arg)?;
                    if self.is_float_type(&arg_type) {
                        arg_info.push((true, float_counter));
                        float_counter += 1;
                    } else {
                        arg_info.push((false, int_counter));
                        int_counter += 1;
                    }
                }

                // Generate all arguments and push to stack
                for (arg, (is_float, _)) in args.iter().zip(arg_info.iter()) {
                    self.generate_node(arg)?;
                    if *is_float {
                        self.emit("    subq $8, %rsp");
                        self.emit("    movsd %xmm0, (%rsp)");
                    } else {
                        self.emit("    pushq %rax");
                    }
                }

                // Pop/move arguments: register args go into registers, stack args stay on stack
                let mut stack_args_count = 0;
                for (is_float, idx) in arg_info.iter().rev() {
                    if *is_float {
                        if *idx < arg_regs_xmm.len() {
                            self.emit(&format!("    movsd (%rsp), {}", arg_regs_xmm[*idx]));
                            self.emit("    addq $8, %rsp");
                        } else {
                            // This argument stays on stack
                            stack_args_count += 1;
                        }
                    } else {
                        if *idx < arg_regs.len() {
                            self.emit(&format!("    popq {}", arg_regs[*idx]));
                        } else {
                            // This argument stays on stack
                            stack_args_count += 1;
                        }
                    }
                }

                self.emit(&format!("    call {}", name));

                // Clean up stack arguments if any
                if stack_args_count > 0 {
                    self.emit(&format!("    addq ${}, %rsp", stack_args_count * 8));
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
                    let arg_type = self.expr_type(arg)?;
                    if self.is_float_type(&arg_type) {
                        arg_info.push((true, float_counter));
                        float_counter += 1;
                    } else {
                        arg_info.push((false, int_counter));
                        int_counter += 1;
                    }
                }

                // Generate all arguments and push to stack
                for (arg, (is_float, _)) in args.iter().zip(arg_info.iter()) {
                    self.generate_node(arg)?;
                    if *is_float {
                        self.emit("    subq $8, %rsp");
                        self.emit("    movsd %xmm0, (%rsp)");
                    } else {
                        self.emit("    pushq %rax");
                    }
                }

                // Pop/move arguments: register args go into registers, stack args stay on stack
                let mut stack_args_count = 0;
                for (is_float, idx) in arg_info.iter().rev() {
                    if *is_float {
                        if *idx < arg_regs_xmm.len() {
                            self.emit(&format!("    movsd (%rsp), {}", arg_regs_xmm[*idx]));
                            self.emit("    addq $8, %rsp");
                        } else {
                            // This argument stays on stack
                            stack_args_count += 1;
                        }
                    } else {
                        if *idx < arg_regs.len() {
                            self.emit(&format!("    popq {}", arg_regs[*idx]));
                        } else {
                            // This argument stays on stack
                            stack_args_count += 1;
                        }
                    }
                }

                // Call through the function pointer
                self.emit("    call *%r11");

                // Clean up stack arguments if any
                if stack_args_count > 0 {
                    self.emit(&format!("    addq ${}, %rsp", stack_args_count * 8));
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
                        self.generate_node(right)?;
                        self.emit("    popq %rcx");

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
                            BinOp::Add | BinOp::Subtract | BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::Comma => {
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
                } else if matches!(operand_type, Type::Char | Type::UChar) {
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
                } else if matches!(operand_type, Type::Char | Type::UChar) {
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
                } else if matches!(operand_type, Type::Char | Type::UChar) {
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
                } else if matches!(operand_type, Type::Char | Type::UChar) {
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
                    Type::FunctionPointer { .. } => {
                        // Function pointers are already addresses - dereferencing is a no-op
                        return Ok(());
                    }
                    _ => return Err("Cannot dereference non-pointer type".to_string()),
                };

                // Check if pointee is a function pointer - if so, don't dereference
                if matches!(pointee_type, Type::FunctionPointer { .. }) {
                    // The address is already in %rax, no dereference needed
                    return Ok(());
                }

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
                } else if matches!(pointee_type, Type::UChar) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(pointee_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
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
                } else if matches!(elem_type, Type::UChar) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(elem_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
                } else {
                    self.emit("    movq (%rax), %rax");
                }
                Ok(())
            }
            AstNode::ArrayInit(_) => Err("Array initializer codegen not implemented".to_string()),
            AstNode::StructInit(_) => Err("Struct initializer codegen not implemented".to_string()),
            AstNode::MemberAccess {
                base,
                member,
                through_pointer,
            } => {
                let field_type = self.member_access_type(base, member, *through_pointer)?;
                self.generate_member_access_address(base, member, *through_pointer)?;
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
                } else if matches!(field_type, Type::UChar) {
                    self.emit("    movzbq (%rax), %rax");
                } else if matches!(field_type, Type::Long | Type::ULong) {
                    self.emit("    movq (%rax), %rax");
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
                    .fields
                    .get(member)
                    .ok_or_else(|| format!("Unknown member '{}' in struct/union", member))?;

                self.emit(&format!("    movq ${}, %rax", field_info.offset));
                Ok(())
            }
            AstNode::VaStart { ap, last_param } => {
                // va_start(ap, last_param)
                // Initialize the va_list structure according to System V AMD64 ABI
                // Structure layout (24 bytes total):
                //   0-3:   gp_offset (unsigned int)
                //   4-7:   fp_offset (unsigned int)
                //   8-15:  overflow_arg_area (void*)
                //   16-23: reg_save_area (void*)

                // Get the address of the va_list variable (ap)
                self.generate_lvalue(ap)?;
                self.emit("    pushq %rax"); // Save va_list address

                // Calculate gp_offset: number of GP parameters * 8
                let gp_offset = self.current_gp_params * 8;
                // Calculate fp_offset: 48 (for 6 GP regs) + number of FP parameters * 16
                let fp_offset = 48 + self.current_fp_params * 16;

                // Set gp_offset (offset 0-3 in va_list)
                self.emit(&format!("    movl ${}, %eax", gp_offset));
                self.emit("    movq (%rsp), %rcx"); // Get va_list address
                self.emit("    movl %eax, 0(%rcx)");

                // Set fp_offset (offset 4-7 in va_list)
                self.emit(&format!("    movl ${}, %eax", fp_offset));
                self.emit("    movq (%rsp), %rcx");
                self.emit("    movl %eax, 4(%rcx)");

                // Set overflow_arg_area (offset 8-15 in va_list)
                // Points to first stack argument at 16(%rbp)
                self.emit("    leaq 16(%rbp), %rax");
                self.emit("    movq (%rsp), %rcx");
                self.emit("    movq %rax, 8(%rcx)");

                // Set reg_save_area (offset 16-23 in va_list)
                if let Some(offset) = self.current_reg_save_area_offset {
                    self.emit(&format!("    leaq {}(%rbp), %rax", offset));
                    self.emit("    movq (%rsp), %rcx");
                    self.emit("    movq %rax, 16(%rcx)");
                }

                self.emit("    addq $8, %rsp"); // Pop va_list address
                self.emit("    xorq %rax, %rax"); // va_start returns void (set rax to 0)
                Ok(())
            }
            AstNode::VaArg { ap, arg_type } => {
                // va_arg(ap, type)
                // Extract the next argument from va_list according to System V AMD64 ABI
                // Algorithm:
                //   - For GP types: check if gp_offset < 48, use reg_save_area or overflow_arg_area
                //   - For FP types: check if fp_offset < 176, use reg_save_area or overflow_arg_area

                // Get address of va_list
                self.generate_lvalue(ap)?;
                self.emit("    movq %rax, %r10"); // Save va_list address in r10

                let is_float = self.is_float_type(arg_type);

                if is_float {
                    // Float/double argument - use fp_offset
                    // Load fp_offset
                    self.emit("    movl 4(%r10), %eax"); // fp_offset is at offset 4
                    self.emit("    cmpl $176, %eax");     // Check if fp_offset < 176
                    let use_stack_label = self.next_label();
                    let done_label = self.next_label();
                    self.emit(&format!("    jge {}", use_stack_label)); // If >= 176, use stack

                    // Use register save area
                    self.emit("    movq 16(%r10), %rcx");  // Load reg_save_area pointer
                    self.emit("    addq %rax, %rcx");      // Add fp_offset
                    self.emit("    movsd (%rcx), %xmm0");  // Load double from register save area
                    self.emit("    addl $16, %eax");       // Increment fp_offset by 16
                    self.emit("    movl %eax, 4(%r10)");   // Store updated fp_offset
                    self.emit("    subq $8, %rsp");
                    self.emit("    movsd %xmm0, (%rsp)");
                    self.emit("    movq (%rsp), %rax");    // Move to rax for return
                    self.emit("    addq $8, %rsp");
                    self.emit(&format!("    jmp {}", done_label));

                    // Use stack (overflow area)
                    self.emit(&format!("{}:", use_stack_label));
                    self.emit("    movq 8(%r10), %rcx");   // Load overflow_arg_area pointer
                    self.emit("    movsd (%rcx), %xmm0");  // Load double from stack
                    self.emit("    addq $8, %rcx");        // Increment overflow_arg_area by 8
                    self.emit("    movq %rcx, 8(%r10)");   // Store updated overflow_arg_area
                    self.emit("    subq $8, %rsp");
                    self.emit("    movsd %xmm0, (%rsp)");
                    self.emit("    movq (%rsp), %rax");
                    self.emit("    addq $8, %rsp");

                    self.emit(&format!("{}:", done_label));
                } else {
                    // Integer/pointer argument - use gp_offset
                    // Load gp_offset
                    self.emit("    movl 0(%r10), %eax"); // gp_offset is at offset 0
                    self.emit("    cmpl $48, %eax");      // Check if gp_offset < 48
                    let use_stack_label = self.next_label();
                    let done_label = self.next_label();
                    self.emit(&format!("    jge {}", use_stack_label)); // If >= 48, use stack

                    // Use register save area
                    self.emit("    movq 16(%r10), %rcx");  // Load reg_save_area pointer
                    self.emit("    addq %rax, %rcx");      // Add gp_offset
                    self.emit("    movq (%rcx), %rax");    // Load value from register save area
                    self.emit("    movl 0(%r10), %edx");   // Reload gp_offset
                    self.emit("    addl $8, %edx");        // Increment gp_offset by 8
                    self.emit("    movl %edx, 0(%r10)");   // Store updated gp_offset
                    self.emit(&format!("    jmp {}", done_label));

                    // Use stack (overflow area)
                    self.emit(&format!("{}:", use_stack_label));
                    self.emit("    movq 8(%r10), %rcx");   // Load overflow_arg_area pointer
                    self.emit("    movq (%rcx), %rax");    // Load value from stack
                    self.emit("    addq $8, %rcx");        // Increment overflow_arg_area by 8
                    self.emit("    movq %rcx, 8(%r10)");   // Store updated overflow_arg_area

                    self.emit(&format!("{}:", done_label));
                }

                Ok(())
            }
            AstNode::VaEnd(ap) => {
                // va_end(ap)
                // This is typically a no-op on x86-64
                self.generate_node(ap)?;
                // No-op: nothing to clean up for va_list on x86-64
                self.emit("    xorq %rax, %rax");
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
        }
    }

    fn generate_lvalue(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
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
        let (struct_or_union_name, is_address) =
            self.resolve_struct_or_union_base(base, through_pointer)?;

        // Try struct first, then union
        let layout = self
            .struct_layouts
            .get(&struct_or_union_name)
            .or_else(|| self.union_layouts.get(&struct_or_union_name))
            .ok_or_else(|| format!("Unknown struct/union type: {}", struct_or_union_name))?;

        let field = layout.fields.get(member).ok_or_else(|| {
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
            _ => Ok(None),
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
            (Ok(left_ty), Ok(right_ty))
                if self.is_integer_type(&left_ty) && self.is_integer_type(&right_ty) =>
            {
                matches!(
                    self.binary_integer_type(&left_ty, &right_ty),
                    Ok(Type::UInt | Type::ULong)
                )
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
        matches!(ty, Type::Float | Type::Double)
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
        // Generate left operand (result in xmm0)
        self.generate_node(left)?;
        // Save xmm0 to stack
        self.emit("    subq $8, %rsp");
        self.emit("    movsd %xmm0, (%rsp)");

        // Generate right operand (result in xmm0)
        self.generate_node(right)?;
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
                | Type::UChar
                | Type::UShort
                | Type::Short
                | Type::Long
                | Type::ULong
        )
    }

    fn promote_integer_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Char | Type::UChar | Type::UShort | Type::Short => Type::Int,
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
            Type::Pointer(pointee) => Ok(Some(self.type_size(pointee)?)),
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
            _ => Err("Array element type unavailable".to_string()),
        }
    }

    fn type_size(&self, ty: &Type) -> Result<i32, String> {
        match ty {
            Type::Int => Ok(4),
            Type::UInt => Ok(4),
            Type::Char => Ok(1),
            Type::UChar => Ok(1),
            Type::UShort => Ok(2),
            Type::Short => Ok(2),
            Type::Long => Ok(8),
            Type::ULong => Ok(8),
            Type::Float => Ok(4),
            Type::Double => Ok(8),
            Type::Pointer(_) | Type::FunctionPointer { .. } => Ok(8),
            Type::Void => Ok(0),
            Type::VaList => Ok(24), // Size of __va_list_tag in System V AMD64 ABI
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
            Type::UChar => Ok(1),
            Type::UShort => Ok(2),
            Type::Short => Ok(2),
            Type::Long => Ok(8),
            Type::ULong => Ok(8),
            Type::Float => Ok(4),
            Type::Double => Ok(8),
            Type::Pointer(_) | Type::FunctionPointer { .. } => Ok(8),
            Type::Void => Ok(1),
            Type::VaList => Ok(8), // Alignment of __va_list_tag in System V AMD64 ABI
            Type::Array(elem, _) => self.type_alignment(elem),
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

    fn convert_type(&mut self, from: &Type, to: &Type) -> Result<(), String> {
        // If types are the same, no conversion needed
        if from == to {
            return Ok(());
        }

        // Helper to determine if a type is signed
        let is_signed = |ty: &Type| matches!(ty, Type::Char | Type::Short | Type::Int | Type::Long);

        match (from, to) {
            // Pointer to pointer - no conversion needed
            (Type::Pointer(_), Type::Pointer(_)) => Ok(()),

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
                if is_signed(from) {
                    self.emit("    movslq %eax, %rax");
                } else {
                    self.emit("    movl %eax, %eax");
                }
                Ok(())
            }

            // Float to integer conversions
            (
                Type::Float | Type::Double,
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
                self.convert_type(&Type::Long, to)?;
                Ok(())
            }

            // Integer to float conversions
            (
                Type::Int
                | Type::UInt
                | Type::Long
                | Type::ULong
                | Type::Char
                | Type::UChar
                | Type::Short
                | Type::UShort,
                Type::Float | Type::Double,
            ) => {
                // First convert to 64-bit integer in rax if needed
                let from_size = self.type_size(from)?;
                if from_size < 8 {
                    self.convert_type(from, &Type::Long)?;
                }
                // Convert int (in rax) to double (in xmm0)
                self.emit("    cvtsi2sd %rax, %xmm0");
                Ok(())
            }

            // Float to float (no conversion needed, both use xmm0)
            (Type::Float, Type::Double) | (Type::Double, Type::Float) => Ok(()),

            // Integer conversions
            _ => {
                let from_size = self.type_size(from)?;
                let to_size = self.type_size(to)?;

                match (from_size, to_size) {
                    // Same size - no conversion needed
                    (a, b) if a == b => Ok(()),

                    // Widening conversions
                    (1, 2) => {
                        if is_signed(from) {
                            self.emit("    movsbw %al, %ax");
                        } else {
                            self.emit("    movzbw %al, %ax");
                        }
                        Ok(())
                    }
                    (1, 4) => {
                        if is_signed(from) {
                            self.emit("    movsbl %al, %eax");
                        } else {
                            self.emit("    movzbl %al, %eax");
                        }
                        Ok(())
                    }
                    (1, 8) => {
                        if is_signed(from) {
                            self.emit("    movsbq %al, %rax");
                        } else {
                            self.emit("    movzbq %al, %rax");
                        }
                        Ok(())
                    }
                    (2, 4) => {
                        if is_signed(from) {
                            self.emit("    movswl %ax, %eax");
                        } else {
                            self.emit("    movzwl %ax, %eax");
                        }
                        Ok(())
                    }
                    (2, 8) => {
                        if is_signed(from) {
                            self.emit("    movswq %ax, %rax");
                        } else {
                            self.emit("    movzwq %ax, %rax");
                        }
                        Ok(())
                    }
                    (4, 8) => {
                        if is_signed(from) {
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

    fn register_struct_layout(&mut self, name: &str, fields: &[StructField]) -> Result<(), String> {
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
            self.struct_layouts.insert(
                name.to_string(),
                StructLayout {
                    fields: HashMap::new(),
                    size: 0,
                    alignment: 1,
                },
            );
            return Ok(());
        }

        // This is a complete definition - calculate layout
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

    fn register_union_layout(&mut self, name: &str, fields: &[StructField]) -> Result<(), String> {
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
            self.union_layouts.insert(
                name.to_string(),
                StructLayout {
                    fields: HashMap::new(),
                    size: 0,
                    alignment: 1,
                },
            );
            return Ok(());
        }

        // This is a complete definition - calculate layout
        let mut max_size = 0;
        let mut max_align = 1;
        let mut field_map = HashMap::new();
        for field in fields {
            let field_size = self.type_size(&field.field_type)?;
            let field_align = self.type_alignment(&field.field_type)?;
            if field_size == 0 {
                return Err(format!("Field '{}' has invalid size", field.name));
            }
            max_size = max_size.max(field_size);
            max_align = max_align.max(field_align);
            // All fields in a union are at offset 0
            field_map.insert(
                field.name.clone(),
                StructFieldInfo {
                    field_type: field.field_type.clone(),
                    offset: 0,
                },
            );
        }
        let size = align_to(max_size, max_align);

        self.union_layouts.insert(
            name.to_string(),
            StructLayout {
                fields: field_map,
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
                } else if matches!(elem_type, Type::Char | Type::UChar) {
                    self.emit(&format!("    movb %al, {}(%rbp)", elem_offset));
                } else if matches!(elem_type, Type::Long | Type::ULong) {
                    self.emit(&format!("    movq %rax, {}(%rbp)", elem_offset));
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
            } else if matches!(elem_type, Type::Char | Type::UChar) {
                self.emit(&format!("    movb $0, {}(%rbp)", offset));
            } else if matches!(elem_type, Type::Long | Type::ULong) {
                self.emit(&format!("    movq $0, {}(%rbp)", offset));
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
        let mut ordered_fields: Vec<StructFieldInfo> = layout.fields.values().cloned().collect();
        ordered_fields.sort_by_key(|info| info.offset);

        let mut positional_index = 0;

        for init_field in init_fields {
            // Determine which field to initialize
            let field_info = if let Some(ref field_name) = init_field.field_name {
                // Designated initializer: look up by name
                layout.fields.get(field_name).ok_or_else(|| {
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
                        Type::Char | Type::UChar => {
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
            layout.fields.get(field_name).ok_or_else(|| {
                format!("Unknown field '{}' in union '{}'", field_name, union_name)
            })?
        } else {
            // Positional initializer: use first field in declaration order
            let mut ordered_fields: Vec<(&String, &StructFieldInfo)> =
                layout.fields.iter().collect();
            ordered_fields.sort_by_key(|(_, info)| info.offset);
            if ordered_fields.is_empty() {
                return Err(format!("Union '{}' has no fields", union_name));
            }
            ordered_fields[0].1
        };

        // Generate code for the value
        self.generate_node(&init_field.value)?;
        let dest_offset = base_offset; // All union fields start at offset 0

        // Store based on field type
        match field_info.field_type {
            Type::Char | Type::UChar => {
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

    fn collect_union_layouts(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::UnionDef { name, fields } = item {
                    self.register_union_layout(name, fields)?;
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
                    is_const,
                    is_volatile,
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
                            is_const: *is_const,
                            is_volatile: *is_volatile,
                        },
                    );
                }
            }
        }
        Ok(())
    }

    fn collect_static_locals(&mut self, node: &AstNode) -> Result<(), String> {
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::Function { body, .. } = item
                    && let Some(body_node) = body {
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
                is_const,
                is_volatile,
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
                            is_const: *is_const,
                            is_volatile: *is_volatile,
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
        if let AstNode::Program(nodes) = node {
            for item in nodes {
                if let AstNode::EnumDef { enumerators, .. } = item {
                    for enumerator in enumerators {
                        if let Some(value) = enumerator.value {
                            self.enum_constants.insert(enumerator.name.clone(), value);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_struct_or_union_base(
        &mut self,
        base: &AstNode,
        through_pointer: bool,
    ) -> Result<(String, bool), String> {
        match base {
            AstNode::Variable(name) => {
                // Check if it's a global variable first
                let symbol_type = if let Some(global) = self.global_variables.get(name) {
                    global.var_type.clone()
                } else {
                    self.symbol_table
                        .get_variable(name)
                        .ok_or_else(|| format!("Undefined variable: {}", name))?
                        .symbol_type
                        .clone()
                };
                match (symbol_type, through_pointer) {
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
        let struct_or_union_name = match (base_type, through_pointer) {
            (Type::Struct(name), false) | (Type::Union(name), false) => name,
            (Type::Pointer(pointee), true) => match *pointee {
                Type::Struct(name) | Type::Union(name) => name,
                _ => return Err("Pointer does not target a struct/union".to_string()),
            },
            _ => return Err("Member access base is not a struct/union".to_string()),
        };

        // Try struct first, then union
        let layout = self
            .struct_layouts
            .get(&struct_or_union_name)
            .or_else(|| self.union_layouts.get(&struct_or_union_name))
            .ok_or_else(|| format!("Unknown struct/union type: {}", struct_or_union_name))?;

        let field = layout.fields.get(member).ok_or_else(|| {
            format!(
                "Unknown field '{}' on struct/union {}",
                member, struct_or_union_name
            )
        })?;
        Ok(field.field_type.clone())
    }

    fn expr_type(&self, expr: &AstNode) -> Result<Type, String> {
        match expr {
            AstNode::IntLiteral(_) => Ok(Type::Int),
            AstNode::FloatLiteral(_) => Ok(Type::Double),
            AstNode::CharLiteral(_) => Ok(Type::Char),
            AstNode::Variable(name) => {
                // Check if it's a global variable first
                if let Some(global) = self.global_variables.get(name) {
                    return Ok(global.var_type.clone());
                }

                // Check if it's a local variable
                if let Some(symbol) = self.symbol_table.get_variable(name) {
                    return Ok(symbol.symbol_type.clone());
                }

                // Assume it's a function name - return a generic function pointer type
                // For now, assume functions return int (can be improved later)
                Ok(Type::FunctionPointer {
                    return_type: Box::new(Type::Int),
                    param_types: vec![],
                })
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
            AstNode::MemberAccess {
                base,
                member,
                through_pointer,
            } => self.member_access_type(base, member, *through_pointer),
            AstNode::BinaryOp { op, left, right } => {
                let left_type = self.expr_type(left)?;
                let right_type = self.expr_type(right)?;
                let integer_type =
                    if self.is_integer_type(&left_type) && self.is_integer_type(&right_type) {
                        Some(self.binary_integer_type(&left_type, &right_type)?)
                    } else {
                        None
                    };
                match op {
                    BinOp::Add => match (left_type.clone(), right_type.clone()) {
                        (Type::Pointer(pointee), Type::Int) => Ok(Type::Pointer(pointee)),
                        (Type::Int, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UInt) => Ok(Type::Pointer(pointee)),
                        (Type::UInt, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UShort) => Ok(Type::Pointer(pointee)),
                        (Type::UShort, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Short) => Ok(Type::Pointer(pointee)),
                        (Type::Short, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UChar) => Ok(Type::Pointer(pointee)),
                        (Type::UChar, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Char) => Ok(Type::Pointer(pointee)),
                        (Type::Char, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::ULong) => Ok(Type::Pointer(pointee)),
                        (Type::ULong, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Long) => Ok(Type::Pointer(pointee)),
                        (Type::Long, Type::Pointer(pointee)) => Ok(Type::Pointer(pointee)),
                        _ => integer_type
                            .clone()
                            .ok_or_else(|| "Invalid operands for pointer addition".to_string()),
                    },
                    BinOp::Subtract => match (left_type, right_type) {
                        (Type::Pointer(pointee), Type::Int) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UInt) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UShort) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::UChar) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Char) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::ULong) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Short) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(pointee), Type::Long) => Ok(Type::Pointer(pointee)),
                        (Type::Pointer(_), Type::Pointer(_)) => Ok(Type::Int),
                        _ => integer_type
                            .ok_or_else(|| "Invalid operands for pointer subtraction".to_string()),
                    },
                    BinOp::ShiftLeft | BinOp::ShiftRight => {
                        if !self.is_integer_type(&left_type) || !self.is_integer_type(&right_type) {
                            return Err("Shift operands must be integers".to_string());
                        }
                        Ok(self.promote_integer_type(&left_type))
                    }
                    BinOp::Multiply
                    | BinOp::Divide
                    | BinOp::Modulo
                    | BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::BitXor => integer_type.ok_or_else(|| {
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
                UnaryOp::Negate | UnaryOp::BitNot => {
                    let operand_type = self.expr_type(operand)?;
                    Ok(self.promote_integer_type(&operand_type))
                }
            },
            AstNode::PrefixIncrement(operand)
            | AstNode::PrefixDecrement(operand)
            | AstNode::PostfixIncrement(operand)
            | AstNode::PostfixDecrement(operand) => self.expr_type(operand),
            AstNode::FunctionCall { .. } | AstNode::IndirectCall { .. } => Ok(Type::Int),
            AstNode::StringLiteral(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            AstNode::Cast { target_type, .. } => Ok(target_type.clone()),
            AstNode::OffsetOf { .. } => Ok(Type::ULong), // offsetof returns size_t (unsigned long)
            AstNode::VaStart { .. } => Ok(Type::Void), // va_start returns void
            AstNode::VaArg { arg_type, .. } => Ok(arg_type.clone()), // va_arg returns the specified type
            AstNode::VaEnd(_) => Ok(Type::Void), // va_end returns void
            _ => Err("Unsupported expression in sizeof".to_string()),
        }
    }

    fn coerce_rax_to_type(&mut self, ty: &Type) {
        match ty {
            Type::Char => self.emit("    movsbq %al, %rax"),
            Type::UChar => self.emit("    movzbq %al, %rax"),
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
                let align = self.type_alignment(&global.var_type)?;
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
                let align = self.type_alignment(&global.var_type)?;
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
                    Type::Char | Type::UChar => {
                        self.emit(&format!("    .byte {}", n));
                    }
                    Type::Short | Type::UShort => {
                        self.emit(&format!("    .short {}", n));
                    }
                    Type::Int | Type::UInt | Type::Enum(_) => {
                        self.emit(&format!("    .long {}", n));
                    }
                    Type::Long | Type::ULong => {
                        self.emit(&format!("    .quad {}", n));
                    }
                    _ => {
                        self.emit(&format!("    .long {}", n));
                    }
                }
                Ok(())
            }
            AstNode::StringLiteral(s) => {
                // For string literals, emit the label reference
                let label = self.add_string_literal(s.clone());
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
            _ => Err(format!("Unsupported global initializer: {:?}", init)),
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
                // Emit as 8-byte double precision value
                self.emit(&format!("    .double {}", value));
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
