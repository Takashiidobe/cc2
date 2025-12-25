use crate::ast::*;
use crate::symbol_table::SymbolTable;

pub struct CodeGenerator {
    output: String,
    symbol_table: SymbolTable,
    label_counter: usize,
    current_function_end_label: Option<String>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            symbol_table: SymbolTable::new(),
            label_counter: 0,
            current_function_end_label: None,
        }
    }

    pub fn generate(&mut self, ast: &AstNode) -> Result<String, String> {
        self.output.clear();
        self.symbol_table = SymbolTable::new();
        self.label_counter = 0;
        self.current_function_end_label = None;
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
            AstNode::Function { name, body, params, .. } => {
                self.symbol_table = SymbolTable::new();

                let param_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                for (i, param) in params.iter().enumerate() {
                    let _offset = self.symbol_table.add_variable(param.name.clone(), param.param_type.clone())?;
                    if i < param_regs.len() {
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
                    if i < param_regs.len() {
                        let symbol = self.symbol_table.get_variable(&param.name).unwrap();
                        self.emit(&format!("    movq {}, {}(%rbp)", param_regs[i], symbol.stack_offset));
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
                let offset = self.symbol_table.add_variable(name.clone(), var_type.clone())?;

                if let Some(init_expr) = init {
                    self.generate_node(init_expr)?;
                    self.emit(&format!("    movq %rax, {}(%rbp)", offset));
                }
                Ok(())
            }
            AstNode::Assignment { name, value } => {
                let offset = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .stack_offset;

                self.generate_node(value)?;
                self.emit(&format!("    movq %rax, {}(%rbp)", offset));
                Ok(())
            }
            AstNode::Variable(name) => {
                let offset = self.symbol_table.get_variable(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .stack_offset;

                self.emit(&format!("    movq {}(%rbp), %rax", offset));
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
                    _ => {
                        self.generate_node(left)?;
                        self.emit("    pushq %rax");
                        self.generate_node(right)?;
                        self.emit("    popq %rcx");

                        match op {
                            BinOp::Add => self.emit("    addq %rcx, %rax"),
                            BinOp::Subtract => {
                                self.emit("    subq %rax, %rcx");
                                self.emit("    movq %rcx, %rax");
                            }
                            BinOp::Multiply => self.emit("    imulq %rcx, %rax"),
                            BinOp::Divide => {
                                self.emit("    movq %rax, %rbx");
                                self.emit("    movq %rcx, %rax");
                                self.emit("    cqto");
                                self.emit("    idivq %rbx");
                            }
                            BinOp::Less => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    setl %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::Greater => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    setg %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::LessEqual => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    setle %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::GreaterEqual => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    setge %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::EqualEqual => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    sete %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::NotEqual => {
                                self.emit("    cmpq %rax, %rcx");
                                self.emit("    setne %al");
                                self.emit("    movzbq %al, %rax");
                            }
                            BinOp::LogicalAnd | BinOp::LogicalOr => unreachable!(),
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
            AstNode::IntLiteral(n) => {
                self.emit(&format!("    movq ${}, %rax", n));
                Ok(())
            }
        }
    }

    fn emit(&mut self, line: &str) {
        self.output.push_str(line);
        self.output.push('\n');
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
