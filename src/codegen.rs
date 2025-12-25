use crate::ast::*;
use crate::symbol_table::SymbolTable;

pub struct CodeGenerator {
    output: String,
    symbol_table: SymbolTable,
    label_counter: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            symbol_table: SymbolTable::new(),
            label_counter: 0,
        }
    }

    pub fn generate(&mut self, ast: &AstNode) -> Result<String, String> {
        self.output.clear();
        self.symbol_table = SymbolTable::new();
        self.label_counter = 0;
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
                    let offset = self.symbol_table.add_variable(param.name.clone(), param.param_type.clone())?;
                    if i < param_regs.len() {
                        self.emit(&format!("    # Store parameter {} at offset {}", param.name, offset));
                    }
                }

                self.emit(&format!("    .globl {}", name));
                self.emit(&format!("{}:", name));
                self.emit("    pushq %rbp");
                self.emit("    movq %rsp, %rbp");

                for (i, param) in params.iter().enumerate() {
                    if i < param_regs.len() {
                        let symbol = self.symbol_table.get_variable(&param.name).unwrap();
                        self.emit(&format!("    movq {}, {}(%rbp)", param_regs[i], symbol.stack_offset));
                    }
                }

                self.generate_node(body)?;

                self.emit("    popq %rbp");
                self.emit("    ret");
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
                    self.emit("    popq %rbp");
                    self.emit("    ret");
                }
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

                for (i, arg) in args.iter().enumerate().rev() {
                    if i >= arg_regs.len() {
                        self.generate_node(arg)?;
                        self.emit("    pushq %rax");
                    }
                }

                for (i, arg) in args.iter().enumerate() {
                    if i < arg_regs.len() {
                        self.generate_node(arg)?;
                        if i > 0 {
                            self.emit("    pushq %rax");
                        }
                    }
                }

                for i in (1..args.len().min(arg_regs.len())).rev() {
                    self.emit(&format!("    popq {}", arg_regs[i]));
                }

                if !args.is_empty() && args.len() <= arg_regs.len() {
                    self.emit(&format!("    movq %rax, {}", arg_regs[0]));
                }

                self.emit(&format!("    call {}", name));

                if args.len() > arg_regs.len() {
                    let stack_args = (args.len() - arg_regs.len()) * 8;
                    self.emit(&format!("    addq ${}, %rsp", stack_args));
                }

                Ok(())
            }
            AstNode::BinaryOp { op, left, right } => {
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
