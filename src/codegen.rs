use crate::ast::*;

pub struct CodeGenerator {
    output: String,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
        }
    }

    pub fn generate(&mut self, ast: &AstNode) -> Result<String, String> {
        self.output.clear();
        self.generate_node(ast)?;
        Ok(self.output.clone())
    }

    fn generate_node(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::Program(functions) => {
                for func in functions {
                    self.generate_node(func)?;
                }
                Ok(())
            }
            AstNode::Function { name, body, .. } => {
                self.emit(&format!("    .globl {}", name));
                self.emit(&format!("{}:", name));
                self.emit("    pushq %rbp");
                self.emit("    movq %rsp, %rbp");

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
