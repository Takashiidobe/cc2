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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_return() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::Return(Some(Box::new(AstNode::IntLiteral(42)))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("main:"));
        assert!(asm.contains("movq $42, %rax"));
        assert!(asm.contains("ret"));
    }
}
