#[cfg(test)]
mod codegen_tests {
    use crate::ast::*;
    use crate::codegen::CodeGenerator;

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

    #[test]
    fn test_generate_addition() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::Return(Some(Box::new(AstNode::BinaryOp {
                        op: BinOp::Add,
                        left: Box::new(AstNode::IntLiteral(2)),
                        right: Box::new(AstNode::IntLiteral(3)),
                    }))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("addq %rcx, %rax"));
    }

    #[test]
    fn test_generate_multiplication() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::Return(Some(Box::new(AstNode::BinaryOp {
                        op: BinOp::Multiply,
                        left: Box::new(AstNode::IntLiteral(3)),
                        right: Box::new(AstNode::IntLiteral(4)),
                    }))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imulq %rcx, %rax"));
    }
}
