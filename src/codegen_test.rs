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

    #[test]
    fn test_generate_address_of_and_dereference() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::VarDecl {
                        name: "x".to_string(),
                        var_type: Type::Int,
                        init: Some(Box::new(AstNode::IntLiteral(5))),
                    },
                    AstNode::VarDecl {
                        name: "p".to_string(),
                        var_type: Type::Pointer(Box::new(Type::Int)),
                        init: Some(Box::new(AstNode::AddressOf(Box::new(
                            AstNode::Variable("x".to_string()),
                        )))),
                    },
                    AstNode::Return(Some(Box::new(AstNode::Dereference(Box::new(
                        AstNode::Variable("p".to_string()),
                    ))))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("leaq"));
        assert!(asm.contains("movq (%rax), %rax"));
    }

    #[test]
    fn test_generate_array_init_and_index() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::VarDecl {
                        name: "a".to_string(),
                        var_type: Type::Array(Box::new(Type::Int), 3),
                        init: Some(Box::new(AstNode::ArrayInit(vec![
                            AstNode::IntLiteral(1),
                            AstNode::IntLiteral(2),
                            AstNode::IntLiteral(3),
                        ]))),
                    },
                    AstNode::Return(Some(Box::new(AstNode::ArrayIndex {
                        array: Box::new(AstNode::Variable("a".to_string())),
                        index: Box::new(AstNode::IntLiteral(1)),
                    }))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imulq $8, %rax"));
        assert!(asm.contains("movq (%rax), %rax"));
    }

    #[test]
    fn test_generate_pointer_addition() {
        let ast = AstNode::Program(vec![
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                body: Box::new(AstNode::Block(vec![
                    AstNode::VarDecl {
                        name: "x".to_string(),
                        var_type: Type::Int,
                        init: Some(Box::new(AstNode::IntLiteral(0))),
                    },
                    AstNode::VarDecl {
                        name: "p".to_string(),
                        var_type: Type::Pointer(Box::new(Type::Int)),
                        init: Some(Box::new(AstNode::AddressOf(Box::new(
                            AstNode::Variable("x".to_string()),
                        )))),
                    },
                    AstNode::Return(Some(Box::new(AstNode::Dereference(Box::new(
                        AstNode::BinaryOp {
                            op: BinOp::Add,
                            left: Box::new(AstNode::Variable("p".to_string())),
                            right: Box::new(AstNode::IntLiteral(2)),
                        },
                    ))))),
                ])),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imulq $8, %rax"));
        assert!(asm.contains("addq %rcx, %rax"));
    }
}
