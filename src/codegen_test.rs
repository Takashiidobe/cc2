#[cfg(test)]
mod codegen_tests {
    use crate::ast::*;
    use crate::codegen::CodeGenerator;

    #[test]
    fn test_generate_return() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![AstNode::Return(Some(
                Box::new(AstNode::IntLiteral(42)),
            ))]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("main:"));
        assert!(asm.contains("movl $42, %eax"));
        assert!(asm.contains("cltq"));
        assert!(asm.contains("ret"));
    }

    #[test]
    fn test_generate_addition() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![AstNode::Return(Some(
                Box::new(AstNode::BinaryOp {
                    op: BinOp::Add,
                    left: Box::new(AstNode::IntLiteral(2)),
                    right: Box::new(AstNode::IntLiteral(3)),
                }),
            ))]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("addl %ecx, %eax"));
    }

    #[test]
    fn test_generate_multiplication() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![AstNode::Return(Some(
                Box::new(AstNode::BinaryOp {
                    op: BinOp::Multiply,
                    left: Box::new(AstNode::IntLiteral(3)),
                    right: Box::new(AstNode::IntLiteral(4)),
                }),
            ))]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imull %ecx, %eax"));
    }

    #[test]
    fn test_generate_address_of_and_dereference() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "x".to_string(),
                    var_type: Type::Int,
                    init: Some(Box::new(AstNode::IntLiteral(5))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::VarDecl {
                    name: "p".to_string(),
                    var_type: Type::Pointer(Box::new(Type::Int)),
                    init: Some(Box::new(AstNode::AddressOf(Box::new(AstNode::Variable(
                        "x".to_string(),
                    ))))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Dereference(Box::new(
                    AstNode::Variable("p".to_string()),
                ))))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("leaq"));
        assert!(asm.contains("movslq (%rax), %rax"));
    }

    #[test]
    fn test_generate_array_init_and_index() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "a".to_string(),
                    var_type: Type::Array(Box::new(Type::Int), 3),
                    init: Some(Box::new(AstNode::ArrayInit(vec![
                        AstNode::IntLiteral(1),
                        AstNode::IntLiteral(2),
                        AstNode::IntLiteral(3),
                    ]))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::ArrayIndex {
                    array: Box::new(AstNode::Variable("a".to_string())),
                    index: Box::new(AstNode::IntLiteral(1)),
                }))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imulq $4, %rax"));
        assert!(asm.contains("movslq (%rax), %rax"));
    }

    #[test]
    fn test_generate_pointer_addition() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "x".to_string(),
                    var_type: Type::Int,
                    init: Some(Box::new(AstNode::IntLiteral(0))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::VarDecl {
                    name: "p".to_string(),
                    var_type: Type::Pointer(Box::new(Type::Int)),
                    init: Some(Box::new(AstNode::AddressOf(Box::new(AstNode::Variable(
                        "x".to_string(),
                    ))))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Dereference(Box::new(
                    AstNode::BinaryOp {
                        op: BinOp::Add,
                        left: Box::new(AstNode::Variable("p".to_string())),
                        right: Box::new(AstNode::IntLiteral(2)),
                    },
                ))))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("imulq $4, %rax"));
        assert!(asm.contains("addq %rcx, %rax"));
    }

    #[test]
    fn test_generate_sizeof_struct() {
        let ast = AstNode::Program(vec![
            AstNode::StructDef {
                name: "Point".to_string(),
                fields: vec![
                    StructField {
                        name: "x".to_string(),
                        field_type: Type::Int,
                        bit_width: None,
                        alignment: None,
                    },
                    StructField {
                        name: "y".to_string(),
                        field_type: Type::Int,
                        bit_width: None,
                        alignment: None,
                    },
                ],
                attributes: TypeAttributes::default(),
            },
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                is_variadic: false,
                body: Some(Box::new(AstNode::Block(vec![AstNode::Return(Some(
                    Box::new(AstNode::SizeOfType(Type::Struct("Point".to_string()))),
                ))]))),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movq $8, %rax"));
    }

    #[test]
    fn test_generate_sizeof_struct_with_padding() {
        let ast = AstNode::Program(vec![
            AstNode::StructDef {
                name: "Mix".to_string(),
                fields: vec![
                    StructField {
                        name: "x".to_string(),
                        field_type: Type::Int,
                        bit_width: None,
                        alignment: None,
                    },
                    StructField {
                        name: "p".to_string(),
                        field_type: Type::Pointer(Box::new(Type::Int)),
                        bit_width: None,
                        alignment: None,
                    },
                ],
                attributes: TypeAttributes::default(),
            },
            AstNode::Function {
                name: "main".to_string(),
                return_type: Type::Int,
                params: vec![],
                is_variadic: false,
                body: Some(Box::new(AstNode::Block(vec![AstNode::Return(Some(
                    Box::new(AstNode::SizeOfType(Type::Struct("Mix".to_string()))),
                ))]))),
            },
        ]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movq $16, %rax"));
    }

    #[test]
    fn test_generate_char_load_store() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "c".to_string(),
                    var_type: Type::Char,
                    init: Some(Box::new(AstNode::IntLiteral(1))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Variable("c".to_string())))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movb %al"));
        assert!(asm.contains("movsbq"));
    }

    #[test]
    fn test_generate_unsigned_int_load() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "u".to_string(),
                    var_type: Type::UInt,
                    init: Some(Box::new(AstNode::IntLiteral(1))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Variable("u".to_string())))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movl %eax"));
        assert!(asm.contains("movl "));
        assert!(!asm.contains("movslq"));
    }

    #[test]
    fn test_generate_unsigned_short_load() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "s".to_string(),
                    var_type: Type::UShort,
                    init: Some(Box::new(AstNode::IntLiteral(2))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Variable("s".to_string())))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movw %ax"));
        assert!(asm.contains("movzwq"));
    }

    #[test]
    fn test_generate_unsigned_long_load() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "l".to_string(),
                    var_type: Type::ULong,
                    init: Some(Box::new(AstNode::IntLiteral(3))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::Variable("l".to_string())))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movq %rax"));
        assert!(asm.contains("movq "));
    }

    #[test]
    fn test_generate_short_long_loads() {
        let ast = AstNode::Program(vec![AstNode::Function {
            name: "main".to_string(),
            return_type: Type::Int,
            params: vec![],
            is_variadic: false,
            body: Some(Box::new(AstNode::Block(vec![
                AstNode::VarDecl {
                    name: "s".to_string(),
                    var_type: Type::Short,
                    init: Some(Box::new(AstNode::IntLiteral(1))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::VarDecl {
                    name: "l".to_string(),
                    var_type: Type::Long,
                    init: Some(Box::new(AstNode::IntLiteral(2))),
                    is_extern: false,
                    is_static: false,
                    is_auto: false,
                    is_register: false,
                    is_const: false,
                    is_volatile: false,
                    alignment: None,
                },
                AstNode::Return(Some(Box::new(AstNode::BinaryOp {
                    op: BinOp::Add,
                    left: Box::new(AstNode::Variable("s".to_string())),
                    right: Box::new(AstNode::Variable("l".to_string())),
                }))),
            ]))),
        }]);

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&ast).unwrap();

        assert!(asm.contains("movswq"));
        assert!(asm.contains("movq"));
    }
}
