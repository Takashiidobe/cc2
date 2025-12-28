use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Program(Vec<AstNode>),
    Function {
        name: String,
        return_type: Type,
        params: Vec<Parameter>,
        body: Option<Box<AstNode>>, // None for forward declarations
    },
    Block(Vec<AstNode>),
    Return(Option<Box<AstNode>>),
    IfStatement {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },
    WhileLoop {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    DoWhileLoop {
        body: Box<AstNode>,
        condition: Box<AstNode>,
    },
    ForLoop {
        init: Option<Box<AstNode>>,
        condition: Option<Box<AstNode>>,
        increment: Option<Box<AstNode>>,
        body: Box<AstNode>,
    },
    StructDef {
        name: String,
        fields: Vec<StructField>,
    },
    EnumDef {
        name: String,
        enumerators: Vec<Enumerator>,
    },
    UnionDef {
        name: String,
        fields: Vec<StructField>,
    },
    VarDecl {
        name: String,
        var_type: Type,
        init: Option<Box<AstNode>>,
        is_extern: bool,
        is_static: bool,
        is_const: bool,
        is_volatile: bool,
    },
    Assignment {
        target: Box<AstNode>,
        value: Box<AstNode>,
    },
    Variable(String),
    FunctionCall {
        name: String,
        args: Vec<AstNode>,
    },
    BinaryOp {
        op: BinOp,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    TernaryOp {
        condition: Box<AstNode>,
        true_expr: Box<AstNode>,
        false_expr: Box<AstNode>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<AstNode>,
    },
    PrefixIncrement(Box<AstNode>),
    PrefixDecrement(Box<AstNode>),
    PostfixIncrement(Box<AstNode>),
    PostfixDecrement(Box<AstNode>),
    AddressOf(Box<AstNode>),
    Dereference(Box<AstNode>),
    ArrayIndex {
        array: Box<AstNode>,
        index: Box<AstNode>,
    },
    ArrayInit(Vec<AstNode>),
    StructInit(Vec<StructInitField>),
    MemberAccess {
        base: Box<AstNode>,
        member: String,
        through_pointer: bool,
    },
    SizeOfType(Type),
    SizeOfExpr(Box<AstNode>),
    Cast {
        target_type: Type,
        expr: Box<AstNode>,
    },
    IntLiteral(i64),
    StringLiteral(String),
    CharLiteral(i64),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    LogicalNot,
    Negate,
    BitNot,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitField {
    pub field_name: Option<String>, // None for positional, Some for designated
    pub value: AstNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<i64>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    UInt,
    Char,
    UChar,
    UShort,
    Short,
    Long,
    ULong,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Struct(String),
    Union(String),
    Enum(String),
}

impl Type {
    pub fn size(&self) -> i32 {
        match self {
            Type::Int => 4,
            Type::UInt => 4,
            Type::Char => 1,
            Type::UChar => 1,
            Type::UShort => 2,
            Type::Short => 2,
            Type::Long => 8,
            Type::ULong => 8,
            Type::Pointer(_) => 8,
            Type::Array(elem, len) => elem.size() * (*len as i32),
            Type::Void | Type::Struct(_) | Type::Union(_) => 0,
            Type::Enum(_) => 4,
        }
    }

    pub fn element_size(&self) -> Option<i32> {
        match self {
            Type::Pointer(pointee) => Some(pointee.size()),
            Type::Array(elem, _) => Some(elem.size()),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Program(stmts) => {
                writeln!(f, "Program")?;
                for stmt in stmts {
                    writeln!(f, "  {}", stmt)?;
                }
                Ok(())
            }
            AstNode::Function { name, .. } => write!(f, "Function({})", name),
            AstNode::Block(_) => write!(f, "Block"),
            AstNode::Return(_) => write!(f, "Return"),
            AstNode::IfStatement { .. } => write!(f, "IfStatement"),
            AstNode::WhileLoop { .. } => write!(f, "WhileLoop"),
            AstNode::DoWhileLoop { .. } => write!(f, "DoWhileLoop"),
            AstNode::ForLoop { .. } => write!(f, "ForLoop"),
            AstNode::StructDef { name, .. } => write!(f, "StructDef({})", name),
            AstNode::UnionDef { name, .. } => write!(f, "UnionDef({})", name),
            AstNode::EnumDef { name, .. } => write!(f, "EnumDef({})", name),
            AstNode::VarDecl { name, .. } => write!(f, "VarDecl({})", name),
            AstNode::Assignment { .. } => write!(f, "Assignment"),
            AstNode::Variable(name) => write!(f, "Variable({})", name),
            AstNode::FunctionCall { name, .. } => write!(f, "FunctionCall({})", name),
            AstNode::BinaryOp { op, .. } => write!(f, "BinaryOp({:?})", op),
            AstNode::TernaryOp { .. } => write!(f, "TernaryOp"),
            AstNode::UnaryOp { op, .. } => write!(f, "UnaryOp({:?})", op),
            AstNode::PrefixIncrement(_) => write!(f, "PrefixIncrement"),
            AstNode::PrefixDecrement(_) => write!(f, "PrefixDecrement"),
            AstNode::PostfixIncrement(_) => write!(f, "PostfixIncrement"),
            AstNode::PostfixDecrement(_) => write!(f, "PostfixDecrement"),
            AstNode::AddressOf(_) => write!(f, "AddressOf"),
            AstNode::Dereference(_) => write!(f, "Dereference"),
            AstNode::ArrayIndex { .. } => write!(f, "ArrayIndex"),
            AstNode::ArrayInit(_) => write!(f, "ArrayInit"),
            AstNode::StructInit(_) => write!(f, "StructInit"),
            AstNode::MemberAccess { member, .. } => write!(f, "MemberAccess({})", member),
            AstNode::SizeOfType(_) => write!(f, "SizeOfType"),
            AstNode::SizeOfExpr(_) => write!(f, "SizeOfExpr"),
            AstNode::Cast { target_type, .. } => write!(f, "Cast({:?})", target_type),
            AstNode::IntLiteral(n) => write!(f, "IntLiteral({})", n),
            AstNode::StringLiteral(s) => write!(f, "StringLiteral(\"{}\")", s),
            AstNode::CharLiteral(c) => write!(f, "CharLiteral({})", c),
            AstNode::Break => write!(f, "Break"),
            AstNode::Continue => write!(f, "Continue"),
        }
    }
}
