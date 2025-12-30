use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Program(Vec<AstNode>),
    Function {
        name: String,
        return_type: Type,
        params: Vec<Parameter>,
        body: Option<Box<AstNode>>, // None for forward declarations
        is_variadic: bool,
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
    TypedefDef {
        name: String,
        target_type: Type,
    },
    VarDecl {
        name: String,
        var_type: Type,
        init: Option<Box<AstNode>>,
        is_extern: bool,
        is_static: bool,
        is_auto: bool,
        is_register: bool,
        is_const: bool,
        is_volatile: bool,
        alignment: Option<i64>,
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
    IndirectCall {
        target: Box<AstNode>,
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
    AlignOfType(Type),
    AlignOfExpr(Box<AstNode>),
    OffsetOf {
        struct_type: Type,
        member: String,
    },
    VaStart {
        ap: Box<AstNode>,
        last_param: Box<AstNode>,
    },
    VaArg {
        ap: Box<AstNode>,
        arg_type: Type,
    },
    VaEnd(Box<AstNode>),
    Cast {
        target_type: Type,
        expr: Box<AstNode>,
    },
    StmtExpr {
        stmts: Vec<AstNode>,
        result: Box<AstNode>,
    },
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    WideStringLiteral(String),
    CharLiteral(i64),
    Label(String),
    Goto(String),
    InlineAsm(String),
    SwitchStatement {
        expr: Box<AstNode>,
        body: Vec<AstNode>,
    },
    Case(i64),
    Default,
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
    Comma,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    LogicalNot,
    Plus,
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
    pub bit_width: Option<u32>, // Some(n) for bit-fields, None for regular fields
    pub alignment: Option<i64>,  // Explicit alignment from _Alignas
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
    Float,
    Double,
    LongDouble,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    FunctionPointer {
        return_type: Box<Type>,
        param_types: Vec<Type>,
    },
    Struct(String),
    Union(String),
    Enum(String),
    VaList,
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
            Type::Float => 4,
            Type::Double => 8,
            Type::LongDouble => 16,  // x86-64 long double is 16 bytes
            Type::Pointer(_) | Type::FunctionPointer { .. } => 8,
            Type::Array(elem, len) => elem.size() * (*len as i32),
            Type::Void | Type::Struct(_) | Type::Union(_) => 0,
            Type::Enum(_) => 4,
            Type::VaList => 24, // Size of __va_list_tag in System V AMD64 ABI
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
            AstNode::TypedefDef { name, .. } => write!(f, "TypedefDef({})", name),
            AstNode::EnumDef { name, .. } => write!(f, "EnumDef({})", name),
            AstNode::VarDecl { name, .. } => write!(f, "VarDecl({})", name),
            AstNode::Assignment { .. } => write!(f, "Assignment"),
            AstNode::Variable(name) => write!(f, "Variable({})", name),
            AstNode::FunctionCall { name, .. } => write!(f, "FunctionCall({})", name),
            AstNode::IndirectCall { .. } => write!(f, "IndirectCall"),
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
            AstNode::AlignOfType(_) => write!(f, "AlignOfType"),
            AstNode::AlignOfExpr(_) => write!(f, "AlignOfExpr"),
            AstNode::OffsetOf { member, .. } => write!(f, "OffsetOf({})", member),
            AstNode::VaStart { .. } => write!(f, "VaStart"),
            AstNode::VaArg { .. } => write!(f, "VaArg"),
            AstNode::VaEnd(_) => write!(f, "VaEnd"),
            AstNode::Cast { target_type, .. } => write!(f, "Cast({:?})", target_type),
            AstNode::StmtExpr { .. } => write!(f, "StmtExpr"),
            AstNode::IntLiteral(n) => write!(f, "IntLiteral({})", n),
            AstNode::FloatLiteral(n) => write!(f, "FloatLiteral({})", n),
            AstNode::StringLiteral(s) => write!(f, "StringLiteral(\"{}\")", s),
            AstNode::WideStringLiteral(s) => write!(f, "WideStringLiteral(L\"{}\")", s),
            AstNode::CharLiteral(c) => write!(f, "CharLiteral({})", c),
            AstNode::Label(name) => write!(f, "Label({})", name),
            AstNode::Goto(name) => write!(f, "Goto({})", name),
            AstNode::InlineAsm(_) => write!(f, "InlineAsm"),
            AstNode::SwitchStatement { .. } => write!(f, "SwitchStatement"),
            AstNode::Case(value) => write!(f, "Case({})", value),
            AstNode::Default => write!(f, "Default"),
            AstNode::Break => write!(f, "Break"),
            AstNode::Continue => write!(f, "Continue"),
        }
    }
}
