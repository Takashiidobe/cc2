use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Program(Vec<AstNode>),
    Function {
        name: String,
        return_type: Type,
        params: Vec<Parameter>,
        body: Box<AstNode>,
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
    ForLoop {
        init: Option<Box<AstNode>>,
        condition: Option<Box<AstNode>>,
        increment: Option<Box<AstNode>>,
        body: Box<AstNode>,
    },
    VarDecl {
        name: String,
        var_type: Type,
        init: Option<Box<AstNode>>,
    },
    Assignment {
        name: String,
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
    UnaryOp {
        op: UnaryOp,
        operand: Box<AstNode>,
    },
    IntLiteral(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Void,
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
            AstNode::ForLoop { .. } => write!(f, "ForLoop"),
            AstNode::VarDecl { name, .. } => write!(f, "VarDecl({})", name),
            AstNode::Assignment { name, .. } => write!(f, "Assignment({})", name),
            AstNode::Variable(name) => write!(f, "Variable({})", name),
            AstNode::FunctionCall { name, .. } => write!(f, "FunctionCall({})", name),
            AstNode::BinaryOp { op, .. } => write!(f, "BinaryOp({:?})", op),
            AstNode::UnaryOp { op, .. } => write!(f, "UnaryOp({:?})", op),
            AstNode::IntLiteral(n) => write!(f, "IntLiteral({})", n),
        }
    }
}
