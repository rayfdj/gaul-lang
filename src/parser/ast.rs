use crate::scanner::token::Token;

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Let { name: String, initializer: Expr },
    Var { name: String, initializer: Expr },
    Fn { name: String, params: Vec<String>, body: Expr },
    ExprStmt(Expr),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    // Control Flow Expressions
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        variable: String,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
    Return(Option<Box<Expr>>),

    // Operator Expressions
    Assign {
        target: Box<Expr>, // can be a variable, or a field
        value: Box<Expr>,
    },
    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        operand: Box<Expr>,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: String,
    },

    // Primary Expressions
    Number(f64),
    Str(String),
    Bool(bool),
    Null,
    Identifier {
        name: String,
        resolved: Option<(usize, usize)>
    },

    // Blocks
    Block {
        declarations: Vec<Declaration>,
        expr: Option<Box<Expr>>,
    },
}