use crate::parser::ast::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Str(String),
    Num(f64),
    Bool(bool),
    Null,
    Fn(String, Vec<String>, Expr),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::Num(n1), Self::Num(n2)) => n1 == n2,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            (Self::Null, Self::Null) => true,
            (Self::Fn(_, _, _), Self::Fn(_, _, _)) => panic!("Cannot compare two functions"),
            (_, _) => false,
        }
    }
}