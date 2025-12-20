use crate::parser::ast::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Str(String),
    Num(f64),
    Bool(bool),
    Null,
    Range(f64, f64),
    Fn(String, Vec<String>, Expr),
    NativeFn {
        name: String,
        arity: Option<usize>,
        func: fn(Vec<Value>) -> Result<Value, String>,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::Num(n1), Self::Num(n2)) => n1 == n2,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            (Self::Null, Self::Null) => true,
            (Self::Range(s1, s2), Self::Range(s3, s4)) => (s1 == s3) && (s2 == s4),
            (Self::Fn(_, _, _), Self::Fn(_, _, _)) => panic!("Cannot compare two functions"),
            (Self::NativeFn { .. }, Self::NativeFn { .. }) => {
                panic!("Cannot compare two functions")
            }
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Range(a, b) => write!(f, "[{}..{})", *a as i64, *b as i64),
            Value::Fn(name, _, _) => write!(f, "<fn {}>", name),
            Value::NativeFn { name, .. } => write!(f, "<native fn {}>", name),
        }
    }
}
