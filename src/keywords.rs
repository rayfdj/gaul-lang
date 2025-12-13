use std::collections::HashMap;
use std::fs;
use anyhow::Result;

use crate::scanner::token::TokenType;

pub fn load_keywords(path: Option<&str>) -> Result<HashMap<String, TokenType>> {
    let map: HashMap<String, String> = match path {
        Some(p) => {
            let contents = fs::read_to_string(p)?;
            serde_json::from_str(&contents)?
        }
        None => default_keywords(),
    };

    let mut keywords = HashMap::new();
    for (key, value) in map {
        if let Some(token_type) = str_to_token_type(&key) {
            keywords.insert(value, token_type);
        }
    }

    Ok(keywords)
}

fn default_keywords() -> HashMap<String, String> {
    HashMap::from([
        ("and".into(), "and".into()),
        ("or".into(), "or".into()),
        ("if".into(), "if".into()),
        ("else".into(), "else".into()),
        ("while".into(), "while".into()),
        ("for".into(), "for".into()),
        ("in".into(), "in".into()),
        ("function".into(), "fn".into()),
        ("return".into(), "return".into()),
        ("true".into(), "true".into()),
        ("false".into(), "false".into()),
        ("null".into(), "null".into()),
        ("let".into(), "let".into()),
        ("var".into(), "var".into()),
        ("class".into(), "class".into()),
        ("this".into(), "this".into()),
        ("super".into(), "super".into()),
    ])
}

fn str_to_token_type(s: &str) -> Option<TokenType> {
    match s {
        "and" => Some(TokenType::And),
        "or" => Some(TokenType::Or),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "while" => Some(TokenType::While),
        "for" => Some(TokenType::For),
        "in" => Some(TokenType::In),
        "function" => Some(TokenType::Function),
        "return" => Some(TokenType::Return),
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "null" => Some(TokenType::Null),
        "let" => Some(TokenType::Let),
        "var" => Some(TokenType::Var),
        "class" => Some(TokenType::Class),
        "this" => Some(TokenType::This),
        "super" => Some(TokenType::Super),
        _ => None,
    }
}