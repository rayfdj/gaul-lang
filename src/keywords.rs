use anyhow::Result;
use std::collections::HashMap;
use std::fs;

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

pub fn default_keywords() -> HashMap<String, String> {
    // The default mapping, from "concept words" to "keywords"
    // For example, "function" is a concept, but by default the keyword we're using in Gaul is "fn"
    // If you're customizing the keywords, say you want to use "fungsi", then you'll map
    // "function" to "fungsi" in the keywords file.
    HashMap::from([
        ("if".into(), "if".into()),
        ("else".into(), "else".into()),
        ("while".into(), "while".into()),
        ("for".into(), "for".into()),
        ("function".into(), "fn".into()),
        ("return".into(), "return".into()),
        ("true".into(), "true".into()),
        ("false".into(), "false".into()),
        ("null".into(), "null".into()),
        ("let".into(), "let".into()),
        ("var".into(), "var".into()),
        ("break".into(), "break".into()),
        ("continue".into(), "continue".into()),
        ("import".into(), "import".into()),
        ("export".into(), "export".into()),
        ("from".into(), "from".into()),
    ])
}

fn str_to_token_type(s: &str) -> Option<TokenType> {
    // this is where the concept words (not the actual keywords, those are mapped above) are mapped
    // to Token Type.
    match s {
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "while" => Some(TokenType::While),
        "for" => Some(TokenType::For),
        "function" => Some(TokenType::Function),
        "return" => Some(TokenType::Return),
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "null" => Some(TokenType::Null),
        "let" => Some(TokenType::Let),
        "var" => Some(TokenType::Var),
        "break" => Some(TokenType::Break),
        "continue" => Some(TokenType::Continue),
        "import" => Some(TokenType::Import),
        "export" => Some(TokenType::Export),
        "from" => Some(TokenType::From),
        _ => None,
    }
}
