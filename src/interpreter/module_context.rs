use crate::scanner::token::TokenType;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;

use crate::interpreter::value::Value;

pub struct ModuleContext {
    pub current_file: Option<PathBuf>,
    pub keywords: HashMap<String, TokenType>,
    pub cache: Rc<RefCell<HashMap<PathBuf, Rc<HashMap<String, Value>>>>>,
    pub loading: Rc<RefCell<HashSet<PathBuf>>>,
}

impl ModuleContext {
    pub fn new(current_file: Option<PathBuf>, keywords: HashMap<String, TokenType>) -> Self {
        Self {
            current_file,
            keywords,
            cache: Rc::new(RefCell::new(HashMap::new())),
            loading: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    pub fn child(&self, file: PathBuf) -> Self {
        Self {
            current_file: Some(file),
            keywords: self.keywords.clone(),
            cache: Rc::clone(&self.cache),
            loading: Rc::clone(&self.loading),
        }
    }
}
