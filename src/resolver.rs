use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::span::Span;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ResolveError {
    pub span: Span,
    pub message: String,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, usize>>,
    loop_depth: usize,
    function_depth: usize,
}

impl Default for Resolver {
    fn default() -> Self {
        let mut resolver = Self {
            scopes: vec![HashMap::new()],
            loop_depth: 0,
            function_depth: 0,
        };
        resolver.define_native_functions();
        resolver
    }
}

impl Resolver {
    pub fn new() -> Self {
        Self::default()
    }

    fn define_native_functions(&mut self) {
        for (name, _native_function) in crate::interpreter::native_function::all_native_functions()
        {
            self.define(name, Span::default()).unwrap();
        }
    }

    fn define(&mut self, name: &str, span: Span) -> Result<(), ResolveError> {
        let current_scope = self.scopes.last_mut().expect("scopes is empty");
        let slot_index = current_scope.len();

        match current_scope.entry(name.to_string()) {
            Entry::Vacant(e) => {
                e.insert(slot_index);
                Ok(())
            }
            Entry::Occupied(e) => Err(ResolveError {
                span,
                message: format!("Variable '{}' already defined in this scope", e.key(),),
            }),
        }
    }

    fn lookup(&self, name: &str) -> Option<(usize, usize)> {
        //println!("RESOLVE lookup '{}', scopes.len()={}", name, self.scopes.len());
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(slot_index) = scope.get(name) {
                return Some((i, *slot_index));
            }
        }
        None
    }

    fn find_closest(&self, name: &str) -> Option<String> {
        let mut best: Option<(usize, String)> = None;
        for scope in self.scopes.iter() {
            for candidate in scope.keys() {
                let dist = strsim::levenshtein(name, candidate);
                if dist <= 2 {
                    match &best {
                        Some((d, _)) if dist < *d => best = Some((dist, candidate.clone())),
                        None => best = Some((dist, candidate.clone())),
                        _ => {}
                    }
                }
            }
        }
        best.map(|(_, s)| s)
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve(&mut self, program: &mut Program) -> Result<(), ResolveError> {
        for declaration in program.declarations.iter_mut() {
            self.resolve_declaration(declaration)?;
        }
        Ok(())
    }

    fn resolve_declaration(&mut self, declaration: &mut Declaration) -> Result<(), ResolveError> {
        let span = declaration.span;
        match &mut declaration.kind {
            // we're merging these two together because mutability will be checked by the
            // interpreter anyway
            DeclarationKind::Let { name, initializer }
            | DeclarationKind::Var { name, initializer } => {
                self.define(name, span)?;
                self.resolve_expression(initializer)?;
            }
            DeclarationKind::Fn { name, params, body } => {
                self.define(name, span)?;
                self.push_scope();
                self.function_depth += 1;

                let body = Rc::get_mut(body).expect("Rc<Expr> should be unique during resolution");
                let result = (|| {
                    for param in params {
                        self.define(param, span)?;
                    }
                    self.resolve_tail_expression(body)
                })();

                self.function_depth -= 1;
                self.pop_scope();
                result?; // propagate error after cleanup
            }
            DeclarationKind::ExprStmt(expr) => self.resolve_expression(expr)?,
        }
        Ok(())
    }

    fn resolve_expression(&mut self, expression: &mut Expr) -> Result<(), ResolveError> {
        let span = expression.span;
        match &mut expression.kind {
            ExprKind::Identifier { name, resolved } => {
                if let Some(resolution) = self.lookup(name) {
                    *resolved = Some(resolution);
                    Ok(())
                } else {
                    let mut message = format!("Undefined variable '{}'", name);
                    if let Some(suggestion) = self.find_closest(name) {
                        message.push_str(&format!(" (did you mean '{}'?)", suggestion));
                    }
                    Err(ResolveError { span, message })
                }
            }
            ExprKind::Assign { target, value } => {
                self.resolve_expression(target)?;
                self.resolve_expression(value)?;
                Ok(())
            }
            ExprKind::Unary { operand, .. } => {
                self.resolve_expression(operand)?;
                Ok(())
            }
            ExprKind::Binary { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            ExprKind::Block { declarations, expr } => {
                if declarations.is_empty() {
                    // No declarations means no new scope at runtime, so don't push one here either
                    match expr {
                        Some(e) => self.resolve_expression(e),
                        None => Ok(()),
                    }
                } else {
                    self.push_scope();

                    let result = (|| {
                        for declaration in declarations {
                            self.resolve_declaration(declaration)?;
                        }
                        match expr {
                            Some(e) => self.resolve_expression(e),
                            None => Ok(()),
                        }
                    })();

                    self.pop_scope();
                    result
                }
            }
            ExprKind::Array { elements } => {
                for element in elements {
                    self.resolve_expression(element)?;
                }
                Ok(())
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_expression(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_expression(else_branch)?;
                };
                Ok(())
            }
            ExprKind::While { condition, body } => {
                self.resolve_expression(condition)?;
                self.loop_depth += 1;
                let result = self.resolve_expression(body);
                self.loop_depth -= 1;
                result
            }
            ExprKind::Range { start, end } => {
                self.resolve_expression(start)?;
                self.resolve_expression(end)?;
                Ok(())
            }
            ExprKind::For {
                variable,
                iterable,
                body,
            } => {
                self.resolve_expression(iterable)?;
                self.push_scope();
                self.loop_depth += 1;

                let result = (|| {
                    self.define(variable, span)?;
                    self.resolve_expression(body)
                })();

                self.loop_depth -= 1;
                self.pop_scope();
                result
            }
            ExprKind::Call {
                callee, arguments, ..
            } => {
                self.resolve_expression(callee)?;
                for arg in arguments {
                    self.resolve_expression(arg)?;
                }
                Ok(())
            }
            ExprKind::Get { object, .. } => {
                self.resolve_expression(object)?;
                Ok(())
            }
            ExprKind::Break => {
                if self.loop_depth == 0 {
                    Err(ResolveError {
                        span,
                        message: "'break' outside of loop".into(),
                    })
                } else {
                    Ok(())
                }
            }
            ExprKind::Continue => {
                if self.loop_depth == 0 {
                    Err(ResolveError {
                        span,
                        message: "'continue' outside of loop".into(),
                    })
                } else {
                    Ok(())
                }
            }
            ExprKind::Return(value) => {
                if self.function_depth == 0 {
                    Err(ResolveError {
                        span,
                        message: "'return' outside of function".into(),
                    })
                } else {
                    if let Some(expr) = value {
                        self.resolve_expression(expr)?;
                    }
                    Ok(())
                }
            }
            ExprKind::Lambda { params, body } => {
                self.push_scope();
                self.function_depth += 1;

                let body = Rc::get_mut(body).expect("Rc<Expr> should be unique during resolution");
                let result = (|| {
                    for param in params {
                        self.define(param, span)?;
                    }
                    self.resolve_tail_expression(body)
                })();

                self.function_depth -= 1;
                self.pop_scope();
                result // propagate error after cleanup
            }
            ExprKind::Map { pairs } => {
                for (key, value) in pairs {
                    self.resolve_expression(key)?;
                    self.resolve_expression(value)?;
                }
                Ok(())
            }
            ExprKind::Num(_) | ExprKind::Str(_) | ExprKind::Bool(_) | ExprKind::Null => Ok(()),
        }
    }

    fn resolve_tail_expression(&mut self, expression: &mut Expr) -> Result<(), ResolveError> {
        match &mut expression.kind {
            ExprKind::Call {
                callee,
                arguments,
                is_tail_call,
            } => {
                // Don't mark method calls (Get callee) as tail calls
                if !matches!(callee.kind, ExprKind::Get { .. }) {
                    *is_tail_call = true;
                }
                self.resolve_expression(callee)?;
                for arg in arguments {
                    self.resolve_expression(arg)?;
                }
                Ok(())
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_tail_expression(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_tail_expression(else_branch)?;
                }
                Ok(())
            }
            ExprKind::Block { declarations, expr } => {
                if declarations.is_empty() {
                    match expr {
                        Some(e) => self.resolve_tail_expression(e),
                        None => Ok(()),
                    }
                } else {
                    self.push_scope();
                    let result = (|| {
                        for declaration in declarations {
                            self.resolve_declaration(declaration)?;
                        }
                        match expr {
                            Some(e) => self.resolve_tail_expression(e),
                            None => Ok(()),
                        }
                    })();
                    self.pop_scope();
                    result
                }
            }
            ExprKind::Return(value) => {
                if self.function_depth == 0 {
                    Err(ResolveError {
                        span: expression.span,
                        message: "'return' outside of function".into(),
                    })
                } else {
                    if let Some(expr) = value {
                        self.resolve_tail_expression(expr)?;
                    }
                    Ok(())
                }
            }
            // Everything else: delegate to normal resolution
            _ => self.resolve_expression(expression),
        }
    }
}
