use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone)]
pub struct ResolveError {
    pub line: usize,
    pub message: String,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, usize>>,
    loop_depth: usize,
    function_depth: usize,
}

impl Resolver {
    pub fn new() -> Self {
        let mut resolver = Self {
            scopes: vec![HashMap::new()],
            loop_depth: 0,
            function_depth: 0,
        };
        resolver.define_native_functions();
        resolver
    }

    fn define_native_functions(&mut self) {
        for (name, _native_function) in crate::interpreter::native_function::all_native_functions()
        {
            self.define(name, 0).unwrap();
        }
    }

    fn define(&mut self, name: &str, line: usize) -> Result<(), ResolveError> {
        let current_scope = self.scopes.last_mut().expect("scopes is empty");
        let slot_index = current_scope.len();

        match current_scope.entry(name.to_string()) {
            Entry::Vacant(e) => {
                e.insert(slot_index);
                Ok(())
            }
            Entry::Occupied(e) => Err(ResolveError {
                line,
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
        let line = declaration.line;
        match &mut declaration.kind {
            // we're merging these two together because mutability will be checked by the
            // interpreter anyway
            DeclarationKind::Let { name, initializer }
            | DeclarationKind::Var { name, initializer } => {
                self.resolve_expression(initializer)?;
                self.define(name, line)?;
            }
            DeclarationKind::Fn { name, params, body } => {
                self.define(name, line)?;
                self.push_scope();
                self.function_depth += 1;

                let result = (|| {
                    for param in params {
                        self.define(param, line)?;
                    }
                    self.resolve_expression(body)
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
        let line = expression.line;
        match &mut expression.kind {
            ExprKind::Identifier { name, resolved } => {
                if let Some(resolution) = self.lookup(name) {
                    *resolved = Some(resolution);
                    Ok(())
                } else {
                    Err(ResolveError {
                        line,
                        message: format!("Undefined variable '{}'", name),
                    })
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
                    self.define(variable, line)?;
                    self.resolve_expression(body)
                })();

                self.loop_depth -= 1;
                self.pop_scope();
                result
            }
            ExprKind::Call { callee, arguments } => {
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
                        line,
                        message: "'break' outside of loop".into(),
                    })
                } else {
                    Ok(())
                }
            }
            ExprKind::Continue => {
                if self.loop_depth == 0 {
                    Err(ResolveError {
                        line,
                        message: "'continue' outside of loop".into(),
                    })
                } else {
                    Ok(())
                }
            }
            ExprKind::Return(value) => {
                if self.function_depth == 0 {
                    Err(ResolveError {
                        line,
                        message: "'return' outside of function".into(),
                    })
                } else {
                    if let Some(expr) = value {
                        self.resolve_expression(expr)?;
                    }
                    Ok(())
                }
            }
            ExprKind::Num(_) | ExprKind::Str(_) | ExprKind::Bool(_) | ExprKind::Null => Ok(()),
            ExprKind::Pipe { .. } => {
                todo!("pipe operator not yet implemented in resolver")
            }
        }
    }
}
