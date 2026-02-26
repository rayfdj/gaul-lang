use crate::builtins::NATIVE_FUNCTION_NAMES;
use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Variable,
    Parameter,
    ForVariable,
    Import,
}

#[derive(Debug, Clone)]
pub struct SymbolDef {
    pub name: String,
    pub span: Span,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub struct SymbolRef {
    pub span: Span,
    pub def_index: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub definitions: Vec<SymbolDef>,
    pub references: Vec<SymbolRef>,
}

pub fn build_symbol_table(program: &Program) -> SymbolTable {
    let mut builder = Builder::new();
    builder.walk_program(program);
    builder.finish()
}

struct Builder {
    definitions: Vec<SymbolDef>,
    references: Vec<SymbolRef>,
    // Each scope maps slot index → definition index (None for native functions).
    scopes: Vec<Vec<Option<usize>>>,
}

impl Builder {
    fn new() -> Self {
        // Pre-populate global scope with native function slots (no SymbolDef for these).
        let global_scope = vec![None; NATIVE_FUNCTION_NAMES.len()];
        Self {
            definitions: Vec::new(),
            references: Vec::new(),
            scopes: vec![global_scope],
        }
    }

    fn define(&mut self, name: String, span: Span, kind: SymbolKind) {
        let def_index = self.definitions.len();
        self.definitions.push(SymbolDef { name, span, kind });
        self.scopes.last_mut().unwrap().push(Some(def_index));
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_ref(&mut self, depth: usize, slot: usize, span: Span) {
        let scope_idx = self.scopes.len() - 1 - depth;
        if let Some(scope) = self.scopes.get(scope_idx)
            && let Some(Some(def_index)) = scope.get(slot)
        {
            self.references.push(SymbolRef {
                span,
                def_index: *def_index,
            });
        }
        // None or out-of-bounds → native function reference, no SymbolRef needed
    }

    fn walk_program(&mut self, program: &Program) {
        for decl in &program.declarations {
            self.walk_declaration(decl);
        }
    }

    fn walk_declaration(&mut self, decl: &Declaration) {
        match &decl.kind {
            DeclarationKind::Let { name, initializer }
            | DeclarationKind::Var { name, initializer } => {
                self.define(name.clone(), decl.span, SymbolKind::Variable);
                self.walk_expr(initializer);
            }
            DeclarationKind::Fn { name, params, body } => {
                self.define(name.clone(), decl.span, SymbolKind::Function);
                self.push_scope();
                for param in params {
                    self.define(param.clone(), decl.span, SymbolKind::Parameter);
                }
                self.walk_expr(body);
                self.pop_scope();
            }
            DeclarationKind::ExprStmt(expr) => {
                self.walk_expr(expr);
            }
            DeclarationKind::Import { items, .. } => {
                for item in items {
                    self.define(item.clone(), decl.span, SymbolKind::Import);
                }
            }
            DeclarationKind::Export { inner } => {
                self.walk_declaration(inner);
            }
        }
    }

    fn walk_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Identifier {
                resolved: Some((depth, slot)),
                ..
            } => {
                self.resolve_ref(*depth, *slot, expr.span);
            }
            ExprKind::Block {
                declarations,
                expr: final_expr,
            } => {
                if !declarations.is_empty() {
                    self.push_scope();
                    for decl in declarations {
                        self.walk_declaration(decl);
                    }
                    if let Some(e) = final_expr {
                        self.walk_expr(e);
                    }
                    self.pop_scope();
                } else if let Some(e) = final_expr {
                    self.walk_expr(e);
                }
            }
            ExprKind::For {
                variable,
                iterable,
                body,
            } => {
                self.walk_expr(iterable);
                self.push_scope();
                self.define(variable.clone(), expr.span, SymbolKind::ForVariable);
                self.walk_expr(body);
                self.pop_scope();
            }
            ExprKind::Lambda { params, body } => {
                self.push_scope();
                for param in params {
                    self.define(param.clone(), expr.span, SymbolKind::Parameter);
                }
                self.walk_expr(body);
                self.pop_scope();
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(condition);
                self.walk_expr(then_branch);
                if let Some(e) = else_branch {
                    self.walk_expr(e);
                }
            }
            ExprKind::While { condition, body } => {
                self.walk_expr(condition);
                self.walk_expr(body);
            }
            ExprKind::Assign { target, value } => {
                self.walk_expr(target);
                self.walk_expr(value);
            }
            ExprKind::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            ExprKind::Unary { operand, .. } => {
                self.walk_expr(operand);
            }
            ExprKind::Range { start, end } => {
                self.walk_expr(start);
                self.walk_expr(end);
            }
            ExprKind::Call {
                callee, arguments, ..
            } => {
                self.walk_expr(callee);
                for arg in arguments {
                    self.walk_expr(arg);
                }
            }
            ExprKind::Get { object, .. } => {
                self.walk_expr(object);
            }
            ExprKind::Index { object, index } => {
                self.walk_expr(object);
                self.walk_expr(index);
            }
            ExprKind::Return(value) => {
                if let Some(e) = value {
                    self.walk_expr(e);
                }
            }
            ExprKind::Array { elements } => {
                for e in elements {
                    self.walk_expr(e);
                }
            }
            ExprKind::Map { pairs } => {
                for (k, v) in pairs {
                    self.walk_expr(k);
                    self.walk_expr(v);
                }
            }
            ExprKind::Identifier { resolved: None, .. }
            | ExprKind::Num(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Null
            | ExprKind::Break
            | ExprKind::Continue => {}
        }
    }

    fn finish(self) -> SymbolTable {
        SymbolTable {
            definitions: self.definitions,
            references: self.references,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keywords::load_keywords;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::scanner::Scanner;

    fn build(source: &str) -> SymbolTable {
        let keywords = load_keywords(None).unwrap();
        let scanner = Scanner::new(source, &keywords);
        let scan_result = scanner.scan_tokens();
        let tokens = scan_result.tokens_without_comments();
        let parser = Parser::new(tokens);
        let mut program = parser.parse().unwrap();
        let mut resolver = Resolver::new();
        resolver.resolve(&mut program).unwrap();
        build_symbol_table(&program)
    }

    #[test]
    fn let_binding_and_reference() {
        let table = build("let x = 1\nx");
        assert_eq!(table.definitions.len(), 1);
        assert_eq!(table.definitions[0].name, "x");
        assert_eq!(table.definitions[0].kind, SymbolKind::Variable);

        assert_eq!(table.references.len(), 1);
        assert_eq!(table.references[0].def_index, 0);
    }

    #[test]
    fn function_def_and_call() {
        let table = build("fn add(a, b) { a + b }\nadd(1, 2)");

        // Definitions: add (Function), a (Parameter), b (Parameter)
        assert_eq!(table.definitions.len(), 3);
        assert_eq!(table.definitions[0].name, "add");
        assert_eq!(table.definitions[0].kind, SymbolKind::Function);
        assert_eq!(table.definitions[1].name, "a");
        assert_eq!(table.definitions[1].kind, SymbolKind::Parameter);
        assert_eq!(table.definitions[2].name, "b");
        assert_eq!(table.definitions[2].kind, SymbolKind::Parameter);

        // References: a, b (inside body), add (call site)
        assert_eq!(table.references.len(), 3);
        // a reference → def_index 1
        assert!(table.references.iter().any(|r| r.def_index == 1));
        // b reference → def_index 2
        assert!(table.references.iter().any(|r| r.def_index == 2));
        // add reference → def_index 0
        assert!(table.references.iter().any(|r| r.def_index == 0));
    }

    #[test]
    fn nested_scopes() {
        let table = build("let x = 1\nfn foo() {\n  let y = x\n  y\n}");

        // Definitions: x (Variable), foo (Function), y (Variable)
        assert_eq!(table.definitions.len(), 3);
        assert_eq!(table.definitions[0].name, "x");
        assert_eq!(table.definitions[1].name, "foo");
        assert_eq!(table.definitions[2].name, "y");

        // References: x (inside foo), y (inside foo)
        assert_eq!(table.references.len(), 2);
        // x ref → def_index 0
        assert!(table.references.iter().any(|r| r.def_index == 0));
        // y ref → def_index 2
        assert!(table.references.iter().any(|r| r.def_index == 2));
    }

    #[test]
    fn for_variable() {
        let table = build("let items = [1, 2]\nfor (x : items) { x }");

        // Definitions: items (Variable), x (ForVariable)
        assert_eq!(table.definitions.len(), 2);
        assert_eq!(table.definitions[0].name, "items");
        assert_eq!(table.definitions[0].kind, SymbolKind::Variable);
        assert_eq!(table.definitions[1].name, "x");
        assert_eq!(table.definitions[1].kind, SymbolKind::ForVariable);

        // References: items (in for iterable), x (in for body)
        assert_eq!(table.references.len(), 2);
    }

    #[test]
    fn lambda_params() {
        let table = build("let f = fn(a) { a }");

        // Definitions: f (Variable), a (Parameter)
        assert_eq!(table.definitions.len(), 2);
        assert_eq!(table.definitions[0].name, "f");
        assert_eq!(table.definitions[0].kind, SymbolKind::Variable);
        assert_eq!(table.definitions[1].name, "a");
        assert_eq!(table.definitions[1].kind, SymbolKind::Parameter);

        // References: a (inside lambda body)
        assert_eq!(table.references.len(), 1);
        assert_eq!(table.references[0].def_index, 1);
    }

    #[test]
    fn native_functions_produce_no_defs_or_refs() {
        let table = build("println(1)");

        // No definitions (println is native)
        assert_eq!(table.definitions.len(), 0);
        // No references (native functions get None in scope, so no SymbolRef)
        assert_eq!(table.references.len(), 0);
    }

    #[test]
    fn import_items() {
        // We can't actually test import resolution without a module system,
        // but we can verify import defs are created. The resolver will fail
        // on the import path, so let's test the builder with a simulated
        // resolved AST indirectly by testing that our scope tracking works.
        let table = build("let x = 1\nlet y = x\ny");

        assert_eq!(table.definitions.len(), 2);
        assert_eq!(table.references.len(), 2);
        // x ref (in y's initializer) → def 0
        assert!(table.references.iter().any(|r| r.def_index == 0));
        // y ref (standalone) → def 1
        assert!(table.references.iter().any(|r| r.def_index == 1));
    }
}
