use std::collections::HashMap;
use std::sync::Mutex;

use gaul_core::analysis::{self, SymbolTable};
use gaul_core::builtins::{
    NATIVE_FUNCTION_NAMES, NATIVE_FUNCTIONS_INFO, NATIVE_METHODS, STANDARD_MODULES,
};
use gaul_core::keywords::load_keywords;
use gaul_core::parser::Parser;
use gaul_core::resolver::Resolver;
use gaul_core::scanner::Scanner;
use gaul_core::scanner::token::{Token, TokenType};
use gaul_core::span::Span;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

// LSP semantic token types we use, in registration order.
// The index in this array becomes the token type ID in the encoded response.
const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,   // 0
    SemanticTokenType::NUMBER,    // 1
    SemanticTokenType::STRING,    // 2
    SemanticTokenType::COMMENT,   // 3
    SemanticTokenType::VARIABLE,  // 4
    SemanticTokenType::OPERATOR,  // 5
    SemanticTokenType::FUNCTION,  // 6
    SemanticTokenType::PARAMETER, // 7
];

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,     // bit 0
    SemanticTokenModifier::DEFAULT_LIBRARY, // bit 1
];

struct DocumentState {
    tokens: Vec<Token>,
    symbols: Option<SymbolTable>,
}

struct Backend {
    client: Client,
    keywords: HashMap<String, TokenType>,
    documents: Mutex<HashMap<Url, DocumentState>>,
}

impl Backend {
    fn analyze(&self, uri: &Url, text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let scanner = Scanner::new(text, &self.keywords);
        let scan_result = scanner.scan_tokens();

        let mut diagnostics: Vec<Diagnostic> = scan_result
            .errors
            .iter()
            .map(|e| span_to_diagnostic(e.span, &e.message, "scan"))
            .collect();

        let tokens_for_parser = scan_result.tokens_without_comments();

        let mut symbols = None;
        let parser = Parser::new(tokens_for_parser);
        match parser.parse() {
            Ok(mut program) => {
                let mut resolver = Resolver::new();
                if let Err(e) = resolver.resolve(&mut program) {
                    diagnostics.push(span_to_diagnostic(e.span, &e.message, "resolve"));
                } else {
                    symbols = Some(analysis::build_symbol_table(&program));
                }
            }
            Err(errors) => {
                for e in &errors {
                    diagnostics.push(span_to_diagnostic(e.span, &e.message, "parse"));
                }
            }
        }

        // Store document state with all tokens (including comments) for semantic tokens
        let all_tokens = scan_result.tokens;
        {
            let mut docs = self.documents.lock().unwrap();
            docs.insert(
                uri.clone(),
                DocumentState {
                    tokens: all_tokens.clone(),
                    symbols,
                },
            );
        }

        (all_tokens, diagnostics)
    }

    async fn on_change(&self, uri: Url, text: String) {
        let (_, diagnostics) = self.analyze(&uri, &text);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

fn span_to_diagnostic(span: Span, message: &str, source: &str) -> Diagnostic {
    // Gaul spans are 1-indexed, LSP is 0-indexed
    let line = span.line.saturating_sub(1) as u32;
    let col = span.col.saturating_sub(1) as u32;
    Diagnostic {
        range: Range {
            start: Position::new(line, col),
            end: Position::new(line, col + span.length as u32),
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(format!("gaul-{}", source)),
        message: message.to_string(),
        ..Default::default()
    }
}

fn span_to_range(span: Span) -> Range {
    let line = span.line.saturating_sub(1) as u32;
    let col = span.col.saturating_sub(1) as u32;
    Range {
        start: Position::new(line, col),
        end: Position::new(line, col + span.length as u32),
    }
}

fn find_token_at_position(tokens: &[Token], pos: Position) -> Option<&Token> {
    let line = pos.line as usize + 1; // LSP 0-indexed → Gaul 1-indexed
    let col = pos.character as usize + 1;
    tokens
        .iter()
        .find(|t| t.span.line == line && col >= t.span.col && col < t.span.col + t.span.length)
}

/// Find the token immediately before the given LSP position.
fn token_before_position(tokens: &[Token], pos: Position) -> Option<&Token> {
    let line = pos.line as usize + 1;
    let col = pos.character as usize + 1;
    tokens
        .iter()
        .rev()
        .find(|t| t.span.line < line || (t.span.line == line && t.span.col + t.span.length <= col))
}

fn keyword_description(tt: &TokenType) -> Option<(&'static str, &'static str)> {
    match tt {
        TokenType::If => Some(("if", "Conditional expression")),
        TokenType::Else => Some(("else", "Else branch")),
        TokenType::While => Some(("while", "While loop")),
        TokenType::For => Some(("for", "For loop")),
        TokenType::Function => Some(("fn", "Function declaration")),
        TokenType::Return => Some(("return", "Return from function")),
        TokenType::Break => Some(("break", "Break out of loop")),
        TokenType::Continue => Some(("continue", "Skip to next iteration")),
        TokenType::Let => Some(("let", "Immutable binding")),
        TokenType::Var => Some(("var", "Mutable binding")),
        TokenType::Import => Some(("import", "Import from module")),
        TokenType::Export => Some(("export", "Export declaration")),
        TokenType::From => Some(("from", "Module source")),
        TokenType::True => Some(("true", "Boolean true")),
        TokenType::False => Some(("false", "Boolean false")),
        TokenType::Null => Some(("null", "Null value")),
        _ => None,
    }
}

fn semantic_token_type(tt: &TokenType) -> Option<u32> {
    match tt {
        // Keywords
        TokenType::Let
        | TokenType::Var
        | TokenType::If
        | TokenType::Else
        | TokenType::While
        | TokenType::For
        | TokenType::Function
        | TokenType::Return
        | TokenType::Break
        | TokenType::Continue
        | TokenType::Import
        | TokenType::Export
        | TokenType::From
        | TokenType::True
        | TokenType::False
        | TokenType::Null => Some(0), // KEYWORD

        TokenType::Number(_) => Some(1),  // NUMBER
        TokenType::String(_) => Some(2),  // STRING
        TokenType::Comment => Some(3),    // COMMENT
        TokenType::Identifier => Some(4), // VARIABLE (default, refined below)

        // Operators
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Star
        | TokenType::Slash
        | TokenType::Percent
        | TokenType::Assign
        | TokenType::Equal
        | TokenType::NotEqual
        | TokenType::Less
        | TokenType::LessEqual
        | TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::And
        | TokenType::Or
        | TokenType::Bang
        | TokenType::PlusEqual
        | TokenType::MinusEqual
        | TokenType::StarEqual
        | TokenType::SlashEqual
        | TokenType::Ampersand
        | TokenType::Pipe
        | TokenType::Caret
        | TokenType::Tilde
        | TokenType::LeftShift
        | TokenType::RightShift
        | TokenType::ApproxEqual
        | TokenType::Range => Some(5), // OPERATOR

        // Punctuation, whitespace, EOF — no semantic token
        _ => None,
    }
}

/// Build a lookup map from (line, col) → def_index for symbol references.
fn build_ref_map(symbols: &SymbolTable) -> HashMap<(usize, usize), usize> {
    symbols
        .references
        .iter()
        .map(|r| ((r.span.line, r.span.col), r.def_index))
        .collect()
}

fn encode_semantic_tokens(tokens: &[Token], symbols: Option<&SymbolTable>) -> Vec<SemanticToken> {
    let ref_map = symbols.map(build_ref_map);

    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for token in tokens {
        let (token_type, modifiers) = if token.token_type == TokenType::Identifier {
            // Try to refine identifier type using symbol table
            if let Some(ref ref_map) = ref_map {
                let symbols = symbols.unwrap();
                let key = (token.span.line, token.span.col);
                if let Some(&def_index) = ref_map.get(&key) {
                    match symbols.definitions[def_index].kind {
                        analysis::SymbolKind::Function => (6u32, 0u32), // FUNCTION
                        analysis::SymbolKind::Parameter => (7, 0),      // PARAMETER
                        _ => (4, 0),                                    // VARIABLE
                    }
                } else if NATIVE_FUNCTION_NAMES.contains(&token.lexeme.as_str()) {
                    (6, 0b10) // FUNCTION + DEFAULT_LIBRARY (bit 1)
                } else {
                    (4, 0) // VARIABLE
                }
            } else {
                (4, 0) // VARIABLE (no symbol info available)
            }
        } else {
            match semantic_token_type(&token.token_type) {
                Some(t) => (t, 0u32),
                None => continue,
            }
        };

        let line = token.span.line.saturating_sub(1) as u32;
        let start = token.span.col.saturating_sub(1) as u32;
        let length = token.span.length as u32;

        if length == 0 {
            continue;
        }

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });

        prev_line = line;
        prev_start = start;
    }

    result
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: TOKEN_TYPES.to_vec(),
                                token_modifiers: TOKEN_MODIFIERS.to_vec(),
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), "\"".into()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "gaul-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.on_change(uri, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // We use FULL sync, so there's exactly one change with the full text
        if let Some(change) = params.content_changes.into_iter().next() {
            self.on_change(uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        {
            let mut docs = self.documents.lock().unwrap();
            docs.remove(&uri);
        }
        // Clear diagnostics for the closed file
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let encoded = encode_semantic_tokens(&doc.tokens, doc.symbols.as_ref());
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: encoded,
        })))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };
        let Some(symbols) = &doc.symbols else {
            return Ok(None);
        };

        // Find which token the cursor is on
        let Some(token) = find_token_at_position(&doc.tokens, pos) else {
            return Ok(None);
        };

        // Look up this token's span in symbol references
        let token_key = (token.span.line, token.span.col);
        let Some(sym_ref) = symbols
            .references
            .iter()
            .find(|r| (r.span.line, r.span.col) == token_key)
        else {
            return Ok(None);
        };

        let def = &symbols.definitions[sym_ref.def_index];
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range: span_to_range(def.span),
        })))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };
        let Some(symbols) = &doc.symbols else {
            return Ok(None);
        };

        #[allow(deprecated)] // DocumentSymbol.deprecated field is itself deprecated
        let lsp_symbols: Vec<DocumentSymbol> = symbols
            .definitions
            .iter()
            .filter(|d| {
                matches!(
                    d.kind,
                    analysis::SymbolKind::Function
                        | analysis::SymbolKind::Variable
                        | analysis::SymbolKind::Import
                )
            })
            .map(|d| {
                let range = span_to_range(d.span);
                DocumentSymbol {
                    name: d.name.clone(),
                    detail: Some(d.detail.clone()),
                    kind: match d.kind {
                        analysis::SymbolKind::Function => SymbolKind::FUNCTION,
                        _ => SymbolKind::VARIABLE,
                    },
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                }
            })
            .collect();

        Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let trigger = params
            .context
            .as_ref()
            .and_then(|c| c.trigger_character.as_deref());

        // (A) Dot completion — return all methods
        if trigger == Some(".") {
            let items: Vec<CompletionItem> = NATIVE_METHODS
                .iter()
                .map(|m| {
                    let sig = format!("{}.{}{}", m.receiver_type, m.name, m.params);
                    CompletionItem {
                        label: m.name.to_string(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!("{} -> {}", sig, m.returns)),
                        documentation: Some(Documentation::String(m.doc.to_string())),
                        ..Default::default()
                    }
                })
                .collect();
            return Ok(Some(CompletionResponse::Array(items)));
        }

        // (B) Import path completion — trigger char is `"`
        if trigger == Some("\"") {
            // Check if a recent token is `from`
            let prev = token_before_position(&doc.tokens, pos);
            let is_import = prev.is_some_and(|t| {
                // The `"` itself isn't tokenized yet (being typed), so prev should be `from`
                // or there might be whitespace tokens. Walk backwards to find `from`.
                t.token_type == TokenType::From
            }) || {
                // Walk backwards through tokens looking for `from` keyword
                let line = pos.line as usize + 1;
                let col = pos.character as usize + 1;
                doc.tokens
                    .iter()
                    .rev()
                    .filter(|t| {
                        t.token_type != TokenType::Newline
                            && (t.span.line < line
                                || (t.span.line == line && t.span.col + t.span.length <= col))
                    })
                    .take(3)
                    .any(|t| t.token_type == TokenType::From)
            };

            if is_import {
                let mut items: Vec<CompletionItem> = STANDARD_MODULES
                    .iter()
                    .map(|m| CompletionItem {
                        label: m.to_string(),
                        kind: Some(CompletionItemKind::MODULE),
                        ..Default::default()
                    })
                    .collect();

                // Scan directory for .gaul files
                if let Ok(path) = uri.to_file_path()
                    && let Some(dir) = path.parent()
                    && let Ok(entries) = std::fs::read_dir(dir)
                {
                    for entry in entries.flatten() {
                        let p = entry.path();
                        if p.extension().is_some_and(|e| e == "gaul")
                            && let Some(stem) = p.file_stem().and_then(|s| s.to_str())
                            && p.as_path() != path.as_path()
                        {
                            items.push(CompletionItem {
                                label: stem.to_string(),
                                kind: Some(CompletionItemKind::FILE),
                                ..Default::default()
                            });
                        }
                    }
                }

                return Ok(Some(CompletionResponse::Array(items)));
            }
        }

        // (C) Normal completion — symbols, native functions, keywords
        let mut items: Vec<CompletionItem> = Vec::new();

        // User-defined symbols
        if let Some(symbols) = &doc.symbols {
            for def in &symbols.definitions {
                let kind = match def.kind {
                    analysis::SymbolKind::Function => CompletionItemKind::FUNCTION,
                    _ => CompletionItemKind::VARIABLE,
                };
                items.push(CompletionItem {
                    label: def.name.clone(),
                    kind: Some(kind),
                    detail: Some(def.detail.clone()),
                    ..Default::default()
                });
            }
        }

        // Native functions
        for info in NATIVE_FUNCTIONS_INFO {
            items.push(CompletionItem {
                label: info.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(format!("{}{} -> {}", info.name, info.params, info.returns)),
                documentation: Some(Documentation::String(info.doc.to_string())),
                ..Default::default()
            });
        }

        // Keywords
        for (lexeme, tt) in &self.keywords {
            let kind = match tt {
                TokenType::True | TokenType::False | TokenType::Null => {
                    CompletionItemKind::CONSTANT
                }
                _ => CompletionItemKind::KEYWORD,
            };
            items.push(CompletionItem {
                label: lexeme.clone(),
                kind: Some(kind),
                ..Default::default()
            });
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let Some(token) = find_token_at_position(&doc.tokens, pos) else {
            return Ok(None);
        };

        // 1. Keyword hover
        if let Some((english, desc)) = keyword_description(&token.token_type) {
            let text = if token.lexeme != english {
                // Localized keyword — show both
                format!("**{}** — {} (`{}`)", token.lexeme, desc, english)
            } else {
                format!("**{}** — {}", english, desc)
            };
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: text,
                }),
                range: Some(span_to_range(token.span)),
            }));
        }

        // Only identifiers from here on
        if token.token_type != TokenType::Identifier {
            return Ok(None);
        }

        // 2. Method call — identifier preceded by a dot
        let prev = token_before_position(&doc.tokens, pos);
        if prev.is_some_and(|t| t.token_type == TokenType::Dot) {
            let methods: Vec<String> = NATIVE_METHODS
                .iter()
                .filter(|m| m.name == token.lexeme)
                .map(|m| {
                    format!(
                        "```\n{}.{}{} -> {}\n```\n{}",
                        m.receiver_type, m.name, m.params, m.returns, m.doc
                    )
                })
                .collect();

            if !methods.is_empty() {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: methods.join("\n\n---\n\n"),
                    }),
                    range: Some(span_to_range(token.span)),
                }));
            }
        }

        // 3. Identifier that's a SymbolRef — show detail
        if let Some(symbols) = &doc.symbols {
            let token_key = (token.span.line, token.span.col);
            if let Some(sym_ref) = symbols
                .references
                .iter()
                .find(|r| (r.span.line, r.span.col) == token_key)
            {
                let def = &symbols.definitions[sym_ref.def_index];
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```\n{}\n```", def.detail),
                    }),
                    range: Some(span_to_range(token.span)),
                }));
            }

            // Also check if the token IS a definition site
            if let Some(def) = symbols
                .definitions
                .iter()
                .find(|d| d.span.line == token.span.line && d.span.col == token.span.col)
            {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```\n{}\n```", def.detail),
                    }),
                    range: Some(span_to_range(token.span)),
                }));
            }
        }

        // 4. Native function
        if let Some(info) = NATIVE_FUNCTIONS_INFO
            .iter()
            .find(|f| f.name == token.lexeme)
        {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "```\n{}{} -> {}\n```\n{}",
                        info.name, info.params, info.returns, info.doc
                    ),
                }),
                range: Some(span_to_range(token.span)),
            }));
        }

        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    // Load keywords: check for .gaul-keywords.json in cwd, fall back to defaults
    let keywords_path = std::env::current_dir()
        .ok()
        .map(|d| d.join(".gaul-keywords.json"))
        .filter(|p| p.exists());
    let keywords = load_keywords(keywords_path.as_deref().and_then(|p| p.to_str()))
        .unwrap_or_else(|_| load_keywords(None).expect("default keywords must load"));

    let (service, socket) = LspService::new(|client| Backend {
        client,
        keywords,
        documents: Mutex::new(HashMap::new()),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
