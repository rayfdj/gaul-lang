use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, RwLock};

use gaul_core::analysis::{self, SymbolDef, SymbolTable};
use gaul_core::builtins::{
    NATIVE_FUNCTION_NAMES, NATIVE_FUNCTIONS_INFO, NATIVE_METHODS, STANDARD_MODULES,
};
use gaul_core::keywords::load_keywords;
use gaul_core::parser::Parser;
use gaul_core::parser::ast::DeclarationKind;
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
    source: String,
    tokens: Vec<Token>,
    symbols: Option<SymbolTable>,
    import_map: HashMap<String, String>,
}

struct FileIndex {
    exports: Vec<(String, Span)>,
    symbols: Vec<SymbolDef>,
}

struct Backend {
    client: Client,
    keywords: RwLock<HashMap<String, TokenType>>,
    documents: Mutex<HashMap<Url, DocumentState>>,
    workspace_root: Mutex<Option<PathBuf>>,
    workspace_index: Mutex<HashMap<PathBuf, FileIndex>>,
}

impl Backend {
    fn analyze(&self, uri: &Url, text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let keywords = self.keywords.read().unwrap();
        let scanner = Scanner::new(text, &keywords);
        let scan_result = scanner.scan_tokens();

        let mut diagnostics: Vec<Diagnostic> = scan_result
            .errors
            .iter()
            .map(|e| span_to_diagnostic(e.span, &e.message, "scan"))
            .collect();

        let tokens_for_parser = scan_result.tokens_without_comments();

        let mut symbols = None;
        let mut import_map = HashMap::new();
        let parser = Parser::new(tokens_for_parser);
        match parser.parse() {
            Ok(mut program) => {
                // Extract import map before resolver
                for decl in &program.declarations {
                    if let DeclarationKind::Import { path, items } = &decl.kind {
                        for item in items {
                            import_map.insert(item.clone(), path.clone());
                        }
                    }
                }

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
                    source: text.to_string(),
                    tokens: all_tokens.clone(),
                    symbols,
                    import_map,
                },
            );
        }

        (all_tokens, diagnostics)
    }

    async fn on_change(&self, uri: Url, text: String) {
        let (_, diagnostics) = self.analyze(&uri, &text);

        // Update workspace index for this file
        if let Ok(path) = uri.to_file_path() {
            let keywords = self.keywords.read().unwrap().clone();
            if let Some(file_index) = build_file_index(&text, &keywords) {
                self.workspace_index
                    .lock()
                    .unwrap()
                    .insert(path, file_index);
            }
        }

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    fn index_workspace(&self, root: &Path) {
        let keywords = self.keywords.read().unwrap().clone();
        let mut files = Vec::new();
        walk_gaul_files(root, &mut files);

        let mut index = self.workspace_index.lock().unwrap();
        for path in files {
            if let Ok(source) = std::fs::read_to_string(&path)
                && let Some(file_index) = build_file_index(&source, &keywords)
            {
                index.insert(path, file_index);
            }
        }
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

fn build_file_index(source: &str, keywords: &HashMap<String, TokenType>) -> Option<FileIndex> {
    let scanner = Scanner::new(source, keywords);
    let scan_result = scanner.scan_tokens();
    let tokens = scan_result.tokens_without_comments();
    let parser = Parser::new(tokens);
    let mut program = parser.parse().ok()?;
    let mut resolver = Resolver::new();
    resolver.resolve(&mut program).ok()?;

    let exports = program
        .declarations
        .iter()
        .filter_map(|decl| {
            if let DeclarationKind::Export { inner } = &decl.kind {
                let name = match &inner.kind {
                    DeclarationKind::Fn { name, .. }
                    | DeclarationKind::Let { name, .. }
                    | DeclarationKind::Var { name, .. } => name.clone(),
                    _ => return None,
                };
                Some((name, inner.span))
            } else {
                None
            }
        })
        .collect();

    let table = analysis::build_symbol_table(&program);
    Some(FileIndex {
        exports,
        symbols: table.definitions,
    })
}

fn walk_gaul_files(dir: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if let Some(name) = path.file_name().and_then(|n| n.to_str())
                && !name.starts_with('.')
                && name != "target"
                && name != "node_modules"
            {
                walk_gaul_files(&path, files);
            }
        } else if path.extension().is_some_and(|e| e == "gaul") {
            files.push(path);
        }
    }
}

fn compute_folding_ranges(tokens: &[Token]) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();

    // Bracket-based folding ({} and [])
    let mut stack: Vec<(u32, bool)> = Vec::new(); // (0-indexed line, true=brace false=bracket)
    for token in tokens {
        match &token.token_type {
            TokenType::LeftBrace => {
                stack.push((token.span.line.saturating_sub(1) as u32, true));
            }
            TokenType::LeftBracket => {
                stack.push((token.span.line.saturating_sub(1) as u32, false));
            }
            TokenType::RightBrace | TokenType::RightBracket => {
                let is_brace = token.token_type == TokenType::RightBrace;
                while let Some((start_line, open_is_brace)) = stack.pop() {
                    if open_is_brace == is_brace {
                        let end_line = token.span.line.saturating_sub(1) as u32;
                        if start_line != end_line {
                            ranges.push(FoldingRange {
                                start_line,
                                start_character: None,
                                end_line,
                                end_character: None,
                                kind: None,
                                collapsed_text: None,
                            });
                        }
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    // Multi-line comments (block comments containing newlines)
    for token in tokens {
        if token.token_type == TokenType::Comment && token.lexeme.contains('\n') {
            let start_line = token.span.line.saturating_sub(1) as u32;
            let newline_count = token.lexeme.chars().filter(|&c| c == '\n').count() as u32;
            ranges.push(FoldingRange {
                start_line,
                start_character: None,
                end_line: start_line + newline_count,
                end_character: None,
                kind: Some(FoldingRangeKind::Comment),
                collapsed_text: None,
            });
        }
    }

    // Consecutive single-line comments on adjacent lines
    let comment_lines: Vec<u32> = tokens
        .iter()
        .filter(|t| t.token_type == TokenType::Comment && !t.lexeme.contains('\n'))
        .map(|t| t.span.line.saturating_sub(1) as u32)
        .collect();
    let mut i = 0;
    while i < comment_lines.len() {
        let group_start = comment_lines[i];
        let mut group_end = group_start;
        while i + 1 < comment_lines.len() && comment_lines[i + 1] == group_end + 1 {
            i += 1;
            group_end = comment_lines[i];
        }
        if group_start != group_end {
            ranges.push(FoldingRange {
                start_line: group_start,
                start_character: None,
                end_line: group_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Comment),
                collapsed_text: None,
            });
        }
        i += 1;
    }

    // Consecutive import lines
    let import_lines: Vec<u32> = tokens
        .iter()
        .filter(|t| t.token_type == TokenType::Import)
        .map(|t| t.span.line.saturating_sub(1) as u32)
        .collect();
    let mut i = 0;
    while i < import_lines.len() {
        let group_start = import_lines[i];
        let mut group_end = group_start;
        while i + 1 < import_lines.len() && import_lines[i + 1] == group_end + 1 {
            i += 1;
            group_end = import_lines[i];
        }
        if group_start != group_end {
            ranges.push(FoldingRange {
                start_line: group_start,
                start_character: None,
                end_line: group_end,
                end_character: None,
                kind: Some(FoldingRangeKind::Imports),
                collapsed_text: None,
            });
        }
        i += 1;
    }

    ranges
}

struct BracketPair {
    open: Range,
    close: Range,
}

impl BracketPair {
    fn full_range(&self) -> Range {
        Range {
            start: self.open.start,
            end: self.close.end,
        }
    }

    fn content_range(&self) -> Range {
        Range {
            start: self.open.end,
            end: self.close.start,
        }
    }
}

fn bracket_kind(tt: &TokenType) -> Option<(u8, bool)> {
    match tt {
        TokenType::LeftBrace => Some((0, true)),
        TokenType::RightBrace => Some((0, false)),
        TokenType::LeftBracket => Some((1, true)),
        TokenType::RightBracket => Some((1, false)),
        TokenType::LeftParen => Some((2, true)),
        TokenType::RightParen => Some((2, false)),
        _ => None,
    }
}

fn compute_bracket_pairs(tokens: &[Token]) -> Vec<BracketPair> {
    let mut stack: Vec<(Range, u8)> = Vec::new();
    let mut pairs = Vec::new();

    for token in tokens {
        if let Some((kind, is_open)) = bracket_kind(&token.token_type) {
            if is_open {
                stack.push((span_to_range(token.span), kind));
            } else {
                while let Some((open_range, open_kind)) = stack.pop() {
                    if open_kind == kind {
                        pairs.push(BracketPair {
                            open: open_range,
                            close: span_to_range(token.span),
                        });
                        break;
                    }
                }
            }
        }
    }

    pairs
}

fn range_contains_pos(range: &Range, pos: &Position) -> bool {
    let after_start = pos.line > range.start.line
        || (pos.line == range.start.line && pos.character >= range.start.character);
    let before_end = pos.line < range.end.line
        || (pos.line == range.end.line && pos.character < range.end.character);
    after_start && before_end
}

fn compute_selection_ranges(
    tokens: &[Token],
    positions: &[Position],
    source: &str,
) -> Vec<SelectionRange> {
    let line_count = source.split('\n').count().max(1);
    let last_line_len = source.split('\n').next_back().map(|l| l.len()).unwrap_or(0);
    let doc_range = Range {
        start: Position::new(0, 0),
        end: Position::new(line_count.saturating_sub(1) as u32, last_line_len as u32),
    };

    let pairs = compute_bracket_pairs(tokens);

    positions
        .iter()
        .map(|pos| {
            let token_range = find_token_at_position(tokens, *pos).map(|t| span_to_range(t.span));

            // Bracket pairs whose full range contains this position, innermost first
            let mut containing: Vec<&BracketPair> = pairs
                .iter()
                .filter(|p| range_contains_pos(&p.full_range(), pos))
                .collect();
            containing.sort_by(|a, b| {
                b.open
                    .start
                    .line
                    .cmp(&a.open.start.line)
                    .then(b.open.start.character.cmp(&a.open.start.character))
            });

            // Build chain from outermost (document) inward
            let mut current = SelectionRange {
                range: doc_range,
                parent: None,
            };

            for pair in containing.iter().rev() {
                let full = pair.full_range();
                if full != current.range {
                    current = SelectionRange {
                        range: full,
                        parent: Some(Box::new(current)),
                    };
                }
                let content = pair.content_range();
                if content != full && content != current.range {
                    current = SelectionRange {
                        range: content,
                        parent: Some(Box::new(current)),
                    };
                }
            }

            if let Some(tr) = token_range
                && tr != current.range
            {
                current = SelectionRange {
                    range: tr,
                    parent: Some(Box::new(current)),
                };
            }

            current
        })
        .collect()
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Store workspace root for file indexing
        if let Some(root_uri) = params.root_uri
            && let Ok(path) = root_uri.to_file_path()
        {
            *self.workspace_root.lock().unwrap() = Some(path);
        }

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
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        // Register file watcher for keyword config changes
        let registration = Registration {
            id: "keyword-watcher".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(
                serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                    watchers: vec![FileSystemWatcher {
                        glob_pattern: GlobPattern::String("**/.gaul-keywords.json".to_string()),
                        kind: Some(WatchKind::Create | WatchKind::Change | WatchKind::Delete),
                    }],
                })
                .unwrap(),
            ),
        };
        if let Err(e) = self.client.register_capability(vec![registration]).await {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("Failed to register file watcher: {}", e),
                )
                .await;
        }

        // Build initial workspace index
        let root = self.workspace_root.lock().unwrap().clone();
        if let Some(root) = root {
            self.index_workspace(&root);
        }

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

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        for change in &params.changes {
            if !change.uri.as_str().ends_with(".gaul-keywords.json") {
                continue;
            }

            let new_keywords = if change.typ == FileChangeType::DELETED {
                load_keywords(None).expect("default keywords must load")
            } else {
                match change.uri.to_file_path() {
                    Ok(path) => load_keywords(path.to_str()).unwrap_or_else(|_| {
                        load_keywords(None).expect("default keywords must load")
                    }),
                    Err(_) => continue,
                }
            };

            {
                let mut kw = self.keywords.write().unwrap();
                *kw = new_keywords;
            }

            // Re-analyze all open documents with new keywords
            let uris_and_sources: Vec<_> = {
                let docs = self.documents.lock().unwrap();
                docs.iter()
                    .map(|(uri, doc)| (uri.clone(), doc.source.clone()))
                    .collect()
            };
            for (uri, source) in uris_and_sources {
                self.on_change(uri, source).await;
            }

            break;
        }
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

        // Extract what we need, then release the documents lock
        let lookup = {
            let docs = self.documents.lock().unwrap();
            let Some(doc) = docs.get(&uri) else {
                return Ok(None);
            };
            let Some(symbols) = &doc.symbols else {
                return Ok(None);
            };
            let Some(token) = find_token_at_position(&doc.tokens, pos) else {
                return Ok(None);
            };
            let token_key = (token.span.line, token.span.col);
            let Some(sym_ref) = symbols
                .references
                .iter()
                .find(|r| (r.span.line, r.span.col) == token_key)
            else {
                return Ok(None);
            };
            let def = symbols.definitions[sym_ref.def_index].clone();
            let module_path = if def.kind == analysis::SymbolKind::Import {
                doc.import_map.get(&def.name).cloned()
            } else {
                None
            };
            (def, module_path)
        };

        let (def, module_path) = lookup;

        // Cross-file: if the definition is an import, look up in workspace index
        if let Some(module_path) = module_path
            && !STANDARD_MODULES.contains(&module_path.as_str())
            && let Ok(current_path) = uri.to_file_path()
        {
            let dir = current_path.parent().unwrap_or(Path::new("."));
            let target_path = if module_path.ends_with(".gaul") {
                dir.join(&module_path)
            } else {
                dir.join(format!("{}.gaul", module_path))
            };

            let index = self.workspace_index.lock().unwrap();
            if let Some(file_idx) = index.get(&target_path)
                && let Some((_, export_span)) =
                    file_idx.exports.iter().find(|(name, _)| name == &def.name)
                && let Ok(target_uri) = Url::from_file_path(&target_path)
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range: span_to_range(*export_span),
                })));
            }
        }

        // Same-file definition
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
        let keywords = self.keywords.read().unwrap();
        for (lexeme, tt) in keywords.iter() {
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

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(compute_folding_ranges(&doc.tokens)))
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let uri = params.text_document.uri;
        let positions = params.positions;
        let docs = self.documents.lock().unwrap();
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };
        Ok(Some(compute_selection_ranges(
            &doc.tokens,
            &positions,
            &doc.source,
        )))
    }

    #[allow(deprecated)] // SymbolInformation.deprecated field is itself deprecated
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();
        let index = self.workspace_index.lock().unwrap();

        let symbols: Vec<SymbolInformation> = index
            .iter()
            .flat_map(|(path, file_idx)| {
                let uri = Url::from_file_path(path).ok();
                let query = &query;
                file_idx.symbols.iter().filter_map(move |sym| {
                    if !query.is_empty() && !sym.name.to_lowercase().contains(query.as_str()) {
                        return None;
                    }
                    let uri = uri.clone()?;
                    Some(SymbolInformation {
                        name: sym.name.clone(),
                        kind: match sym.kind {
                            analysis::SymbolKind::Function => SymbolKind::FUNCTION,
                            _ => SymbolKind::VARIABLE,
                        },
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri,
                            range: span_to_range(sym.span),
                        },
                        container_name: path
                            .file_name()
                            .and_then(|n| n.to_str())
                            .map(|s| s.to_string()),
                    })
                })
            })
            .collect();

        Ok(Some(symbols))
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
        keywords: RwLock::new(keywords),
        documents: Mutex::new(HashMap::new()),
        workspace_root: Mutex::new(None),
        workspace_index: Mutex::new(HashMap::new()),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use gaul_core::keywords::load_keywords;
    use gaul_core::scanner::Scanner;

    fn scan(source: &str) -> Vec<Token> {
        let keywords = load_keywords(None).unwrap();
        let scanner = Scanner::new(source, &keywords);
        scanner.scan_tokens().tokens
    }

    #[test]
    fn folding_multi_line_block() {
        let tokens = scan("fn foo() {\n  let x = 1\n  x\n}");
        let ranges = compute_folding_ranges(&tokens);
        assert!(
            ranges
                .iter()
                .any(|r| r.start_line == 0 && r.end_line == 3 && r.kind.is_none())
        );
    }

    #[test]
    fn folding_nested_blocks() {
        let tokens = scan("fn foo() {\n  if(true) {\n    1\n  }\n}");
        let ranges = compute_folding_ranges(&tokens);
        let block_ranges: Vec<_> = ranges.iter().filter(|r| r.kind.is_none()).collect();
        assert_eq!(block_ranges.len(), 2);
    }

    #[test]
    fn folding_multi_line_array() {
        let tokens = scan("let x = [\n  1,\n  2,\n  3\n]");
        let ranges = compute_folding_ranges(&tokens);
        assert!(
            ranges
                .iter()
                .any(|r| r.start_line == 0 && r.end_line == 4 && r.kind.is_none())
        );
    }

    #[test]
    fn folding_consecutive_comments() {
        let tokens = scan("// line 1\n// line 2\n// line 3\nlet x = 1");
        let ranges = compute_folding_ranges(&tokens);
        assert!(ranges.iter().any(|r| r.start_line == 0
            && r.end_line == 2
            && r.kind == Some(FoldingRangeKind::Comment)));
    }

    #[test]
    fn folding_consecutive_imports() {
        let source = "import { foo } from \"a\"\nimport { bar } from \"b\"\nlet x = 1";
        let tokens = scan(source);
        let ranges = compute_folding_ranges(&tokens);
        assert!(ranges.iter().any(|r| r.start_line == 0
            && r.end_line == 1
            && r.kind == Some(FoldingRangeKind::Imports)));
    }

    #[test]
    fn selection_range_nested_brackets() {
        let source = "fn foo() {\n  let x = [1, 2]\n}";
        let tokens = scan(source);
        let one_token = tokens.iter().find(|t| t.lexeme == "1").unwrap();
        let pos = Position::new(
            one_token.span.line.saturating_sub(1) as u32,
            one_token.span.col.saturating_sub(1) as u32,
        );
        let result = compute_selection_ranges(&tokens, &[pos], source);
        assert_eq!(result.len(), 1);

        // Walk the chain and count depth
        let mut depth = 0;
        let mut current = Some(&result[0]);
        while let Some(r) = current {
            depth += 1;
            current = r.parent.as_deref();
        }
        // token + array content + array full + paren content + paren full + block content + block full + document
        assert!(depth >= 4, "expected at least 4 levels, got {}", depth);
    }

    #[test]
    fn selection_range_outermost_is_document() {
        let source = "let x = 1";
        let tokens = scan(source);
        let pos = Position::new(0, 4); // on 'x'
        let result = compute_selection_ranges(&tokens, &[pos], source);
        assert_eq!(result.len(), 1);

        // Walk to the outermost range
        let mut current = &result[0];
        while let Some(ref parent) = current.parent {
            current = parent;
        }
        assert_eq!(current.range.start, Position::new(0, 0));
    }
}
