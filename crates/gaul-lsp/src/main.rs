use std::collections::HashMap;
use std::sync::Mutex;

use gaul_core::analysis::{self, SymbolTable};
use gaul_core::builtins::NATIVE_FUNCTION_NAMES;
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
                    detail: None,
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
