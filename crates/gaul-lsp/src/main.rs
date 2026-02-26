use std::collections::HashMap;
use std::sync::Mutex;

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
    SemanticTokenType::KEYWORD,  // 0
    SemanticTokenType::NUMBER,   // 1
    SemanticTokenType::STRING,   // 2
    SemanticTokenType::COMMENT,  // 3
    SemanticTokenType::VARIABLE, // 4
    SemanticTokenType::OPERATOR, // 5
];

struct DocumentState {
    tokens: Vec<Token>,
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

        let parser = Parser::new(tokens_for_parser);
        match parser.parse() {
            Ok(mut program) => {
                let mut resolver = Resolver::new();
                if let Err(e) = resolver.resolve(&mut program) {
                    diagnostics.push(span_to_diagnostic(e.span, &e.message, "resolve"));
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
        TokenType::Identifier => Some(4), // VARIABLE

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

fn encode_semantic_tokens(tokens: &[Token]) -> Vec<SemanticToken> {
    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for token in tokens {
        let Some(token_type) = semantic_token_type(&token.token_type) else {
            continue;
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
            token_modifiers_bitset: 0,
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
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
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

        let encoded = encode_semantic_tokens(&doc.tokens);
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: encoded,
        })))
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
