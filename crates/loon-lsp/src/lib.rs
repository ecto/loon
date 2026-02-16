use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use dashmap::DashMap;

mod convert;
mod document;

use convert::{offset_to_position, position_to_offset, span_to_range};
use document::{CheckerState, DocumentState};

use loon_lang::ast::{node_at_offset, ExprKind};
use loon_lang::types::Type;

pub struct LoonLanguageServer {
    client: Client,
    documents: DashMap<Url, DocumentState>,
}

impl LoonLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    async fn on_change(&self, uri: Url, text: String, version: i32) {
        let state = DocumentState::new(&text, version);

        // Convert diagnostics
        let lsp_diags: Vec<Diagnostic> = state
            .diagnostics
            .iter()
            .filter_map(|d| {
                let span = d.primary_span()?;
                let range = span_to_range(&state.rope, span);
                Some(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(d.code.to_string())),
                    source: Some("loon".to_string()),
                    message: d.what.clone(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                })
            })
            .collect();

        self.client
            .publish_diagnostics(uri.clone(), lsp_diags, Some(version))
            .await;

        self.documents.insert(uri, state);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for LoonLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "loon-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "loon-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.on_change(doc.uri, doc.text, doc.version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        // We use full sync, so there's exactly one change with the full text
        if let Some(change) = params.content_changes.into_iter().next() {
            self.on_change(uri, change.text, version).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        // Clear diagnostics
        self.client
            .publish_diagnostics(uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.rope, pos);

        // Find the AST node at this offset
        let Some(node) = node_at_offset(&doc.exprs, offset) else {
            return Ok(None);
        };

        let Some(ref checker) = doc.checker else {
            return Ok(None);
        };

        // Look up the resolved type
        if let Some(resolved) = checker.get_resolved_type(node.id) {
            let range = span_to_range(&doc.rope, node.span);

            let label = match &node.kind {
                ExprKind::Symbol(name) => format!("**{name}**: `{resolved}`"),
                _ => format!("`{resolved}`"),
            };

            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: label,
                }),
                range: Some(range),
            }));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.rope, pos);

        let Some(ref checker) = doc.checker else {
            return Ok(None);
        };

        // Find a reference at this offset
        let ref_info = checker.references.iter().find(|r| {
            r.span.start <= offset && offset < r.span.end
        });

        let Some(ref_info) = ref_info else {
            return Ok(None);
        };

        // Look up the definition
        let Some(def_info) = checker.lookup_definition(&ref_info.name) else {
            return Ok(None);
        };

        let target_uri = if let Some(ref file) = def_info.file {
            Url::from_file_path(file).unwrap_or_else(|_| uri.clone())
        } else {
            uri.clone()
        };

        let range = span_to_range(&doc.rope, def_info.name_span);

        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range,
        })))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(ref checker) = doc.checker else {
            return Ok(None);
        };

        // Get the text before the cursor to extract a prefix
        let offset = position_to_offset(&doc.rope, pos);
        let text = doc.rope.to_string();
        let prefix = extract_prefix(&text, offset);

        let items: Vec<CompletionItem> = checker
            .visible_names
            .iter()
            .filter(|(name, _)| prefix.is_empty() || name.starts_with(&prefix))
            .map(|(name, scheme)| {
                let kind = classify_completion_kind(&scheme.ty);
                CompletionItem {
                    label: name.clone(),
                    kind: Some(kind),
                    detail: Some(format!("{}", scheme.ty)),
                    ..Default::default()
                }
            })
            .collect();

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(ref checker) = doc.checker else {
            return Ok(None);
        };

        let mut hints = Vec::new();

        // Walk the AST looking for let bindings and defn params
        for expr in &doc.exprs {
            collect_inlay_hints(expr, checker, &doc.rope, &mut hints);
        }

        Ok(Some(hints))
    }
}

/// Extract the identifier prefix at the cursor position.
fn extract_prefix(text: &str, offset: usize) -> String {
    let bytes = text.as_bytes();
    let mut start = offset;
    while start > 0 {
        let b = bytes[start - 1];
        if b.is_ascii_alphanumeric() || b == b'_' || b == b'-' || b == b'?' || b == b'!' {
            start -= 1;
        } else {
            break;
        }
    }
    text[start..offset].to_string()
}

/// Classify a type into a CompletionItemKind.
fn classify_completion_kind(ty: &Type) -> CompletionItemKind {
    match ty {
        Type::Fn(_, _) => CompletionItemKind::FUNCTION,
        Type::Con(_, _) => CompletionItemKind::ENUM,
        _ => CompletionItemKind::VARIABLE,
    }
}

/// Recursively collect inlay hints for let bindings and defn params.
fn collect_inlay_hints(
    expr: &loon_lang::ast::Expr,
    checker: &CheckerState,
    rope: &ropey::Rope,
    hints: &mut Vec<InlayHint>,
) {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(head) = &items[0].kind {
                match head.as_str() {
                    "let" => {
                        // [let name val] or [let mut name val]
                        let binding_idx = if items.len() > 2 {
                            if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut") {
                                2
                            } else {
                                1
                            }
                        } else {
                            1
                        };
                        if binding_idx < items.len() {
                            if let ExprKind::Symbol(name) = &items[binding_idx].kind {
                                if name != "_" {
                                    if let Some(resolved) = checker.get_resolved_type(items[binding_idx].id) {
                                        let pos = offset_to_position(rope, items[binding_idx].span.end);
                                        hints.push(InlayHint {
                                            position: pos,
                                            label: InlayHintLabel::String(format!(": {resolved}")),
                                            kind: Some(InlayHintKind::TYPE),
                                            text_edits: None,
                                            tooltip: None,
                                            padding_left: Some(false),
                                            padding_right: Some(true),
                                            data: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                    "fn" if items.len() >= 2 && matches!(&items[1].kind, ExprKind::Symbol(_)) => {
                        // [fn name [params...] body...]
                        if items.len() >= 3 {
                            if let ExprKind::List(params) = &items[2].kind {
                                for param in params {
                                    if let ExprKind::Symbol(pname) = &param.kind {
                                        if pname != "_" {
                                            if let Some(resolved) = checker.get_resolved_type(param.id) {
                                                let pos = offset_to_position(rope, param.span.end);
                                                hints.push(InlayHint {
                                                    position: pos,
                                                    label: InlayHintLabel::String(format!(": {resolved}")),
                                                    kind: Some(InlayHintKind::TYPE),
                                                    text_edits: None,
                                                    tooltip: None,
                                                    padding_left: Some(false),
                                                    padding_right: Some(true),
                                                    data: None,
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            // Recurse into all list items
            for item in items {
                collect_inlay_hints(item, checker, rope, hints);
            }
        }
        ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_inlay_hints(item, checker, rope, hints);
            }
        }
        ExprKind::Map(pairs) => {
            for (k, v) in pairs {
                collect_inlay_hints(k, checker, rope, hints);
                collect_inlay_hints(v, checker, rope, hints);
            }
        }
        _ => {}
    }
}

pub async fn run_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(LoonLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use document::DocumentState;

    #[test]
    fn hover_returns_type_for_symbol() {
        let src = "[let x 42]\nx";
        let state = DocumentState::new(src, 1);
        assert!(state.checker.is_some());

        let checker = state.checker.as_ref().unwrap();

        // Find the node for `x` on line 2 (offset 11)
        let node = node_at_offset(&state.exprs, 11);
        assert!(node.is_some());
        let node = node.unwrap();

        let ty = checker.get_resolved_type(node.id);
        assert!(ty.is_some());
        assert_eq!(*ty.unwrap(), Type::Int);
    }

    #[test]
    fn completion_returns_visible_names() {
        let src = "[fn add [x y] [+ x y]]";
        let state = DocumentState::new(src, 1);
        assert!(state.checker.is_some());

        let checker = state.checker.as_ref().unwrap();
        let name_strs: Vec<&str> = checker.visible_names.iter().map(|(n, _)| n.as_str()).collect();
        assert!(name_strs.contains(&"add"), "should contain 'add'");
        assert!(name_strs.contains(&"+"), "should contain '+'");
    }

    #[test]
    fn extract_prefix_basic() {
        assert_eq!(extract_prefix("hello add", 9), "add");
        assert_eq!(extract_prefix("[+ x y]", 4), "x");
        assert_eq!(extract_prefix("empty?", 6), "empty?");
        assert_eq!(extract_prefix("[let ", 5), "");
    }

    #[test]
    fn classify_fn_vs_variable() {
        assert_eq!(
            classify_completion_kind(&Type::Fn(vec![Type::Int], Box::new(Type::Int))),
            CompletionItemKind::FUNCTION
        );
        assert_eq!(
            classify_completion_kind(&Type::Int),
            CompletionItemKind::VARIABLE
        );
    }

    #[test]
    fn definition_and_reference_tracking() {
        let src = "[fn add [x y] [+ x y]]\n[add 1 2]";
        let state = DocumentState::new(src, 1);
        assert!(state.checker.is_some());

        let checker = state.checker.as_ref().unwrap();

        // 'add' should be in definitions
        let def = checker.lookup_definition("add");
        assert!(def.is_some(), "should have definition for 'add'");

        // There should be references to 'add'
        let add_refs: Vec<_> = checker.references.iter().filter(|r| r.name == "add").collect();
        assert!(!add_refs.is_empty(), "should have references to 'add'");
    }

    #[test]
    fn inlay_hints_for_let() {
        let src = "[let x 42]";
        let state = DocumentState::new(src, 1);
        assert!(state.checker.is_some());

        let checker = state.checker.as_ref().unwrap();
        let mut hints = Vec::new();
        for expr in &state.exprs {
            collect_inlay_hints(expr, checker, &state.rope, &mut hints);
        }
        // Should have at least one hint for `x`
        assert!(!hints.is_empty(), "should have inlay hints for let binding");
    }
}
