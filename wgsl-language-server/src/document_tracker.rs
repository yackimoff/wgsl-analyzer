use std::collections::HashMap;

use lsp_types::{
    CompletionItem, DidChangeTextDocumentParams, DocumentFormattingParams, DocumentSymbol,
    Position, PublishDiagnosticsParams, Range, TextDocumentItem, TextEdit, Uri,
};
use naga::{
    Module,
    front::wgsl::ParseError,
    valid::{Capabilities, ModuleInfo, ValidationError, ValidationFlags, Validator},
};

use crate::{
    completions::CompletionProvider,
    fmt,
    pretty_error::error_context::ModuleContext,
    range_tools::string_range,
    symbol_provider::SymbolProvider,
    wgsl_error::{parse_error_to_lsp_diagnostic, validation_error_to_lsp_diagnostic},
};

fn preprocess_includes(
    content: &str,
    _uri: &Uri,
    documents: &HashMap<Uri, TrackedDocument>,
) -> String {
    fn find_module<'a>(module: &str, documents: &'a HashMap<Uri, TrackedDocument>) -> Option<&'a TrackedDocument> {
        for doc in documents.values() {
            let path = doc.uri.path().as_str();
            let without_ext = path.trim_start_matches('/').trim_end_matches(".wgsl");
            let doc_module = without_ext.replace('/', "::");
            if doc_module.ends_with(module) {
                return Some(doc);
            }
        }
        None
    }

    let mut result = String::new();

    for line in content.lines() {
        let trimmed = line.trim_start();
        let directive = trimmed.strip_prefix("#include").or_else(|| trimmed.strip_prefix("#import"));
        if let Some(rest) = directive {
            let module = rest.trim().trim_end_matches(';').trim();
            if let Some(doc) = find_module(module, documents) {
                result.push_str(&doc.content);
                result.push('\n');
                continue;
            }
        }

        result.push_str(line);
        result.push('\n');
    }

    result
}

pub struct TrackedDocument {
    pub uri: Uri,
    pub content: String,
    #[allow(unused)]
    pub version: i32,
    pub compilation_result: Option<CompilationResult>,
    pub last_valid_module: Option<Module>,
}

type CompilationResult =
    Result<(Module, Result<ModuleInfo, naga::WithSpan<ValidationError>>), ParseError>;

impl TrackedDocument {

    pub fn get_lsp_diagnostics(&self) -> Vec<lsp_types::Diagnostic> {
        let Some(compilation_result) = &self.compilation_result else {
            return vec![];
        };

        match compilation_result {
            Err(parse_error) => {
                parse_error_to_lsp_diagnostic(parse_error, &self.content, &self.uri)
            }
            Ok((module, Err(validation_error))) => validation_error_to_lsp_diagnostic(
                validation_error,
                &self.content,
                &self.uri,
                module,
            ),
            _ => vec![],
        }
    }

    pub fn module_context(&self) -> Option<ModuleContext> {
        self.last_valid_module.as_ref().map(|module| ModuleContext {
            module,
            code: &self.content,
        })
    }
}

pub struct DocumentTracker {
    validator: Validator,
    documents: HashMap<Uri, TrackedDocument>,
}

impl DocumentTracker {
    pub fn new() -> Self {
        Self {
            validator: naga::valid::Validator::new(ValidationFlags::all(), Capabilities::all()),
            documents: Default::default(),
        }
    }

    pub fn insert(&mut self, doc: TextDocumentItem) {
        let document = TrackedDocument {
            uri: doc.uri.to_owned(),
            content: doc.text.clone(),
            version: doc.version,
            compilation_result: None,
            last_valid_module: None,
        };

        let uri = doc.uri.clone();
        self.documents.insert(doc.uri, document);
        self.compile_document(&uri);
    }

    pub fn update(&mut self, change: DidChangeTextDocumentParams) {
        if let Some(doc) = self.documents.get_mut(&change.text_document.uri) {
            for change in change.content_changes {
                if let Some(range) = change.range {
                    let range = string_range(&doc.content, range);
                    doc.content.replace_range(range, &change.text);
                } else {
                    doc.content = change.text;
                }
            }
        }

        self.compile_document(&change.text_document.uri);
    }

    pub fn remove(&mut self, uri: &Uri) {
        self.documents.remove(uri);
    }

    pub fn get_diagnostics(&self) -> Vec<PublishDiagnosticsParams> {
        let mut diagnostics = vec![];

        for (url, document) in &self.documents {
            let lsp_diagnostics: Vec<_> = document.get_lsp_diagnostics().into_iter().collect();

            diagnostics.push(PublishDiagnosticsParams {
                uri: url.clone(),
                diagnostics: lsp_diagnostics,
                version: None,
            })
        }

        diagnostics
    }

    pub fn get_completion(&self, url: &Uri, position: &Position) -> Vec<CompletionItem> {
        let mut completions = vec![];

        if let Some(doc) = self.documents.get(url) {
            completions.extend(doc.get_completions(position))
        }

        completions
    }

    pub fn get_symbols(&self) -> Vec<DocumentSymbol> {
        self.documents
            .values()
            .flat_map(|doc| doc.get_symbols())
            .collect()
    }

    pub fn format_document(&self, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
        let document = self.documents.get(&params.text_document.uri)?;
        let result = fmt::pretty_print_ast(&document.content, &params.options)?;

        Some(vec![TextEdit::new(
            Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX)),
            result,
        )])
    }

    fn compile_document(&mut self, uri: &Uri) {
        let (content, doc_uri) = if let Some(doc) = self.documents.get(uri) {
            (doc.content.clone(), doc.uri.clone())
        } else {
            return;
        };

        let expanded = preprocess_includes(&content, &doc_uri, &self.documents);

        let doc = self.documents.get_mut(uri).unwrap();
        self.validator.reset();
        let result = match naga::front::wgsl::parse_str(&expanded) {
            Err(parse_error) => Err(parse_error),
            Ok(module) => {
                doc.last_valid_module = Some(module.clone());
                let validation_result = self.validator.validate(&module);
                Ok((module, validation_result))
            }
        };

        doc.compilation_result = Some(result);
    }
}
