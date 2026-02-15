use crate::syntax::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub fn report_error(filename: &str, source: &str, message: &str, span: Span) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, source);
    let diagnostic = Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span.start..span.end)]);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
}
