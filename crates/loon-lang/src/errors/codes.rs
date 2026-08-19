/// Error and warning codes for Loon diagnostics.
/// E01xx = parse errors
/// E02xx = type errors
/// E03xx = ownership errors
/// E04xx = effect errors
/// E05xx = module errors
/// W01xx = warnings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // Parse errors
    E0100, // unexpected character
    E0101, // unexpected token
    E0102, // unclosed delimiter
    E0103, // invalid literal

    // Type errors
    E0200, // type mismatch
    E0201, // unbound symbol
    E0202, // arity mismatch
    E0203, // infinite type
    E0204, // signature mismatch
    E0205, // missing trait impl
    E0206, // non-exhaustive match
    E0207, // field mismatch
    E0208, // dimension mismatch
    E0209, // condition can never be falsy (warning)

    // Ownership errors
    E0300, // use after move
    E0301, // mutate immutable
    E0302, // double mutable borrow

    // Effect errors
    E0400, // unhandled effect
    E0401, // undeclared effect
    E0402, // unknown effect operation
    E0403, // effect mismatch
    E0404, // effect not granted (pkg.oo capability violation)

    // Module errors
    E0500, // unresolved module
    E0501, // private symbol
    E0502, // circular dependency

    // Placement errors
    E0600, // construct not allowed inside a kernel
    E0601, // kernel signature is not placeable
    E0602, // kernel writes outside its own work item

    // Warnings
    W0100, // transparent wildcard
}

impl ErrorCode {
    pub fn as_str(&self) -> &'static str {
        match self {
            ErrorCode::E0100 => "E0100",
            ErrorCode::E0101 => "E0101",
            ErrorCode::E0102 => "E0102",
            ErrorCode::E0103 => "E0103",
            ErrorCode::E0200 => "E0200",
            ErrorCode::E0201 => "E0201",
            ErrorCode::E0202 => "E0202",
            ErrorCode::E0203 => "E0203",
            ErrorCode::E0204 => "E0204",
            ErrorCode::E0205 => "E0205",
            ErrorCode::E0206 => "E0206",
            ErrorCode::E0207 => "E0207",
            ErrorCode::E0208 => "E0208",
            ErrorCode::E0209 => "E0209",
            ErrorCode::E0300 => "E0300",
            ErrorCode::E0301 => "E0301",
            ErrorCode::E0302 => "E0302",
            ErrorCode::E0400 => "E0400",
            ErrorCode::E0401 => "E0401",
            ErrorCode::E0402 => "E0402",
            ErrorCode::E0403 => "E0403",
            ErrorCode::E0404 => "E0404",
            ErrorCode::E0600 => "E0600",
            ErrorCode::E0601 => "E0601",
            ErrorCode::E0602 => "E0602",
            ErrorCode::E0500 => "E0500",
            ErrorCode::E0501 => "E0501",
            ErrorCode::E0502 => "E0502",
            ErrorCode::W0100 => "W0100",
        }
    }

    pub fn is_warning(&self) -> bool {
        matches!(self, ErrorCode::W0100 | ErrorCode::E0209)
    }

    pub fn category(&self) -> &'static str {
        match self {
            ErrorCode::E0100 | ErrorCode::E0101 | ErrorCode::E0102 | ErrorCode::E0103 => "parse",
            ErrorCode::E0200
            | ErrorCode::E0201
            | ErrorCode::E0202
            | ErrorCode::E0203
            | ErrorCode::E0204
            | ErrorCode::E0205
            | ErrorCode::E0206
            | ErrorCode::E0207
            | ErrorCode::E0208
            | ErrorCode::E0209 => "type",
            ErrorCode::E0300 | ErrorCode::E0301 | ErrorCode::E0302 => "ownership",
            ErrorCode::E0400
            | ErrorCode::E0401
            | ErrorCode::E0402
            | ErrorCode::E0403
            | ErrorCode::E0404 => "effect",
            ErrorCode::E0500 | ErrorCode::E0501 | ErrorCode::E0502 => "module",
            ErrorCode::E0600 | ErrorCode::E0601 | ErrorCode::E0602 => "placement",
            ErrorCode::W0100 => "warning",
        }
    }

    pub fn title(&self) -> &'static str {
        match self {
            ErrorCode::E0100 => "unexpected character",
            ErrorCode::E0101 => "unexpected token",
            ErrorCode::E0102 => "unclosed delimiter",
            ErrorCode::E0103 => "invalid literal",
            ErrorCode::E0200 => "type mismatch",
            ErrorCode::E0201 => "unbound symbol",
            ErrorCode::E0202 => "arity mismatch",
            ErrorCode::E0203 => "infinite type",
            ErrorCode::E0204 => "signature mismatch",
            ErrorCode::E0205 => "missing trait implementation",
            ErrorCode::E0206 => "non-exhaustive match",
            ErrorCode::E0207 => "field mismatch",
            ErrorCode::E0208 => "dimension mismatch",
            ErrorCode::E0209 => "condition is always truthy",
            ErrorCode::E0300 => "use after move",
            ErrorCode::E0301 => "mutate immutable binding",
            ErrorCode::E0302 => "double mutable borrow",
            ErrorCode::E0400 => "unhandled effect",
            ErrorCode::E0401 => "undeclared effect",
            ErrorCode::E0402 => "unknown effect operation",
            ErrorCode::E0403 => "effect mismatch",
            ErrorCode::E0404 => "effect not granted",
            ErrorCode::E0600 => "not allowed inside a kernel",
            ErrorCode::E0601 => "kernel signature is not placeable",
            ErrorCode::E0602 => "kernel writes outside its own work item",
            ErrorCode::E0500 => "unresolved module",
            ErrorCode::E0501 => "private symbol",
            ErrorCode::E0502 => "circular dependency",
            ErrorCode::W0100 => "transparent wildcard",
        }
    }
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
