"""
pygments.lexers.loon
~~~~~~~~~~~~~~~~~~~~

Lexer for the Loon programming language (https://loonlang.com/).

Loon is a functional language with invisible types, safe ownership, and
algebraic effects. Source files use the `.oo` extension (from l-oo-n); the
older `.loon` extension is supported as a fallback.

Syntax at a glance:
    - s-expressions with square brackets:  [head args...]
    - persistent vector:                    #[a b c]
    - set:                                  #{a b c}
    - map:                                  {:key val ...}
    - tuple:                                (a, b, c)
    - line comment:                         ; ...

Drop this file in pygments/lexers/ and run `make mapfiles` in the Pygments
repo to regenerate the lexer map.

:copyright: Copyright 2026 the Loon contributors.
:license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import (
    Comment, Keyword, Name, Number, Operator, Punctuation, String, Text,
)

__all__ = ['LoonLexer']


class LoonLexer(RegexLexer):
    """
    Lexer for the Loon programming language.

    .. versionadded:: 2.19
    """

    name = 'Loon'
    url = 'https://loonlang.com/'
    aliases = ['loon']
    filenames = ['*.oo', '*.loon']
    mimetypes = ['text/x-loon']

    # Special forms — parser-recognized, not user-callable.
    special_forms = (
        'fn', 'let', 'if', 'cond', 'match', 'do', 'when', 'unless',
        'type', 'effect', 'handle', 'resume', 'perform',
        'pipe', 'use', 'import', 'export', 'module',
        'test', 'assert-eq', 'assert', 'defmacro', 'quote', 'quasiquote',
        'unquote', 'unquote-splicing',
    )

    # Built-in functions and values in the prelude.
    builtins = (
        'println', 'print', 'fmt', 'str',
        'map', 'filter', 'reduce', 'each', 'take', 'drop', 'range',
        'sort-by', 'group-by', 'entries', 'keys', 'values',
        'len', 'empty?', 'first', 'last', 'rest', 'cons',
        'split', 'join', 'lowercase', 'uppercase', 'trim',
        'mod', 'abs', 'min', 'max', 'floor', 'ceil', 'round',
        'not', 'and', 'or',
        'true', 'false', 'nil',
    )

    # Core types for syntax highlighting of annotations / literals.
    core_types = (
        'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
        'f32', 'f64', 'bool', 'char', 'String', 'Vec', 'Map', 'Set',
        'Option', 'Result', 'IO', 'Fail', 'State',
    )

    tokens = {
        'root': [
            # Whitespace and commas (commas are whitespace in Loon).
            (r'[,\s]+', Text),

            # Line comments.
            (r';.*?$', Comment.Single),

            # String literals with escape support.
            (r'"', String, 'string'),

            # Numeric literals with optional type suffix.
            (r'-?\d+\.\d+([eE][+-]?\d+)?(f32|f64)?', Number.Float),
            (r'-?\d+(i8|i16|i32|i64|u8|u16|u32|u64)?', Number.Integer),

            # Character literals.
            (r"\\[a-zA-Z0-9]+", String.Char),

            # Keywords (the colon-prefixed kind, not special forms).
            (r':[\w!?*+<>=/\-]+', Name.Constant),

            # Effect-qualified operations like IO.println, Fs.read-file.
            (r'\b[A-Z][\w!?*+<>=/\-]*\.[\w!?*+<>=/\-]+', Name.Function),

            # Booleans — must come before generic symbols.
            (r'\b(true|false)\b', Keyword.Constant),

            # Special forms — keep in sync with `special_forms`.
            (words(special_forms, prefix=r'(?<![\w\-])',
                   suffix=r'(?![\w\-])'), Keyword),

            # Prelude built-ins.
            (words(builtins, prefix=r'(?<![\w\-])',
                   suffix=r'(?![\w\-])'), Name.Builtin),

            # Core types / constructors — capitalized identifiers.
            (words(core_types, prefix=r'(?<![\w\-])',
                   suffix=r'(?![\w\-])'), Keyword.Type),
            (r'\b[A-Z][\w\-]*\b', Name.Class),

            # Collection literal prefixes — Pygments only tokenizes, so
            # we emit the prefix and fall through; nesting is the parser's
            # problem, not ours.
            (r'#\[|#\{', Punctuation),

            # Brackets, braces, parens.
            (r'[\[\]\(\)\{\}]', Punctuation),

            # Symbols / identifiers. Loon symbols can include many sigils.
            (r'[\w!?*+<>=/\-][\w!?*+<>=/\-]*', Name),
        ],

        'string': [
            (r'"', String, '#pop'),
            (r'\\.', String.Escape),
            # Interpolation: {expr}
            (r'\{[^}]*\}', String.Interpol),
            (r'[^"\\{]+', String),
        ],
    }
