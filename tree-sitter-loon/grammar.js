/// <reference types="tree-sitter-cli/dsl" />

module.exports = grammar({
  name: "loon",

  extras: $ => [/\s/, /,/],

  rules: {
    source_file: $ => repeat($._form),

    _form: $ => choice(
      $.list,
      $.vector,
      $.set,
      $.map,
      $.tuple,
      $.float_literal,
      $.number,
      $.string,
      $.boolean,
      $.keyword,
      $.symbol,
      $.comment,
    ),

    // S-expression: [head args...]
    list: $ => seq("[", repeat($._form), "]"),

    // Persistent vector: #[a b c]
    vector: $ => seq("#[", repeat($._form), "]"),

    // Set: #{a b c}
    set: $ => seq("#{", repeat($._form), "}"),

    // Map: {:key val ...}
    map: $ => seq("{", repeat(seq($._form, $._form)), "}"),

    // Tuple: (a, b)
    tuple: $ => seq("(", repeat($._form), ")"),

    // Line comment: ; ...
    comment: $ => /;[^\n]*/,

    // Float literal (higher precedence than number)
    float_literal: $ => token(prec(1, /-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?(f32|f64)?/)),

    // Integer number with optional type suffix
    number: $ => token(/-?[0-9]+(i32|i64|u32|u64)?/),

    // Double-quoted string with escape support
    string: $ => token(/"([^"\\]|\\.)*"/),

    // Boolean literals
    boolean: $ => choice("true", "false"),

    // Keyword: :name
    keyword: $ => token(/:[a-zA-Z_][a-zA-Z0-9_-]*/),

    // Symbol: identifiers and operators
    symbol: $ => choice(
      token(/[a-zA-Z_+\-*!<>=&%][a-zA-Z0-9_\-?!.*%\/]*/),
      "=>",
      "?",
      "/",
      "->",
    ),
  },
});
