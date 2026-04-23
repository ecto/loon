/*
Language: Loon
Author: Loon contributors
Website: https://loonlang.com/
Description: Functional language with invisible types, safe ownership, and
  algebraic effects. S-expressions in square brackets.
Category: functional
*/

export default function(hljs) {
  const SYMBOL_RE = '[a-zA-Z_][\\w!?*+<>=/\\-]*';

  const SPECIAL_FORMS = [
    'fn', 'let', 'if', 'cond', 'match', 'do', 'when', 'unless',
    'type', 'effect', 'handle', 'resume', 'perform',
    'pipe', 'use', 'import', 'export', 'module',
    'test', 'assert-eq', 'assert', 'defmacro',
    'quote', 'quasiquote', 'unquote', 'unquote-splicing'
  ];

  const BUILTINS = [
    'println', 'print', 'fmt', 'str',
    'map', 'filter', 'reduce', 'each', 'take', 'drop', 'range',
    'sort-by', 'group-by', 'entries', 'keys', 'values',
    'len', 'empty?', 'first', 'last', 'rest', 'cons',
    'split', 'join', 'lowercase', 'uppercase', 'trim',
    'mod', 'abs', 'min', 'max', 'floor', 'ceil', 'round',
    'not', 'and', 'or'
  ];

  const LITERALS = ['true', 'false', 'nil'];

  const COMMENT = hljs.COMMENT(';', '$');

  const KEYWORD = {
    className: 'symbol',
    begin: ':[\\w!?*+<>=/\\-]+'
  };

  const NUMBER = {
    className: 'number',
    variants: [
      { begin: '-?\\d+\\.\\d+([eE][+-]?\\d+)?(f32|f64)?' },
      { begin: '-?\\d+(i8|i16|i32|i64|u8|u16|u32|u64)?' }
    ],
    relevance: 0
  };

  const STRING = {
    className: 'string',
    begin: '"',
    end: '"',
    contains: [
      hljs.BACKSLASH_ESCAPE,
      {
        className: 'subst',
        begin: '\\{',
        end: '\\}',
        excludeBegin: false,
        excludeEnd: false
      }
    ]
  };

  const TYPE = {
    className: 'type',
    begin: '\\b[A-Z][\\w\\-]*\\b'
  };

  const EFFECT_OP = {
    className: 'title.function',
    begin: '\\b[A-Z][\\w\\-]*\\.[\\w!?*+<>=/\\-]+'
  };

  return {
    name: 'Loon',
    aliases: ['oo'],
    keywords: {
      $pattern: '[\\w!?*+<>=/\\-]+',
      keyword: SPECIAL_FORMS.join(' '),
      built_in: BUILTINS.join(' '),
      literal: LITERALS.join(' ')
    },
    contains: [
      COMMENT,
      STRING,
      NUMBER,
      KEYWORD,
      EFFECT_OP,
      TYPE
    ]
  };
}
