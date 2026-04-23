# -*- coding: utf-8 -*- #
# frozen_string_literal: true

# Rouge lexer for the Loon programming language (https://loonlang.com/).
#
# Drop this file in lib/rouge/lexers/ of the Rouge source tree. Used by
# Jekyll / GitHub Pages / Middleman for syntax highlighting of fenced code
# blocks tagged ```loon.

module Rouge
  module Lexers
    class Loon < RegexLexer
      title 'Loon'
      desc 'A functional language with invisible types, safe ownership, and algebraic effects (loonlang.com)'
      tag 'loon'
      aliases 'oo'
      filenames '*.oo', '*.loon'
      mimetypes 'text/x-loon'

      def self.special_forms
        @special_forms ||= Set.new %w(
          fn let if cond match do when unless
          type effect handle resume perform
          pipe use import export module
          test assert-eq assert defmacro
          quote quasiquote unquote unquote-splicing
        )
      end

      def self.builtins
        @builtins ||= Set.new %w(
          println print fmt str
          map filter reduce each take drop range
          sort-by group-by entries keys values
          len empty? first last rest cons
          split join lowercase uppercase trim
          mod abs min max floor ceil round
          not and or
        )
      end

      def self.literals
        @literals ||= Set.new %w(true false nil)
      end

      id = /[a-zA-Z_][\w!?*+<>=\/\-]*/
      num_suffix_int = /(?:i8|i16|i32|i64|u8|u16|u32|u64)?/
      num_suffix_float = /(?:f32|f64)?/

      state :root do
        rule %r/[,\s]+/, Text
        rule %r/;.*?$/, Comment::Single

        # Strings with interpolation.
        rule %r/"/, Str, :string

        # Numbers.
        rule %r/-?\d+\.\d+(?:[eE][+-]?\d+)?#{num_suffix_float}/, Num::Float
        rule %r/-?\d+#{num_suffix_int}/, Num::Integer

        # Keywords (:foo).
        rule %r/:[\w!?*+<>=\/\-]+/, Name::Constant

        # Effect-qualified ops: IO.println, Fs.read-file.
        rule %r/\b[A-Z][\w\-]*\.[\w!?*+<>=\/\-]+/, Name::Function

        # Booleans.
        rule %r/\b(?:true|false|nil)\b/, Keyword::Constant

        # Collection literal prefixes — fall through to the punctuation
        # rules for the bracket itself.
        rule %r/#(?=[\[\{])/, Punctuation

        # Brackets.
        rule %r/[\[\]\(\)\{\}]/, Punctuation

        # Identifiers — classify against the keyword sets.
        rule id do |m|
          name = m[0]
          if self.class.special_forms.include?(name)
            token Keyword
          elsif self.class.builtins.include?(name)
            token Name::Builtin
          elsif self.class.literals.include?(name)
            token Keyword::Constant
          elsif name =~ /^[A-Z]/
            token Name::Class
          else
            token Name
          end
        end
      end

      state :string do
        rule %r/"/, Str, :pop!
        rule %r/\\./, Str::Escape
        rule %r/\{[^}]*\}/, Str::Interpol
        rule %r/[^"\\{]+/, Str
      end
    end
  end
end
