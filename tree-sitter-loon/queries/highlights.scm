; Comments
(comment) @comment

; Literals
(string) @string
(number) @number
(float_literal) @number.float
(boolean) @boolean
(keyword) @label

; Keywords: first symbol in list matching known forms
(list . (symbol) @keyword
  (#match? @keyword "^(defn|fn|let|if|match|type|use|pub|effect|handle|do|when|sig|trait|impl|test|try|mut|str|fmt)$"))

; Function definitions: symbol after defn
(list . (symbol) @keyword (symbol) @function
  (#eq? @keyword "defn"))

; Type definitions: symbol after type
(list . (symbol) @keyword (symbol) @type
  (#eq? @keyword "type"))

; Effect definitions
(list . (symbol) @keyword (symbol) @type
  (#eq? @keyword "effect"))

; Trait definitions
(list . (symbol) @keyword (symbol) @type
  (#eq? @keyword "trait"))

; Fat arrow and operators
(symbol) @operator
  (#match? @operator "^(=>|\\|>|->|\\+|-|\\*|/|%|>|<|>=|<=|=|not|and|or)$")

; Function calls: first symbol in list (when not a keyword)
(list . (symbol) @function.call)

; Everything else
(symbol) @variable
