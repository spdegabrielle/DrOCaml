#lang scheme

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide get-token
         get-comment-token
         get-string-token)

(define (ret lexeme type paren start-pos end-pos)
  (values lexeme type paren (position-offset start-pos) (position-offset end-pos)))

(define-lex-abbrevs
  [digit (:/ "0" "9")]
  [digit2 (:/ "0" "1")]
  [digit8 (:/ "0" "7")]
  [digit10 digit]
  [digit16 (:/ "af" "AF" "09")]
  [unicode
   (:or
    (:: "u" (:** 1 4 digit16))
    (:: "U" (:** 1 6 digit16)))]
  [keyword
   (:or
    "as" "assert" "do" "done" "downto" "else" "exception" "for" "fun"
    "function" "if" "lazy" "match" "mutable" "new" "private" "then"
    "to" "try" "when" "while" "with")]
  [governing-keyword
   (:or
    "and" "class" "constraint" "external" "functor" "in"
    "include" "inherit" "initializer" "let" "method"
    "module" "object" "open" "rec" "type" "val" "virtual")]
  [open-close (:or "begin" "end" "sig" "struct")]
  [true-false (:or "true" "false")]
  [ident (:: alphabetic (:* (:or alphabetic numeric "'" "_")))]
  [int-expr (:: numeric (:* (:or numeric "_")))]
  [int-literal (:: (:? "-") int-expr)]
  [decimal-expr (:: "." (:* (:or numeric "_")))]
  [bad-exponent-expr (:: (:or "e" "E") (:? (:or "+" "-")))]
  [exponent-expr (:: (:or "e" "E") (:? (:or "+" "-")) int-expr)]
  [float-literal (:: int-literal (:? decimal-expr) (:? exponent-expr))]
  [bad-float-literal (:: int-literal (:? decimal-expr) bad-exponent-expr)]
  [text-operator (:or "asr" "land" "lor" "lsl" "lsr" "lxor" "mod" "or" "of")]
  [operator
   (:or
    "=" "<" ">" "@" "^" "|" "&" "+" "-" "*" "/" "$" "%"
    "." "::" "_" ";" "," ":" "->")]
  [paren (:or "(" ")" "[" "]" "{" "}")]
  [comment-start (:or "(*" "(*)")]
  [comment-end "*)"]
  [non-comment-tag
   (:or
    (:~ "(" ")")
    (:: "(" (:~ "*"))
    (:: (:~ "*") ")")
    (:: "(" (:~ "*") ")"))]
  [comment-inside (:: (:+ non-comment-tag))]
  [comment-through-end (:: (:* non-comment-tag) "*)")]
  [double-semi ";;"]
  [char-elt
   (:or
    (:~ (:or "\"" "\\"))
    "\\\\"
    "\\\""
    "\\'"
    "\\n"
    "\\t"
    "\\b"
    "\\r"
    (:: "\\" (:** 1 3 digit8))
    (:: "\\x" (:** 1 2 digit16)))]
  [char-literal (:: "\'" char-elt "\'")]
  [bad-str (:: "\"" (:* (:~ "\"" "\\")
                   (:: "\\" any-char))
               (:? "\\" "\""))]
  [str (:: "\"" (:* char-elt) "\"")])

; get-comment-token: input-port -> (values string symbol boolean number number)
(define get-comment-token
  (lexer
   [comment-through-end (ret lexeme 'comment-end #f start-pos end-pos)]
   [comment-start (find-end-of-comment
                   input-port lexeme
                   (position-offset start-pos)
                   (position-offset end-pos))]
   [comment-inside (ret lexeme 'comment-inside #f start-pos end-pos)]
   [(eof) (values lexeme 'eof #f #f #f)]
   [any-char (ret lexeme 'error #f start-pos end-pos)]))

; find-end-of-comment: input-port string number number ->
;   (values string symbol boolean number number)
(define (find-end-of-comment input-port lexeme start-pos end-pos)
  (define-values (result-lexeme result-status paren comment-start-pos comment-end-pos)
    (get-comment-token input-port))
  (cond [(eq? result-status 'eof)
         (values lexeme 'error paren start-pos end-pos)]
        [(eq? result-status 'comment-end)
         (values (string-append lexeme result-lexeme)
                 'comment
                 paren start-pos comment-end-pos)]
        [(eq? result-status 'comment-inside)
         (find-end-of-comment
          input-port
          (string-append lexeme result-lexeme)
          start-pos comment-end-pos)]
        [(eq? result-status 'comment)
         (find-end-of-comment
          input-port
          (string-append lexeme result-lexeme)
          start-pos comment-end-pos)]
        [(eq? result-status 'error)
         (values (string-append lexeme result-lexeme)
                 'error
                 paren start-pos comment-end-pos)]))

(define get-string-token
  (lexer
   [str (ret lexeme 'string #f start-pos end-pos)]
   [bad-str (ret lexeme 'error #f start-pos end-pos)]
   [any-char (ret lexeme 'error #f start-pos end-pos)]
   [(special)
    (ret "" 'no-color #f start-pos end-pos)]
   [(special-comment)
    (ret "" 'no-color #f start-pos end-pos)]))

(define ocaml-lexer
  (lexer
   [whitespace (ret lexeme 'white-space #f start-pos end-pos)]
   [(eof) (values lexeme 'eof #f #f #f)]
   [double-semi (ret lexeme 'double-semi #f start-pos end-pos)]
   [comment-start (find-end-of-comment
                   input-port lexeme
                   (position-offset start-pos) (position-offset end-pos))]
   [char-literal (ret lexeme 'string #f start-pos end-pos)]
   [str (ret lexeme 'string #f start-pos end-pos)]
   [text-operator (ret lexeme 'operator #f start-pos end-pos)]
   [paren (ret lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]
   [open-close (ret lexeme 'governing-keyword #f start-pos end-pos)]
   [int-literal (ret lexeme 'number #f start-pos end-pos)]
   [float-literal (ret lexeme 'number #f start-pos end-pos)]
   [bad-float-literal (ret lexeme 'error #f start-pos end-pos)]
   [true-false (ret lexeme 'true-false #f start-pos end-pos)]
   [keyword (ret lexeme 'keyword #f start-pos end-pos)]
   [governing-keyword (ret lexeme 'governing-keyword #f start-pos end-pos)]
   [ident (ret lexeme 'identifier #f start-pos end-pos)]
   [operator (ret lexeme 'operator #f start-pos end-pos)]
   [bad-str (ret lexeme 'error #f start-pos end-pos)]
   [any-char (ret lexeme 'error #f start-pos end-pos)]
   [(special)
    (ret "" 'no-color #f start-pos end-pos)]
   [(special-comment)
    (ret "" 'no-color #f start-pos end-pos)]))

(define get-token ocaml-lexer)
