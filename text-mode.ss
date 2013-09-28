#lang scheme/gui

(require framework/framework
         "util.ss"
         string-constants/string-constant
         (prefix-in ocaml: "keymap.ss")
         (prefix-in lex: "lexer.ss"))

(provide color-prefs-table
         short-sym->pref-name
         short-sym->style-name
         extend-color-preferences-panel
         text-mode-mixin
         repl-submit
         matches-language
         text-mode%)

;Set the OCaml editing colors
(define color-prefs-table
  `([keyword ,(make-object color% "purple") ,"keyword"]
    [governing-keyword ,(make-object color% "blue") ,"governing keyword"]
    [true-false ,(make-object color% "black") ,"true or false"]
    [string ,(make-object color% "forestgreen") ,"string"]
    [number ,(make-object color% "black") ,"number"]
    [comment ,(make-object color% 194 116 31) ,"comment"]
    [error ,(make-object color% "red") ,"error"]
    [identifier ,(make-object color% 38 38 128) ,"identifier"]
    [operator ,(make-object color% "brown") ,"operator"]
    [parenthesis ,(make-object color% "hotpink") ,"operator"]
    [default ,(make-object color% "black") ,"default"]))

#;(define editor-prefs-table
    `([indent-pipe-to-match]))

;; short-sym->pref-name : symbol -> symbol
;; returns the preference name for the color prefs
(define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))

;; short-sym->style-name : symbol->string
;; converts the short name (from the table above) into a name in the editor list
;; (they are added in by `color-prefs:register-color-pref', called below)
(define (short-sym->style-name sym)
  (case sym
    [('double-semi) "ocaml:syntax-coloring:scheme:operator"]
    [else (format "ocaml:syntax-coloring:scheme:~a" sym)]))

#;(define (extend-editor-preferences-panel parent) ())

;; extend-color-preferences-panel : vertical-panel -> void
;; adds in the configuration for the OCaml colors to the prefs panel
(define (extend-color-preferences-panel parent)
  (for-each
   (lambda (line)
     (let ([sym (first line)])
       (color-prefs:build-color-selection-panel 
        parent
        (short-sym->pref-name sym)
        (short-sym->style-name sym)
        (format "~a" sym))))
   color-prefs-table))

(define text-mode-mixin
  (mixin (color:text-mode<%> mode:surrogate-text<%>) (mode:surrogate-text<%>)
    (define orig-filters #f)
    (define/override (on-disable-surrogate text)
      (when orig-filters (finder:default-filters orig-filters) (set! orig-filters #f))
      (send text set-use-ocaml-indenter #f)
      (keymap:remove-chained-keymap text ocaml:keymap)
      (super on-disable-surrogate text))
    (define/override (on-enable-surrogate text)
      (unless orig-filters (set! orig-filters (finder:default-filters)))
      (finder:default-filters '(("OCaml" "*.ml")))
      (send (send text get-keymap) chain-to-keymap ocaml:keymap #t)
      (send text set-use-ocaml-indenter #t)
      (send text begin-edit-sequence)
      (super on-enable-surrogate text)
      (send text set-load-overwrites-styles #f)
      (let
          ([bw (box 0)]
           [bu (box #f)]
           [tab-size (send text get-tab-size)])
        (unless
            (and
             (null? (send text get-tabs #f bw bu))
             (= tab-size (unbox bw))
             (not (unbox bu)))
          (send text set-tabs null (send text get-tab-size) #f)))
      (send text set-styles-fixed #t)
      (send text end-edit-sequence))
    
    (super-new
     (get-token lex:get-token)
     (token-sym->style short-sym->style-name)
     (matches
      '((|(| |)|)
        (|[| |]|)
        (|{| |}|))))))

(define (repl-submit editor position)
  (define defs (send editor get-definitions-text))
  (define tab (if (object? defs) (send defs get-tab) #f))
  (define debug-process-obj (if (object? tab) (send tab ocaml:get-debug-process) #f))
  (if debug-process-obj
      #t
      (let*-values
          ([(in) (open-input-string (send editor get-text position))]
           [(expr index) (ocaml:read-expr in)])
        (not (memq expr `(,eof error))))))

(define (matches-language name-list)
  (and (equal? (first name-list) (string-constant experimental-languages))
       (equal? (second name-list) "OCaml")))

(define text-mode% (text-mode-mixin color:text-mode%))

