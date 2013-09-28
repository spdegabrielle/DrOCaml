#lang scheme/unit

(require drscheme/tool
         framework/framework
         scheme/class
         scheme/contract
         scheme/list
         (prefix-in lang: "language.ss")
         (prefix-in debug: "debugger.ss")
         (prefix-in typecheck: "typecheck.ss")
         (prefix-in indent: "indentation.ss")
         (prefix-in mode: "text-mode.ss"))

(import drscheme:tool^)
(export drscheme:tool-exports^)
(define (get-lang settings)
  (drscheme:language-configuration:language-settings-language
   settings))
(define (get-settings-thunk)
  (drscheme:language-configuration:language-settings-settings
   (preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))))
(define (phase1) (void))
(define (phase2)
  (drscheme:language-configuration:add-language
   (make-object
       ((drscheme:language:get-default-mixin)
        (lang:language% drscheme:language:language<%>))))
  
  (drscheme:language:register-capability 'ocaml:debug-button (flat-contract boolean?) #f)
  (drscheme:language:register-capability 'ocaml:typecheck-button (flat-contract boolean?) #f)
  
  (drscheme:get/extend:extend-interactions-text
   (lang:interactions-text-mixin drscheme:rep:text<%>))
  (drscheme:get/extend:extend-definitions-text
   (lang:definitions-text-mixin drscheme:unit:definitions-text<%>))
  (drscheme:get/extend:extend-unit-frame
   (debug:unit-frame-mixin drscheme:unit:frame<%> get-settings-thunk get-lang))
  (drscheme:get/extend:extend-tab
   (debug:tab-mixin drscheme:unit:tab<%>))
  (drscheme:get/extend:extend-definitions-text
   (debug:definitions-text-mixin drscheme:unit:definitions-text<%> get-settings-thunk))
  (drscheme:get/extend:extend-unit-frame
   (typecheck:unit-frame-mixin drscheme:unit:frame<%> get-settings-thunk get-lang))
  (drscheme:get/extend:extend-definitions-text
   (typecheck:definitions-text-mixin drscheme:unit:definitions-text<%>))
  (drscheme:get/extend:extend-interactions-text
   (indent:indent-mixin drscheme:rep:text<%>))
  (drscheme:get/extend:extend-definitions-text
   (indent:indent-mixin drscheme:unit:definitions-text<%>))
  
  (drscheme:modes:add-mode
   "OCaml mode" (make-object mode:text-mode%) mode:repl-submit mode:matches-language)
  #;(preferences:add-to-editor-checkbox-panel extend-editor-preferences-panel)
  (color-prefs:add-to-preferences-panel "OCaml" mode:extend-color-preferences-panel)
  (for-each
   (λ (line)
     (let ([sym (first line)]
           [color (second line)])
       (color-prefs:register-color-preference
        (mode:short-sym->pref-name sym)
        (mode:short-sym->style-name sym)
        color)))
   mode:color-prefs-table))
