#lang scheme/gui

(require framework/framework
         string-constants/string-constant
         "util.ss"
         (prefix-in ld: "language-defs.ss"))

(provide interactions-text-mixin
         definitions-text-mixin
         language%)

(define (interactions-text-mixin drscheme:rep:text<%>)
  (mixin (drscheme:rep:text<%>) ()
    (inherit scroll-to-position last-position)
    (inherit-field prompt-position)
    (define delayed-prompt-position 0)
    (define delayed-highlight-args #f)
    (super-new)
    (define/augment (on-submit)
      (inner (void) on-submit)
      (set! delayed-prompt-position prompt-position))
    (define/public (delayed-highlight-error args)
      (set! delayed-highlight-args args))
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (scroll-to-position (last-position)))
    (define/override (insert-prompt)
      (super insert-prompt)
      (when delayed-highlight-args
        (send this highlight-error this
              (+ 1 (first delayed-highlight-args))
              (+ 1 (second delayed-highlight-args)))
        (set! delayed-highlight-args #f)))
    (define/override (kill-evaluation)
      (when (eq? (system-type 'os) 'windows)
        (let-values ([(proc in out err)
                      (subprocess #f #f #f "c:\\cygwin\\bin\\killall.exe" "-v" "-9" "ocamlrun")])
          (subprocess-wait proc)
          (sleep 0.1)))
      (super kill-evaluation))
    (define/public (ocaml:found-error?) (if delayed-highlight-args #t #f))
    (define/public (set-delayed-prompt-position pos) (set! delayed-prompt-position pos))
    (define/public (get-prompt-position) delayed-prompt-position)))

(define (definitions-text-mixin drscheme:unit:definitions-text<%>)
  (mixin (drscheme:unit:definitions-text<%> scheme:text<%>) (ocaml:definitions-text<%>)
    (define ocaml:error-unhighlight-thunk #f)
    (define delayed-prompt-position #f)
    (inherit
      highlight-range
      set-position)
    (super-new)
    (define/public (delayed-highlight-error args)
      (ocaml:reset-highlighting)
      (set-position (first args))
      (set!
       ocaml:error-unhighlight-thunk
       (highlight-range (first args) (second args) (make-object color% "Pink"))))
    (define/augment (on-change)
      (ocaml:reset-highlighting)
      (inner (void) on-change))
    (define/pubment (ocaml:reset-highlighting)
      (inner (void) ocaml:reset-highlighting) 
      (set-delayed-prompt-position 0)
      (when ocaml:error-unhighlight-thunk
        (ocaml:error-unhighlight-thunk)
        (set! ocaml:error-unhighlight-thunk #f)))
    (define/pubment (ocaml:clean-up)
      (inner (void) ocaml:clean-up))
    (define/public (ocaml:found-error?) (if ocaml:error-unhighlight-thunk #t #f))
    (define/public (set-delayed-prompt-position pos) (set! delayed-prompt-position pos))
    (define/public (get-prompt-position) delayed-prompt-position)))

(define (language% drscheme:language:language<%>)
  (class* object% (drscheme:language:language<%>)
    (super-new)
    
    (define/public (marshall-settings settings)
      (list
       (ocaml:lang-settings-toplevel settings)
       (ocaml:lang-settings-compiler settings)
       (ocaml:lang-settings-debugger settings)
       (ocaml:lang-settings-modules settings)
       (ocaml:lang-settings-includes settings)))
    
    (define/public (unmarshall-settings input)
      (if (and
           (list? input)
           (eq? (procedure-arity ocaml:make-lang-settings) (length input)))
          (apply ocaml:make-lang-settings input)
          (default-settings)))
    
    (define/public (default-settings)
      (if (eq? (system-type 'os) 'windows)
          (ocaml:make-lang-settings
           (path->string (or (find-executable-path "ocaml.exe" #f)
                             (build-path "c:\\cygwin\\bin\\ocaml.exe")))
           (path->string (or (find-executable-path "ocamlc.exe" #f)
                             (build-path "c:\\cygwin\\bin\\ocamlc.exe")))
           (path->string (or (find-executable-path "ocamldebug.exe" #f)
                             (build-path "c:\\cygwin\\bin\\ocamldebug.exe")))
           "" "")
          (ocaml:make-lang-settings
           (path->string (or (find-executable-path "ocaml" #f)
                             (build-path "/usr/local/bin/ocaml")))
           (path->string (or (find-executable-path "ocamlc" #f)
                             (build-path "/usr/local/bin/ocamlc")))
           (path->string (or (find-executable-path "ocamldebug" #f)
                             (build-path "/usr/local/bin/ocamldebug")))
           "" "")))
    
    (define/public (default-settings? settings) #t)
      ;(equal? settings (default-settings))

    (define/public (order-manuals manuals) (values manuals #t))

    (define/public (front-end/complete-program port settings)
      (match-let ([(struct ocaml:process (_ in out err))
                   (ld:ocaml-process-info)])
        (if (subprocess? (ocaml:process-proc (ld:ocaml-process-info)))
            (begin
              ;(send (object-name port) ocaml:reset-highlighting)
              (λ () (ld:process-one-expr port in out err 0 #;(send (object-name port) get-prompt-position)
                                         )))
            (begin
              (ocaml:not-installed)
              (λ () eof)))))
    
    (define/public (front-end/finished-complete-program settings)
      (void))
    
    (define/public (front-end/interaction port settings)
      (cond
        [(ocaml:process? (ld:ocamldebug-process-info))
         ; If the debugger is running, pass input to it.
         (match (ld:ocamldebug-process-info)
           ((struct ocaml:process (_ in out err))
            (λ ()
              (write-string (read-line port) out)
              (newline out)
              (flush-output out)
              eof)))]
        [(ocaml:process? (ld:ocaml-process-info))
         ; If not, pass input to the toplevel.
         (match (ld:ocaml-process-info)
           ((struct ocaml:process (_ in out err))
            (λ () (ld:process-one-expr port
                                       in
                                       out
                                       err
                                       0 #;(send (object-name port) get-prompt-position)
                                       ))))]
        [else
         ; If neither is possible, then probably the toplevel isn't installed properly (or at all).
         (ocaml:not-installed)
         (λ () eof)]))
    
    (define/public (config-panel _parent)
      (letrec ([parent
                (new vertical-panel%
                     [parent _parent]
                     [alignment '(center center)])]
               
               [locations-panel
                (new group-box-panel%
                     [label "Locations"]
                     [parent parent]
                     [alignment '(left center)])]
               
               [toplevel
                (new text-field%
                     [label (if (eq? (system-type 'os) 'windows)
                                "OCaml Toplevel (ocaml.exe)"
                                "OCaml Toplevel (ocaml)")]
                     [style '(single vertical-label)]
                     [parent locations-panel])]
               
               [compiler
                (new text-field%
                     [label (if (eq? (system-type 'os) 'windows)
                                "OCaml Compiler (ocamlc.exe)"
                                "OCaml Compiler (ocamlc)")]
                     [style '(single vertical-label)]
                     [parent locations-panel])]
               
               [debugger
                (new text-field%
                     [label (if (eq? (system-type 'os) 'windows)
                                "OCaml Debugger (ocamldebug.exe)"
                                "OCaml Debugger (ocaml)")]
                     [style '(single vertical-label)]
                     [parent locations-panel])]
               
               [modules
                (new text-field%
                     [label "Additional modules to load (separated by spaces):"]
                     [style '(single vertical-label)]
                     [parent locations-panel])]
               
               [includes
                (new text-field%
                     [label "Include path (directories separated by semicolons):"]
                     [style '(single vertical-label)]
                     [parent locations-panel])])
        
        (case-lambda
          [()
           (apply
            ocaml:make-lang-settings
            (map (λ (x) (send x get-value)) (list toplevel compiler debugger modules includes)))]
          [(settings)
           (when (and settings (ocaml:lang-settings? settings))
             (send toplevel set-value (ocaml:lang-settings-toplevel settings))
             (send compiler set-value (ocaml:lang-settings-compiler settings))
             (send debugger set-value (ocaml:lang-settings-debugger settings))
             (send modules set-value (ocaml:lang-settings-modules settings))
             (send includes set-value (ocaml:lang-settings-includes settings)))])))
    
    (define/public (on-execute settings run-in-user-thread)
      (let ([user-thread #f])
        (run-in-user-thread
         (λ ()
           (set! user-thread (current-thread))))
        (let ([process-info (ld:start-ocaml-process settings)])
          (begin
            (run-in-user-thread
             (λ ()
               (ld:ocaml-process-info process-info)))
            (thread
             (λ ()
               (thread-wait user-thread)
               (ld:stop-ocaml-process process-info)))))))
    
    (define/public (extra-repl-information x y) #f)    
    
    (define/public (first-opened) (void))
    
    (define/public (render-value/format value settings port width)
      (display value))
    
    (define/public (render-value value settings port)
      (display value))
    
    (define/public (capability-value key)
      (match key
        ('drscheme:language-menu-title "&OCaml")
        ('drscheme:define-popup (cons "let" "let ..."))
        ('ocaml:debug-button #t)
        ('ocaml:typecheck-button #t)
        (_ #f)))
    
    (define/public (create-executable settings parent program-filename)
      (define executable-name (path-replace-suffix program-filename ".exe"))
      (with-handlers
          ([exn:fail? (λ (exn) (ocaml:not-installed))])
        (let*-values
            ([(lsm) (ocaml:lang-settings-modules settings)]
             [(lsi)
              (let
                  ([path-list
                    (filter
                     (λ (x) (not (regexp-match "^[ \t\n]*$" x)))
                     (regexp-split ";" (ocaml:lang-settings-includes settings)))])
                (apply string-append (map (λ (x) (format "-I ~a " x)) path-list)))]
             [(args)
              (filter
               (λ (x) (not (equal? x "")))
               (list
                #f #f #f
                (ocaml:lang-settings-compiler settings)
                "-o" executable-name
                lsm
                lsi
                program-filename))]
             [(proc in out err)
              (apply subprocess args)])
          (subprocess-wait proc)
          (unless (= (subprocess-status proc) 0)
            (message-box
             "Compilation error"
             "The file failed to compile."
             parent
             'ok)))))
    
    
    (define/public (get-reader-module) #f)
    (define/public (get-metadata) #f)
    (define/public (metadata->settings metadata) #f)
    (define/public (get-metadata-lines) #f)
    
    (define/public (get-language-position) (list (string-constant experimental-languages) "OCaml"))
    (define/public (get-language-name) "OCaml")
    (define/public (get-style-delta) #f)
    (define/public (get-language-numbers) (list 1000 -3447))
    (define/public (get-one-line-summary) "The OCaml language")
    (define/public (get-language-url) "http://caml.inria.fr")
    (define/public (get-comment-character) (values "*" #\*))))
