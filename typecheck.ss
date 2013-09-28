#lang scheme/gui

(require framework/framework
         "util.ss"
         mrlib/switchable-button
         (prefix-in td: "typecheck-defs.ss"))

(provide definitions-text-mixin unit-frame-mixin)

;; overrides methods that make sure that type annotations go away appropriately.
;; adds a begin/end-edit-sequence to the insertion and deletion
;;  to ensure that the on-change method isn't called until after
;;  the annotations are cleared.
(define (definitions-text-mixin drscheme:unit:definitions-text<%>)
  (mixin (drscheme:unit:definitions-text<%> scheme:text<%> ocaml:definitions-text<%>) ()
    (inherit
      begin-edit-sequence
      end-edit-sequence
      get-canvas
      get-end-position
      get-start-position
      get-top-level-window
      highlight-range
      scroll-to-position)
    
    ;; set at mode begin
    (define ocaml:in-typecheck #f)
    (define ocaml:type-annotations #f)
    
    ;; set often
    (define ocaml:current-annotation #f)
    (define ocaml:current-annotation-unhighlight-thunk #f)
    
    (super-new)
    
    (define/augment (ocaml:reset-highlighting)
      (inner (void) ocaml:reset-highlighting)
      (when ocaml:current-annotation-unhighlight-thunk
        (ocaml:current-annotation-unhighlight-thunk)
        (set! ocaml:current-annotation-unhighlight-thunk #f))
      (set! ocaml:current-annotation #f)
(when (and (get-canvas) (send (get-canvas) get-top-level-window))
  (send (send (get-canvas) get-top-level-window)
	open-status-line 'ocaml:type-check)
  (send (send (get-canvas) get-top-level-window)
	update-status-line 'ocaml:type-check #f)))
    
    (define (ocaml:jump-to-annotation pos)
      (define best-annot
        (hash-ref
         ocaml:type-annotations
         pos
         (λ () #f)))
      (cond
        [(not best-annot) (ocaml:reset-highlighting)]
        [(eq? best-annot ocaml:current-annotation) (void)]
        [else
         (ocaml:reset-highlighting)
         (set! ocaml:current-annotation best-annot)
         (send (send (get-canvas) get-top-level-window)
               update-status-line 'ocaml:type-check (td:annot-type best-annot))
         (set! ocaml:current-annotation-unhighlight-thunk
               (highlight-range
                (td:annotation-start-char best-annot)
                (td:annotation-end-char best-annot)
                (make-object color% "PaleGreen")))]))
    
    #;(define/public (ocaml:get-type-at-selection)
        (ocaml:jump-to-enclosing-annotation
         (get-start-position)
         (get-end-position)))
    
    (define/augment (on-delete start len)
      (begin-edit-sequence)
      (inner (void) on-delete start len))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (ocaml:clean-up)
      (end-edit-sequence))
    
    (define/augment (on-insert start len)
      (begin-edit-sequence)
      (inner (void) on-insert start len))
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (ocaml:clean-up)
      (end-edit-sequence))
    
    (define/override (on-event event)
      (when ocaml:type-annotations
        (cond
          [(or (send event moving?) (send event leaving?))
           (let-values ([(pos text) (ocaml:get-pos/text event this)])
             (when pos
               (ocaml:jump-to-annotation pos)))]
          [else #f]))
      (super on-event event))
    
    (define/public (ocaml:set-typecheck annots)
      (ocaml:clean-up)
      (set! ocaml:in-typecheck #t)
      (set! ocaml:type-annotations annots))
    
    (define/augment (ocaml:clean-up)
      (inner (void) ocaml:clean-up)
      (when ocaml:in-typecheck
        #;(keymap:remove-chained-keymap this td:keymap)
        (set! ocaml:in-typecheck #f)
        (set! ocaml:type-annotations #f)
        (set! ocaml:current-annotation #f)
        (ocaml:reset-highlighting)))))

(define (unit-frame-mixin drscheme:unit:frame<%> get-settings-thunk get-lang)
  (mixin (drscheme:unit:frame<%> ocaml:unit:frame<%>) ()
    
    (inherit
      get-button-panel 
      get-definitions-canvas 
      get-definitions-text
      get-interactions-text
      get-current-tab
      open-status-line
      close-status-line
      update-status-line
      ensure-rep-hidden
      save)
    
    (super-new)
    
    (define ocaml:typecheck-button-parent-panel
      (new horizontal-panel%
           [parent (get-button-panel)]
           [stretchable-width #f]
           [stretchable-height #f]))
    (define ocaml:typecheck-button
      (new button% #;switchable-button%
           [label "Save and Check Types"]
           [parent ocaml:typecheck-button-parent-panel]
           ;[bitmap (make-object bitmap% (build-path "icons" "type.png"))]
           [callback (λ (button evt) (save) (ocaml:typecheck-button-callback (send button get-parent)))]))
    
    (define/public (ocaml:typecheck-button-callback parent)
      (define defs (get-definitions-text))
      (define the-settings (get-settings-thunk))
      (when (send ocaml:typecheck-button is-enabled?)
        (if (ocaml:lang-settings? the-settings)
            (begin
              (open-status-line 'ocaml:type-check)
              (update-status-line 'ocaml:type-check
                                  (format "Type checking for ~a in progress..." 
                                          (send defs get-filename)))
              (let
                  ([annots
                    (td:compile-and-get-dtypes
                     parent
                     (send defs get-filename)
                     the-settings)])
                (if annots
                    (send defs ocaml:set-typecheck annots)
                    (begin
                      (update-status-line 'ocaml:type-check #f)
                      (message-box
                       "Error"
                       "There was an error while typechecking your program."))))
              (ensure-rep-hidden)
              #;(send (send defs get-keymap) chain-to-keymap td:keymap #t))
            (message-box
             "Wrong language"
             "You can only use the typechecker with the OCaml language."))))
    
    (define/augment (ocaml:update-button-visibility/settings settings)
      (inner (void) ocaml:update-button-visibility/settings settings)
      (when (object? ocaml:typecheck-button-parent-panel)
        (let ([visible? (send (get-lang settings) capability-value 'ocaml:typecheck-button)])
          (send ocaml:typecheck-button-parent-panel change-children
                (λ (l)
                  (if visible?
                      (list ocaml:typecheck-button)
                      '()))))))
    
    (define/public (ocaml:typecheck:get-button) ocaml:typecheck-button)
    ;(send (get-button-panel) change-children
    ;      (λ (l)
    ;        (cons ocaml:typecheck-button-parent-panel
    ;              (remove ocaml:typecheck-button-parent-panel l))))
    (inherit ocaml:update-button-visibility/tab)
    (ocaml:update-button-visibility/tab (get-current-tab))))
