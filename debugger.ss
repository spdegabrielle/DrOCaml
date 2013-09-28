#lang scheme/gui

(require framework/framework
         "util.ss"
         mrlib/switchable-button
         (prefix-in dd: "debugger-defs.ss"))

(provide
 definitions-text-mixin
 tab-mixin
 unit-frame-mixin)

(define (definitions-text-mixin drscheme:unit:definitions-text<%> get-settings-thunk)
  (mixin (drscheme:unit:definitions-text<%> scheme:text<%> ocaml:definitions-text<%>) ()
    (inherit
      backward-match
      classify-position
      forward-match
      get-tab
      get-text
      get-top-level-window
      highlight-range
      last-position
      scroll-to-position)
    (define ocaml:debug:in-handler #f)
    (define ocaml:debug:err-handler #f)
    (define ocaml:debug-current-time #f)
    (define ocaml:debug-current-direction #f)
    (define ocaml:debug-highlight-start #f)
    (define ocaml:debug-highlight-end #f)
    (define ocaml:debug-unhighlight-thunk #f)
    (define ocaml:debug-value-highlight-start #f)
    (define ocaml:debug-value-highlight-end #f)
    (define ocaml:debug-value-unhighlight-thunk #f)
    (define ocaml:debug-breakpoints (make-hash))
    
    (define bp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define bp-brush (send the-brush-list find-or-create-brush "red" 'solid))
    (define bp-mo-pen (send the-pen-list find-or-create-pen "darkgray" 1 'solid))
    (define bp-mo-brush (send the-brush-list find-or-create-brush "pink"
                              'solid))
    (define bp-tmp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define bp-tmp-brush (send the-brush-list find-or-create-brush "yellow"
                               'solid))
    (define pc-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-brush (send the-brush-list find-or-create-brush "forestgreen" 'solid))
    (define pc-err-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-err-brush (send the-brush-list find-or-create-brush "red" 'solid))
    (define pc-brk-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define pc-brk-brush (send the-brush-list find-or-create-brush "gray" 'solid))
    
    (super-new)
    
    (define/public (ocaml:debug:get-breakpoints) ocaml:debug-breakpoints)
    
    (define (average . values)
      (/ (apply + values) (length values)))
    
    (define/augment (after-set-next-settings settings)
      (inner (void) after-set-next-settings settings)
      (let ([frame (send (get-tab) get-frame)])
        (when frame
          (send frame ocaml:update-button-visibility/settings settings))))
    
    (define/public (ocaml:get-direction) ocaml:debug-current-direction)
    
    (define/public (ocaml:debug:set-in-handler handler)
      (set! ocaml:debug:in-handler handler))
    
    (define/public (ocaml:debug:set-err-handler handler)
      (set! ocaml:debug:err-handler handler))
    
    (define/public (ocaml:debug:pause-handlers)
      (when ocaml:debug:in-handler
        (thread-suspend ocaml:debug:in-handler))
      (when ocaml:debug:err-handler
        (thread-suspend ocaml:debug:err-handler)))
    
    (define/public (ocaml:debug:resume-handlers)
      (when ocaml:debug:in-handler
        (thread-resume ocaml:debug:in-handler))
      (when ocaml:debug:err-handler
        (thread-resume ocaml:debug:err-handler)))
    
    (define/public (ocaml:set-debug-time time)
      (let ([old-time (or ocaml:debug-current-time 0)]
            [new-time (or time 0)])
        (set! ocaml:debug-current-time time)
        (if (< old-time new-time)
            (set! ocaml:debug-current-direction 'forward)
            (set! ocaml:debug-current-direction 'backward))))
    
    (define/public (ocaml:set-debug-status message)
      (define frame (send (get-tab) get-frame))
      (if ocaml:debug-current-time
          (send frame update-status-line 'ocaml:debugger
                (clean-status (format "(Time: ~a) ~a" ocaml:debug-current-time message)))
          (send frame update-status-line 'ocaml:debugger (clean-status message))))
    
    (define/augment (ocaml:clean-up)
      (inner (void) ocaml:clean-up)
      (ocaml:reset-highlighting)
      (set! ocaml:debug:in-handler #f)
      (set! ocaml:debug:err-handler #f)
      (set! ocaml:debug-current-time #f)
      (set! ocaml:debug-current-direction #f)
      (send (get-tab) ocaml:kill-debug)
      (send (send (get-tab) get-frame) ocaml:hide-debug))
    
    (inherit
      dc-location-to-editor-location
      editor-location-to-dc-location
      invalidate-bitmap-cache
      get-canvas)
    
    (define/private (find-char-box text left-pos right-pos)
      (let ([xlb (box 0)]
            [ylb (box 0)]
            [xrb (box 0)]
            [yrb (box 0)])
        (send text position-location left-pos xlb ylb #t)
        (send text position-location right-pos xrb yrb #f)
        (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location
                                             (unbox xlb) (unbox ylb))]
                      [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                      [(xr-off yr-off) (send text editor-location-to-dc-location
                                             (unbox xrb) (unbox yrb))]
                      [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
          (values xl yl xr yr))))
    
    (define (clean-status s)
(substring (regexp-replace* #rx"\n" s " ") 0 (min (string-length s) 200)))
    
    (define/override (on-event event)
      (if (ocaml:lang-settings? (get-settings-thunk))
          (cond
            #;[(send event button-down? 'right)
             (let-values ([(pos text) (ocaml:get-pos/text event this)])
               (if (and pos text)
                   (let ([menu (make-object popup-menu% #f)]
                         [break-status (hash-ref ocaml:debug-breakpoints pos (lambda () #f))])
                     (make-object menu-item%
                       (if break-status
                           "Remove pause at this point"
                           "Pause at this point")
                       menu
                       (lambda (item evt)
                         (hash-set! ocaml:debug-breakpoints pos (not break-status))
                         (when (send (get-tab) ocaml:get-debug-process)
                           (dd:update-breakpoints (get-tab) ocaml:debug-breakpoints))
                         (invalidate-bitmap-cache)))
                     (send (get-canvas) popup-menu menu
                           (+ 1 (inexact->exact (floor (send event get-x))))
                           (+ 1 (inexact->exact (floor (send event get-y))))))
                   (super on-event event)))]
            [(send (get-tab) ocaml:get-debug-process)
             (cond
               [(or (send event moving?) (send event leaving?))
                (let-values ([(pos text) (ocaml:get-pos/text event this)])
                  (when pos
                    (ocaml:jump-to-debug-token pos)))
                (super on-event event)]
               [(send event button-down? 'middle)]
               [(send event button-up? 'middle)
                (let-values ([(pos text) (ocaml:get-pos/text event this)])
                  (when pos
                    (ocaml:set-breakpoint pos)))]
               [else (super on-event event)])]
            [else (super on-event event)])
          (super on-event event)))
    
    (define/override (on-paint before dc left top right bottom dx dy draw-caret)
      (super on-paint before dc left top right bottom dx dy draw-caret)
      (when (not before)
        (hash-for-each
         ocaml:debug-breakpoints
           (lambda (pos enabled?)
             (when (and (>= pos 0) enabled? #;(or enabled? (and mouse-over-pos (= mouse-over-pos pos))))
               (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                             [(diameter) (max 0 (- xr xl))]
                             [(yoff) (/ (- yr yl diameter) 2)])
                 (let ([op (send dc get-pen)]
                       [ob (send dc get-brush)])
                   (case enabled?
                     [(#t) (send dc set-pen bp-pen)
                           (send dc set-brush bp-brush)]
                     [(#f) (send dc set-pen bp-mo-pen)
                           (send dc set-brush bp-mo-brush)]
                     [else (send dc set-pen bp-tmp-pen)
                           (send dc set-brush bp-tmp-brush)])
                   ;(drscheme:arrow:draw-arrow dc xl yl xr yr dx dy)
                   (send dc draw-ellipse (+ xl dx) (+ yl dy yoff) diameter diameter)
                   #;
                   (send dc draw-polygon stop-sign
                         (+ xl dx)
                         (+ yl dy 2))
                   (send dc set-pen op)
                   (send dc set-brush ob)))))))
        (let ([pos (send (get-tab) get-pc)])
          (when pos
            (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                          [(ym) (average yl yr)])
              (let ([op (send dc get-pen)]
                    [ob (send dc get-brush)])
                (case (send (get-tab) get-break-status)
                  [(error) (send dc set-pen pc-err-pen)
                           (send dc set-brush pc-err-brush)]
                  [(break) (send dc set-pen pc-brk-pen)
                           (send dc set-brush pc-brk-brush)]
                  [else    (send dc set-pen pc-pen)
                           (send dc set-brush pc-brush)]))
             #;(drscheme:arrow:draw-arrow dc xl ym xr ym dx dy))
            )))
    
    (define/public (ocaml:jump-to-debug-token pos)
      (define id-end (forward-match pos (last-position)))
      (define id-start (and id-end (backward-match id-end 0)))
      (when
          (and
           id-start
           (> id-end pos)
           (eq? 'identifier (classify-position id-start)))
        (ocaml:set-debug-value-highlighting id-start id-end)
        (dd:inspect-value (get-tab) (get-text id-start id-end))))
    
    (define/public (ocaml:set-breakpoint pos)
      (define-values (num address line-num start-char end-char)
        (dd:update-breakpoints (get-tab) pos))
      (highlight-range pos (add1 pos) (make-object color% "Red"))
      (scroll-to-position pos)
      #;(define id-end (forward-match pos (last-position)))
      #;(define id-start (and id-end (backward-match id-end 0)))
      #;(when
            (and
             id-start
             (> id-end pos)
             (eq? 'identifier (classify-position id-start)))
          (ocaml:set-debug-highlighting id-start id-end)
          (dd:inspect-value (get-tab) (get-text id-start id-end))))
    
    (define/augment (ocaml:reset-highlighting)
      (define frame (send (get-tab) get-frame))
      (inner (void) ocaml:reset-highlighting)
      (when ocaml:debug-unhighlight-thunk
        (ocaml:debug-unhighlight-thunk)
        (set! ocaml:debug-highlight-start #f)
        (set! ocaml:debug-highlight-end #f)
        (set! ocaml:debug-unhighlight-thunk #f))
      (ocaml:reset-minor-debug-highlighting))
    
    (define/public (ocaml:reset-minor-debug-highlighting)
      (define frame (send (get-tab) get-frame))
      (when ocaml:debug-value-unhighlight-thunk
        (ocaml:debug-value-unhighlight-thunk)
        (set! ocaml:debug-value-highlight-start #f)
        (set! ocaml:debug-value-highlight-end #f)
        (set! ocaml:debug-value-unhighlight-thunk #f))
(send frame open-status-line 'ocaml:debugger)
      (send frame update-status-line 'ocaml:debugger #f))
    
    (define/public (ocaml:set-debug-value-highlighting start end)
      (unless (and
               (eq? ocaml:debug-value-highlight-start start)
               (eq? ocaml:debug-value-highlight-end end))
        (ocaml:reset-minor-debug-highlighting)
        (set! ocaml:debug-value-highlight-start start)
        (set! ocaml:debug-value-highlight-end end)
        (set! ocaml:debug-value-unhighlight-thunk
              (highlight-range
               start end
               (make-object color% "MediumGoldenrod")
               #f #f 'high))))
    
    (define/public (ocaml:set-debug-highlighting start end)
      (unless (and
               (eq? ocaml:debug-highlight-start start)
               (eq? ocaml:debug-highlight-end end))
        (ocaml:reset-highlighting)
        (set! ocaml:debug-highlight-start start)
        (set! ocaml:debug-highlight-end end)
        (set! ocaml:debug-unhighlight-thunk
              (highlight-range start end (make-object color% "lavender")))))))

(define (tab-mixin drscheme:unit:tab<%>)
  (mixin (drscheme:unit:tab<%>) ()
    (inherit get-frame)
    ;; set at mode begin
    (define ocaml:in-debug #f)
    (define ocaml:debug-process #f)
    (super-new)
    (define/override (break-callback)
      (ocaml:kill-debug)
      (send (get-frame) ocaml:hide-debug)
      (super break-callback))
    (define/public (ocaml:kill-debug)
      (when (ocaml:process? ocaml:debug-process)
        (let ([proc (ocaml:process-proc ocaml:debug-process)])
          (subprocess-kill proc #f)
          (subprocess-kill proc #t)))
      (set! ocaml:debug-process #f))
    (define/public (ocaml:get-debug-process) ocaml:debug-process)
    (define/public (ocaml:set-debug-process process-obj)
      (set! ocaml:debug-process process-obj))))

(define (unit-frame-mixin drscheme:unit:frame<%> get-settings-thunk get-lang)
  (mixin (drscheme:unit:frame<%>) (ocaml:unit:frame<%>)
    (inherit
      get-button-panel
      get-definitions-canvas
      get-definitions-text
      get-interactions-text
      get-current-tab
      open-status-line
      close-status-line
      update-status-line
      save)
    
    (define ocaml:debug-parent-panel 'uninitialized-debug-parent-panel)
    (define ocaml:debug-panel 'uninitialized-debug-panel)
    (define/override (get-definitions/interactions-panel-parent)
      (set! ocaml:debug-parent-panel
            (make-object vertical-panel%
              (super get-definitions/interactions-panel-parent)))
      (set! ocaml:debug-panel (instantiate horizontal-panel% ()
                                (parent ocaml:debug-parent-panel)
                                (stretchable-height #f)
                                (alignment '(center center))
                                (style '(border))))
      (send ocaml:debug-parent-panel change-children (λ (l) null))
      #;(instantiate button% ()
          (label "Hide")
          (parent ocaml:debug-panel)
          (callback (λ (x y) (ocaml:hide-debug)))
          (stretchable-height #t))
      (make-object vertical-panel% ocaml:debug-parent-panel))
    
    (super-new)
    
    (define ocaml:debugger-button-parent-panel
      (new horizontal-panel%
           [parent (get-button-panel)]
           [stretchable-width #f]
           [stretchable-height #f]))
    (define ocaml:debugger-button
      (new button% #;switchable-button%
           [label "Save and Debug"]
           [parent ocaml:debugger-button-parent-panel]
           ;[bitmap (make-object bitmap% (build-path "icons" "debug.png"))]
           [callback
            (λ (button evt)
              (save)
              (dd:debug-callback
               (get-definitions-text)
               (send button get-parent)
               (get-settings-thunk)))]))
    
    (define/public (ocaml:hide-debug)
      (when (and (object? ocaml:debug-parent-panel)
  (member ocaml:debug-panel (send ocaml:debug-parent-panel get-children)))
        (send ocaml:debug-parent-panel change-children
              (λ (l) (remq ocaml:debug-panel l)))))
    
    (define/public (ocaml:show-debug)
      (unless (member ocaml:debug-panel (send ocaml:debug-parent-panel get-children))
        (send ocaml:debug-parent-panel change-children
              (λ (l) (cons ocaml:debug-panel l)))))
    
    (define/override (execute-callback)
      (when (eq? (system-type 'os) 'windows)
        (let-values ([(proc in out err)
                      (subprocess #f #f #f "c:\\cygwin\\bin\\killall.exe" "-v" "-9" "ocamlrun")])
          (subprocess-wait proc)
          (sleep 0.1)))
      (send (get-current-tab) ocaml:kill-debug)
      (super execute-callback))
    
    (define/augment (on-close)
      (inner (void) on-close)
      (when (eq? (system-type 'os) 'windows)
        (let-values ([(proc in out err)
                      (subprocess #f #f #f "c:\\cygwin\\bin\\killall.exe" "-v" "-9" "ocamlrun")])
          (subprocess-wait proc)
          (sleep 0.1))))
    
    ;; single-action: reverse
    ;; "Execute backwards until the previous breakpoint or the beginning of the program."
    (define ocaml:reverse-button
      (instantiate button% ()
        [label "Reverse"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "reverse" 'backward))]
        [enabled #t]))
    
    ;; repeat-action: previous
    ;; "Execute backwards until the previous event, skipping any function calls."
    (define ocaml:previous-button
      (instantiate button% ()
        [label "Previous"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "previous" 'backward))]
        [enabled #t]))
    
    ;; repeat-action: start
    ;; "Execute backwards until just before the current function call."
    (define ocaml:start-button
      (instantiate button% ()
        [label "Start"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "start" 'backward))]
        [enabled #t]))
    
    ;; repeat-action: backstep
    ;; "Execute backwards until the previous event, entering any function calls."
    (define ocaml:backstep-button
      (instantiate button% ()
        [label "Backstep"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "backstep" 'backward))]
        [enabled #t]))
    
    ;; break-process: break
    ;; "Stop execution."
    (define ocaml:break-button
      (instantiate button% ()
        [label "Break"]
        [parent ocaml:debug-panel]
        [callback (λ (button evt) (dd:break-callback (get-current-tab)))]
        [enabled #t]))
    
    ;; repeat-action: step
    ;; "Execute forwards until the next event, entering any function calls."
    (define ocaml:step-button
      (instantiate button% ()
        [label "Step"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "step" 'forward))]
        [enabled #t]))
    
    ;; repeat-action: finish
    ;; "Execute forwards until just after the current function call."
    (define ocaml:finish-button
      (instantiate button% ()
        [label "Finish"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "finish" 'forward))]
        [enabled #t]))
    
    ;; repeat-action: next
    ;; "Execute forwards until the next event, skipping any function calls."
    (define ocaml:next-button
      (instantiate button% ()
        [label "Next"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "next" 'forward))]
        [enabled #t]))
    
    ;; single-action: run
    ;; "Execute forwards until the next breakpoint or the end of the program."
    (define ocaml:run-button
      (instantiate button% ()
        [label "Run"]
        [parent ocaml:debug-panel]
        [callback
         (λ (button evt)
           (dd:repeat-action-callback (get-current-tab) "run" 'forward))]
        [enabled #t]))
    
    (define/augment (on-tab-change old-tab new-tab)
      (inner (void) on-tab-change old-tab new-tab)
      (ocaml:update-button-visibility/tab new-tab))
    (define/public (ocaml:update-button-visibility/tab tab)
      (ocaml:update-button-visibility/settings (send (send tab get-defs) get-next-settings)))

    (define/pubment (ocaml:update-button-visibility/settings settings)
      (inner (void) ocaml:update-button-visibility/settings settings)
      (when (object? ocaml:debugger-button-parent-panel)
        (let ([visible? (send (get-lang settings) capability-value 'ocaml:debug-button)])
          (send ocaml:debugger-button-parent-panel change-children
                (λ (l)
                  (if visible?
                      (list ocaml:debugger-button)
                      '()))))))
    
    (define/public (ocaml:debugger:get-button) ocaml:debugger-button)
    (send (get-button-panel) change-children
          (λ (l)
            (cons ocaml:debugger-button-parent-panel
                  (remove ocaml:debugger-button-parent-panel l))))
    (ocaml:update-button-visibility/tab (get-current-tab))))
