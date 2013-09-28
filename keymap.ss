#lang scheme

(require framework/framework)

(provide setup-keymap
         keymap)

(define (setup-keymap keymap)        
  (local ((define (add-edit-function name call-method)
            (send keymap add-function name
                  (λ (edit event)
                    (call-method edit)))))
    (add-edit-function "tabify-at-caret"  
                       (λ (x) (send x tabify-selection)))
    (add-edit-function "do-return"  
                       (λ (x) 
                         (send x insert-return)))
    #;(add-edit-function "do-pipe"  
                         (λ (x) 
                           (send x insert-pipe))))
  
  (send keymap add-function "balance-parens"
        (λ (edit event)
          (send edit balance-parens event)))
  
  (send keymap map-function "TAB" "tabify-at-caret")
  
  (send keymap map-function "return" "do-return")
  (send keymap map-function "s:return" "do-return")
  (send keymap map-function "s:c:return" "do-return")
  (send keymap map-function "a:return" "do-return")
  (send keymap map-function "s:a:return" "do-return")
  (send keymap map-function "c:a:return" "do-return")
  (send keymap map-function "c:s:a:return" "do-return")
  (send keymap map-function "c:return" "do-return")
  (send keymap map-function "d:return" "do-return")
  
  #;(send keymap map-function "|" "do-pipe") 
  (send keymap map-function ")" "balance-parens")
  (send keymap map-function "]" "balance-parens")
  (send keymap map-function "}" "balance-parens")
  
  (let ([map-meta
         (λ (key func)
           (keymap:send-map-function-meta keymap key func))]
        [map
         (λ (key func)
           (send keymap map-function key func))])
    
    #;(map-meta "|" "do-pipe")
    (map-meta "return" "do-return")
    (map-meta "s:return" "do-return")
    (map-meta "s:c:return" "do-return")
    (map-meta "a:return" "do-return")
    (map-meta "s:a:return" "do-return")
    (map-meta "c:a:return" "do-return")
    (map-meta "c:s:a:return" "do-return")
    (map-meta "c:return" "do-return")))

(define keymap (make-object keymap:aug-keymap%))
(setup-keymap keymap)
(define (get-keymap) keymap)
