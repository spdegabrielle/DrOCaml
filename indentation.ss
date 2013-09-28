#lang scheme

(require framework/framework)

(provide indent-mixin)
(define (indent-mixin other-interface)
  (mixin (scheme:text<%> other-interface) (scheme:text<%> other-interface)
    (inherit
      get-start-position
      last-position
      position-paragraph
      classify-position
      paragraph-start-position
      get-limit
      backward-containing-sexp
      backward-match
      get-character
      delete
      insert
      forward-match
      get-text
      find-up-sexp
      find-string
      tabify-on-return?
      begin-edit-sequence
      end-edit-sequence
      set-position)
    (define use-ocaml-indenter #f)
    (super-new)
    (define/public (set-use-ocaml-indenter bool)
      (set! use-ocaml-indenter bool))
    (define/override tabify
      (lambda ([pos (get-start-position)])
        (if use-ocaml-indenter
            (ocaml:try-indent pos)
            (super tabify pos))))
    
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (when
          (and
           use-ocaml-indenter
           (= len 1)
           (not (eq? (classify-position (max 0 start)) 'parenthesis))
           (or (not (eq? (classify-position (max 0 (sub1 start))) (classify-position start)))
               (eq? (classify-position (max 0 (sub1 start))) 'governing-keyword)))
        (ocaml:try-indent start)))
    
    (define-struct indent-match (pos level keyword))
    (define-struct indent-base (keyword level))
    
    #;(define/public (insert-pipe)
      (if (tabify-on-return?) ;; one implies the other
          (begin 
            (begin-edit-sequence)
            (insert #\|)
            (tabify (get-start-position))
            (set-position 
             (let loop ([new-pos (get-start-position)])
               (if (let ([next (get-character new-pos)])
                     (and (char-whitespace? next)
                          (not (char=? next #\newline))))
                   (loop (add1 new-pos))
                   new-pos)))
            (end-edit-sequence))
          (insert #\|)))
    
    (define find-offset
      (lambda (pos [offset 0])
        (define c (get-character pos))
        (cond
          [(char=? c #\tab)
           (find-offset (add1 pos) (+ offset (- 8 (modulo offset 8))))]
          [(char=? c #\newline)
           (cons offset pos)]
          [(char-whitespace? c)
           (find-offset (add1 pos) (add1 offset))]
          [else
           (cons offset pos)])))
    
    (define (visual-offset pos)
      (define (loop p)
        (if (= p -1)
            0
            (let ([c (get-character p)])
              (cond
                [(char=? c #\null) 0]
                [(char=? c #\tab)
                 (let ([o (loop (sub1 p))])
                   (+ o (- 8 (modulo o 8))))]
                [(char=? c #\newline) 0]
                [else (add1 (loop (sub1 p)))]))))
      (loop (sub1 pos)))
    
    (define (do-indent para amt)
      (define pos-start (paragraph-start-position para))
      (define curr-offset (find-offset pos-start))
      (unless (= amt (car curr-offset))
        (delete pos-start (cdr curr-offset))
        (insert
         (make-string (if (> amt 0) amt 0) #\space)
         pos-start)))
    
    (define/override (do-paste start time)
      (let ([old use-ocaml-indenter])
        (set! use-ocaml-indenter #f)
        (super do-paste start time)
        (set! use-ocaml-indenter old)))
    
    (define/public (get-token-forward pos)
      (define id-end (forward-match pos (last-position)))
      (define id-start (and id-end (backward-match id-end 0)))
      (if (and id-start (> id-end pos))
          (values id-start (token-to-sym (get-text id-start id-end)))
          (values #f #f)))
    
    (define/public (get-token-backward pos)
      (define id-start (backward-match pos 0))
      (define id-end (and id-start (forward-match id-start (last-position))))
      (if (and id-end (< id-start pos))
          (values id-start (token-to-sym (get-text id-start id-end)))
          (values #f #f)))
    
    (define (token-to-sym token-text)
      (and (> (string-length token-text) 0)
           (string->symbol token-text)))
    
    (define (get-line-indent pos) (get-para-indent (position-paragraph pos)))
    
    (define (get-para-indent para)
      (if para
          (car (find-offset (paragraph-start-position para)))
          0))
    
    (define (incr-prev-indent para n)
      (do-indent para (+ (get-para-indent (sub1 para)) n)))
    
    (define (match-prev-indent para) (incr-prev-indent para 0))
    
    (define (match-comment-indent para)
      (define this-pos (cdr (find-offset (paragraph-start-position para))))
      (define prev-pos (cdr (find-offset (paragraph-start-position (sub1 para)))))
      (define this-start-text (get-text this-pos (+ this-pos 1)))
      (define prev-start-text (get-text prev-pos (+ prev-pos 2)))
      (if (not (equal? prev-start-text "(*"))
          (match-prev-indent para)
          (if (equal? this-start-text "*")
              (incr-prev-indent para 1)
              (incr-prev-indent para 3))))
    
    (define ocaml:possible-bases
      (make-immutable-hash
       (list
       [cons 'in (list (make-indent-base 'let 0)
                  (make-indent-base 'in 0)
                  (make-indent-base 'and 0))]
       #;[cons 'and (list (make-indent-base 'let 0)
                     (make-indent-base 'with 0))]
         [cons 'module (list (make-indent-base 'sig 2)
                        (make-indent-base 'struct 2)
                        (make-indent-base 'type 0)
                        (make-indent-base 'exception 0)
                        (make-indent-base 'val 0)
                        (make-indent-base 'let 0)
                        (make-indent-base 'module 0)
                        (make-indent-base 'end 0))]
         [cons 'end (list (make-indent-base 'sig 0)
                     (make-indent-base 'struct 0))]
         [cons 'sig (list (make-indent-base 'module 2))]
         [cons 'struct (list (make-indent-base 'module 2))]
         [cons 'then (list (make-indent-base 'if 0))]
         [cons 'else (list (make-indent-base 'if 0)
                      (make-indent-base 'then 0))]
         [cons 'type (list (make-indent-base 'type 0)
                      (make-indent-base 'val 0)
                      (make-indent-base 'exception 0)
                      (make-indent-base 'end 0)
                      (make-indent-base 'let 0)
                      (make-indent-base 'module 0)
                      (make-indent-base 'with 0)
                      (make-indent-base 'sig 2)
                      (make-indent-base 'struct 2))]
         [cons 'val (list (make-indent-base 'type 0)
                     (make-indent-base 'val 0)
                     (make-indent-base 'end 0)
                     (make-indent-base 'exception 0)
                     (make-indent-base 'with 0)
                     (make-indent-base 'module 0)
                     (make-indent-base 'sig 2)
                     (make-indent-base 'struct 2))]
         [cons 'exception (list (make-indent-base 'type 0)
                           (make-indent-base 'val 0)
                           (make-indent-base 'end 0)
                           (make-indent-base 'exception 0)
                           (make-indent-base 'let 0)
                           (make-indent-base 'module 0)
                           (make-indent-base 'with 0)
                           (make-indent-base 'sig 2)
                           (make-indent-base 'struct 2))]
         [cons 'let (list (make-indent-base 'type 0)
                     (make-indent-base 'val 0)
                     (make-indent-base 'exception 0)
                     (make-indent-base 'module 0)
                     (make-indent-base 'end 0)
                     (make-indent-base 'let 0)
                     (make-indent-base 'sig 2)
                     (make-indent-base 'struct 2)
                     (make-indent-base 'try 2))]
         [cons 'with (list (make-indent-base 'match 0)
                      (make-indent-base 'try 0))]
         )))
    
    ;; find-base-for-keyword: number -> (union #f number)
    ;; Given a position pos, takes the first token after that position,
    ;; and finds the level of indentation which the bottom one should
    ;; use as a base.
    (define (ocaml:find-base-indent-for-keyword keyword pos min-pos)
      (define lookup-bases (hash-ref ocaml:possible-bases keyword (λ () #f)))
      (if lookup-bases
          (ocaml:find-base-indent lookup-bases pos min-pos)
          (make-indent-match pos (get-line-indent pos) #f)))
    
    (define (get-token-offset pos)
      (define line-start-pos (paragraph-start-position (position-paragraph pos)))
      (- pos line-start-pos))
    
    ;; handle-and: pos (number) -> indent (number)
    ;; Determines whether the "and" at the given position
    ;; is bound to a "with" or to a "let" (possibly with a
    ;; pattern-matching "with" intervening).
    (define (ocaml:handle-and pos min-pos)
      (define bases-for-and (list (make-indent-base 'match 0)))
      (let-values ([(back-pos back-token)
                    (get-token-backward pos)])
        (cond [(< pos min-pos)
               (get-line-indent min-pos)]
              [(not back-token)
               (get-line-indent pos)]
              [(eq? back-token '->)
               (ocaml:handle-and
                (find-string "match" 'backward pos 'eof #f)
                min-pos)]
              [(eq? back-token 'with)
               (get-token-offset back-pos)]
              [(eq? back-token 'let)
               (get-token-offset back-pos)]
              [(eq? back-token 'and)
               (get-token-offset back-pos)]
              [else
               (ocaml:handle-and back-pos min-pos)])))
    
    (define (first-token-on-line? pos token)
      (define start-of-line (paragraph-start-position
                             (position-paragraph pos)))
      (define-values (first-token-pos first-token-on-line)
        (get-token-forward start-of-line))
      (eq? token first-token-on-line))
    
    (define (ocaml:handle-pipe pos min-pos)
      (let*-values ([(back-pos back-token)
                     (get-token-backward pos)])
        (cond [(< pos min-pos)
               (+ (get-line-indent min-pos) 2)]
              [(not back-token)
               (get-line-indent pos)]
              [(and (first-token-on-line? back-pos back-token)
                    (eq? back-token '\|))
               (get-line-indent back-pos)]
              [(eq? back-token 'with)
               (+ (get-line-indent back-pos) 2)]
              [else
               (ocaml:handle-pipe back-pos min-pos)])))
    
    (define (ocaml:handle-in pos min-pos)
      (let*-values ([(back-pos back-token)
                     (get-token-backward pos)])
        (cond [(< pos min-pos)
               (+ (get-line-indent min-pos) 2)]
              [(not back-token)
               (get-line-indent pos)]
              [(and (first-token-on-line? back-pos back-token)
                    (memq back-token '(in and let)))
               (get-line-indent back-pos)]
              [else
               (ocaml:handle-in back-pos min-pos)])))
    
    (define (ocaml:find-base-indent bases pos min-pos)
      (define other-bottoms
        (hash-map
         ocaml:possible-bases
         (λ (key value) key)))
      (define (mem-base token bases)
        (cond [(eq? bases '()) #f]
              [(eq? token (indent-base-keyword (first bases)))
               (first bases)]
              [else (mem-base token (rest bases))]))
      (if (> pos min-pos)
          (if bases
              (let*-values ([(back-pos back-token)
                             (get-token-backward pos)]
                            [(matched-base)
                             (mem-base back-token bases)])
                (cond [(not back-pos)
                       (make-indent-match pos (get-line-indent pos) #f)]
                      [(and matched-base
                            (indent-base-keyword matched-base))
                       (make-indent-match back-pos
                                          (+ (indent-base-level matched-base)
                                             (get-token-offset back-pos))
                                          back-token)]
                      [(memq back-token other-bottoms)
                       (ocaml:find-base-indent
                        bases
                        (indent-match-pos
                         (ocaml:find-base-indent-for-keyword back-token back-pos min-pos))
                        min-pos)]
                      [else
                       (ocaml:find-base-indent bases back-pos min-pos)]))
              (make-indent-match pos (get-token-offset pos) #f))
          (make-indent-match min-pos (get-token-offset min-pos) #f)))
    
    (define (ocaml:handle-single-semi pos min-pos)
      (let*-values ([(back-pos back-token)
                     (get-token-backward pos)])
        (cond [(< pos min-pos)
               (+ (get-line-indent min-pos) 2)]
              [(not back-token)
               (get-line-indent pos)]
              [(memq back-token '(= ->))
               (+ (get-line-indent back-pos) 2)]
              [else
               (ocaml:handle-single-semi back-pos min-pos)])))
    
    (define (find-double-semi pos)
      (define double-semi-pos (find-string ";;" 'backward pos 'eof #f))
      (cond [(< pos 0) #f]
            [(not double-semi-pos) #f]
            [(eq? (classify-position double-semi-pos) 'operator)
             double-semi-pos]
            [else (find-double-semi double-semi-pos)]))
    
    ;; First step. Rule out the cases which are easy to indent
    ;; and for which most of our inner definitions wouldn't make sense.
    (define/public (ocaml:try-indent pos)
      (define para (position-paragraph pos))
      (define token-type (classify-position (sub1 (paragraph-start-position para))))
      (define start-of-line (paragraph-start-position para))
      ;; "last" is the start of the token or clause just before "pos"
      (define last (or (backward-match start-of-line 0) 0))
      (define last-para (position-paragraph last))
      (define first-token-type (classify-position 0))
      (cond [(= para 0)
             ;; If it's the first paragraph, we want no indentation
             (do-indent para 0)]
            [(memq token-type '(string error))
             ;; If it's a string or an error, don't touch it
             ;; Note: we might want to change this to match previous indent
             ;; Maybe make it an option?
             (void)]
            [(eq? token-type 'comment)
             ;; If it's a comment, match the previous text or *
             (match-comment-indent para)]
            [(eq? (classify-position last) 'double-semi)
             ;; If the last token was a double-semi, we want no indentation
             (do-indent para 0)]
            [(eq? first-token-type 'white-space)
             ;; If there are no tokens in the document, we want no indentation
             (do-indent para 0)]
            [else
             ;; Otherwise, we have to do things the hard way
             (let-values ([(start-keyword-pos start-keyword)
                           (get-token-forward start-of-line)])
               (if start-keyword-pos
                   (ocaml:indent para
                                 start-keyword-pos
                                 start-keyword
                                 (classify-position start-keyword-pos))
                   (ocaml:indent para
                                 start-of-line
                                 #f
                                 #f)))]))
    
    ;; If we get here, then the following invariant should hold:
    ;; * It's not the first line.
    ;; * We're not in a comment.
    ;; * We're not right after a ";;".
    ;; * There is at least one symbol in the document.
    ;; If these things aren't true, that's the fault of
    ;; the calling procedure.
    (define/public (ocaml:indent para keyword-pos keyword start-token-type)
      (define start-of-line (paragraph-start-position para))
      ;; "last" is the start of the token or clause just before "pos"
      (define last (or (backward-match start-of-line 0) 0))
      (define last-para (position-paragraph last))
      (define last-keyword
        (let ([last-end (or (forward-match last (last-position)) (last-position))])
          (string->symbol (get-text last last-end))))
      ;; "contains" is the start of the initial sub-clause
      ;;  in the clause that contains "pos". If pos is outside
      ;;  all clauses, this will be the beginning of the file.
      (define contains
        (max (or (backward-containing-sexp start-of-line 0) 0)
             (or (find-double-semi start-of-line) 0)))
      (define contain-para (and contains (position-paragraph contains)))
      (define contains-para-start (paragraph-start-position (position-paragraph contains)))
      ;; "prev-keyword" is the first token on the immediately preceding line
      (define-values (prev-keyword-pos prev-keyword)
        (get-token-forward (paragraph-start-position last-para)))
      (define prev-token-type
        (classify-position (if prev-keyword-pos prev-keyword-pos 0)))
      ;; "two-back-keyword" is the first token on the line preceding prev
      (define two-back-para (max 0 (sub1 last-para)))
      (define-values (two-back-keyword-pos two-back-keyword)
        (get-token-forward (paragraph-start-position two-back-para)))
      (define two-back-token-type
        (classify-position (if two-back-keyword-pos two-back-keyword-pos 0)))
      ;; "previous-indent" is the indentation level of the
      ;; immediately preceding line
      (define previous-indent
        (car (find-offset (paragraph-start-position last-para))))
      (define (incr-indent n) (do-indent para (+ previous-indent n)))
      (define-values (first-token-pos first-token-in-text)
        (get-token-forward 0))
      #;(when (not (char=? (get-character (sub1 start-of-line))
                           #\newline))
          (insert #\newline (paragraph-start-position para)))
      (cond
        [(not keyword) (incr-indent 0)]
        [(eq? keyword-pos first-token-pos) (incr-indent 0)]
        [(eq? keyword 'and)
         (do-indent para
                    (ocaml:handle-and start-of-line contains))]
        [(eq? keyword '\|)
         (do-indent para
                    (ocaml:handle-pipe start-of-line contains))]
        [(and (eq? keyword 'let)
              (memq last-keyword '(= ->)))
         (incr-indent 2)]
        [(eq? last-keyword '|,|)
         (do-indent para (- contains contains-para-start))]
        [(eq? keyword 'in)
         (do-indent para
                    (ocaml:handle-in start-of-line contains))]
        [(and (eq? prev-keyword 'let)
              (eq? last-keyword 'in)
              (incr-indent 0))]
        [(memq keyword (hash-map ocaml:possible-bases (λ (key value) key)))
         (do-indent para
                    (indent-match-level (ocaml:find-base-indent-for-keyword
                                         keyword start-of-line contains)))]
        ;; If we get here, then the keyword is nothing special,
        ;; so try to handle the situation where one expression
        ;; is on multiple lines
        [(eq? last-keyword '|;|)
         (if (and (> contains 0)
                  (memq (string->symbol (get-text (sub1 contains) contains))
                   '(|(| |[|)))
             (do-indent para (- contains contains-para-start))
             (do-indent para
                        (ocaml:handle-single-semi last contains)))]
        [(= contain-para last-para)
         ;; So far, the S-exp containing "pos" was all on
         ;;  one line (possibly not counting the opening paren),
         ;;  so indent to follow the first S-exp's end
         (do-indent para (+ 2 (- contains contains-para-start)))]
        [(memq prev-token-type '(keyword governing-keyword operator))
         (incr-indent 2)]              
        [(memq two-back-token-type '(keyword governing-keyword operator))
         (incr-indent 2)]
        [else 
         ;; No particular special case, so indent to match previous line
         (incr-indent 0)]))))
