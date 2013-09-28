#lang scheme/gui

(require (prefix-in lex: "lexer.ss")
         framework/framework)

(provide (prefix-out ocaml: (all-defined-out)))

(define-struct lang-settings (toplevel compiler debugger modules includes))
(define-struct process (proc in out err))

(define (clear-all-text port)
  (when (sync-char-ready? port)
    (read-char port)
    (clear-all-text port)))

;;; Copied from collects/drscheme/syncheck.ss, from DrScheme v301.
(define (get-pos/text event -editor)
  (let ([event-x (send event get-x)]
        [event-y (send event get-y)]
        [on-it? (box #f)])
    (let loop ([editor -editor])
      (let-values ([(x y) (send editor dc-location-to-editor-location event-x event-y)])
        (cond
          [(is-a? editor text%)
           (let ([pos (send editor find-position x y #f on-it?)])
             (cond
               [(not (unbox on-it?)) (values #f #f)]
               [else
                (let ([snip (send editor find-snip pos 'after-or-none)])
                  (if (and snip
                           (is-a? snip editor-snip%))
                      (loop (send snip get-editor))
                      (values pos editor)))]))]
          [(is-a? editor pasteboard%)
           (let ([snip (send editor find-snip x y)])
             (if (and snip
                      (is-a? snip editor-snip%))
                 (loop (send snip get-editor))
                 (values #f #f)))]
          [else (values #f #f)])))))

(define (strip-crlf filename)
  (define outfilename (path-replace-suffix filename ".crlf-ml"))
  (define infile (open-input-file filename 'text))
  (define outfile (open-output-file outfilename 'replace))
  (define (copy-all)
    (define next (read-char infile))
    (when (not (eof-object? next))
      (write-char next outfile)
      (copy-all)))
  (copy-all)
  (flush-output outfile)
  (close-input-port infile)
  (close-output-port outfile)
  outfilename)

(define definitions-text<%>
  (interface (scheme:text<%>)
    ocaml:reset-highlighting
    ocaml:clean-up))

(define unit:frame<%>
  (interface ()
    ocaml:update-button-visibility/settings
    ocaml:update-button-visibility/tab))

(define read-expr
  (case-lambda 
    [(port) (read-expr port 0)]
    [(port index)
     (define-values (in out) (make-pipe))
     (define (get-char the-char)
       (when (sync-char-ready? port)
         (let ([next (peek-char port)])
           (if (eq? the-char next)
               (begin
                 (write-char (read-char port) out)
                 next)
               #f))))
     (define (loop in-block)
       (define next (if (sync-char-ready? port) (peek-char port) eof))
       (cond
         [(eof-object? next) in-block]
         [(eq? next #\s)
          (write-char (read-char port) out)
          (cond
            [(get-char #\i)
             =>
             (λ (next)
               (if (and (get-char #\g)
                        (get-char #\space))
                   (loop (add1 in-block))
                   (loop in-block)))]
            [(get-char #\t)
             =>
             (λ (next)
               (if (and
                    (get-char #\r)
                    (get-char #\u)
                    (get-char #\c)
                    (get-char #\t)
                    (get-char #\space))
                   (loop (add1 in-block))
                   (loop in-block)))]
            [else (loop in-block)])]
         [(eq? next #\e)
          (write-char (read-char port) out)
          (if (> in-block 0)
              (if (and
                   (get-char #\n)
                   (get-char #\d)
                   (get-char #\space))
                  (loop (sub1 in-block))
                  (loop in-block))
              (loop in-block))]
         [(eq? next #\")
          (let-values
              ([(str _a _b _c _d)
                 (with-handlers ([exn:fail:read? (λ (exn) "")])
                   (lex:get-string-token port))])
            (write-string str out)
            (loop in-block))]
         [(eq? next #\()
          (if (eq? (if (sync-char-ready? port) (peek-char port 1) eof) #\*)
              (let-values
                  ([(str _a _b _c _d)
                    (with-handlers ([exn:fail:read? (λ (exn) "")])
                      (lex:get-comment-token port))])
                (write-string (make-string (string-length str) #\space) out)
                (loop in-block))
              (begin
                (write-char (read-char port) out)
                (loop in-block)))]
         [(equal? next #\;)
          (write-char (read-char port) out)
          (if (> in-block 0)
              (loop in-block)
              (if
               (eq? (if (sync-char-ready? port) (peek-char port) eof) #\;)
               (begin
                 (write-char (read-char port) out)
                 #t)
               (loop in-block)))]
         [else
          (write-char (read-char port) out)
          (loop in-block)]))
     (define found-expr (loop 0))
     (define new-index (+ index (pipe-content-length in)))
     (define outport (open-output-string))
     (define (move-string)
             (when (> (pipe-content-length in) 0)
               (write-byte (read-byte in) outport)
               (move-string)))
     (define outstring (begin (move-string) (get-output-string outport)))
     (if (equal? true found-expr)
         (values outstring new-index)
         (let ([string-without-whitespace (regexp-replace* "[ \t\n]" outstring "")])
           (if (> (string-length string-without-whitespace) 0)
               (values (string-append outstring (expr-ends found-expr)) index)
               (values eof index))))]))

(define (expr-ends n)
  (cond
    [(zero? n) " ;; "]
    [(> n 0) (string-append " end " (expr-ends (sub1 n)))]))

;; if the port isn't ready, give it a bit of time to synchronize
(define (sync-char-ready? port)
  (or
   (char-ready? port)
   (begin
     (sleep .05)
     (char-ready? port))))

;; sync-read-live-avail: port -> (values boolean string)
(define (sync-read-line-avail port)
  (define out (open-output-string))
  (define (loop)
    (if (sync-char-ready? port)
        (let ([next (read-char port)])
          (cond
            [(eq? next #\newline)
             (values #t (get-output-string out))]
            [(eq? next eof)
             (values #f (get-output-string out))]
            [else
             (begin
               (write-char next out)
               (loop))]))
        (values #f (get-output-string out))))
  (loop))

(define (not-installed)
    (message-box
     "OCaml not found"
     "There was an error running the OCaml process. Check your languge configuration and try again. Alternatively, OCaml might not be properly installed. You may continue to use DrOCaml as an editor, but you will not be able to execute code."))

