#lang scheme/gui

(require "util.ss")

(provide (all-defined-out))

(define ocaml-process-info (make-parameter false))
(define ocamldebug-process-info (make-parameter false))

(define (start-ocaml-process settings)
  (with-handlers
      ([exn:fail? (λ (exn) (printf "failed~n") (ocaml:not-installed) false)])
    (putenv "TERM" "dumb")
    (local [(define lsm (ocaml:lang-settings-modules settings))
            (define lsi
              (let
                  ([path-list
                    (filter
                     (λ (x) (not (regexp-match "^[ \t\n]*$" x)))
                     (regexp-split ";" (ocaml:lang-settings-includes settings)))])
                (apply string-append (map (λ (x) (format "-I ~a " x)) path-list))))
            (define args
              (filter
               (λ (x) (not (equal? x "")))
               (list
                false
                false
                false
                (ocaml:lang-settings-toplevel settings)
                lsm
                lsi)))
            (define-values (proc in out err)
              (apply subprocess args))]
      (ocaml:clear-all-text err)
      (ocaml:clear-all-text in)
      (ocaml:make-process proc in out err))))

(define (stop-ocaml-process process-info)
  (define proc (ocaml:process-proc process-info))
  (when (and (subprocess? proc) (eq? (subprocess-status proc) 'running))
    (subprocess-kill proc true)))

(define (read-through-whitespace port)
  (let ([next (peek-char port)])
    (when (eq? #\space next)
      (read-char port)
      (read-through-whitespace port))))

(define process-one-expr
  (case-lambda
    [(port in out err index)
     (define-values (expr new-index) (ocaml:read-expr port))
     #;(send (object-name port) set-delayed-prompt-position (+ (if index index 0) new-index))
     (process-one-expr port in out err index expr new-index)]
    [(port in out err index expr new-index)
     (cond
       #;[(send (object-name port) ocaml:found-error?) eof]
       [(eof-object? expr) eof]
       #;[(send (object-name port) ocaml:found-error?) eof]
       [else
        (local ((define out-string (open-output-string))
                (define (loop)
                  (define-values (more? next-line) (ocaml:sync-read-line-avail in))
                  (if more?
                      (begin
                        (write-string (format "~a~n" next-line) out-string)
                        (loop))
                      (cond
                        [(equal? next-line "# ") #f]
                        [else
                         (write-string (format "~a~n" next-line) out-string)
                         #t])))
                (define (err-loop)
                  (define-values (more? next-line)
                    (ocaml:sync-read-line-avail err))
                  (when (not (equal? next-line ""))
                    (write-string next-line out-string)
                    (newline out-string))
                  (when more? (err-loop))))
          (begin
            (write-string expr out)
            (newline out)
            (flush-output out)
            (err-loop)
            (read-through-whitespace in)
            (let*
                ([need-line (loop)]
                 [output (get-output-string out-string)]
                 [error-match (regexp-match "Characters ([0-9]*)-([0-9]*):\n[^W]" output)])
              #;(when error-match
                (send
                 (object-name port)
                 delayed-highlight-error
                 (list
                  (+ index (string->number (second error-match)))
                  (+ index (string->number (third error-match))))))
              (if need-line
                  (begin
                    (display output)
                    (process-one-expr port in out err 0 (read-line) 0))
                  #`#,output))))])]))

