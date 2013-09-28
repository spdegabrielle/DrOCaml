#lang scheme

(require "util.ss")

(provide annotation
         annotation-start-char
         annotation-end-char
         annot-type
         compile-and-get-dtypes
         format-annot)

(define (compile-and-get-dtypes parent fname settings)
  (with-handlers
      ([exn:fail? (λ (exn) (ocaml:not-installed) #f)])
    (define filename
      (path->string 
       (if (eq? (system-type 'os) 'windows)
           (ocaml:strip-crlf fname)
           fname)))
    (define executable-name (path-replace-suffix fname ".exe"))
    (define lsm (ocaml:lang-settings-modules settings))
    (define annot-filename (path->string (path-replace-suffix filename ".annot")))
    (define _ (when (file-exists? annot-filename) (delete-file annot-filename) (sleep 0.01)))
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
        #f #f #f
        (ocaml:lang-settings-compiler settings)
        "-o" (path->string executable-name)
        "-dtypes"
        lsm
        lsi
        "-impl" filename)))
    (define-values (proc in out err)
      (apply subprocess args))
    (define annot-table (make-hash))
    (subprocess-wait proc)
    (call-with-input-file annot-filename (λ (port) (get-annotations annot-table port)))))

(define-struct annotation (start-char end-char type))
  
(define (get-one-annotation port)
  (define begin-char
    (begin
      (read port) ;; garbage: begin-line
      (read port) ;; garbage: begin-line-char
      (read port)))
  (define end-char
    (begin
      (read port) ;; garbage: filename-2
      (read port) ;; garbage: end-line
      (read port) ;; garbage: end-line-char
      (read port)))
  (define raw-type-desc
    (begin
      (read port) ;; garbage: "type"
      (regexp-replace* (regexp "[|]['][|] ") (format "~v" (read port)) "'")))
  (define type-desc-length (string-length raw-type-desc))
  (define type-desc (substring raw-type-desc 1 (- type-desc-length 1)))
  (make-annotation begin-char end-char type-desc))

(define readtable-no-quote
  (make-readtable
   #f
   #\' 'non-terminating-macro (λ (a b c d e f) #'\')
   #\` 'non-terminating-macro (λ (a b c d e f) #'\`)
   #\, 'non-terminating-macro (λ (a b c d e f) #'\,)))

(define (get-annotations annot-table port)
  (define (loop)
    (define string-test (read port)) ;; garbage: filename-1
    (when (not (eof-object? string-test))
      (let ([one-annot (get-one-annotation port)])
        (insert-one-annotation annot-table one-annot)
        (loop))))
  (current-readtable readtable-no-quote)
  (loop)
  (current-readtable #f)
  annot-table)

(define (insert-one-annotation annot-table annot)
  (define (insert-one-pos pos)
    (define ht-status (hash-ref annot-table pos (λ () #f)))
    (unless ht-status
      (hash-set! annot-table pos annot)))
  (define (loop start end)
    (unless (>= start end)
      (insert-one-pos start)
      (loop (add1 start) end)))
  (loop (annotation-start-char annot) (annotation-end-char annot)))

#;(define (get-first-and-last-end-chars annot-table)
    (define end-char-list
      (hash-map
       annot-table
       (λ (key v) key)))
    (values
     (apply min end-char-list)
     (apply max end-char-list)))

(define (annot-type annot)
  (format "~a" (annotation-type annot)))

(define (format-annot annot)
  (format "~a -> ~a \"~a\""
          (annotation-start-char annot)
          (annotation-end-char annot)
          (annotation-type annot)))

