#lang scheme/gui

(require "util.ss")

(provide
 debug-callback
 compile-for-debug
 start-debugger
 repeat-action-callback
 break-callback
 inspect-value
 update-breakpoints)
 
(define (debug-callback defs parent settings)
  (define fname (send defs get-filename))
  (define tab (send defs get-tab))
  (define frame (send tab get-frame))
  (define-values (file-path file-name file-is-dir?)
    (split-path fname))
  (define executable-name
    (compile-for-debug
     parent
     settings
     (if (eq? (system-type 'os) 'windows)
         (ocaml:strip-crlf fname)
         fname)))
  (if executable-name
      (begin
        (send defs ocaml:clean-up)
        (send frame ocaml:show-debug)
        (send frame ensure-rep-shown #t)
        (let-values ([(proc in out err) (start-debugger executable-name tab settings)])
          (send tab ocaml:set-debug-process (ocaml:make-process proc in out err)))
        (update-breakpoints tab (send (send tab get-defs) ocaml:debug:get-breakpoints))
        (start-debug-threads tab)
        #;(send (send defs get-keymap) chain-to-keymap ocaml:debugger-keymap #t))
      (message-box
       "Compilation error"
       "This program cannot compile. Please fix the errors and try again.")))

(define (start-debug-threads tab)
  (define defs (send tab get-defs))
  (define process-obj (send tab ocaml:get-debug-process))
  (define proc (ocaml:process-proc process-obj))
  (define in (ocaml:process-in process-obj))
  (define out (ocaml:process-out process-obj))
  (define err (ocaml:process-err process-obj))
  (send defs ocaml:debug:set-in-handler (thread (debugger-output-handler tab in)))
  (send defs ocaml:debug:set-err-handler (thread (debugger-output-handler tab err))))

(define (compile-for-debug parent settings filename)
  (with-handlers
      ([exn:fail? (λ (exn) (ocaml:not-installed) #f)])
    (define executable-name (path-replace-suffix filename ".exe"))
    (if (ocaml:lang-settings? settings)
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
                "-g"
                "-o" (path->string executable-name)
                lsm
                lsi
                "-impl" (path->string filename)))]
             [(proc in out err) (apply subprocess args)])
          (subprocess-wait proc)
          (if (= (subprocess-status proc) 0)
              executable-name
              (begin
                (message-box
                 "Compilation error"
                 "This program cannot compile. Please fix the errors and try again.")
                #f)))
    (begin
      (message-box
       "Wrong language"
       "You can only use the debugger with the OCaml language.")
      #f))))

(define (start-debugger executable-name tab settings)
  (with-handlers
      ([exn:fail?
        (λ (exn)
          (ocaml:not-installed)
          (send tab ocaml:kill-debug)
          (values #f #f #f #f))])
    (define-values (file-path file-name file-is-dir?) (split-path executable-name))
    (define ocamldebug-executable (ocaml:lang-settings-debugger settings))
    (define _1 (current-directory file-path))
		(define _2 (putenv "PATH" (string-append "/usr/local/bin:/usr/bin:/bin:" (getenv "PATH"))))
    (define-values (proc in out err)
      (subprocess
       #f #f #f
       ocamldebug-executable
       "-emacs"
       (path->string file-name)))
    (send (send tab get-frame) open-status-line 'ocaml:debugger)
    (write-string "frame\n" out)
    (flush-output out)
		(ocaml:clear-all-text err)
		(ocaml:clear-all-text in)
    (values proc in out err)))

(define (repeat-action-callback tab message direction)
  (define process-obj (send tab ocaml:get-debug-process))
  (define out (ocaml:process-out process-obj))
  (send (send tab get-defs) ocaml:reset-highlighting)
  (write-string message out)
  (newline out)
  (flush-output out))

(define (ocaml:display-debugger-output tab message)
  (define ints (send tab get-ints))
  (define (thunk)
    (send ints display-results message))
  (send ints run-in-evaluation-thread thunk))

;; runs on separate thread
(define (debugger-output-handler tab port)
  (λ ()
    (define defs (send tab get-defs))
    (define ints (send tab get-ints))
    (define-values (in-pipe out-pipe) (make-pipe))
    (define (read-chars-loop)
      (define process-obj (send tab ocaml:get-debug-process))
      (when process-obj
        (let ([out (ocaml:process-out process-obj)]
              [next (read-char port)])
          (when (not (eq? next eof))
             (write-char next out-pipe)
						 (ocaml:display-debugger-output tab (list next)))
          (when (eq? next #\newline)
            (handle out (read-line in-pipe)))
          (read-chars-loop))))
    (define (handle out response-text)
      (cond
        [(regexp-match "Breakpoint : ([0-9]+)" response-text)]
        [(regexp-match "Time : ([0-9]+) - pc : [0-9]+ - module [A-Z][a-z]*" response-text)
         =>
         (λ (x) (send defs ocaml:set-debug-time (string->number (second x))))]
        [(regexp-match "Time : ([0-9]+)" response-text)
         =>
         (λ (x) (send defs ocaml:set-debug-time (string->number (second x))))]
        [(equal? response-text "Beginning of program.")
         (send defs ocaml:set-debug-status "At beginning of program")
         (send defs scroll-to-position 0)]
        [(equal? response-text "Program exit.")
         (send defs ocaml:set-debug-status "At end of program")
         (send defs scroll-to-position (send defs last-position))]
        [(equal? "H" response-text)]
        [(and
          (>= (string-length response-text) 3)
          (equal? (substring response-text 0 3) "M"))
         (local ((define parts
                   ;; if on windows, start after the drive identifier
                   ;; the colon (C:) will screw us up
                   (regexp-split
                    ":"
                    (substring
                     response-text
                     (if (eq? (system-type 'os) 'windows)
                         3
                         5))))
                 (define drive-prefix
                   ;; if on windows, find out what the drive is: we'll need to know
                   (and
                    (eq? (system-type 'os) 'windows)
                    (substring response-text 3 5)))
                 (define file-path
                   ;; if on windows, merge in the drive prefix
                   (normal-case-path
                    (string->path
                     (if (eq? (system-type 'os) 'windows)
                         (string-append drive-prefix (car parts)) ;; hee hee
                         (car parts))))) ;; i crack myself up
                 (define start (string->number (second parts)))
                 (define lame-ocaml-version (< (length parts) 4))
                 (define end
                   (if lame-ocaml-version
                       start
                       (string->number (third parts))))
                 (define event-type
                   (if lame-ocaml-version
                       (third parts)
                       (fourth parts)))
                 (define-values (debug-path debug-filename debug-dir?)
                   (split-path file-path))
                 (define-values (defs-path defs-filename defs-dir?)
                   (split-path (send defs get-filename))))
           (if (or (equal? debug-filename defs-filename)
                   (equal? debug-filename (path-replace-suffix defs-filename ".crlf.ml")))
               (begin
                 (send defs scroll-to-position start #f end)
                 (send defs ocaml:set-debug-highlighting start end)
                 (send defs ocaml:set-debug-status (format "Type ~a" event-type)))
               (let ([direction (send defs ocaml:get-direction)])
                 (cond
                   [(eq? direction 'forward)
                    (write-string "step" out)
                    (newline out)
                    (flush-output out)]
                   [(eq? direction 'backward)
                    (write-string "backstep" out)
                    (newline out)
                    (flush-output out)]
                   [else
                    ;; This should never happen!
                    (write-string "reverse" out)
                    (newline out)
                    (flush-output out)]))))]))
    (read-chars-loop)))

(define (break-callback tab)
  (define defs (send tab get-defs))
  (define process-obj (send tab ocaml:get-debug-process))
  (define proc (ocaml:process-proc process-obj))
  (define in (ocaml:process-in process-obj))
  (define out (ocaml:process-out process-obj))
  (define err (ocaml:process-err process-obj))
  (when (subprocess? proc)
    ;; should only send SIGINT
    (send defs ocaml:debug:pause-handlers)
    (subprocess-kill proc #f)
    (ocaml:clear-all-text in)
    (ocaml:clear-all-text err)
    (send defs ocaml:debug:resume-handlers)))

(define (inspect-value tab name)
  (define frame (send tab get-frame))
  (define defs (send tab get-defs))
  (define ints (send tab get-ints))
  (define process-obj (send tab ocaml:get-debug-process))
  (define proc (ocaml:process-proc process-obj))
  (define in (ocaml:process-in process-obj))
  (define out (ocaml:process-out process-obj))
  (define err (ocaml:process-err process-obj))
  (define (loop message)
    (define-values (more? response)
      (if (ocaml:sync-char-ready? err)
          (ocaml:sync-read-line-avail err)
          (ocaml:sync-read-line-avail in)))
    (if (or (equal? response "Interrupted.")
            (equal? response "")
            (equal? response "(ocd) "))
        (begin
          (ocaml:clear-all-text in)
          (ocaml:clear-all-text err)
          message)
        (loop (string-append message response))))
  (when (subprocess? proc)
    (send defs ocaml:debug:pause-handlers)
    (write-string (format "print ~a~n" name) out)
    (flush-output out)
    (send defs ocaml:set-debug-status (format "~a" (loop "")))
    (send defs ocaml:debug:resume-handlers)))

(define (remove-all-breakpoints tab)
  (define frame (send tab get-frame))
  (define defs (send tab get-defs))
  (define ints (send tab get-ints))
  (define process-obj (send tab ocaml:get-debug-process))
  (define proc (ocaml:process-proc process-obj))
  (define in (ocaml:process-in process-obj))
  (define out (ocaml:process-out process-obj))
  (define err (ocaml:process-err process-obj))
  (define-values (response? response-text)
    (begin
      (write-string "delete\n" out)
      (flush-output)
      (ocaml:sync-read-line-avail in)))
  (when (equal? response-text "Delete all breakpoints ? (y or n) ")
    (write-string "y\n" out)
    (flush-output)))

(define (update-breakpoints tab breakpoints)
  (define frame (send tab get-frame))
  (define defs (send tab get-defs))
  (define ints (send tab get-ints))
  (define process-obj (send tab ocaml:get-debug-process))
  (define proc (ocaml:process-proc process-obj))
  (define in (ocaml:process-in process-obj))
  (define out (ocaml:process-out process-obj))
  (define err (ocaml:process-err process-obj))
  (define module-name
    (let-values
        ([(_ filename __) (split-path (send defs get-filename))])
         (path-replace-suffix filename "")))
  (define (update-one pos value)
    (when value
      (write-string (format "break @ ~a # ~a~n" module-name pos) out)
      (flush-output out)))
  (remove-all-breakpoints tab)
  (hash-for-each breakpoints update-one))

