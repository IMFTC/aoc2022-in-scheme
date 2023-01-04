#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 format)
             (rnrs bytevectors)
             (ice-9 iconv)
             (ice-9 binary-ports))

(define input "input13.txt")


(define (get-lists-from-file file)
  (eval-string
   (call-with-input-file file
     (lambda (file-port)
       (call-with-output-string
         (lambda (string-port)
           ;; Replace all #\, with #\space. Also wrap the whole file
           ;; content inside an extra "[" and "]" pair and prefix it
           ;; with "'", so we can get this outer list with eval-string,
           ;; which otherwise would return only the last list.
           (display "'[" string-port)
           (let loop ((byte (get-u8 file-port)))
             (unless (eof-object? byte)
               (put-u8 string-port (if (= byte (char->integer #\,))
                                    (char->integer #\space)
                                    byte))
               (loop (get-u8 file-port))))
           (display "]" string-port)))))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 "TODO")
         (sol2 "TODO"))
    (format #t "input-file: ~a\n" input-file)

    (format #t "~a\n" (get-lists-from-file input-file))
    (when (null? (cdr args))
      (unless (equal? sol1 "TODO") (error "Wrong solution sol1!"))
      (unless (equal? sol2 "TODO") (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
