#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 format)
             (ice-9 textual-ports))

(define input "input.txt")



(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 "TODO")
         (sol2 "TODO"))
    (when (null? (cdr args))
      (unless (equal? sol1 "TODO") (error "Wrong solution sol1!"))
      (unless (equal? sol2 "TODO") (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
