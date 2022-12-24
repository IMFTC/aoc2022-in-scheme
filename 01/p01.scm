#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports))

(define input "input.txt")

(define (get-sorted-sums file)
  (call-with-input-file input
    (lambda (port)
      (let loop ((line (get-line port))
                 (current-sum 0)
                 (list-of-sums '()))
        (cond ((eof-object? line)
               (sort list-of-sums >))
              ((string-null? line)
               (loop (get-line port)
                     0
                     (cons current-sum list-of-sums)))
              (else
               (let ((n (string->number line)))
                 (loop (get-line port)
                       (+ current-sum n)
                       list-of-sums))))))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sorted-sums (get-sorted-sums input-file))
         (sol1 (apply + (list-head sorted-sums 1)))
         (sol2 (apply + (list-head sorted-sums 3))))
    (when (null? (cdr args))
      (unless (= sol1 71300) (error "Wrong solution sol1!"))
      (unless (= sol2 209691) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
