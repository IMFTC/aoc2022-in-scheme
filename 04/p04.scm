#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1)
             (ice-9 exceptions))

(define input "input.txt")

(define (contained-assignment? assignment)
  (match-let*
      (((r1 r2) (string-split assignment #\,))
       ((s1 e1) (map string->number (string-split r1 #\-)))
       ((s2 e2) (map string->number (string-split r2 #\-))))
    (<= (* (- s2 s1) (- e2 e1)) 0)))

(define (load-assignments file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((result '())
                 (line (get-line port)))
        (if (eof-object? line)
            result
            (unless (string-null? line)
              (format #t "~a\n" line)
              (loop (cons line result)
                    (get-line port))))))))

(define (solution-1 file)
  (fold
   (lambda (assignment sum)
     (if (contained-assignment? assignment)
         (1+ sum)
         sum))
   0
   (load-assignments file)))

;;; A different input file can be specified as first command line argument
(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 (solution-1 input-file)))
    (or (and (= sol1 503)) (raise-exception "Wrong solution(s)!"))
    (format #t "Solution 1: ~a\n" sol1)))
