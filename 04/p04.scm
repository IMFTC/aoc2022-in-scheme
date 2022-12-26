#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1))

(define input "input.txt")

(define (overlapping-assignment? assignment kind)
  (match-let*
      (((r1 r2) (string-split assignment #\,))
       ((s1 e1) (map string->number (string-split r1 #\-)))
       ((s2 e2) (map string->number (string-split r2 #\-))))
    (case kind
      ((contained) (<= (* (- s2 s1) (- e2 e1)) 0))
      ((overlap) (not (or (< e1 s2) (< e2 s1)))))))

(define (load-assignments file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((result '())
                 (line (get-line port)))
        (if (eof-object? line)
            result
            (unless (string-null? line)
              (loop (cons line result)
                    (get-line port))))))))

(define (solution-1 assignments)
  (fold
   (lambda (assignment sum)
     (if (overlapping-assignment? assignment 'contained)
         (1+ sum)
         sum))
   0
   assignments))

(define (solution-2 assignments)
  (fold
   (lambda (assignment sum)
     (if (overlapping-assignment? assignment 'overlap)
         (1+ sum)
         sum))
   0
   assignments))

;;; A different input file can be specified as first command line argument
(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (assignments (load-assignments input-file))
         (sol1 (solution-1 assignments))
         (sol2 (solution-2 assignments)))
    (when (null? (cdr args))
      (unless (= sol1 503) (raise-exception "Wrong solution sol1!"))
      (unless (= sol2 827) (raise-exception "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
