#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (rnrs base)
             (ice-9 match)
             (srfi srfi-1))

(define input "input.txt")

(define (partition-list lst)
  "Return a list of lists, where the first list holds the elements of
LST up to the first #f element, the 2nd list all elements between the
first occurrence of #f and so on."
  (fold (lambda (e prev)
          (cond (e (set-car! prev (cons e (car prev)))
                   prev)
                (else (cons '() prev))))
        (list '())                      ;; not '(())!
        lst))

(define (sum-lists lsts)
  "((1 2) () (3) (4 5)) -> (3 0 3 9)"
  (fold (lambda (l prev)
          (cons (apply + l) prev))
        '()
        lsts))

(define (solution-1b)
  (apply max
         (sum-lists
          (partition-list
           (map string->number
                (string-split
                 (call-with-input-file input
                   (lambda (port)
                     (get-string-all port)))
                 #\newline))))))

(define (solution-1a)
  (call-with-input-file input
    (lambda (port)
      (let loop ((line (get-line port))
                 (current-sum 0)
                 (max-sum 0))
        (cond ((eof-object? line)
               (max max-sum current-sum))
              ((string-null? line)
               (loop (get-line port)
                     0
                     (max max-sum current-sum)))
              (else
               (let ((n (string->number line)))
                 (loop (get-line port)
                       (+ current-sum n)
                       max-sum))))))))

(define (solution-2)
  (call-with-input-file input
    (lambda (port)
      (apply +
       (list-head
        (sort
         (sum-lists
          (partition-list
           (map string->number
                (string-split
                 (get-string-all port)
                 #\newline))))
         >)
        3)))))

(define (main args)
  (let ((sol1a (solution-1a))
        (sol1b (solution-1b))
        (sol2 (solution-2)))
    (assert (= sol1a 71300))
    (assert (= sol1b 71300))
    (assert (= sol2 209691))
    (format #t "solution 1a: ~a\n" sol1a)
    (format #t "solution 1b: ~a\n" sol1b)
    (format #t "solution 2: ~a\n" sol2)))
