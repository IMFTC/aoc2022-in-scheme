#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-26)
             (srfi srfi-1))

(define input "input.txt")

(define (make-computer)
  (define (run-program program-text)
    "Run the string PROGRAM-TEXT as a program and return the number of
spaces visited by HEAD."
    (let* ((count-visits-ht (make-hash-table))
           (head (make-s64vector 2 0))
           (tail (make-s64vector 2 0))
           (get-x (cut s64vector-ref <> 0))
           (get-y (cut s64vector-ref <> 1))
           (set-x! (cut s64vector-set! <> 0 <>))
           (set-y! (cut s64vector-set! <> 1 <>))
           (move! (lambda (vec direction)
                    (case direction
                      ((#\R) (set-x! vec (1+ (get-x vec))))
                      ((#\L) (set-x! vec (1- (get-x vec))))
                      ((#\U) (set-y! vec (1+ (get-y vec))))
                      ((#\D) (set-y! vec (1- (get-y vec))))))))
      ;; assume start position (0, 0)
      (hash-set! count-visits-ht (cons 0 0) 1)
      (define (exec-instruction instruction)
        (match (string-split instruction #\space)
          (("") #f)
          ((direction steps)
           (let ((direction (car (string->list direction))))
             (let loop ((steps (string->number steps)))
               (when (> steps 0)
                 ;; move head
                 (move! head direction)
                 ;; get positions after the head has moved
                 (let* ((dx (- (get-x head) (get-x tail)))
                        (dy (- (get-y head) (get-y tail))))
                   (cond ((and (<= (abs dx) 1) (<= (abs dy) 1)))
                         ((= dy 0)
                          (cond ((>= dx 2) (move! tail #\R))
                                ((<= dx -2) (move! tail #\L))))
                         ((= dx 0)
                          (cond ((>= dy 2) (move! tail #\U))
                                ((<= dy -2) (move! tail #\D))))
                         (else
                          ;; head and tail are not on the same row or column
                          (begin (if (> dx 0) (move! tail #\R))
                                 (if (< dx 0) (move! tail #\L))
                                 (if (> dy 0) (move! tail #\U))
                                 (if (< dy 0) (move! tail #\D)))))
                   (hash-set! count-visits-ht (cons (get-x tail) (get-y tail))
                              (hash-ref count-visits-ht (cons (get-x tail) (get-y tail)) 0)))
                 (loop (1- steps))))))))
      (map exec-instruction (string-split program-text #\newline))
      (hash-count (const #t) count-visits-ht)))

  (lambda args
    (apply (case (car args)
             ((run-program) run-program))
           (cdr args))))

(define (get-program-text-from-file file)
  (call-with-input-file file
    (lambda (port)
      (get-string-all port))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (program-text (get-program-text-from-file input))
         (computer (make-computer))
         (sol1 (computer 'run-program program-text)))
    (when (null? (cdr args))
      (unless (= sol1 6044) (error "Wrong solution sol1!")))
    (format #t "Solution 1: ~a\n" sol1)))
