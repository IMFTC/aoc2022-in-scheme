#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-26))            ; cut

(define input "input.txt")

(define (get-program-text-from-file file)
  (call-with-input-file file
    (lambda (port)
      (get-string-all port))))

(define (make-computer)
  (define (run-program program-text n-knots)
    "Run the string PROGRAM-TEXT as a program and return the number of
spaces visited by TAIL of a rope with N-KNOTS knots."
    (let* ((count-visits-ht (make-hash-table))
           (get-x (cut s64vector-ref <> 0))
           (get-y (cut s64vector-ref <> 1))
           (set-x! (cut s64vector-set! <> 0 <>))
           (set-y! (cut s64vector-set! <> 1 <>))
           (move! (lambda (vec direction)
                    (case direction
                      ((#\R) (set-x! vec (1+ (get-x vec))))
                      ((#\L) (set-x! vec (1- (get-x vec))))
                      ((#\U) (set-y! vec (1+ (get-y vec))))
                      ((#\D) (set-y! vec (1- (get-y vec)))))))
           (knots (make-list n-knots #f)))
      ;; put a (x,y) coordinates vector in the car of every list pair,
      ;; initialized with (0,0)
      (let loop ((rest knots))
        (unless (null? rest)
          (set-car! rest (make-s64vector 2 0))
          (loop (cdr rest))))

      (let ((head (car knots))
            (tail (car (last-pair knots))))
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
                   ;; loop over all lead-follow pairs updating the
                   ;; position of the follower
                   (let loop ((lead knots)
                              (follow (cdr knots)))
                     (let* ((lead-xy (car lead))
                            (follow-xy (car follow))
                            ;; get positions after the lead-xy has moved
                            (dx (- (get-x lead-xy) (get-x follow-xy)))
                            (dy (- (get-y lead-xy) (get-y follow-xy))))
                       (cond ((and (<= (abs dx) 1) (<= (abs dy) 1)))
                             ((= dy 0)
                              (cond ((>= dx 2) (move! follow-xy #\R))
                                    ((<= dx -2) (move! follow-xy #\L))))
                             ((= dx 0)
                              (cond ((>= dy 2) (move! follow-xy #\U))
                                    ((<= dy -2) (move! follow-xy #\D))))
                             (else
                              ;; lead-xy and follow-xy are not on the same row or column
                              (begin (if (> dx 0) (move! follow-xy #\R))
                                     (if (< dx 0) (move! follow-xy #\L))
                                     (if (> dy 0) (move! follow-xy #\U))
                                     (if (< dy 0) (move! follow-xy #\D))))))
                     (unless (null? (cdr follow))
                       ;; follow becomes the new lead
                       (loop follow (cdr follow))))
                   ;; after all lead-follow pairs have been iterated, mark the new position of the tail
                   (hash-set! count-visits-ht (cons (get-x tail) (get-y tail))
                              (1+ (hash-ref count-visits-ht (cons (get-x tail) (get-y tail)) 0)))
                   (loop (1- steps))))))))

        (for-each exec-instruction (string-split program-text #\newline))
        (hash-count (const #t) count-visits-ht))))

  (lambda args
    (apply (case (car args)
             ((run-program) run-program))
           (cdr args))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (program-text (get-program-text-from-file input-file))
         (computer (make-computer))
         (sol1 (computer 'run-program program-text 2))
         (sol2 (computer 'run-program program-text 10)))
    (when (null? (cdr args))
      (unless (= sol1 6044) (error "Wrong solution sol1!"))
      (unless (= sol2 2384) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
