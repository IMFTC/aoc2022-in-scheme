#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 q)
             (ice-9 binary-ports)
             (rnrs bytevectors))

(define input "input.txt")

(define (get-input-as-bytes file)
  (call-with-input-file file
    (lambda (port)
      (get-bytevector-all port))))

(define (make-magic-queue size)
  (let ((q (make-q))
        ;; maps items (chars) to the number of occurrences in the
        ;; queue. Integer values: A-Z < a-z <= z (122).
        (occurrences (make-bytevector 123 0))
        (total-chars 0)
        (unique-chars 0)
        (chars '()))
    ;; runs in O(1)
    (define (push c)
      (if (>= total-chars size)
          ;; if the queue is full, first pop the oldest item
          (let* ((dequeued (deq! q))
                 (n (bytevector-u8-ref occurrences dequeued)))
            (bytevector-u8-set! occurrences dequeued (1- n))
            (when (= n 1)
              (set! unique-chars (1- unique-chars))))
          ;; the queue was not full yet
          (set! total-chars (1+ total-chars)))
      ;; push the new char to the queue
      (enq! q c)
      (let ((n (bytevector-u8-ref occurrences c)))
        (bytevector-u8-set! occurrences c (1+ n))
        (when (= n 0)
          (set! unique-chars (1+ unique-chars))))
      ;; return value
      (= unique-chars size))
    (lambda args
      (apply (case (car args)
               ((push) push))
             (cdr args)))))

(define (search-start bytevector marker-size)
  (let ((q (make-magic-queue marker-size))
        (len (bytevector-length bytevector)))
    (let loop ((idx 0))
      (if (< idx len)
          (if (q 'push (bytevector-u8-ref bytevector idx))
              (1+ idx)                  ; return
              (loop (1+ idx)))
          #f))))                        ; nothing found

;;; A different input file can be specified as first command line argument
(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 (search-start (get-input-as-bytes input-file) 4))
         (sol2 (search-start (get-input-as-bytes input-file) 14)))
    (when (null? (cdr args))
      (or (= sol1 1848) (error "Wrong solution sol1!"))
      (or (= sol2 2308) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
