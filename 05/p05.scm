#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 binary-ports)
             (ice-9 match)
             (rnrs bytevectors))

(define input "input.txt")

(define (get-input-as-bytes file)
  (call-with-input-file file
    (lambda (port)
      (get-bytevector-all port))))

(define (copy-stacks stacks)
  (let* ((length (vector-length stacks))
         (stacks-copy (make-vector length)))
    (let loop ((n 0))
      (when (< n length)
        (vector-set! stacks-copy n
                     (list-copy (vector-ref stacks n)))
        (loop (1+ n))))
    stacks-copy))

;;; This time we parse raw bytes (it's only ASCII), more
;;; cumbersome but should run faster than using lists of chars etc.
(define (make-byte-parser bytevec)
  (define* (make-find-char-procedure char)
    "Return a procedure (PROC [START-INDEX]) that searches for the
    (ASCII) char CHAR in BYTEVEC.  START-INDEX is optional and
    specifies first byte at which the search starts.

    The return value of PROC is the index of the first occurrence of char
    or #f it wasn't found."
    (let ((char-i (char->integer char))
          (vec-len (bytevector-length bytevec)))
      (lambda* (#:optional (start-index 0))
        (let loop ((idx start-index))
          (if (< idx vec-len)
              (if (= char-i (bytevector-u8-ref bytevec idx))
                  idx
                  (loop (1+ idx)))
              #f)))))

  ;; very simple, doesn't handle < 0, no error checks
  (define (parse-decimal-number start end)
    (let loop ((pos start)
               (result 0))
      (if (>= pos end) result
          (loop (1+ pos)
                (+ (* 10 result) (- (bytevector-u8-ref bytevec pos) 48)))))) ; 48 is #\0))

  ;; used for testing only
  (define* (peeks pos #:optional (length 15))
    (let loop ((count 0))
      (when (< count length)
        (format #t "~a" (integer->char (bytevector-u8-ref bytevec (+ pos count))))
        (loop (1+ count)))))

  (let* ((vec-len (bytevector-length bytevec))
         (find-1 (make-find-char-procedure #\1))
         (find-nl (make-find-char-procedure #\newline))
         (find-space (make-find-char-procedure #\space))
         (find-m (make-find-char-procedure #\m))
         (space-int (char->integer #\space))
         (nl-int (char->integer #\newline))
         (line-length (1+ (find-nl)))
         (n-stacks (/ line-length 4))
         (n-rows (/ (- (find-1) 1) line-length)))

    (define (get-stacks)
      (let ((stacks (make-vector n-stacks '())))
        ;; build the crate stacks as lists, starting from the bottom
        (let row-loop ((row (1- n-rows)))
          (when (>= row 0)
            (let ((offset (1+ (* row line-length))))
              (let stack-loop ((col 0))
                (when (< col n-stacks)
                  (let ((c (bytevector-u8-ref bytevec (+ offset (* col 4)))))
                    (unless (= c space-int)
                      (vector-set! stacks col
                                   (cons c (vector-ref stacks col)))))
                  (stack-loop (1+ col)))))
            (row-loop (1- row))))
        stacks))

    ;; parse the program into a list of lists ((number-of-crates from to) ... )
    (define (get-program)
      (let loop ((byte-offset (1+ (* (1+ n-rows) line-length)))
                 (program '()))
        ;;(peeks byte-offset)
        (cond ((or (>= byte-offset vec-len)
                   (= (bytevector-u8-ref bytevec byte-offset) nl-int))
               (if (>= byte-offset vec-len)
                   (format #t "[~a] End of program text signaled by end of file\n" byte-offset)
                   (format #t "[~a] End of program text signaled by newline\n" byte-offset))
               (reverse! program))
              ;; a program line looks like this: "move 1 from 1 to 4"
              (else
               (let* ((move-start (1+ (find-space byte-offset)))
                      (move-end (find-space move-start))
                      (from-start (+ move-end 6))
                      (from-end (find-space from-start))
                      (to-start (+ from-end 4))
                      (to-end (find-nl to-start)))
                 (loop (1+ to-end)
                       (cons (list (parse-decimal-number move-start move-end)
                                   ;; 1- to get 0 based indices for stacks
                                   (1- (parse-decimal-number from-start from-end))
                                   (1- (parse-decimal-number to-start to-end)))
                             program)))))))
    (lambda args
      (apply (case (car args)
               ((get-stacks) get-stacks)
               ((get-program) get-program)
               (else (error "Invalid parser procedure!")))
             (cdr args)))))

(define (run-program program stacks reverse-order)
  (let ((stacks (copy-stacks stacks)))
    (for-each
     (match-lambda
       ((n from-stack to-stack)
        (let* ((from-list (vector-ref stacks from-stack))
               (to-list (vector-ref stacks to-stack))
               (crates (list-head from-list n)))
          (vector-set! stacks to-stack
                       (append! ((if reverse-order reverse identity) crates)
                                to-list))
          (vector-set! stacks from-stack (list-tail from-list n)))))
     program)
    ;; return result
    (list->string
     (let loop ((i 0)
                (result '()))
       (if (< i (vector-length stacks))
           (loop (1+ i)
                 (cons (if (null? (vector-ref stacks i))
                           #\space
                           (integer->char (car (vector-ref stacks i))))
                       result))
           (reverse result))))))

;;; A different input file can be specified as first command line argument
(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (assignments (get-input-as-bytes input-file))
         (parser (make-byte-parser (get-input-as-bytes input-file)))
         (program (parser 'get-program))
         (stacks (parser 'get-stacks))
         (sol1 (run-program program stacks #t))
         (sol2 (run-program program stacks #f)))
    (when (null? (cdr args))
      (unless (equal? sol1 "JCMHLVGMG") (error "Wrong solution sol1!"))
      (unless (equal? sol2 "LVMRWSSPZ") (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
