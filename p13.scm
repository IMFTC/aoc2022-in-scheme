#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 format)
             (rnrs bytevectors)
             (ice-9 iconv)
             (ice-9 q)
             (ice-9 control)
             (ice-9 binary-ports))

(define input "input13.txt")

(define (load-lists-from-file file)
  (eval-string
   (call-with-input-file file
     (lambda (file-port)
       (call-with-output-string
         (lambda (string-port)
           (display "'[" string-port)
           (let loop ((byte (get-u8 file-port)))
             (unless (eof-object? byte)
               (put-u8 string-port
                       (if (= byte (char->integer #\,))
                           (char->integer #\space)
                           byte))
               (loop (get-u8 file-port))))
           (display "]" string-port)))))))

(define (in-order? list1 list2)
  (define lq (make-q))
  (define rq (make-q))
  (map (lambda (e) (q-push! lq e)) (cons #f (reverse list1)))
  (map (lambda (e) (q-push! rq e)) (cons #f (reverse list2)))
  ;; we assume both queues are not empty!
  (call/ec
   (lambda (return)
     (let lp ((l (q-pop! lq))
              (r (q-pop! rq)))
       ;;(format #t "comparing: ~a vs ~a\nl stack: ~a\nr stack: ~a\n\n" l r (car lq) (car rq))
       (cond
        ((not l) (if r (return #t)))
        ((not r) (return #f))
        ((and (number? l) (number? r))
         (cond ((> l r) (return #f))
               ((< l r) (return #t))))
        ((number? l)
         (q-push! lq #f) (q-push! lq l)
         (map (lambda (e) (q-push! rq e)) (cons #f (reverse r))))
        ((number? r)
         (q-push! rq #f) (q-push! rq r)
         (map (lambda (e) (q-push! lq e)) (cons #f (reverse l))))
        (else
         (map (lambda (e) (q-push! lq e)) (cons #f (reverse l)))
         (map (lambda (e) (q-push! rq e)) (cons #f (reverse r)))))
       (cond ((q-empty? lq) #t)
             ((q-empty? rq) #f)
             ((lp (q-pop! lq)
                  (q-pop! rq))))))))

(define (count-in-order-list-pairs lists)
  (let lp ((lists lists)
           (idx 1)
           (idx-sum 0))
    (cond ((null? lists)
           idx-sum)
          (else
           (lp (cddr lists)
               (1+ idx)
               (+ idx-sum (if (in-order? (car lists) (cadr lists)) idx 0)))))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (lists (load-lists-from-file input-file))
         (sol1 (count-in-order-list-pairs lists))
         (sol2 "TODO"))
    (format #t "input-file: ~a\n" input-file)

    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)
    (when (null? (cdr args))
      (unless (equal? sol1 5503) (error "Wrong solution sol1!"))
      (unless (equal? sol2 "TODO") (error "Wrong solution sol2!")))))
