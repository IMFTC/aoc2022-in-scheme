#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 q)
             (ice-9 control)
             (ice-9 binary-ports))

(define input "input13.txt")
(define divider-packages '(((2)) ((6))))

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
  (let ((lq (make-q))
        (rq (make-q)))
    (map (lambda (e) (q-push! lq e)) (cons #f (reverse list1)))
    (map (lambda (e) (q-push! rq e)) (cons #f (reverse list2)))
    (call/ec
     (lambda (return)
       (let lp ((l (q-pop! lq)) (r (q-pop! rq)))
         (cond
          ((not l) (if r (return #t)))
          ((not r) (return #f))
          ((and (number? l) (number? r))
           ;; note that we ignore the case (= l r)
           (cond ((> l r) (return #f))
                 ((< l r) (return #t))))
          (else
           ;; if at least one of l, r is a list, push both to the
           ;; respective stacks, denoting the end of a list with #f
           (map (lambda (e) (q-push! lq e))
                (cons #f (reverse (if (number? l) (list l) l))))
           (map (lambda (e) (q-push! rq e))
                (cons #f (reverse (if (number? r) (list r) r))))))
         (cond ((q-empty? lq) #t)
               ((q-empty? rq) #f)
               (else (lp (q-pop! lq) (q-pop! rq)))))))))

(define (count-in-order-list-pairs lists)
  (let lp ((lists lists) (idx 1) (idx-sum 0))
    (if (null? lists)
        idx-sum
        (lp (cddr lists) (1+ idx)
            (+ idx-sum (if (in-order? (car lists) (cadr lists)) idx 0))))))

(define (get-decoder-key lists divider-packages)
  ;; find both indices and multiply them in one list pass
  (let lp ((rest (sort-list (cons* (car divider-packages)
                                   (cadr divider-packages)
                                   lists)
                            in-order?))
           (i 1)
           (result #f))
    (if (member (car rest) divider-packages)
        (if result
            (* i result)                ;return value
            (lp (cdr rest) (1+ i) i))
        (lp (cdr rest)
            (1+ i)
            result))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (lists (load-lists-from-file input-file))
         (sol1 (count-in-order-list-pairs lists))
         (sol2 (get-decoder-key lists divider-packages)))
    (format #t "input-file: ~a\n" input-file)
    (when (null? (cdr args))
      (unless (equal? sol1 5503) (error "Wrong solution sol1!"))
      (unless (equal? sol2 20952) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
