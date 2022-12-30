#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)              ;records
             (srfi srfi-26))            ;cut

(define input "input.txt")

(define-record-type <monkey>
  (make-monkey number items inspections operation test if-true if-false)
  monkey?
  (number monkey-number)
  (items monkey-items set-monkey-items!) ;list of integers
  (inspections monkey-inspections set-monkey-inspections!) ;integer
  (operation monkey-operation)           ;procedure
  (test monkey-test)                     ;integer
  (if-true monkey-if-true)               ;integer
  (if-false monkey-if-false))            ;integer

(define (parse-monkey port)
  (define (parse-items)
    (map string->number
         (map (cut string-trim <> #\space)
              (string-split
               (list-ref (string-split (get-line port) #\:) 1)
               #\,))))

  (define (parse-operation)
    (match (string-split
            (string-trim
             (list-ref (string-split (get-line port) #\=) 1)
             #\space)
            #\space)
      ((lhs op rhs)
       ;; return a procedure that takes the old
       ;; value as input and returns the new value
       (lambda (old)
         ((case (string->symbol op) ((+) +) ((-) -) ((*) *) ((/) /)
                (else (error "unsupported operand")))
          (if (equal? lhs "old") old (string->number lhs))
          (if (equal? rhs "old") old (string->number rhs)))))))

  (let ((first-line (get-line port)))
    (cond
     ((eof-object? first-line)
      #f)                               ;signal end of file
     (else
      ;; We don't parse the monkey number, it will be reflected by
      ;; the monkey's position in the monkey-list returned by
      ;; parse-monkeys-from-file.
      (unless (string-prefix? "Monkey " first-line)
        (error "First line parse-monkey read from port did not start with 'Monkey'"))
      (let* ((number (string->number (string-trim-right (substring/read-only first-line 7) #\:)))
             (items (parse-items))
             (operation (parse-operation))
             ;; the following three are position dependent and very
             ;; unreliable in practice
             (test (string->number (substring/read-only (get-line port) 21)))
             (if-true (string->number (substring/read-only (get-line port) 29)))
             (if-false (string->number (substring/read-only (get-line port) 30))))
        ;; read empty line from port
        (get-line port)
        (make-monkey number items 0 operation test if-true if-false))))))

(define (parse-monkeys-from-file file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((monkey-list '()))
        (let ((monkey (parse-monkey port)))
          (if monkey
              (loop (cons monkey monkey-list))
              (reverse monkey-list)))))))

(define* (run-monkey-rounds! monkey-list rounds #:optional (divide-worry #t))
  ;; The numbers in part 2 get really big here and computation takes
  ;; too long, but since we are not interested in the actual
  ;; worry-levels but just in the remainders when dividing by any of
  ;; the test numbers we use the fact that the test numbers are primes
  ;; (assuming they are always primes on purpose for all inputs) and
  ;; can just work modulo their product
  ;;
  ;;    test_1 * .... * test_n,
  ;;
  ;; where test_i is the test number of monkey i, since this product
  ;; is their lowest common denominator (LCD).
  ;;
  ;; To be more precise: we use the fact that, given integers
  ;; m_1,...,m_k that are not 0, and
  ;;
  ;;    m := LCD(m_1,...,m_k),
  ;;
  ;; their lowest common denominator, it holds that
  ;;
  ;;    a ≡ b (mod m_k) for k = 1,...,k ⇔ a ≡ b (mod m).
  ;;
  ;; (Source: Bundschuh, Einführung in die Zahlentheorie, 5. Auflage, S. 79)
  (let ((tests-lcd (apply * (map monkey-test monkey-list))))
    (let loop ((rnd 0))
      (when (< rnd rounds)
        (for-each
         (lambda (monkey)
           (match monkey
             (($ <monkey> number items inspections operation test if-true if-false)
              ;; increase the monkey's total inspection (item) count
              ;;(format #t "round: ~a, monkey: ~a\n" rnd (monkey-number monkey))
              (set-monkey-inspections! monkey (+ (length items) inspections))
              ;; transfer of items: we first collect the thrown items in
              ;; two lists and when we are done we append the lists to
              ;; the items of the other two monkeys
              (let ((monkey-true (list-ref monkey-list if-true))
                    (monkey-false (list-ref monkey-list if-false)))
                ;;(format #t "items: ~a\n" items)
                (let loop-items ((remaining-items items)
                                 (list-true '())
                                 (list-false '()))
                  (cond ((null? remaining-items)
                         ;; update items lists of involved monkeys
                         (set-monkey-items! monkey '())
                         (set-monkey-items! monkey-true
                                            (append (monkey-items monkey-true)
                                                    (reverse list-true)))
                         (set-monkey-items! monkey-false
                                            (append (monkey-items monkey-false)
                                                    (reverse list-false))))
                        (else
                         (let ((item (car remaining-items)))
                           (let* ((worry-level (if divide-worry
                                                   (floor-quotient (operation item) 3)
                                                   (floor-remainder (operation item) tests-lcd)))
                                  (divisible (zero? (floor-remainder worry-level test))))
                             ;; (format #t "item with old wl: ~4,d, new wl: ~4,d → monkey ~a\n"
                             ;;         item worry-level (if divisible if-true if-false))
                             (loop-items (cdr remaining-items)
                                         (if divisible (cons worry-level list-true)
                                             list-true)
                                         (if divisible list-false
                                             (cons worry-level list-false))))))))))))
         monkey-list)
        (loop (1+ rnd)))))
  monkey-list)

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 (apply * (list-head
                         (sort (map monkey-inspections
                                    (run-monkey-rounds!
                                     (parse-monkeys-from-file input-file) 20))
                               >)
                         2)))
         (sol2 (apply * (list-head
                         (sort (map monkey-inspections
                                    (run-monkey-rounds!
                                     (parse-monkeys-from-file input-file) 10000 #f))
                               >)
                         2))))
    (when (null? (cdr args))
      (unless (equal? sol1 90882) (error "Wrong solution sol1!"))
      (unless (equal? sol2 30893109657) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
