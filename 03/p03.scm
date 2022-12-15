#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 control)
             (ice-9 match)
             (srfi srfi-1)
             (rnrs base))

(define input "input.txt")

;; Create a vector acting as a hash table to map integer values of
;; chars to priorities. 122 is the highest int value of all chars
;; a-z,A-Z, thus we need a vector of length 123.
;;
;; Lowercase item types a through z have priorities 1 through 26.
;; Uppercase item types A through Z have priorities 27 through 52.
(define char->priority (make-vector 123 #f))
(let* ((A (char->integer #\A))
       (Z (char->integer #\Z))
       (diff (- (char->integer #\a) A)))
  (let loop ((i A))
    (vector-set! char->priority (+ i diff) (+ (- i A) 1)) ; lowercase
    (vector-set! char->priority i (+ (- i A) 27)) ; uppercase
    (unless (>= i Z)
      (loop (1+ i)))))

(define (get-priority rucksack)
  ;; (format #t "current rucksack:\n~a\n" rucksack)
  (let* ((content (string->list rucksack))
         (seen-chars (make-vector 123 #f))
         (len/2 (/ (length content) 2)))
    (call/ec                           ; allows to escape from fold by
     (lambda (return)                  ; calling RETURN
       ;; O(n) where n is total number of items in both compartments
       (fold
        (lambda (char idx)
          ;; (format #t " looking at: ~a (~a)\n" char idx)
          (if (< idx len/2)
              ;; as long as we are in the first half, remember found chars
              (or (vector-ref seen-chars (char->integer char))
                  (vector-set! seen-chars (char->integer char) #t))

              ;; in the second half of the list we just check for occurrences
              (if (vector-ref seen-chars (char->integer char))
                  (return (vector-ref char->priority (char->integer char)))))
          (1+ idx))                     ; next idx
        0                               ; start idx
        content)))))

(define (get-group-priority rucksacks)
  "Given a list RUCKSACKS of rucksack content strings, return their
priority."
  ;; This is in O(n) where n is the total length of all rucksack
  ;; contents.
  (let ((seen-chars (make-vector 123 0))
        (n (length rucksacks)))
    (call/ec
     (lambda (return)
       (let loop ((ruck rucksacks)
                  (marker 1))
         (let ((content (string->list (car ruck))))
           (for-each
            (lambda (char)
              (if (= (vector-ref seen-chars (char->integer char)) (1- marker))
                  (if (= marker n)
                      ;; there is only one item type present in each n
                      ;; lists, so here we can return.
                      (return (vector-ref char->priority (char->integer char)))
                      (vector-set! seen-chars (char->integer char) marker))))
            content)
           (loop (cdr ruck) (1+ marker)))
         rucksacks)))))

(define (solution-1 rucksacks)
  (apply + (map get-priority rucksacks)))

(define* (solution-2 rucksacks)
  (assert (= (remainder (length rucksacks) 3) 0))
  (let loop ((rest rucksacks)
             (sum 0))
    (match rest
      (() sum)
      ((r1 r2 r3 . rest)
       (loop rest (+ sum (get-group-priority (list r1 r2 r3))))))))

(define (load-rucksacks file)
  "Processes the input file and return a list of strings, where each
string represents the content of a single rucksack. "
  (filter-map (lambda (line)
                (if (string-null? line) #f line))
   (string-split
    (call-with-input-file file
      (lambda (port)
        (get-string-all port)))
    #\newline)))

(define (main args)
  (assert (= (vector-ref char->priority (char->integer #\a)) 1))
  (assert (= (vector-ref char->priority (char->integer #\z)) 26))
  (assert (= (vector-ref char->priority (char->integer #\A)) 27))
  (assert (= (vector-ref char->priority (char->integer #\Z)) 52))

  (let* ((rucksacks (load-rucksacks input))
         (sol1 (solution-1 rucksacks))
         (sol2 (solution-2 rucksacks)))
    (assert (= sol1 7848))
    (assert (= sol2 2616))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
