#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 control)
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
  ;(format #t "current rucksack:\n~a\n" rucksack)
  (let* ((content (string->list rucksack))
         (seen-chars (make-vector 123 #f))
         (len/2 (/ (length content) 2)))

    (call/ec                           ; allows to escape from fold by
     (lambda (return)                  ; calling RETURN
       ;; O(n) where n is total number of items in both compartments
       (fold
        (lambda (char idx)
          ;(format #t " looking at: ~a (~a)\n" char idx)
          ;; as long as we are in the first half, remember found chars
          (if (< idx len/2)
              (or (vector-ref seen-chars (char->integer char))
                  ;; as long as we are in the first half, remember found chars
                  (vector-set! seen-chars (char->integer char) #t))

              ;; in the second half of the list we just check for occurrences
              (if (vector-ref seen-chars (char->integer char))
                  (return (vector-ref char->priority (char->integer char)))))
          (1+ idx))                     ; next indx
        0                               ; start idx
        content)))))

(define (get-rucksacks)
  (string-split
   (call-with-input-file input
     (lambda (port)
       (get-string-all port)))
   #\newline))

(define (main args)
  (assert (= (vector-ref char->priority (char->integer #\a)) 1))
  (assert (= (vector-ref char->priority (char->integer #\z)) 26))
  (assert (= (vector-ref char->priority (char->integer #\A)) 27))
  (assert (= (vector-ref char->priority (char->integer #\Z)) 52))
  (let ((sol1 (apply + (map get-priority (get-rucksacks)))))
    (assert (= sol1 7848))
    (format #t "Solution 1: ~a\n" sol1)))
