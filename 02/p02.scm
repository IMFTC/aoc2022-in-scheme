#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (rnrs base))

;;; First char in each row is what opponent will play
;;; A: rock, B: paper, C: scissors
;;; Third char is our response
;;; X: rock, Y: paper, Z: scissors
;;; We use convention:
;;; 0: rock, 1: paper, 2: scissors
;;;
;;; Scores per round:
;;; 1, 2, 3 points if WE play rock, paper, scissors
;;; + 0 if we lost, 3 for a draw, 6 if we won
(define input "input.txt")

;; The i-th index contains what i beats, where i and the content is
;; one of 0: rock, 1: paper, 2: scissors.
(define beats #1(2 0 1))

;;; Create an array that maps the two indices OP and WE -> score
;;; with indices 0: rock, 1: paper, 2: scissors.
;;; The resulting points-array looks like this:
;;;  #2((4 8 3)
;;;     (1 5 9)
;;;     (7 2 6))
(define points-array (make-array 0 3 3))
(array-index-map!
 points-array
 (lambda (op we)
   (+ (cond
       ((= (array-ref beats we) op) 6)  ; 6 points if we win
       ((= op we) 3)                    ; 3 points if we draw
       (else 0))                        ; 0 points if we loose
      (+ we 1))))                       ; additional 1, 2, 3 points if
                                        ; WE play rock, paper,
                                        ; scissors

(define (get-score game)
  "Takes a string like \"A Y\" and returns the score."
  (cond ((= (string-length game) 3)
         (let* ((chars (string->list game))
                (op (- (char->integer (list-ref chars 0))
                       (char->integer #\A)))
                (we (- (char->integer (list-ref chars 2))
                       (char->integer #\X))))
           (array-ref points-array op we)))
        (else 0)))

(define (main args)
  (let* ((games
          (string-split
           (call-with-input-file input
             (lambda (port)
               (get-string-all port)))
           #\newline))
         (sol1 (apply +
                (map get-score
                     games))))
    (assert (= 14375 sol1))
    (format #t "Solution 1: ~a\n" sol1)))
