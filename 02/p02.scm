#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports))

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


(define (beats play)
  ;; 0(rock) <beats 1(paper) <beats 2(scissors) <beats 0(rock)
  (floor-remainder (- play 1) 3))

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
       ((= (beats we) op) 6)            ; 6 points if we win
       ((= op we) 3)                    ; 3 points if we draw
       (else 0))                        ; 0 points if we loose
      (+ we 1))))                       ; additional 1, 2, 3 points if
                                        ; WE play rock, paper,
                                        ; scissors

(define move-array (make-array 0 3 3))
(array-index-map!
 move-array
 (lambda (op outcome)
   (case outcome
     ((0) (beats op))                   ; we must loose
     ((1) op)                           ; we must draw
     ((2) (beats (beats op))))))        ; we must win

(define (get-score game-line solution-part)
  "Takes a string GAME of the form \"A Y\" and returns the score."
  (cond ((= (string-length game-line) 3)
         (let* ((chars (string->list game-line))
                (op (- (char->integer (list-ref chars 0))
                       (char->integer #\A)))
                (we (- (char->integer (list-ref chars 2))
                       (char->integer #\X))))
           (array-ref points-array
                      op
                      (case solution-part
                        ((1) we)
                        ((2) (array-ref move-array op we))))))
        (else 0)))

(define (get-games file)
  (string-split
   (call-with-input-file file
     (lambda (port) (get-string-all port)))
   #\newline))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (games (get-games input-file))
         (sol1 (apply +(map (lambda (g) (get-score g 1)) games)))
         (sol2 (apply +(map (lambda (g) (get-score g 2)) games))))
    (when (null? (cdr args))
      (unless (= sol1 14375) (error "Wrong solution sol1!"))
      (unless (= sol2 10274) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
