#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match))

(define input "input.txt")

(define (parse-array-from-file file)
  (call-with-input-file file
    (lambda (port)
      (list->typed-array
       'u8 2
       (map (lambda (line)
              (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                   (string->list line)))
            (filter (negate string-null?)
                    (string-split (get-string-all port)
                                  #\newline)))))))

(define (count-visible-trees array)
  (match-let (((rows cols) (array-dimensions array)))
    ;; First we create a new array of the same size to mark seen
    ;; trees.
    (let ((seen-trees (make-typed-array 'u8 0 rows cols)))

      (define (mark-rows array seen-trees fwd-seen-val bkd-seen-val)
        (let loop-rows ((r 0))
          (when (< r rows)
            (let ((last-seen-forward -1))
              (let loop-cols-forward ((c 0) (max-height -1))
                (when (< c cols)
                  (let ((height (array-ref array r c)))
                    (when (> height max-height)
                      (array-set! seen-trees fwd-seen-val r c)
                      ;; remember the column index of the last tree seen from the 'left'
                      (set! last-seen-forward c))
                    (loop-cols-forward (1+ c) (max height max-height)))))
              (let loop-cols-backward ((c (1- cols)) (max-height -1))
                ;; When looking from the right we only have to check
                ;; trees up to (and including) the last tree seen from
                ;; the left, since all trees left of it must be lower
                ;; and therefore can't be visible from the right.
                (when (>= c last-seen-forward)
                  (let ((height (array-ref array r c)))
                    (when (> height max-height)
                      (array-set! seen-trees
                                  (+ (array-ref seen-trees r c) bkd-seen-val) r c))
                    (loop-cols-backward (1- c) (max height max-height))))))
            (loop-rows (1+ r)))))

      (mark-rows array seen-trees 1 2)
      ;; Run the same function on a shared transposed array (colums <-> rows)
      (mark-rows (make-shared-array array (lambda (x y) (list y x)) cols rows)
                 (make-shared-array seen-trees (lambda (x y) (list y x)) cols rows) 4 8)
      ;; (print-array seen-trees)
      (let ((count 0))
        (array-for-each
         (lambda (e) (if (> e 0) (set! count (1+ count))))
         seen-trees)
        count))))

(define (get-highest-scenic-score array)
  (match-let (((rows cols) (array-dimensions array)))
    (let ((scenic-score (make-typed-array 'u32 1 rows cols)))

      (define (scenic-for-rows coord-transform rows cols)
        (let ((array (make-shared-array array coord-transform rows cols))
              (scenic-score (make-shared-array scenic-score coord-transform rows cols)))
          (let loop-rows ((r 0))
            (when (< r rows)
              ;; Maps height to the last index where a tree of that height
              ;; was seen in the current row.
              (let* ((last-index-with-height (make-u32vector 10 0)))
                (let loop-cols ((c 0))
                  (when (< c cols)
                    (let* ((height (array-ref array r c))
                           ;; Find the distance to the closest tree to
                           ;; the left with the same or larger
                           ;; height. This is in O(1) (Finding the
                           ;; maximum of up to 10 values.)
                           (scenic-dist (let loop ((h height)
                                                   (closest-blocker 0))
                                          (if (>= h 10)
                                              (- c closest-blocker)
                                              (loop (1+ h)
                                                    (max (u32vector-ref last-index-with-height h)
                                                         closest-blocker))))))
                      (u32vector-set! last-index-with-height height c)
                      ;; (format #t "r:~a c:~a scenic-dist: ~a, height: ~a, last-index-with-height: ~a\n"
                      ;;         r c scenic-dist height last-index-with-height)
                      (array-set! scenic-score (* scenic-dist (array-ref scenic-score r c)) r c))
                    (loop-cols (1+ c)))))
              (loop-rows (1+ r))))))

      (map (match-lambda ((transform rc1 rc2)
                          (scenic-for-rows transform rc1 rc2)))
           (list (list (lambda (r c) (list r c)) rows cols) ; rows left
                 (list (lambda (r c) (list r (- cols c 1))) rows cols) ; rows right
                 (list (lambda (r c) (list c r)) cols rows) ; cols up
                 (list (lambda (r c) (list (- rows c 1) r)) cols rows))) ; cols down

      (let ((max-score 0))
        (array-for-each (lambda (score)
                          (if (> score max-score)
                              (set! max-score score)))
                        scenic-score)
        max-score))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (array (parse-array-from-file input-file))
         (sol1 (count-visible-trees array))
         (sol2 (get-highest-scenic-score array)))
    (when (null? (cdr args))
      (or (= sol1 1829) (error "Wrong solution sol1!"))
      (or (= sol2 291840) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
