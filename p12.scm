#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 receive)
             (ice-9 match)
             (ice-9 binary-ports)
             (srfi srfi-11)             ;let-values
             (rnrs bytevectors)
             (libaoc heap))

(define input "input12.txt")

(define (get-map-array-from-file file)
  "Returns 4 values: MAPVECTOR ROWS COLS (S_ROW . S_COL) (E_ROW
. E_COL), which are the map as a 2-dimensional u8 array, the map size
as rows and cols, and two pairs specifying the start and end
coordinates."
  (call-with-input-file file
    (lambda (port)
      ;;                     1+ for #\newline byte
      (let ((bytes-per-line (1+ (string-length (get-line port)))))
        (seek port SEEK_SET 0)
        (let* ((bv (get-bytevector-all port))
               (nbytes (bytevector-length bv))
               (rows (/ nbytes bytes-per-line)) ;1+ for #\newline
               (start #f) (end #f))
          (do ((i 0 (1+ i)))
              ((>= i nbytes)           ;break condition
               (values
                ;; array representation of the underlying bytevector
                (make-shared-array
                 bv (lambda (r c) (list (+ (* r bytes-per-line) c))) rows (1- bytes-per-line))
                rows (1- bytes-per-line)
                start
                end))
            ;; the chars #\S,#\E have a lower integer value than a-z
            (if (< (bytevector-u8-ref bv i) (char->integer #\a))
                (case (bytevector-u8-ref bv i)
                  ((83)               ;(char->integer #\S)
                   (set! start (receive (r c) (floor/ i bytes-per-line) (cons r c)))
                   (bytevector-u8-set! bv i (char->integer #\a)))
                  ((69)               ;(char->integer #\E)
                   (set! end (receive (r c) (floor/ i bytes-per-line) (cons r c)))
                   (bytevector-u8-set! bv i (char->integer #\z)))))))))))

(define (dijkstra map-array start height-delta-test goal-test)
  (match-let* (((rows cols) (array-dimensions map-array))
               (start-node (make-node start 0))
               (visited-nodes-ht (make-hash-table (* rows cols)))
               (heap (make-heap (* rows cols) <)))

    (define (get-neighbors pos height-delta-test)
      (let* ((pos-height (array-ref map-array (car pos) (cdr pos))))
        (filter!
         (match-lambda
           ((row . col)
            (height-delta-test
             pos-height
             (array-ref map-array row col))))
         (match pos
           ((row . col)
            `(,@(if (> col 0)         `(,(cons row (1- col))) '())
              ,@(if (< row (1- rows)) `(,(cons (1+ row) col)) '())
              ,@(if (< col (1- cols)) `(,(cons row (1+ col))) '())
              ,@(if (> row 0)         `(,(cons (1- row) col)) '())))))))

    (call/cc
     (lambda (return)
       (let loop ((node start-node))
         ;; (format #t "dijkstra node: ~a -> \n" node)
         (when (goal-test node map-array)
           (return node))
         (match node
           (($ <node> pos key)
            (for-each
             (lambda (nb-pos)           ; (row . col) pair
               (let ((nb-node (hash-ref visited-nodes-ht nb-pos)))
                 (cond (nb-node
                        (when (< (1+ key) (node-key nb-node))
                          (heap-raise-node! heap nb-node (1+ key))))
                       (else
                        (let ((nb-node (make-node nb-pos (1+ key))))
                          (hash-set! visited-nodes-ht nb-pos nb-node)
                          (heap-insert! heap nb-node))))))
             (get-neighbors pos height-delta-test))
            (when (> (heap-size heap) 0)
              (loop (heap-extract-top! heap))))))
       (error "No path found!")))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args))))
    (let-values (((map-array rows cols start end)
                  (get-map-array-from-file input-file)))
      ;; for part 1
      (define (sol1-step-ok? pos-height nb-height)
        (<= (- nb-height pos-height) 1))
      (define (sol1-goal-test node map-array)
        (equal? (node-data node) end))

      ;; for part 2: For part 2 the task reduces to finding a path
      ;; backwards from the goal to the first encountered "a"
      ;; position.
      (define (sol2-step-ok? pos-height nb-height)
        (<= (- pos-height nb-height) 1))
      (define (sol2-goal-test node map-array)
        (= (array-ref map-array
                      (car (node-data node))
                      (cdr (node-data node)))
           (char->integer #\a)))

      (format #t "input data: map dim: ~a, start: ~a, end: ~a\n"
              (array-dimensions map-array) start end)
      (let ((sol1 (node-key (dijkstra map-array start
                                      sol1-step-ok? sol1-goal-test)))
            (sol2 (node-key (dijkstra map-array end
                                      sol2-step-ok? sol2-goal-test))))
        (when (null? (cdr args))
          (unless (equal? sol1 517) (error "Wrong solution sol1!"))
          (unless (equal? sol2 512) (error "Wrong solution sol2!")))
        (format #t "Solution 1: ~a\n" sol1)
        (format #t "Solution 2: ~a\n" sol2)))))
