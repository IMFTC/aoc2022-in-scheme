#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 format)
             (ice-9 match)
             (srfi srfi-1)
             (ice-9 textual-ports))

(define input "input10.txt")

(define (get-signal-strength-sum file start-cycle interval)
  (call-with-input-file file
    (lambda (port)
      (let loop ((cycle 1)
                 (x 1)
                 (sum 0)
                 (line (get-line port)))
        (cond ((eof-object? line)
               sum)
              (else
               ;; (format #t "[~4'0,d] x: ~4,d strength: ~5,d ~a\n" cycle x (* cycle x) line)
               (let ((sum-cycle (= 0 (floor-remainder (- cycle start-cycle) interval))))
                 (match (string-split line #\space)
                   (("noop")
                    (loop (1+ cycle)
                          x
                          (+ sum (if sum-cycle (* cycle x) 0))
                          (get-line port)))
                   (("addx" n)
                    (loop (1+ cycle)
                          x
                          (+ sum (if sum-cycle (* cycle x) 0))
                          ;; call a dummy command "addx2" that represents
                          ;; the 2nd cycle of the addx command
                          (string-join `("addx2" ,n))))
                   (("addx2" n)              ; dummy instruction
                    (loop (1+ cycle)
                          (+ x (string->number n))
                          (+ sum (if sum-cycle (* cycle x) 0))
                          (get-line port)))))))))))

(define* (draw-screen file)
  (call-with-input-file file
    (lambda (port)
      (let ((screen-width 40))
        (let loop ((cycle 1)
                   (x 1)                ; register value = pos of sprite center
                   (line (get-line port)))
          (unless (eof-object? line)
            (let* ((crt-xpos (floor-remainder (1- cycle) screen-width))
                   (draw (<= (abs (- crt-xpos x)) 1)))
              (format #t "~a" (if draw "#" " "))
              (if (= crt-xpos (1- screen-width))
                  (format #t "\n")))
            (match (string-split line #\space)
              (("noop")
               (loop (1+ cycle)
                     x
                     (get-line port)))
              (("addx" n)
               (loop (1+ cycle)
                     x
                     (string-join `("addx2" ,n))))
              (("addx2" n)              ; dummy instruction
               (loop (1+ cycle)
                     (+ x (string->number n))
                     (get-line port))))))))))

(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 (get-signal-strength-sum input-file 20 40))
         (sol2 (with-output-to-string (lambda () (draw-screen input-file)))))
    (when (null? (cdr args))
      (unless (= sol1 14540) (error "Wrong solution sol1!"))
      (unless (equal? sol2 "\
#### #  # #### #### #### #  #  ##  #### \n\
#    #  #    # #       # #  # #  #    # \n\
###  ####   #  ###    #  #### #      #  \n\
#    #  #  #   #     #   #  # #     #   \n\
#    #  # #    #    #    #  # #  # #    \n\
#### #  # #### #    #### #  #  ##  #### \n")
        (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2:\n~a\n" sol2)))
