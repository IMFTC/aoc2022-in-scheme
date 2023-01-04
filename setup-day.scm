#!/usr/local/bin/guile \
-e main -s
!#

(use-modules (ice-9 exceptions)
             (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 receive)
             (web client)
             (web http)
             (web response))

(define aoc-cookie-file "session-cookie")

(define (main args)
  (let ((arg1 (cdr args)))
    (when (null? arg1)
      (format (current-error-port) "Error: No day given.\n
Usage: ~a DAY\nDAY is a number 1..24."
              (car args))
      (exit 1))
    (let* ((day (string->number (car arg1)))
           (number-string (format #f "~2,'0d" day))
           (program-file (string-append "p" number-string ".scm"))
           (input-file (string-append "input" number-string ".txt")))
      (format #t "Creating program file '~a'\n" program-file)
      (call-with-output-file program-file
        (lambda (port)
          (put-string port (string-append
                            "#!/usr/local/bin/guile \\\n
-e main -s
!#

(use-modules (ice-9 format)
             (ice-9 textual-ports))

(define input \"input" number-string ".txt\")


(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (sol1 \"TODO\")
         (sol2 \"TODO\"))
    (when (null? (cdr args))
      (unless (equal? sol1 \"TODO\") (error \"Wrong solution sol1!\"))
      (unless (equal? sol2 \"TODO\") (error \"Wrong solution sol2!\")))
    (format #t \"Solution 1: ~a\\n\" sol1)
    (format #t \"Solution 2: ~a\\n\" sol2)))
"))))
      (format #t "chmod 755 '~a'\n" program-file)
      (chmod program-file #o755)
      (let ((cookie-string (call-with-input-file aoc-cookie-file get-line)))
        (receive (response body)
            (http-get (string-append
                       "https://adventofcode.com/2022/day/"
                       (number->string day) "/input")
                      #:headers `((cookie . ,cookie-string)))
          (format #t "response: ~a\n" (response-code response))
          (format #t "Writing input data to '~a'\n" input-file)
          (call-with-output-file input-file
            (lambda (port)
              (put-string port body)
              (format #t "chmod 444 '~a'\n" input-file)
              (chmod port #o444))))))))
