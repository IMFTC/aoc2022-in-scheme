#!/bin/guile3.0 \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9))             ;records

(define input "input.txt")

(define-record-type <file>
  (make-file name type size content parent)
  file?
  (name file-name)
  (type file-type)
  (size file-size set-file-size!)       ; used for directories
  (content file-content set-file-content!)  ; used for directories
  (parent file-parent))

(define (get-canonical-path file)
  (let loop ((f file)
             (path '()))
    (cond ((equal? (file-name f) "/")
           (string-append (string-join path "/" 'prefix)
                          (if (eqv? (file-type file) 'directory) "/" "")))
          ((loop (file-parent f) (cons (file-name f) path))))))

(define (parse-file-system-tree file)
  "Returns a record of type <file> with the file-name \"root\" with
file-content populated recursively as parsed from the shell output in
FILE."
  (define root (make-file "/" 'directory #f '? #f))
  (define (parse-dir-content! dir port)
    "Parse the content of DIR (<file> type) from PORT and set the content
list of DIR. Returns #f of eof-object was read, otherwise #t is
returned and PORT is ready for PARSE-CD-COMMANDS."
    (let loop ((content '()))
      (let* ((offset (ftell port))
             (line (get-line port)))
        (cond ((eof-object? line)
               (set-file-content! dir content)
               #f)
              ((or (string-null? line)
                   (string-prefix? "$" line))
               (seek port offset SEEK_SET)
               (set-file-content! dir content)
               #t)
              (else
               (match (string-split line #\space)
                 (("dir" name)
                  (loop (cons (make-file name 'directory #f '? dir) content)))
                 ((size name)
                  (loop (cons (make-file name 'file (string->number size) #f dir) content)))))))))

  (define (parse-cd-commands start-dir port)
    "Returns the directory in which we end up, leaves PORT ready for
PARSE-DIR-CONTENT with the returned DIR."
    (let loop ((cwd start-dir))
      (let* ((offset (ftell port))
             (line (get-line port)))
        (match (string-split line #\space)
          (("$" "ls")
           cwd)
          (("$" "cd" "/") ; special case hack since start dir is unknown
           (loop root))
          (("$" "cd" "..")
           (loop (file-parent cwd)))
          (("$" "cd" target-dir)
           (loop (find (lambda (f) (equal? (file-name f) target-dir))
                       (file-content cwd))))))))
  (call-with-input-file file
    (lambda (port)
      (let loop ((dir (parse-cd-commands #f port)))
        (if (parse-dir-content! dir port)
            (loop (parse-cd-commands dir port))
            root)))))

(define (get-tree-in-post-order fs-root)
  "Return a list with all files and directories in post order."
  ;; This procedure is tail recursive (it uses a list as a stack).
  (define (walk todo-stack
                nodes-in-post-order)
    (cond ((null? todo-stack)
           ;; before returning calculate and set dir sizes
           (map
            (lambda (node)
              (if (eq? (file-type node) 'directory)
                  ;; Since the list is in post-order, it's
                  ;; guaranteed that the size of all contained dirs
                  ;; has already been already set.
                  (set-file-size! node (apply + (map file-size (file-content node)))))
              (format #t "~10a ~a\n" (file-size node) (get-canonical-path node))
              node)
            nodes-in-post-order))       ; return result
          (else
           (let ((node (car todo-stack)))
             (if (eq? (file-type node) 'directory)
                 (walk (append (file-content node) (cdr todo-stack))
                       (cons node nodes-in-post-order))
                 (walk (cdr todo-stack)
                       (cons node nodes-in-post-order)))))))
  (walk (list fs-root) '()))

(define (get-dirs-with-size minmax size tree-list)
  (filter-map
   (lambda (node)
     (match node
       (($ <file> fname ftype fsize fcontent fparent)
        (if (eq? ftype 'directory)
            (if ((case minmax ((max) <=) ((min) >=)) fsize size)
                node
                #f)
            #f))))
   tree-list))

;; A different input file can be specified as first command line argument
(define (main args)
  (let* ((input-file (if (null? (cdr args)) input (cadr args)))
         (root (parse-file-system-tree input-file))
         (tree-post-order (get-tree-in-post-order root))
         (sol1 (apply +
                (map (lambda (n) (file-size n))
                     (get-dirs-with-size
                      'max
                      100000
                      tree-post-order))))
         (sol2 (apply min
                      (map (lambda (n) (file-size n))
                           (get-dirs-with-size
                            'min
                            (- 30000000 (- 70000000 (file-size root)))
                            tree-post-order)))))

    (when (null? (cdr args))
      (or (= sol1 1583951) (error "Wrong solution sol1!"))
      (or (= sol2 214171) (error "Wrong solution sol2!")))
    (format #t "Solution 1: ~a\n" sol1)
    (format #t "Solution 2: ~a\n" sol2)))
