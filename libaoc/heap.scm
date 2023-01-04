#!/usr/local/bin/guile \
-e main -s
!#

(define-module (libaoc heap)
  #:use-module (srfi srfi-9)
  #:export (make-heap
            heap?
            heap-insert!
            heap-extract-top!
            heap-raise-node!
            heap-node
            heap-size
            heap-max-size
            heap-empty?

            <node>
            make-node
            node?
            node-data
            node-key
            node-idx))

(define-record-type <heap>
  (%make-heap vec
              size
              max-size
              in-order?)
  heap?
  (vec heap-vec)
  (size heap-size set-heap-size!)
  (max-size heap-max-size)
  (in-order? heap-order-porc))

(define-record-type <node>
  (%make-node data key idx)
  node?
  (data node-data)
  (key node-key set-node-key!)
  (idx node-idx set-node-idx!))

(define* (make-heap max-size #:optional (in-order? <))
  "Create a new heap that can hold up to MAX-SIZE <nodes>. The optional
IN-ORDER? argument must be a procedure taking two arguments, which,
when called on two <node>s as (IN-ORDER? node1 node2) returns #t if
node1 has a higher priority than node2 and #f otherwise. If IN-ORDER?
is not given, it defaults to ‘<’, meaning the heap will be a min
heap.

The only data type that should be added to the heap is ‘<node>’, which
is created with ‘make-node’."
  (%make-heap (make-vector max-size #f)
              0
              max-size
              in-order?))

(define (make-node data key)
  "Create new <node> for insertion into a <heap> created with
‘make-heap’ DATA is the data which will not be changed by heap
operation and can be retrieved from a node with ‘node-data’.  The KEY
value will be used for ordering."
  (%make-node data key #f))

(define (heap-insert! h node)
  (let ((size (heap-size h)))
    (when (>= size (heap-max-size h))
      (error (format #f "heap-insert! failed: heap full (max-size: ~a)"
                     (heap-max-size h))))
    ;; add the new node to the first free space
    (vector-set! (heap-vec h) size node)
    (set-node-idx! node size)
    (set-heap-size! h (1+ size))
    ;; bubble up key until the heap property is restored
    (heap-try-raise! (heap-vec h) size node (heap-order-porc h))))

(define (heap-node heap idx)
  "Return the node stored in heap HEAP at index IDX."
  (if (< idx (heap-size heap))
      (vector-ref (heap-vec heap) idx)
      (error (format #f "index ~a outside valid indices. Current heap size is ~a."
                     idx (heap-size heap)))))

(define (heap-raise-node! heap node new-key)
  "Set the key of NODE to NEW-KEY and rise the node as far as necessary
to restore the heap property. Raise an error if the NEW-KEY is not
'better' than the old key."
  (let ((vec (heap-vec heap))
        (idx (node-idx node))
        (in-order? (heap-order-porc heap)))
    (cond ((in-order? new-key (node-key node))
           (set-node-key! node new-key)
           (heap-try-raise! vec idx node in-order?)
           (format #t "raising key from ~a to ~a\n" (node-key node) new-key))
          (else
           (error (format #f "Can't raise node with old key ~a to new key ~a (compare proc: ~a)"
                          (node-key node) new-key in-order?))))))

(define (heap-try-raise! vec idx node in-order?)
  (unless (zero? idx)
    (let* ((parent-idx (floor (/ (1- idx) 2)))
           (parent-node (vector-ref vec parent-idx)))
      (cond ((in-order? (node-key node) (node-key parent-node))
             (vector-set! vec parent-idx node)
             (vector-set! vec idx parent-node)
             (set-node-idx! node parent-idx)
             (set-node-idx! parent-node idx)
             (heap-try-raise! vec parent-idx node in-order?))))))

(define (heap-extract-top! h)
  "Remove and return the top key from HEAP."
  (if (< (heap-size h) 1)
      (error "empty heap!"))
  (let* ((vec (heap-vec h))
         (size (heap-size h))
         (tmp-top-node (vector-ref vec 0)))
    ;; the last element in the vector replaces the first
    (vector-set! vec 0 (vector-ref vec (1- size)))
    (set-heap-size! h (1- size))
    (heapify! vec 0 (1- size) (heap-order-porc h))
    ;; set idx to #f to indicate the node is no longer in the heap
    (set-node-idx! tmp-top-node #f)
    tmp-top-node))

(define (heap-peek-top h)
  (if (< (heap-size h) 1)
      (error "empty heap!")
      (vector-ref (heap-vec h) 0)))

(define (heap-empty? h)
  (> (heap-size h) 0))

(define (heapify! vec idx size in-order?)
  ;; recursively pushes the node at idx down the tree as far as
  ;; necessary to restore the heap property
  (let* ((node (vector-ref vec idx))
         (left-child-idx (+ 1 (* 2 idx)))
         (right-child-idx (+ 2 (* 2 idx)))
         ;; swap-idx: index of the node that should move to the idx position
         (swap-idx (if (and (< left-child-idx size)
                            (in-order? (node-key (vector-ref vec left-child-idx))
                                       (node-key node)))
                       left-child-idx
                       idx))
         (swap-idx (if (and (< right-child-idx size)
                            (in-order? (node-key (vector-ref vec right-child-idx))
                                       (node-key (vector-ref vec swap-idx))))
                       right-child-idx
                       swap-idx)))
    (unless (= swap-idx idx)
      ;; swap keys at idx and swap-idx (left or right child)
      (vector-set! vec idx (vector-ref vec swap-idx))
      (vector-set! vec swap-idx node)
      (set-node-idx! node swap-idx)
      (set-node-idx! (vector-ref vec idx) idx)
      (heapify! vec swap-idx size in-order?))))

(define (heap-clear! h)
  "Clear the heap. Note that this only sets SIZE to 0, the content of
vec stays around and will be overwritten as the heap gets filled
again."
  (set-heap-size! h 0))
