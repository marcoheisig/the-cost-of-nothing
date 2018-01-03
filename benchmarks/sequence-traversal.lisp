;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/sequence-traversal
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/sequence-traversal)

(define-benchmark sequence-traversal (:sequences)
  (let ((report-fns
          (list
           (list-and-count)
           (list-and-loop)
           (generic-list-and-generic-count))))
    (lambda (stream)
      (format stream "Counting the number of NIL entries in a sequence.~%")
      (loop for report-fn in report-fns do
        (funcall report-fn stream)))))

(defvar n 40 "Length of sequences to benchmark.")

(defun sequence-traversal-report-fn (description x0 t0 x1 t1)
  (multiple-value-bind (y-intersection slope)
      (y-intersection-and-slope x0 t0 x1 t1)
    (lambda (stream)
      (format stream "~A: ~A plus ~A per element.~%"
              description
              (quantity-string y-intersection "seconds")
              (quantity-string slope "seconds")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Built-in Lists

(defun list-and-loop ()
  (let ((list-1 (make-list 1 :initial-element 42))
        (list-n (make-list n :initial-element 42)))
    (declare (list list-1 list-n))
    (sequence-traversal-report-fn
     "Built-in lists, CL:LOOP"
     1 (nested-benchmark
         (touch list-1)
         (benchmark
           (loop for elt in list-1 counting (null elt))))
     n (nested-benchmark
         (touch list-n)
         (benchmark
           (loop for elt in list-n counting (null elt)))))))

(defun list-and-count ()
  (let ((list-1 (make-list 1 :initial-element 42))
        (list-n (make-list n :initial-element 42)))
    (declare (list list-1 list-n))
    (sequence-traversal-report-fn
     "Built-in lists, CL:COUNT"
     1 (nested-benchmark
         (touch list-1)
         (benchmark (count nil list-1)))
     n (nested-benchmark
         (touch list-n)
         (benchmark (count nil list-n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lists of Structs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS Lists

(defgeneric generic-car+cdr (generic-list))

(defgeneric generic-nil-count (sequence))

(defclass generic-list () ())

(defclass generic-cons (generic-list)
  ((%car :initarg :car :accessor generic-car)
   (%cdr :initarg :cdr :accessor generic-cdr)))

(defclass generic-null (generic-list) ())

(defmethod generic-nil-count ((generic-null generic-null)) 0)

(defmethod generic-car+cdr ((generic-null generic-null))
  (values generic-null generic-null))

(defmethod generic-car+cdr ((generic-cons generic-cons))
  (values (slot-value generic-cons '%car)
          (slot-value generic-cons '%cdr)))

(defun make-generic-list (length &key initial-element)
  (let ((result (make-instance 'generic-null)))
    (loop repeat length do
      (setf result
            (make-instance 'generic-cons
              :car initial-element
              :cdr result)))
    result))

(defmethod generic-nil-count ((generic-cons generic-cons))
  ;; Two CLOS optimizations are used here:
  ;; 1. CAR+CDR are accessed using a single generic function, cutting the
  ;;    dispatch overhead by 50%
  ;; 2. The protocol guarantees that the CDR of an instance of GENERIC-NULL
  ;;    is EQ to it. Furthermore, since the argument to GENERIC-NIL-COUNT
  ;;    must be a proper list, the termination test can be done using EQ.
  (loop with car = nil and cdr = nil
        for current = generic-cons then cdr
        do (multiple-value-setq (car cdr)
             (generic-car+cdr current))
        until (eq cdr current)
        count (null car)))

(defun generic-list-and-generic-count ()
  (let ((list-1 (make-generic-list 1 :initial-element 42))
        (list-n (make-generic-list n :initial-element 42)))
    (sequence-traversal-report-fn
     "CLOS conses, generic car+cdr"
     1 (nested-benchmark
         (touch list-1)
         (benchmark (generic-nil-count list-1)))
     50 (nested-benchmark
          (touch list-n)
          (benchmark (generic-nil-count list-n))))))
