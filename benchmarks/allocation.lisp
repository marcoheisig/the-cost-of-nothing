;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/allocation
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/allocation)

(defmacro n-slot-struct (name n)
  `(defstruct ,name
     ,@(loop for i below n
             collect `(,(gensym) 42 :type fixnum))))

(defclass A () ())
(defclass B (A) ())
(defclass C (A) ())
(defclass D (B C) ())

(defmacro n-slot-class (name n)
  `(defclass ,name (D)
     ,(loop for i below n
            collect `(,(gensym) :type fixnum :initform 42))))

(n-slot-struct  0-slot-struct  0)
(n-slot-struct 50-slot-struct 50)

(n-slot-class  0-slot-class  0)
(n-slot-class 50-slot-class 50)

(define-benchmark allocation (:allocation :consing)
  (let ((list-1-time
          (benchmark (list nil)))
        (list-2000-time
          (benchmark (make-list 2000 :initial-element nil)))
        (simple-vector-1-time
          (benchmark (make-array 1)))
        (simple-vector-2000-time
          (benchmark (make-array 2000)))
        (ub8-array-1-time
          (benchmark (make-array 1 :element-type '(unsigned-byte 8))))
        (ub8-array-2000-time
          (benchmark (make-array 2000 :element-type '(unsigned-byte 8))))
        (0-slot-struct-time
          (benchmark (make-0-slot-struct)))
        (50-slot-struct-time
          (benchmark (make-50-slot-struct)))
        (0-slot-class-time
          (benchmark (make-instance '0-slot-class)))
        (50-slot-class-time
          (benchmark (make-instance '50-slot-class))))
    (lambda (stream)
      (flet ((report (x0 y0 x1 y1)
               (multiple-value-bind (y-intersection slope)
                   (y-intersection-and-slope x0 y0 x1 y1)
                 (format stream "~A plus ~A per slot/element.~%"
                         (quantity-string y-intersection "seconds")
                         (quantity-string slope "seconds")))))
        (format stream "Allocating lists: ")
        (report 1 list-1-time 2000 list-2000-time)
        (format stream "Allocating simple-vectors: ")
        (report 1 simple-vector-1-time 2000 simple-vector-2000-time)
        (format stream "Allocating (unsigned-byte 8) vectors: ")
        (report 1 ub8-array-1-time 2000 ub8-array-2000-time)
        (format stream "Allocating structs: ")
        (report 1 0-slot-struct-time 50 50-slot-struct-time)
        (format stream "Allocating classes: ")
        (report 1 0-slot-class-time 50 50-slot-class-time)))))
