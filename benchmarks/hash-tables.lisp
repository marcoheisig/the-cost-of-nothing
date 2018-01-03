;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/hash-tables
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/hash-tables)

(declaim (notinline benchmark-hash-table))
(defun benchmark-hash-table (test keys)
  (let ((table (make-hash-table :test test))
        (keys (shuffle (apply #'vector keys))))
    (declare (type (simple-array t (*)) keys))
    (loop for key across keys do
      (setf (gethash key table) nil))
    (/ (nested-benchmark
         (touch keys)
         (loop for key across keys do
           (benchmark (gethash key table))))
       (length keys))))

(define-benchmark hash-tables (:hash-tables)
  (let ((eq (benchmark-hash-table #'eq (iota 40)))
        (eql (benchmark-hash-table #'eql (iota 40)))
        (equal-0 (benchmark-hash-table #'equal (iota 40)))
        (equal-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (benchmark-hash-table #'equal keys)))
        (equalp-0 (benchmark-hash-table #'equalp (iota 40)))
        (equalp-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (benchmark-hash-table #'equalp keys))))
    (lambda (stream)
      (format stream "Cost of a single hash table lookup:~%")

      (format stream "EQ hash table: ~A~%"
              (quantity-string eq "seconds"))

      (format stream "EQL hash table: ~A~%"
              (quantity-string eql "seconds"))

      (multiple-value-bind (y-intersection slope)
          (y-intersection-and-slope 0 equal-0 100 equal-100)
        (format stream "EQUAL hash table: ~A and ~A per cons.~%"
                (quantity-string y-intersection "seconds")
                (quantity-string slope "seconds")))

      (multiple-value-bind (y-intersection slope)
          (y-intersection-and-slope 0 equalp-0 100 equalp-100)
        (format stream "EQUALP hash table: ~A and ~A per cons.~%"
                (quantity-string y-intersection "seconds")
                (quantity-string slope "seconds"))))))
