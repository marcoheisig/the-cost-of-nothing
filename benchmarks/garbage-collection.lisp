;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/garbage-collection
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core :trivial-garbage))

(in-package :the-cost-of-nothing/benchmarks/garbage-collection)

(define-benchmark garbage-collection (:gc)
  (let ((gc-time
          (nested-benchmark
            (touch (make-list 10))
            (benchmark (gc))))
        (full-gc-time
          (nested-benchmark
            (touch (make-list 10))
            (benchmark (gc :full t)))))
    (lambda (stream)
      (format stream "~A for (GC), ~A for (GC :FULL T).~%"
              (quantity-string gc-time "seconds")
              (quantity-string full-gc-time "seconds")))))

