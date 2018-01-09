;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/numerics
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/numerics)

(defmacro with-unsafe-optimizations (&body body)
  `(locally (declare (optimize
                      (speed 3)
                      (safety 0)
                      (debug 0)
                      (compilation-speed 0)
                      (space 0)))
     ,@body))

(progn
  (defun crunch-single-floats (length dst src-1 src-2)
    (declare (type (simple-array single-float (*)) dst src-1 src-2))
    #1=
    (with-unsafe-optimizations
      (loop for index fixnum below length do
        (setf (aref dst index)
              (+ (* (aref src-1 index) 1/2)
                 (* (aref src-2 index) 1/4))))))
    (defun crunch-double-floats (length dst src-1 src-2)
      (declare (type (simple-array double-float (*)) dst src-1 src-2))
      #1#))

(defun flops (element-type &optional (length 50)
              &aux (initial-element (coerce 42 element-type)))
  (let ((a1 (make-array
             length
             :element-type element-type
             :initial-element initial-element))
        (a2 (make-array
             length
             :element-type element-type
             :initial-element initial-element)))
    (let ((flop/run (* 3 length))
          (time/run
            (etypecase initial-element
              (single-float
               (benchmark (crunch-single-floats length a1 a2 a2)))
              (double-float
               (benchmark (crunch-double-floats length a1 a2 a2))))))
      (/ flop/run time/run))))

(define-benchmark number-crunching (:flops :numerics)
  (let ((single-float-flops (flops 'single-float))
        (double-float-flops (flops 'double-float)))
    (lambda (stream)
      (format stream "Floating-point operations per second:~%")
      (format stream "Single-float: ~A~%"
              (quantity-string single-float-flops "Flops"))
      (format stream "Double-float: ~A~%"
              (quantity-string double-float-flops "Flops")))))
