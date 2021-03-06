(in-package :the-cost-of-nothing)

(define-memo-function flops (&key (element-type 'single-float))
  (let* ((length 100)
         (initial-element (coerce 0 element-type))
         (a1 (make-array
              length
              :element-type element-type
              :initial-element initial-element))
         (a2 (make-array
              length
              :element-type element-type
              :initial-element initial-element))
         (crunch
           (compiled-lambda
            `(length dst src-1 src-2)
            `(declare (type (simple-array ,element-type (*)) dst src-1 src-2))
            `(declare (optimize (speed 3) (safety 0) (debug 0)))
            `(loop for index fixnum below length do
              (setf (aref dst index)
                    (+ (* (aref src-1 index) 1/2)
                       (* (aref src-2 index) 1/4)))))))
    (/ (* 3 length)
       (benchmark (funcall crunch length a1 a2 a2)))))

(defun print-numerics-report (&optional (stream *standard-output*))
  (format stream "~&~%== Numerics ==~%")
  (format stream "Flops (single-float): ") (finish-output stream)
  (write-si-unit (flops :element-type 'single-float) "flops" stream)
  (terpri stream)
  (finish-output stream)
  (format stream "Flops (double-float): ") (finish-output stream)
  (write-si-unit (flops :element-type 'double-float) "flops" stream)
  (terpri stream)
  (finish-output stream))
