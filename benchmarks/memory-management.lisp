(in-package :the-cost-of-nothing)

(define-memo-function cons-cost ()
  (benchmark
   (cons nil nil)))

(define-memo-function make-list-cost (size &key (initial-element nil))
  (benchmark
   (make-list size :initial-element initial-element)))

(define-memo-function make-sequence-cost (result-type length &key (initial-element nil))
  (let ((fast-make-sequence
          (compiled-lambda
           `(length initial-element)
           `(make-sequence ',result-type length :initial-element initial-element))))
    (benchmark
     (funcall fast-make-sequence length initial-element))))

(define-memo-function make-struct-cost (&key (number-of-slots 0))
  ;; TODO
  (values))

(define-memo-function make-instance-cost (&key (number-of-slots 0))
  ;; TODO
  (values)
  )

(define-memo-function gc-cost (&key full)
  (benchmark
   (trivial-garbage:gc :full full)))

(defun print-memory-management-report (&optional (stream *standard-output*))
  (format stream "~&~%== Memory Management ==~%")
  (format stream "Cost of allocating a cons cell: ") (finish-output stream)
  (print-time (cons-cost) stream) (terpri stream) (finish-output stream)
  (format stream "Cost of garbage collection: ") (finish-output stream)
  (print-time (gc-cost) stream) (terpri stream) (finish-output stream)
  (format stream "Cost of full garbage collection: ") (finish-output stream)
  (print-time (gc-cost :full t) stream) (terpri stream) (finish-output stream))
