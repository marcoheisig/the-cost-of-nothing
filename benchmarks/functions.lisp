(in-package :the-cost-of-nothing)

(define-memo-function funcall-cost
    (&key (mandatory-arguments 0)
          (optional-arguments 0)
          (keyword-arguments 0)
          (rest-arguments 0))
  (let ((lambda-list (make-lambda-list :mandatory-arguments mandatory-arguments
                                       :optional-arguments optional-arguments
                                       :keyword-arguments keyword-arguments
                                       :rest-arguments rest-arguments))
        (argument-list (make-argument-list :mandatory-arguments mandatory-arguments
                                           :optional-arguments optional-arguments
                                           :keyword-arguments keyword-arguments
                                           :rest-arguments rest-arguments)))
    (let ((caller (compiled-lambda
                   `(callee)
                   `(funcall callee ,@argument-list)))
          (callee (compiled-lambda
                   lambda-list
                   `(declare (ignore ,@(lambda-list-bindings lambda-list))))))
      (benchmark
       (funcall caller callee)))))

(defun print-functions-report (&optional (stream *standard-output*))
  (format stream "~&~%== Functions ==~%")
  (loop for n below 7 do
    (format stream "FUNCALL with ~R mandatory argument~:p: " n)
    (finish-output stream)
    (print-time (funcall-cost :mandatory-arguments n) stream)
    (terpri stream)
    (finish-output stream))
  (loop for n below 7 do
    (format stream "FUNCALL with ~R optional argument~:p: " n)
    (finish-output stream)
    (print-time (funcall-cost :optional-arguments n) stream)
    (terpri stream)
    (finish-output stream))
  (loop for n below 7 do
    (format stream "FUNCALL with ~R keyword argument~:p: " n)
    (finish-output stream)
    (print-time (funcall-cost :keyword-arguments n) stream)
    (terpri stream)
    (finish-output stream))
  (loop for n below 7 do
    (format stream "FUNCALL with ~R rest argument~:p: " n)
    (finish-output stream)
    (print-time (funcall-cost :rest-arguments n) stream)
    (terpri stream)
    (finish-output stream)))
