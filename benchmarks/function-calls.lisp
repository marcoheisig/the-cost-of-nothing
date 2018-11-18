;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(fmemo:define-memo-function funcall-cost
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
