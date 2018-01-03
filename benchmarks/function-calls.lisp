;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/function-calls
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/function-calls)

(define-benchmark function-calls (:funcall)
  (lambda (stream)
    (format stream "TODO~%")))
