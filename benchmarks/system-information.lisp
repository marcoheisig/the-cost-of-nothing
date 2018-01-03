;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/system-information
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/system-information)

(define-benchmark system-information (:machine :host)
  (lambda (stream)
    (format stream "Implementation: ~:[unknown~;~:*~a~]~@[ ~a~]~%"
            (lisp-implementation-type)
            (lisp-implementation-version))
    (format stream "Machine: ~:[unknown~;~:*~a~]~@[ ~a~]~%"
            (machine-type)
            (machine-version))
    (format stream "Hostname: ~a~%" (machine-instance))))
