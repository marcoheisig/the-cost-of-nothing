;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(cl:in-package #:common-lisp-user)

(defpackage #:the-cost-of-nothing
  (:use #:closer-common-lisp)
  (:export

   ;; Benchmarking
   #:touch
   #:bench
   #:benchmark
   #:benchmark-thunk
   #:print-time

   ;; Monitoring
   #:measurement
   #:measurement-value
   #:measurement-name
   #:measurement-context
   #:measurement-timestamp
   #:make-measurement
   #:monitor
   #:measure
   #:with-monitoring-region
   #:call-with-monitoring-region
   #:monitoring-region-start
   #:monitoring-region-end

   ;; Predefined Benchmarks
   #:report
   #:flops
   #:cons-cost
   #:make-list-cost
   #:make-sequence-cost
   #:funcall-cost))
