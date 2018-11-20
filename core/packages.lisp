(cl:in-package #:common-lisp-user)

(defpackage #:the-cost-of-nothing
  (:use #:closer-common-lisp)
  (:export

   ;; Utilities
   #:write-si-unit
   #:print-time

   ;; Benchmarking
   #:touch
   #:bench
   #:benchmark
   #:benchmark-thunk

   ;; Monitoring
   #:measurement
   #:measurement-value
   #:measurement-name
   #:measurement-context
   #:measurement-timestamp
   #:make-measurement
   #:measurementp
   #:monitor
   #:with-monitoring-region
   #:monitoring-region-start
   #:monitoring-region-end

   ;; Predefined Benchmarks
   #:print-report
   #:funcall-cost
   #:cons-cost
   #:gc-cost
   #:make-list-cost
   #:make-sequence-cost
   #:flops))
