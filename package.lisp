;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :the-cost-of-nothing
  (:use :closer-common-lisp :alexandria :trivial-garbage)
  (:export
   #:as-time
   #:as-flops
   #:enlighten-me!
   #:bench
   #:nbench
   #:benchmark
   #:nested-benchmark
   #:measure-execution-time-of-thunk
   #:measure-execution-time))
