;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/all
  (:use
   :alexandria
   :closer-common-lisp
   :the-cost-of-nothing/benchmarks/system-information
   :the-cost-of-nothing/benchmarks/allocation
   :the-cost-of-nothing/benchmarks/function-calls
   :the-cost-of-nothing/benchmarks/garbage-collection
   :the-cost-of-nothing/benchmarks/sequence-traversal
   :the-cost-of-nothing/benchmarks/hash-tables))
