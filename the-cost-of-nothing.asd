(defsystem "the-cost-of-nothing"
  :description "Determine the cost of things in Common Lisp."
  :long-description
  "This library provides portable and sophisticated benchmark functions. It
comes bundled with an extensive test suite that describes the performance
of the currently used Lisp implementation, e.g. with respect to garbage
collection, sequence traversal, CLOS and floating-point performance."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "GPLv3"
  :class :package-inferred-system
  :depends-on ("the-cost-of-nothing/core/all"
               "the-cost-of-nothing/benchmarks/all")
  :perform
  (test-op (o c) (uiop:symbol-call "THE-COST-OF-NOTHING" "SHOW-BENCHMARK-RESULTS!")))

(register-system-packages "the-cost-of-nothing/benchmarks/all" '(:the-cost-of-nothing/benchmarks))
(register-system-packages "the-cost-of-nothing/core/all" '(:the-cost-of-nothing/core))
(register-system-packages "closer-mop" '(:closer-common-lisp))
