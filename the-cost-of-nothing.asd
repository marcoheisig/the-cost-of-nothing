(defsystem :the-cost-of-nothing
  :description "Determine the cost of things in Common Lisp."
  :long-description
  "This library provides portable and sophisticated benchmark functions. It
comes bundled with an extensive test suite that describes the performance
of the currently used Lisp implementation, e.g. with respect to garbage
collection, sequence traversal, CLOS and floating-point performance."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "GPLv3"
  :depends-on (:alexandria :trivial-garbage :closer-mop)
  :perform (test-op :after (o s)
                    (symbol-call '#:the-cost-of-nothing
                                 '#:enlighten-me!))
  :components
  ((:file "package")
   (:file "utilities"           :depends-on ("package"))
   (:file "the-cost-of-nothing" :depends-on ("utilities"))
   (:static-file "COPYING")))
