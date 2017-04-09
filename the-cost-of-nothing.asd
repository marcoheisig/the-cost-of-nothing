(defsystem :the-cost-of-nothing
  :description "Determine the cost of things in Common Lisp."
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
