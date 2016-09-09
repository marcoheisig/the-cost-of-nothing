(in-package :asdf-user)

(defsystem :the-cost-of-nothing
  :description "Determine the cost of things in Common Lisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "GPLv3"
  :depends-on (:alexandria :trivial-garbage)
  :perform (test-op :after (o s)
                    (symbol-call '#:the-cost-of-nothing
                                 '#:enlighten-me!))
  :components
  ((:file "package")
   (:file "utilities" :depends-on ("package"))
   (:file "enlighten-me!" :depends-on ("utilities"))
   (:static-file "COPYING")))
