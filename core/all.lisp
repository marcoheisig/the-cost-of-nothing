;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/core/all
  (:nicknames :the-cost-of-nothing :the-cost-of-nothing/core)
  (:use-reexport
   :the-cost-of-nothing/core/macros
   :the-cost-of-nothing/core/measure-execution-time
   :the-cost-of-nothing/core/print-benchmark-results
   :the-cost-of-nothing/core/utilities))
