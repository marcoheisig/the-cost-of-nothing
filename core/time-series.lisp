(in-package :the-cost-of-nothing)

(defclass time-series ()
  ((%measurements :initform '() :accessor measurements)))

