;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defclass time-series ()
  ((%measurements :initform '() :accessor measurements)))

