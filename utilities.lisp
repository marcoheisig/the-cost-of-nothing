;;; © 2016-2017 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun as-time (time)
  (cond
    ((< time 1.d-6)
     (format nil "~,2F nanoseconds" (* time 1.d9)))
    ((< time 1.d-3)
     (format nil "~,2F microseconds" (* time 1.d6)))
    ((< time 1.d-0)
     (format nil "~,2F milliseconds" (* time 1.d3)))
    (t
     (format nil "~,2F seconds" time))))

(defun as-flops (flops)
  (cond
    ((> flops 1.d11)
     (format nil "~,2F terraFLOPS" (/ flops 1.d12)))
    ((> flops 1.d8)
     (format nil "~,2F gigaFLOPS" (/ flops 1.d9)))
    ((> flops 1.d5)
     (format nil "~,2F megaFLOPS" (/ flops 1.d6)))
    ((> flops 1.d2)
     (format nil "~,2F kiloFLOPS" (/ flops 1.d3)))
    (t
     (format nil "~,2F FLOPS" flops))))

(declaim (notinline touch))
(defun touch (object)
  "Protect OBJECT from compiler optimization."
  (declare (ignore object))
  (values))
