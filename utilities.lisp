;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun benchmark (f+overhead &optional (overhead #'identity))
  (gc) ; expensive, but crucial for reasonable results
  (let* ((before (%benchmark overhead))
         (time (%benchmark f+overhead))
         (after (%benchmark overhead)))
    (- time (* (+ before after) 0.5d0))))

(defun %benchmark (f)
  (do ((invocations 1 (* 2 invocations)))
      ((> invocations (expt 2 30)) 0.0d0)
    (let ((time (funcall-time f invocations)))
      (when (> time (* 20 (/ internal-time-units-per-second)))
        (return-from %benchmark
          (/ time invocations))))))

(defun funcall-time (function &rest arguments)
  (let ((time (get-internal-real-time))
        result)
    (unwind-protect (apply function arguments)
      (setf result (coerce (/ (- (get-internal-real-time) time)
                              internal-time-units-per-second)
                           'double-float)))
    result))

(defmacro run-time (form)
  (with-gensyms (invocations i)
    `(benchmark
      (lambda (,invocations)
        (dotimes (,i ,invocations)
          (touch ,form)))
      (lambda (invocations)
        (dotimes (i invocations)
          (touch nil))))))

(defun time-string (time)
  (cond
    ((< time 1.d-6)
     (format nil "~,2F nanoseconds" (* time 1.d9)))
    ((< time 1.d-3)
     (format nil "~,2F microseconds" (* time 1.d6)))
    ((< time 1.d-0)
     (format nil "~,2F milliseconds" (* time 1.d3)))
    (t
     (format nil "~,2F seconds" time))))

(defun flops-string (flops)
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
  "Essentially equivalent to IDENTITY, but prevents OBJECT from being
  allocated on the stack of the caller."
  object)
