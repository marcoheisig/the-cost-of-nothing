;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun runtime (f)
  (do ((invocations 1 (* 2 invocations)))
      ((> invocations (expt 2 30)) 0.0d0)
    (let ((time (measure-runtime f invocations)))
      (when (> time 0.1)
        (return-from runtime (/ time invocations))))))

(defun measure-runtime (function &rest arguments)
  (gc)
  (let ((time (get-internal-real-time))
        result)
    (unwind-protect (apply function arguments)
      (setf result (coerce (/ (- (get-internal-real-time) time)
                              internal-time-units-per-second)
                           'double-float)))
    result))

(defun print-time (time)
  (cond
    ((< time 1.d-7)
     (format t "> ~,2F nanoseconds~%" (* time 1.d9)))
    ((< time 1.d-4)
     (format t "> ~,2F microseconds~%" (* time 1.d6)))
    ((< time 1.d-1)
     (format t "> ~,2F milliseconds~%" (* time 1.d3)))
    (t
     (format t "> ~,2F seconds~%" time))))
