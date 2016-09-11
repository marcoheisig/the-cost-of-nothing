;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun runtime (f)
  (do ((invocations 1 (* 2 invocations)))
      ((> invocations (expt 2 30)) 0.0d0)
    (let ((time (funcall-time f invocations)))
      (when (> time 0.1)
        (return-from runtime (/ time invocations))))))

(defun funcall-time (function &rest arguments)
  (gc) ; expensive, but crucial for reasonable results
  (let ((time (get-internal-real-time))
        result)
    (unwind-protect (apply function arguments)
      (setf result (coerce (/ (- (get-internal-real-time) time)
                              internal-time-units-per-second)
                           'double-float)))
    result))

(defun print-time (time &optional (stream *standard-output*))
  (cond
    ((< time 1.d-6)
     (format stream "~,2F nanoseconds" (* time 1.d9)))
    ((< time 1.d-3)
     (format stream "~,2F microseconds" (* time 1.d6)))
    ((< time 1.d-0)
     (format stream "~,2F milliseconds" (* time 1.d3)))
    (t
     (format stream "~,2F seconds" time))))

(defmacro unroll ((var n &optional (unroll 4)) &body body)
  "Similar to DOTIMES."
  (once-only (n)
    (with-gensyms (remainder i)
      `(let ((,remainder (rem ,n ,unroll)))
         (do ((,i 0 (+ ,i ,unroll)))
             ((>= ,i (- ,n ,remainder)))
           (declare (type fixnum ,i))
           ,@(loop for offset below unroll
                   collect
                   `(let ((,var (+ ,i ,offset)))
                      ,@body)))
         (do ((,var (- ,n ,remainder) (+ ,var 1)))
             ((>= ,var ,n))
           ,@body)))))

(declaim (notinline touch))
(defun touch (object)
  "Essentially equivalent to IDENTITY, but prevents OBJECT from being
  allocated on the stack of the caller."
  object)
