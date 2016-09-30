;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun benchmark (f-plus-overhead &optional (overhead #'identity))
  "The function F-PLUS-OVERHEAD must invoke a certain operation N times for
any given integer N. The function OVERHEAD should execute the same code,
but without this operation. BENCHMARK returns a double-float describing the
average duration of one such operation in seconds."
  (gc) ; expensive, but crucial for reasonable results
  (let* ((overhead (%benchmark overhead))
         (time (%benchmark f-plus-overhead)))
    (max (- time overhead) 0.0d0)))

(defparameter *minimum-sampling-time*
  (* 40 (/ internal-time-units-per-second)))

(defun %benchmark (f)
  (let ((invocations 1)
        (time 0.0d0))
    (loop
      (setf time (funcall-time f invocations))

      ;; stopping criterium #1: enough sampling time
      (when (> time *minimum-sampling-time*)
        (return (/ time invocations)))

      ;; increase the number of invocations
      (setf invocations
            (if (zerop time)
                (* invocations 2)
                (* invocations
                   (ceiling (* 1.1 *minimum-sampling-time*)
                            time))))

      ;; stopping criterium #2: insanely many invocations
      (when (> invocations most-positive-fixnum)
        (return 0.0d0)))))

(defun funcall-time (function &rest arguments)
  "Naively measure the time it takes to invoke FUNCTION with the given
ARGUMENTS. Returns the run time in seconds as a double-float."
  (let ((time (get-internal-real-time)))
    (apply function arguments)
    (coerce (/ (- (get-internal-real-time) time)
               internal-time-units-per-second)
            'double-float)))

(defmacro run-time (form)
  (with-gensyms (invocations i)
    `(benchmark
      (lambda (,invocations)
        (dotimes (,i ,invocations)
          (touch ,form)))
      (lambda (,invocations)
        (dotimes (,i ,invocations)
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
  "Essentially equivalent to IDENTITY, but protects OBJECT from compiler
  optimizations."
  object)
