;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/core/measure-execution-time
  (:use :alexandria :closer-common-lisp)
  (:export
   #:measure-execution-time-of-thunk
   #:measure-execution-time
   #:touch))

(in-package :the-cost-of-nothing/core/measure-execution-time)

(declaim (notinline touch))
(defun touch (object)
  "Protect OBJECT from compiler optimization."
  (declare (ignore object))
  (values))

(defun measure-execution-time-of-thunk (thunk)
  "Execute THUNK and return the execution time of THUNK in seconds as a
single-float."
  (let* ((t0 (get-internal-run-time))
         (_  (funcall thunk))
         (t1 (get-internal-run-time)))
    (declare (ignore _))
    (coerce (/ (- t1 t0) internal-time-units-per-second)
            'single-float)))

(defun measure-execution-time
    (fun &key
           (overhead #'identity)
           (timeout 2.0)
           (min-effective-samples 100)
           (min-sample-time 0.1)
           (invocation-growth 1.77))
  "The function FUN must invoke a certain operation N times for any given
integer N. The function OVERHEAD should execute the same code, but without
this operation.

Returns three values:
duration   - a single-float denoting the duration of the operation in seconds
confidence - a single-flot between 0.0 (garbage) and 1.0 (absolute confidence)
iterations - the number N of iterations used to determine the result"
  (declare (function fun overhead)
           (type (single-float (0.0)) timeout min-sample-time)
           (type (single-float (1.0)) invocation-growth)
           (type (integer 1) min-effective-samples))
  (loop
    ;; in each attempt, increase the number of iterations
    for iterations of-type unsigned-byte = 3
      then (floor (* iterations invocation-growth))
    ;; measure the cost of ITERATIONS runs of FUN
    for benchtime of-type single-float
      = (max 0.0 (measure-execution-time-of-thunk
                  (lambda ()
                    (funcall fun iterations))))
    ;; only the difference between the run time of FUN and the run time of
    ;; OVERHEAD counts as SAMPLE-TIME
    for sample-time of-type single-float
      = (max 0e0 (- benchtime
                    (measure-execution-time-of-thunk
                     (lambda ()
                       (funcall overhead iterations)))))
    ;; the measurement process involves both the MIN-SAMPLE-TIME constraint
    ;; and the MIN-EFFECTIVE-SAMPLES constraint
    for confidence of-type single-float
      = (if (= 0e0 benchtime)
            0.0
            (let ((sample-confidence
                    (/ (* iterations (/ sample-time benchtime))
                       min-effective-samples))
                  (time-confidence
                    (/ sample-time min-sample-time)))
              (coerce
               (* (min 1.0 sample-confidence)
                  (min 1.0 time-confidence))
               'single-float)))
    ;; check the two stopping criteria
    until (or (= confidence 1.0)
              (> benchtime timeout))
    finally
       (return
         (values (/ sample-time iterations) confidence iterations))))
