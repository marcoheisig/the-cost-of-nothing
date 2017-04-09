;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(declaim (notinline touch))
(defun touch (object)
  "Protects OBJECT from compiler optimization."
  (declare (ignore object))
  (values))

(defmacro benchmark (form)
  `(nested-benchmark (benchmark ,form)))

(defmacro nested-benchmark (&body body)
  (with-gensyms (iterations)
    `(measure-execution-time
      (lambda (,iterations)
        (loop :repeat ,iterations :do
          (macrolet ((benchmark (form) `(touch ,form)))
            ,@body)))
      :overhead
      (lambda (,iterations)
        (loop :repeat ,iterations :do
          (macrolet ((benchmark (form)
                        (declare (ignore form))
                        `(touch nil)))
            ,@body))))))

(defun measure-execution-time-of-thunk (thunk)
  "Execute THUNK and return the execution time of THUNK in seconds as a
double-float."
  (let* ((t0 (get-internal-run-time))
         (_  (funcall thunk))
         (t1 (get-internal-run-time)))
    (declare (ignore _))
    (coerce (/ (- t1 t0) internal-time-units-per-second)
            'double-float)))

(defun measure-execution-time (fun &key (overhead #'identity) (timeout 2.0))
  "The function FUN must invoke a certain operation N times for any given
integer N. The function OVERHEAD should execute the same code, but without
this operation. An attempt is made to benchmark no longer than TIMEOUT
seconds.

Returns three values:
duration    - a double-float denoting the duration of the operation in seconds
confidence  - a single-flot between 0.0 (garbage) and 1.0 (absolute confidence)
iterations  - the number N of iterations used to determine the result"
  (let ((min-effective-samples 100)
        (min-sampletime        0.1)
        (invocation-growth     1.8))
    (gc) ; expensive, but crucial for reasonable results
    (loop
      :for iterations :of-type unsigned-byte := 3
        :then (floor (* iterations invocation-growth))
      :for benchtime :of-type double-float := 0d0
        :then (measure-execution-time-of-thunk
               (lambda ()
                 (funcall fun iterations)))
      :for sampletime :of-type double-float := 0d0
        :then (- benchtime
                 (measure-execution-time-of-thunk
                  (lambda ()
                    (funcall overhead iterations))))
      :for confidence :of-type single-float := 0.0
        :then (if (not (and (plusp benchtime)
                            (plusp sampletime)))
                  0.0
                  (let ((sample-confidence
                          (/ (* iterations (/ sampletime benchtime))
                             min-effective-samples))
                        (time-confidence
                          (/ sampletime min-sampletime)))
                    (coerce
                     (* (min 1.0 sample-confidence)
                        (min 1.0 time-confidence))
                     'single-float)))
      :until (or (> benchtime timeout)
                 (= confidence 1d0))
      :finally
         (return
           (values
            (/ (max sampletime 0d0) iterations)
            confidence
            iterations)))))

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

