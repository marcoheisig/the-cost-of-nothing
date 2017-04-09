;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(declaim (notinline touch))
(defun touch (object)
  "Protects OBJECT from compiler optimization."
  (declare (ignore object))
  (values))

(defmacro measure (form)
  (declare (ignore form))
  (error "MEASURE is only valid inside a BENCHMARK form."))

(defmacro benchmark ((iteration-variable) &body body)
  `(measure-execution-time
    (lambda (,iteration-variable)
      (declare (type integer ,iteration-variable))
      (macrolet ((measure (form)
                   `(touch ,form)))
        ,@body))
    (lambda (,iteration-variable)
      (declare (type integer ,iteration-variable))
      (macrolet ((measure (form)
                   (declare (ignore form))
                   `(touch nil)))
        ,@body))))

(defmacro trivial-benchmark (form)
  (with-gensyms (iterations)
    `(benchmark (,iterations)
       (loop repeat ,iterations do
         (measure ,form)))))

(defun measure-execution-time-of-thunk (thunk)
  (let* ((t0 (get-internal-run-time))
         (_  (funcall thunk))
         (t1 (get-internal-run-time)))
    (declare (ignore _))
    (coerce (/ (- t1 t0) internal-time-units-per-second)
            'double-float)))

(defun measure-execution-time (fun &optional (overhead #'identity))
  "The function FUN must invoke a certain operation n times for any given
integer n. The function OVERHEAD should execute the same code, but without
this operation.

Returns three values:
duration    - a double-float denoting the duration of one operation in seconds
confidence  - a single-flot between 0.0 (garbage) and 1.0 (absolute confidence)
invocations - the number of invocations used to determine the result"
  (let ((min-effective-samples 100)
        (max-benchtime         1.6)
        (min-sampletime        0.1)
        (invocation-growth     1.8))
    (gc) ; expensive, but crucial for reasonable results
    (loop
      :for invocations :of-type unsigned-byte := 3
        :then (floor (* invocations invocation-growth))
      :for benchtime :of-type double-float := 0d0
        :then (measure-execution-time-of-thunk
               (lambda ()
                 (funcall fun invocations)))
      :for sampletime :of-type double-float := 0d0
        :then (- benchtime
                 (measure-execution-time-of-thunk
                  (lambda ()
                    (funcall overhead invocations))))
      :for confidence :of-type single-float := 0.0
        :then (if (not (and (plusp benchtime)
                            (plusp sampletime)))
                  0.0
                  (let ((sample-confidence
                          (/ (* invocations (/ sampletime benchtime))
                             min-effective-samples))
                        (time-confidence
                          (/ sampletime min-sampletime)))
                    (coerce
                     (* (min 1.0 sample-confidence)
                        (min 1.0 time-confidence))
                     'single-float)))
      :until (or (> benchtime max-benchtime)
                 (= confidence 1d0))
      :finally
         (return
           (values
            (/ (max sampletime 0d0) invocations)
            confidence
            invocations)))))

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

