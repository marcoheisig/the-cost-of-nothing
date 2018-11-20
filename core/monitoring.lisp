(in-package :the-cost-of-nothing)

(defvar *context* '()
  "A list of the names of all surrounding monitoring regions.")

(defvar *measurements* nil
  "An object suitable as a second argument to STORE-MEASUREMENT.")

(defstruct (measurement
            (:predicate measurementp)
            (:constructor make-measurement (value name)))
  (value nil)
  (name nil)
  (context *context*)
  (timestamp (make-timestamp)))

(defmacro monitor (form &key (name `',form))
  "Monitor VALUE by storing a measurement in *MEASUREMENTS*.  If
*MEASUREMENTS* is NIL, no measurement is recorded.

The keyword argument NAME can be used to describe the nature of the
measurement.  The default name of an measurement is the VALUE form that is
the first argument of this macro."
  (let((value (gensym)))
    `(let ((,value ,form))
       (%monitor ,value ,name)
       ,value)))

(defun %monitor (value name)
  (let ((storage *measurements*))
    (unless (null storage)
      (let ((measurement (make-measurement value name)))
        (store-measurement measurement storage))))
  (values))

(defgeneric store-measurement (measurement storage)
  (:method (measurement (null null))
    (declare (ignore measurement))
    (values)))

(defmethod print-object ((measurement measurement) stream)
  (if *print-pretty*
      (format stream "~@<#<~;~S ~_~@{~S ~S~^ ~_~}~;>~:>"
              (class-name (class-of measurement))
              :value (measurement-value measurement)
              :name (measurement-name measurement)
              :timestamp (measurement-timestamp measurement))
      (print-unreadable-object (measurement stream :type t)
        (format stream "~@{~S ~S~^ ~}"
                :value (measurement-value measurement)
                :name (measurement-name measurement)
                :timestamp (measurement-timestamp measurement)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Monitoring Regions

(defstruct (monitoring-region-start
            (:include measurement)
            (:constructor make-monitoring-region-start (name)))
  "A measurement that is only emitted when entering a monitoring
region. Its value is the corresponding monitoring region end, or NIL, when
the region is still active.")

(defstruct (monitoring-region-end
            (:include measurement)
            (:constructor make-monitoring-region-end (name start)))
  "A measurement that is only emitted when leaving a monitoring region.
Its value is the corresponding monitoring region start.")

(defmacro with-monitoring-region ((name) &body body)
  "Execute BODY in a region monitoring NAME.  This entails the following things:

1. The special variable *MEASUREMENTS* is bound to itself, to ensure that
   it has the same value throughout the entire monitoring block.

1. An MONITORING-REGION-START measurement is stored right before executing BODY.
   Its value is NIL initially, but later set to the corresponding
   MONITORING-REGION-END measurement.

2. For the dynamic extent of BODY, NAME is prepended to the context of all
   measurements.

3. A MONITORING-REGION-END measurement is stored once control is transferred
   outside of BODY.  Its value is the corresponding MONITORING-REGION-START
   measurement."
  `(call-with-monitoring-region ',name (lambda () ,@body)))

(defun call-with-monitoring-region (name thunk)
  (let* ((*measurements* *measurements*)
         (start (make-monitoring-region-start name)))
    (store-measurement start *measurements*)
    (unwind-protect
         (let ((*context* (cons name *context*)))
           (funcall thunk))
      (let ((end (make-monitoring-region-end name start)))
        (setf (measurement-value start) end)
        (store-measurement end *measurements*)))))

(defmethod print-object ((monitoring-region-start monitoring-region-start) stream)
  (print-unreadable-object (monitoring-region-start stream :type t)
    (format stream "~@{~S ~S~^ ~}"
            :name (measurement-name monitoring-region-start)
            :timestamp (measurement-timestamp monitoring-region-start))))

(defmethod print-object ((monitoring-region-end monitoring-region-end) stream)
  (print-unreadable-object (monitoring-region-end stream :type t)
    (format stream "~@{~S ~S~^ ~}"
            :name (measurement-name monitoring-region-end)
            :timestamp (measurement-timestamp monitoring-region-end))))
