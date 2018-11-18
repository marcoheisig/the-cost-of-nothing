(in-package :the-cost-of-nothing)

(defvar *context* '()
  "A list of the names of all surrounding monitoring regions.")

(defvar *measurements* nil
  "An object suitable as a second argument to STORE-MEASUREMENT.")

(defclass measurement ()
  ((%value :initarg :value :reader measurement-value)
   (%name :initarg :name :reader measurement-name)
   (%context :initarg :context :reader measurement-context)
   (%timestamp :initarg :timestamp :reader measurement-timestamp))
  (:default-initargs
   :value nil
   :name nil
   :context *context*
   :timestamp (make-timestamp)))

(defgeneric measure (value name class storage)
  (:argument-precedence-order storage value class name)
  (:method ((value t) (name t) (class t) (storage null))
    (declare (ignore value name class storage))
    (values))
  (:documentation
  "Create an instance of CLASS with the given NAME and VALUE and store it
in STORAGE.  CLASS must be a subtype of MEASUREMENT."))

(defmacro monitor (form &key (name `',form) (class ''measurement))
  "Monitor VALUE by storing a measurement in *MEASUREMENTS*.

The keyword argument NAME can be used to describe the nature of the
measurement.  The default name of an measurement is the VALUE form that is
the first argument of this macro.

The keyword argument CLASS can be used to specify the class of the
measurement object.  It defaults to the class MEASUREMENT."
  `(measure ,form ,name ,class *measurements*))

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

(defclass monitoring-region-start (measurement)
  ((%value :initarg :end :reader monitoring-region-end))
  (:documentation
   "An measurement that is only emitted when entering a monitoring region. Its
value is the corresponding monitoring region end, or NIL, when the region is
still active."))

(defclass monitoring-region-end (measurement)
  ((%value :initarg :start :reader monitoring-region-start))
  (:documentation
   "An measurement that is only emitted when leaving a monitoring region.
Its value is the corresponding monitoring region start."))

(defun call-with-monitoring-region (name thunk)
  (let* ((*measurements* *measurements*)
         (start (make-instance 'monitoring-region-start :name name :end nil)))
    (store-measurement start *measurements*)
    (unwind-protect
         (let ((*context* (cons name *context*)))
           (funcall thunk))
      (let ((end (make-instance 'monitoring-region-end :name name :start start)))
        (setf (slot-value start '%value) end)
        (store-measurement end *measurements*)))))

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
