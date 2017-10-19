;;; © 2016-2017 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defparameter *questions* ())

(defmacro answer (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *questions*)))

(defun display (format &rest args)
  (apply #'format t format args)
  (finish-output))

(defun enlighten-me! ()
  (gc :full t) ; tabula rasa
  (dolist (question (reverse *questions*))
    (fresh-line)
    (display "~a~%" question)
    (funcall (symbol-function question))
    (fresh-line)
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; macros to generate benchmarkable entities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nth-slot        (n) (intern (format nil "SLOT~D" n)))
  (defun nth-arg         (n) (intern (format nil "ARG~D" n)))
  (defun nth-keyword-arg (n) (intern (format nil "KEYWORD~D" n)))
  (defun nth-keyword     (n) (make-keyword (format nil "KEYWORD~D" n))))

(defmacro n-slot-struct (name n)
  `(defstruct ,name
     ,@(loop for i below n
             collect
             `(,(nth-slot i) 0 :type fixnum))))

(defmacro n-slot-class (name n)
  `(defclass ,name ()
     ,(loop for i below n
            collect
            `(,(nth-slot i) :type fixnum :initform 0))))

(defmacro n-arg-defun (name n)
  (let ((args (loop for i below n collect (nth-arg i))))
    `(progn
       (declaim (notinline ,name))
       (defun ,name (,@args)
         (declare (ignore ,@args))))))

(defmacro n-arg-defmethod (name n)
  (let ((args (loop for i below n collect (nth-arg i))))
    `(defgeneric ,name (,@args)
       (:method (,@args) (declare (ignore ,@args))))))

(defmacro n-keyword-defun (name n)
  (let ((args (loop for i below n collect (nth-keyword-arg i))))
    `(defun ,name (&key ,@args)
       ;; actually use the arguments to trigger keyword parsing
       (or ,@args))))

(defmacro n-arg-call (n function)
  `(,function ,@(loop for i below n collect i)))

(defmacro n-keyword-call (n function)
  `(,function
    ,@(loop for i below n
            append `(,(nth-keyword i) ,i))))

(n-slot-struct  0-slot-struct  0)
(n-slot-struct 50-slot-struct 50)

(n-slot-class  0-slot-class  0)
(n-slot-class 50-slot-class 50)

(n-arg-defun  0-arg-defun  0)
(n-arg-defun 50-arg-defun 50)

(n-keyword-defun  1-keyword-defun  1)
(n-keyword-defun 20-keyword-defun 20)

(n-arg-defmethod  0-arg-defmethod  0)
(n-arg-defmethod 50-arg-defmethod 50)

(defclass some-class ()
  ((slot-1 :accessor slot-1 :initform 42)))

(defstruct some-struct
  (slot-1 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core functionality benchmarks

(answer |What Lisp system is this?|
  (display "~:[Something weird~;~:*~a~]"
           (lisp-implementation-type))
  (display "~@[ ~a~]"
           (lisp-implementation-version))
  (display ", running on a ~:[strange system~;~:*~a~]"
           (machine-type))
  (display "~@[ ~a~].~%"
           (machine-version)))

(answer |What is the cost of allocating objects?|
  ;; CONS
  (display "~a for allocating conses.~%"
           (as-time
            (benchmark
             (cons nil nil))))
  ;; MAKE-ARRAY
  (flet ((array-cost (bytes)
           (benchmark
            (make-array bytes :element-type '(unsigned-byte 8)))))
    (let* ((x0 4)
           (x1 100000)
           (y0 (array-cost x0))
           (y1 (array-cost x1))
           (slope (/ (- y1 y0) (- x1 x0))))
      (display "~a, plus ~a per byte for allocating arrays.~%"
               (as-time y0)
               (as-time slope))))
  ;; MAKE-STRUCT
  (let* (( y0 (benchmark (make-0-slot-struct)))
         (y50 (benchmark (make-50-slot-struct)))
         (slope (/ (- y50 y0) 50)))
    (display "~a, plus ~a per slot for allocating structs.~%"
             (as-time y0)
             (as-time slope)))
  ;; MAKE-INSTANCE
  ;; CLOS warmup
  (loop repeat 100 do (touch (make-instance  '0-slot-class)))
  (loop repeat 100 do (touch (make-instance '50-slot-class)))
  (let* (( y0 (benchmark (make-instance '0-slot-class)))
         (y50 (benchmark (make-instance '50-slot-class)))
         (slope (/ (- y50 y0) 50)))
    (display "~a, plus ~a per slot for instantiating classes.~%"
             (as-time y0)
             (as-time slope))))

(answer |What is the cost of garbage collection?|
  (display "~a for a normal GC, ~a for a full GC.~%"
           (as-time
            (nested-benchmark
              (cons nil nil)
             (benchmark (gc))))
           (as-time
            (nested-benchmark
             (cons nil nil)
             (benchmark (gc :full t))))))

(answer |What is the cost of a function call?|
  (let* (( 0-arg-cost (benchmark (n-arg-call  0 0-arg-defun)))
         (50-arg-cost (benchmark (n-arg-call 50 50-arg-defun)))
         (slope (/ (- 50-arg-cost 0-arg-cost) 50)))
    (display "~a plus ~a per argument for calling a DEFUN.~%"
             (as-time 0-arg-cost)
             (as-time slope)))
  ;; call each defmethod several times before benchmarking it to give CLOS
  ;; time to set up its stuff
  (loop repeat 100 do (touch (n-arg-call  0  0-arg-defmethod)))
  (loop repeat 100 do (touch (n-arg-call 50 50-arg-defmethod)))
  (let* (( 0-arg-cost (benchmark (n-arg-call  0 0-arg-defmethod)))
         (50-arg-cost (benchmark (n-arg-call 50 50-arg-defmethod)))
         (slope (/ (- 50-arg-cost 0-arg-cost) 50)))
    (display "~a plus ~a per argument for calling a DEFMETHOD.~%"
             (as-time 0-arg-cost)
             (as-time slope)))
  (let* (( 1-keyword-defun-cost (benchmark (n-keyword-call 1 1-keyword-defun)))
         (20-keyword-defun-cost (benchmark (n-keyword-call 20 20-keyword-defun)))
         (slope (/ (- 20-keyword-defun-cost 1-keyword-defun-cost) 19)))
    (display "~a per keyword argument.~%"
             (as-time slope))))

(answer |What is the cost of accessing a SLOT?|
  (let* ((struct (make-some-struct))
         (class (make-instance 'some-class))
         (struct-cost
           (benchmark
            (some-struct-slot-1 struct)))
         (class-cost
           (benchmark
            (slot-1 class))))
    (display "~a for a struct, ~a for a class."
             (as-time struct-cost)
             (as-time class-cost))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; list and sequence function benchmarks

(declaim (notinline benchmark-find))
(defun benchmark-find (sequence)
  (benchmark
   (find #\! sequence)))

(answer |What is the cost of FINDing things?|
  (let* ((length 100)
         (string (make-array length :element-type 'character
                                    :initial-element #\_))
         (list (make-list length))
         (vector (make-array length :initial-element nil)))
    (display "~a per character of a string.~%"
             (as-time (/ (benchmark-find string) length)))
    (display "~a per item in a list.~%"
             (as-time (/ (benchmark-find list) length)))
    (display "~a per element of of a vector.~%"
             (as-time (/ (benchmark-find vector) length)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hash table benchmarks

(declaim (notinline benchmark-hash-table))
(defun benchmark-hash-table (test keys)
  (let ((table (make-hash-table :test test))
        (keys (shuffle (apply #'vector keys))))
    (declare (type (simple-array t (*)) keys))
    (loop for key across keys do
      (setf (gethash key table) nil))
    (/ (nested-benchmark
         (loop for key across keys do
           (benchmark (gethash key table))))
       (length keys))))

(answer |What is the cost of a hash table lookup?|
  (let ((eq (benchmark-hash-table #'eq (iota 40)))
        (eql (benchmark-hash-table #'eql (iota 40)))
        (equal-0 (benchmark-hash-table #'equal (iota 40)))
        (equal-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (benchmark-hash-table #'equal keys)))
        (equalp-0 (benchmark-hash-table #'equalp (iota 40)))
        (equalp-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (benchmark-hash-table #'equalp keys))))
    (display "~a for an EQ hash table.~%"
             (as-time eq))
    (display "~a for an EQL hash table.~%"
             (as-time eql))
    (display "~a plus ~a per cons for an EQUAL hash table.~%"
             (as-time equal-0)
             (as-time
              (/ (- equal-100 equal-0) 100)))
    (display "~a plus ~a per cons for an EQUALP hash table.~%"
             (as-time equalp-0)
             (as-time
              (/ (- equalp-100 equalp-0) 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; number crunching benchmarks

;; My sincere thanks to Robert Strandh for this elegant idea
(defmacro with-specialized-array-types (arrays element-types &body body)
  (if (null element-types)
      `(error "The arrays ~a have no matching element type." (list ,@arrays))
      `(if (and ,@(loop for a in arrays collect
                        `(typep ,a '(simple-array ,(car element-types) (*)))))
           ,@body
           (with-specialized-array-types ,arrays ,(cdr element-types)
             ,@body))))

(declaim (notinline crunch))
(defun crunch (length a b c)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (compilation-speed 0)
                     (space 0)))
  (with-specialized-array-types (a b c)
      (single-float double-float)
    (loop for i fixnum below length do
      (setf (aref c i) (* (+ (aref a i) (aref b i)) 1/2)))))

(defun flops (element-type)
  (let ((length 400)
        (initial-element (coerce 0 element-type)))
    (let ((a1 (make-array
               length
               :element-type element-type
               :initial-element initial-element))
          (a2 (make-array
               length
               :element-type element-type
               :initial-element initial-element)))
      (let ((flop/run (* 2 2 length))
            (time/run (benchmark
                       (progn
                         (crunch length a1 a1 a2)
                         (crunch length a2 a2 a1)))))
        (/ flop/run time/run)))))

(answer |How many floating-point operations can this system do per second?|
  (display "~a in single precision and ~a in double precision.~%"
           (as-flops (flops 'single-float))
           (as-flops (flops 'double-float))))
