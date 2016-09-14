;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defparameter *questions* ())

(defmacro answer (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *questions*)))

(defun enlighten-me! ()
  (gc :full t) ; tabula rasa
  (dolist (question (reverse *questions*))
    (format t "~a~%" question)
    (funcall (symbol-function question))
    (fresh-line)
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core functionality

(answer |What Lisp system is this?|
  (format t "~:[Something weird~;~:*~a~]"
          (lisp-implementation-type))
  (format t "~@[ ~a~]"
          (lisp-implementation-version))
  (format t ", running on a ~:[strange system~;~:*~a~]"
          (machine-type))
  (format t "~@[ ~a~].~%"
          (machine-version)))

(answer |What is the cost of allocating objects?|
  (format t "~a for allocating conses.~%"
          (time-string (run-time (cons nil nil))))
  (flet ((array-cost (bytes)
           (run-time
             (make-array
              bytes
              :element-type '(unsigned-byte 8)))))
    (let* ((x0 4)
           (x1 100000)
           (y0 (array-cost x0))
           (y1 (array-cost x1))
           (slope (/ (- y1 y0) (- x1 x0))))
      (format t "~a, plus ~a per byte for allocating arrays.~%"
              (time-string y0)
              (time-string slope))))
  (let* ((y0 (run-time (make-0-slot-struct)))
         (y50 (run-time (make-50-slot-struct)))
         (slope (/ (- y50 y0) 50)))
    (format t "~a, plus ~a per slot for allocating structs.~%"
            (time-string y0)
            (time-string slope)))
  (let* ((y0 (run-time (make-instance '0-slot-class)))
         (y50 (run-time (make-instance '50-slot-class)))
         (slope (/ (- y50 y0) 50)))
    (format t "~a, plus ~a per slot for instantiating classes.~%"
            (time-string y0)
            (time-string slope))))

(defmacro n-slot-struct (name n)
  (let ((slots
          (loop for i below n
                collect
                `(,(intern (format nil "SLOT-~d" i))
                  0 :type fixnum))))
    `(defstruct ,name ,@slots)))

(n-slot-struct 0-slot-struct 0)

(n-slot-struct 50-slot-struct 50)

(defmacro n-slot-class (name n)
  (let ((slots
          (loop for i below n
                collect
                `(,(intern (format nil "SLOT-~d" i))
                  :type fixnum :initform 0))))
    `(defclass ,name () ,slots)))

(n-slot-class 0-slot-class  0)

(n-slot-class 50-slot-class 50)

(answer |What is the cost of garbage collection?|
  (format t "~a for a normal GC, ~a for a full GC.~%"
          (time-string
           (run-time
             (prog1 (touch (cons nil nil))
               (gc))))
          (time-string
           (run-time
             (prog1 (touch (cons nil nil))
               (gc :full t))))))

(answer |What is the cost of a function call?|
  (let* ((0-arg-cost
           (run-time
             (0-arg-defun)))
         (50-arg-cost
           (run-time
             (apply #'50-arg-defun '#.(iota 50))))
         (slope (/ (- 50-arg-cost 0-arg-cost) 50)))
    (format t "~a plus ~a per argument for calling a DEFUN.~%"
            (time-string 0-arg-cost)
            (time-string slope)))
  (let* ((0-arg-cost
           (run-time
             (0-arg-defmethod)))
         (50-arg-cost
           (run-time
             (apply #'50-arg-defmethod '#.(iota 50))))
         (slope (/ (- 50-arg-cost 0-arg-cost) 50)))
    (format t "~a plus ~a per argument for calling a DEFMETHOD.~%"
            (time-string 0-arg-cost)
            (time-string slope))))

(defmacro n-arg-defun (name n)
  (let ((args (loop for i below n collect (gensym))))
    `(progn
       (declaim (notinline ,name))
       (defun ,name (,@args)
         (declare (ignore ,@args))))))

(n-arg-defun 0-arg-defun 0)

(n-arg-defun 50-arg-defun 50)

(defmacro n-arg-defmethod (name n)
  (let ((args (loop for i below n collect (gensym))))
    `(defgeneric ,name (,@args)
       (:method (,@args) (declare (ignore ,@args))))))

(n-arg-defmethod 0-arg-defmethod 0)

(n-arg-defmethod 50-arg-defmethod 50)

(answer |What is the cost of checking for equality?|
  )

;; TODO CLOS benchmarks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lists and sequences

(answer |What is the cost of FINDing things?|
  (let* ((length 100)
         (string (make-array length :element-type 'character
                                    :initial-element #\_))
         (list (make-list length))
         (vector (make-array length :initial-element nil)))
    (format t "~a per character of a string.~%"
            (time-string (/ (find-run-time string) length)))
    (format t "~a per item in a list.~%"
            (time-string (/ (find-run-time list) length)))
    (format t "~a per element of of a vector.~%"
            (time-string (/ (find-run-time vector) length)))))

(defun find-run-time (sequence)
  (run-time
    (find #\! sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hash tables

(answer |What is the cost of a hash table lookup?|
  (let ((eq (hash-run-time #'eq (iota 40)))
        (eql (hash-run-time #'eql (iota 40)))
        (equal-0 (hash-run-time #'equal (iota 40)))
        (equal-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (hash-run-time #'equal keys)))
        (equalp-0 (hash-run-time #'equalp (iota 40)))
        (equalp-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (hash-run-time #'equalp keys))))
    (format t "~a for an EQ hash table.~%"
            (time-string eq))
    (format t "~a for an EQL hash table.~%"
            (time-string eql))
    (format t "~a plus ~a per cons for an EQUAL hash table.~%"
            (time-string equal-0)
            (time-string
             (/ (- equal-100 equal-0) 100)))
    (format t "~a plus ~a per cons for an EQUALP hash table.~%"
            (time-string equalp-0)
            (time-string
             (/ (- equalp-100 equalp-0) 100)))))

(defun hash-run-time (test keys)
  (let ((hash-table (make-hash-table :test test))
        (keys (apply #'vector keys)))
    (loop for key across keys do
      (setf (gethash key hash-table) nil))
    (let ((keys (shuffle keys))
          tmp)
      (values
       (benchmark
        (lambda (invocations)
          (declare (type fixnum invocations)
                   (type (simple-array t (*)) keys))
          (dotimes (i (floor invocations (length keys)))
            (dotimes (i (length keys))
              (setf tmp (gethash (svref keys i) hash-table))))))
       tmp))))

;; TODO hash table versus alist

;; TODO hash table rehash cost

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; number crunching

(answer |How many floating-point operations can this system do per second?|
  (format t "~a in single precision and ~a in double precision.~%"
          (flops-string (max (flops 'single-float)
                             (flops '(complex single-float))))
          (flops-string (max (flops 'double-float)
                             (flops '(complex double-float))))))

(defmacro define-number-cruncher (name type coefficient)
  `(defun ,name (length a b c)
     (declare
      (optimize (speed 3)
                (safety 0)
                (debug 0)
                (compilation-speed 0)
                (space 0))
      (type (simple-array ,type (*)) a b c))
     (loop for i fixnum below length do
       (setf (aref c i) (* (+ (aref a i) (aref b i))
                           ,coefficient)))))

(define-number-cruncher crunch-c64 (complex double-float) 0.5d0)
(define-number-cruncher crunch-c32 (complex single-float) 0.5)
(define-number-cruncher crunch-f64 double-float 0.5d0)
(define-number-cruncher crunch-f32 single-float 0.5)

(defun flops (element-type)
  (let* ((length 400)
         (initial-element (coerce 0.0 element-type))
         (a1 (make-array
              length
              :element-type element-type
              :initial-element initial-element))
         (a2 (make-array
              length
              :element-type element-type
              :initial-element initial-element)))
    (/
     (benchmark
      (lambda (invocations)
        (let* ((flop/traversal
                 (cond
                   ((subtypep element-type 'complex) (* 4 length))
                   (t (* 2 length))))
               (traversals (/ invocations flop/traversal)))
          (loop for iteration below traversals by 2 do
            (let ((f (etypecase initial-element
                       (single-float #'crunch-f32)
                       (double-float #'crunch-f64)
                       ((complex single-float) #'crunch-c32)
                       ((complex double-float) #'crunch-c64))))
              (funcall f length a1 a1 a2)
              (funcall f length a2 a2 a1)))))))))
