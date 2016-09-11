;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defparameter *questions* ())

(defmacro answer (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *questions*)))

(defun enlighten-me! ()
  (dolist (question (reverse *questions*))
    (format t "~a~%" question)
    (funcall (symbol-function question))
    (fresh-line)
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core functionality

(answer |What is the cost of calling CONS?|
  (print-time
   (runtime
    (lambda (invocations)
      (let (tmp)
        (dotimes (i invocations)
          (push 42 tmp)))))))

(answer |What is the cost of allocating arrays?|
  (flet ((alloc-cost (bytes)
           (runtime
            (lambda (invocations)
              (dotimes (i invocations)
                (touch
                 (make-array bytes :element-type '(unsigned-byte 8))))))))
    (let* ((x0 4)
           (x1 100000)
           (y0 (alloc-cost x0))
           (y1 (alloc-cost x1))
           (slope (/ (- y1 y0) (- x1 x0))))
      (format t "~a, plus ~a per byte allocated."
              (print-time y0 nil)
              (print-time slope nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; special forms

(answer |What is the cost of calling a FLET?|
  (print-time
   (runtime
    (lambda (invocations)
      (flet ((f (x y) (eq x y)))
        (dotimes (i invocations)
          (f "foo" 5)))))))

(declaim (notinline binary-defun))
(defun binary-defun (a b)
  (declare (ignore a b)))

(answer |What is the cost of calling a DEFUN?|
  (print-time
   (runtime
    (lambda (invocations)
      (dotimes (i invocations)
        (binary-defun i i))))))

(defmacro n-arg-defmethod (name n)
  (let ((args (loop for i below n collect (gensym))))
    `(defgeneric ,name (,@args)
       (:method (,@args) (declare (ignore ,@args))))))

(n-arg-defmethod 0-arg-defmethod 0)

(n-arg-defmethod 50-arg-defmethod 50)

(answer |What is the cost of calling a DEFMETHOD?|
  (let* ((0-arg-cost
           (runtime
            (lambda (invocations)
              (dotimes (i invocations)
                (0-arg-defmethod)))))
         (50-args (iota 50))
         (50-arg-cost
           (runtime
            (lambda (invocations)
              (dotimes (i invocations)
                (apply #'50-arg-defmethod 50-args))))))
    (let ((slope (/ (- 50-arg-cost 0-arg-cost) 50)))
      (format t "~a plus ~a per argument.~%"
              (print-time 0-arg-cost nil)
              (print-time slope nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; list operations

(answer |What is the cost of checking a single list element?|
  (print-time
   (runtime
    (let ((l (make-circular-list 2 :initial-element 42)))
      (lambda (invocations)
        (dotimes (i invocations)
          (when (/= (car l) 42) (return :?))
          (setf l (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hash tables

(defun hash-runtime (&key test keys)
  (let ((hash-table (make-hash-table :test test))
        (keys (apply #'vector keys)))
    (loop for key across keys do
      (setf (gethash key hash-table) nil))
    (let ((keys (shuffle keys))
          tmp)
      (values
       (runtime
        (lambda (invocations)
          (declare (type fixnum invocations)
                   (type (simple-array t (*)) keys))
          (dotimes (i (floor invocations (length keys)))
            (unroll (i (length keys) 8)
              (setf tmp (gethash (svref keys i) hash-table))))))
       tmp))))

(answer |What is the cost of a hash table lookup?|
  (let ((eq (hash-runtime :test #'eq :keys (iota 40)))
        (eql (hash-runtime :test #'eql :keys (iota 40)))
        (equal-0 (hash-runtime :test #'equal :keys (iota 40)))
        (equal-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (hash-runtime :test #'equal
                          :keys keys)))
        (equalp-0 (hash-runtime :test #'equalp :keys (iota 40)))
        (equalp-100
          (let ((keys (loop for i below 40
                            collect (make-list 100 :initial-element i))))
            (hash-runtime :test #'equalp
                          :keys keys))))
    (format t "~a for an EQ hash table.~%"
            (print-time eq nil))
    (format t "~a for an EQL hash table.~%"
            (print-time eql nil))
    (format t "~a plus ~a per cons for an EQUAL hash table.~%"
            (print-time equal-0 nil)
            (print-time
             (/ (- equal-100 equal-0) 100) nil))
    (format t "~a plus ~a per cons for an EQUALP hash table.~%"
            (print-time equalp-0 nil)
            (print-time
             (/ (- equalp-100 equalp-0) 100) nil))))

;; TODO hash table versus alist

;; TODO hash table rehash cost

;; TODO hash table rehash size
