;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/benchmarks/function-calls
  (:use :closer-common-lisp :alexandria :the-cost-of-nothing/core))

(in-package :the-cost-of-nothing/benchmarks/function-calls)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nth-arg (n)
    (intern (format nil "ARG-~D" n)))
  (defun nth-kwd (n)
    (intern (format nil "ARG-~D" n) :keyword)))

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
  (let ((args (loop for i below n collect (nth-arg i))))
    `(defun ,name (&key ,@args)
       ;; pretend to use the arguments to trigger keyword parsing
       (or ,@args))))

(defmacro n-arg-call (n function)
  `(,function ,@(loop for i below n collect 42)))

(defmacro n-keyword-call (n function)
  `(,function
    ,@(loop for i below n
            append `(,(nth-kwd i) 42))))

(n-arg-defun  0-arg-defun  0)
(n-arg-defun 20-arg-defun 20)
(n-keyword-defun  1-keyword-defun  1)
(n-keyword-defun 20-keyword-defun 20)
(n-arg-defmethod  0-arg-defmethod  0)
(n-arg-defmethod 20-arg-defmethod 20)

(define-benchmark function-calls (:funcall)
  (let ((defun-0-time
          (benchmark (n-arg-call 0 0-arg-defun)))
        (defun-20-time
          (benchmark (n-arg-call 20 20-arg-defun)))
        (defmethod-0-time
          (benchmark (n-arg-call 0 0-arg-defmethod)))
        (defmethod-20-time
          (benchmark (n-arg-call 20 20-arg-defmethod)))
        (kwd-defun-0-time
          (benchmark (n-keyword-call 1 1-keyword-defun)))
        (kwd-defun-20-time
          (benchmark (n-keyword-call 20 20-keyword-defun))))
    (lambda (stream)
      (format stream "Cost of a single function call:~%")
      (multiple-value-bind (y-intersection slope)
          (y-intersection-and-slope 0 defun-0-time 20 defun-20-time)
        (format stream "DEFUN: ~A plus ~A per mandatory argument.~%"
                (quantity-string y-intersection "seconds")
                (quantity-string slope "seconds")))
      (multiple-value-bind (y-intersection slope)
          (y-intersection-and-slope 0 kwd-defun-0-time 20 kwd-defun-20-time)
        (format stream "DEFUN + &key: ~A plus ~A per keyword argument.~%"
                (quantity-string y-intersection "seconds")
                (quantity-string slope "seconds")))
      (multiple-value-bind (y-intersection slope)
          (y-intersection-and-slope 0 defmethod-0-time 20 defmethod-20-time)
        (format stream "DEFMETHOD: ~A plus ~A per mandatory argument.~%"
                (quantity-string y-intersection "seconds")
                (quantity-string slope "seconds"))))))
