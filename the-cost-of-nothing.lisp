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
    (funcall (symbol-function question))))

(answer |What is the cost of calling a FLET?|
  (print-time
   (runtime
     (lambda (invocations)
       (flet ((f (x y) (eq x y)))
         (dotimes (i invocations)
           (f "foo" 5)))))))

(declaim (notinline binary-defun-stub))
(defun binary-defun-stub (a b)
  (declare (ignore a b)))

(answer |What is the cost of calling a binary DEFUN?|
  (print-time
   (runtime
     (lambda (invocations)
       (dotimes (i invocations)
         (binary-defun-stub i i))))))

(answer |What is the cost of checking a single list element?|
  (print-time
   (runtime
     (let ((l (make-circular-list 2 :initial-element 42)))
       (lambda (invocations)
         (dotimes (i invocations)
           (when (/= (car l) 42) (return :?))
           (setf l (cdr l))))))))

(defun hash-runtime (&key test keys)
  (let ((hash-table (make-hash-table :test test))
        (keys (apply #'vector keys)))
    (loop for key across keys do
      (setf (gethash key hash-table) nil))
    (let ((keys (shuffle keys))
          (size (length keys))
          tmp)
      (runtime
       (lambda (invocations)
         (declare (type fixnum invocations)
                  (type (simple-array t) keys))
         (dotimes (i (floor invocations size))
           (loop for key across keys do
             (setf tmp (gethash key hash-table))))
         tmp)))))

(answer |What is the cost of a EQL hash table lookup?|
  (print-time
     (hash-runtime :test #'eql :keys (iota 20))))

(answer |What is the cost of a EQAL hash table lookup with a single cons?|
  (let ((keys (loop for i below 20
                    collect (cons i i))))
    (print-time
     (hash-runtime :test #'equal
                   :keys keys))))

(answer |What is the cost of a EQAL hash table lookup with ten conses?|
  (let ((keys (loop for i below 20
                    collect (list i (list (list i i) (list i i i))))))
    (print-time
     (hash-runtime :test #'equal
                   :keys keys))))
