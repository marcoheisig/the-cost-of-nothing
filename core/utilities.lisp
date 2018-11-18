(in-package :the-cost-of-nothing)

(defun compiled-lambda (lambda-list &rest body)
  (compile nil `(lambda ,lambda-list ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defvar *si-prefix-alist*
  '(("yotta" . 1d+24)
    ("zetta" . 1d+21)
    ("exa"   . 1d+18)
    ("peta"  . 1d+15)
    ("tera"  . 1d+12)
    ("giga"  . 1d+09)
    ("mega"  . 1d+06)
    ("kilo"  . 1d+03)
    (""      . 1d+00)
    ("milli" . 1d-03)
    ("micro" . 1d-06)
    ("nano"  . 1d-09)
    ("pico"  . 1d-12)
    ("femto" . 1d-15)
    ("atto"  . 1d-18)
    ("zepto" . 1d-21)
    ("yocto" . 1d-24)))

(defun write-si-unit (quantity unit stream)
  (check-type quantity float)
  (check-type unit string)
  (destructuring-bind (prefix . factor)
      (or (rassoc-if
           (lambda (x)
             (declare (double-float x))
             (> (/ quantity x) 1d0))
           *si-prefix-alist*)
          '("" . 1d0))
    (format stream "~,2F ~A~A" (/ quantity factor) prefix unit)))

(defun print-time (time &optional (stream *standard-output*))
  (write-si-unit time "seconds" stream)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memoization

(defun call-with-memoization (key table thunk)
  (multiple-value-bind (values present-p)
      (gethash key table)
    (values-list
     (if (not present-p)
         (setf (gethash key table)
               (multiple-value-list
                (funcall thunk)))
         values))))

(defmacro with-memoization (key-form &body body)
  `(call-with-memoization
    ,key-form
    (load-time-value (make-hash-table :test 'equal))
    (lambda () ,@body)))

(defmacro define-memo-function (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (with-memoization (list ,@(lambda-list-bindings lambda-list))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda Lists

(defun nth-keyword-name (n)
  (intern (format nil "KEYWORD-~D" n) :the-cost-of-nothing))

(defun make-lambda-list (&key (mandatory-arguments 0)
                           (optional-arguments 0)
                           (keyword-arguments 0)
                           (rest-arguments 0))
  `(,@(loop repeat mandatory-arguments
            collect (gensym))
    ,@(if (zerop optional-arguments)
          `()
          `(&optional
            ,@(loop repeat optional-arguments
                    collect `(,(gensym) nil ,(gensym)))))
    ,@(if (zerop keyword-arguments)
          `()
          `(&key
            ,@(loop for index below keyword-arguments
                    for keyword-name = (nth-keyword-name index)
                    collect `((,keyword-name ,(gensym)) nil ,(gensym)))))
    ,@(if (zerop rest-arguments)
          `()
          `(&rest ,(gensym)))))

(defun make-argument-list (&key (mandatory-arguments 0)
                             (optional-arguments 0)
                             (keyword-arguments 0)
                             (rest-arguments 0))
  `(,@(make-list mandatory-arguments)
    ,@(make-list optional-arguments)
    ,@(loop for index below keyword-arguments
            for keyword-name = (nth-keyword-name index)
            append `(',keyword-name nil))
    ,@(make-list rest-arguments)))

(defun lambda-list-bindings (lambda-list)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (append required
            (mapcar #'first optional)
            (mapcar #'third optional)
            (mapcar #'cadar keyword)
            (mapcar #'third keyword)
            (if (not rest) '() (list rest)))))

