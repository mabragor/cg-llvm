(in-package #:cl-user)

(defpackage #:cg-llvm
  (:use
   #:cl
   #:iterate
   #:esrap-liquid
   #:optima
   )
  (:export
   #:cg-llvm-parse
   #:emit-lisp-repr
   #:parse-lisp-repr
   #:emit-text-repr
   #:*context*
   #:llvm-return
   #:unconditional-branch
   #:conditional-branch
   #:switch
   #:indirect-branch
   #:mk-typed-value
   #:pointer
   #:wildcard-equal
   ))

(in-package :cg-llvm)

(defun literal-string (x)
  x)

(defun literal-char (x)
  x)

(defmacro |test| (&rest args)
  (cons '||
	(mapcar (lambda (arg)
		  `(prog1-v ,arg (format t "debug||:: ~s" ',arg)))
		args)))

(defun princify (arg)
  (with-output-to-string (str)
    (princ arg str)))

(defmacro interpol (&rest args)
  `(text ,@ (mapcar (lambda (form)
		      (if (stringp form)
			  form
			  `(princify ,form)))
		    args)))

(defun keywordify (x)
  (intern (string x)
	  "KEYWORD"))

(defun %stringify-symbol (x)
  (string-downcase (string x)))

#+nil
(defun stringify-symbol (x)
  (cg-common-ground:stringify-symbol x))

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))
(defun join (joinee &rest lst)
  (joinl joinee lst))

(defun parse-out-keywords (kwd-lst lambda-list)
  "Return list (KWD1 KWD2 ... KWDn . OTHER-ARGS) collecting all keyword-looking pairs of arguments in lambda list"
  (let ((kwds (make-array (length kwd-lst) :initial-element nil)))
    (iter (generate elt in lambda-list)
	  (if (keywordp (next elt))
	      (setf (elt kwds (position elt kwd-lst :test #'eq)) (next elt))
	      (collect elt into res))
	  (finally (return (nconc (iter (for kwd in-vector kwds)
					(collect kwd))
				  res))))))

(defun underscorize (name)
  (format nil "~{~a~^_~}" (cl-ppcre:split "-" (string-downcase name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mash-sym-names (&rest syms)
    (intern (joinl "-" (mapcar #'string syms))))
  (defun append-instruction-to-sym (sym)
    (mash-sym-names sym 'instruction)))
