;;;; package.lisp

(defpackage #:cg-llvm
  (:use
   #:cl
   #:cg-common-ground
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
