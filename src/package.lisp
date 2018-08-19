;;;; package.lisp

(defpackage #:cg-llvm
  (:use
   #:cl
   #:cg-common-ground
   #:iterate
   #:lol-re
   #:esrap-liquid
   #:optima
   ;;#:defmacro-enhance
   #:cl-read-macro-tokens)
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
  (cons '|| (mapcar (lambda (arg)
		      `(prog1-v ,arg (print ',arg)))
		    args)))
