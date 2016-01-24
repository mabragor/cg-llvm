;;;; package.lisp

(defpackage #:cg-llvm
  (:use #:cl #:cg-common-ground #:iterate #:lol-re #:esrap-liquid #:optima #:defmacro-enhance
	#:cl-read-macro-tokens)
  (:export #:cg-llvm-parse #:emit-lisp-repr #:parse-lisp-repr #:emit-text-repr
	   #:*context* #:llvm-return #:unconditional-branch #:conditional-branch #:switch
	   #:indirect-branch
	   #:mk-typed-value
	   #:pointer
	   #:wildcard-equal
	   ))

