;;;; package.lisp

(defpackage #:cg-llvm
  (:use #:cl #:cg-common-ground #:iterate #:lol-re #:esrap-liquid #:optima #:defmacro-enhance)
  (:export #:cg-llvm-parse #:emit-lisp-repr #:parse-lisp-repr #:emit-text-repr
	   #:*context* #:llvm-return #:unconditional-branch #:conditional-branch #:switch
	   #:indirect-branch #:invoke #:resume #:unreachable
	   #:add #:sub #:mul #:shl
	   #:fadd #:fsub #:smul #:fdiv #:frem
	   #:udiv #:sdiv #:lshr #:ashr
	   #:urem #:srem #:llvm-and #:llvm-or #:llvm-xor
	   #:extractelement #:insertelement #:shufflevector
	   #:mk-typed-value
	   #:extractvalue #:insertvalue
	   #:alloca #:llvm-load #:store #:fence #:cmpxchg #:atomicrmw #:getelementptr))

