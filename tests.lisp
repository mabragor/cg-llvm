(in-package :cl-user)

(defpackage :cg-llvm-tests
  (:use :cl :cg-llvm :fiveam :iterate :cl-read-macro-tokens)
  (:shadowing-import-from #:cg-llvm #:join)
  (:export #:run-tests))

(in-package :cg-llvm-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(def-suite cg-llvm)
(in-suite cg-llvm)

(defun run-tests ()
  (let ((results (run 'cg-llvm)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (equal 'cg-llvm::void (emit-lisp-repr (cg-llvm-parse 'llvm-type "void"))))
  (is (equal 24 (slot-value (cg-llvm-parse 'integer-type "i24") 'cg-llvm::nbits)))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (emit-lisp-repr (cg-llvm-parse 'float-type ,expr))))))
    (frob (float 64 32)  "double")
    (frob (float 16 8) "half")
    (frob (float 32 16) "float")
    (frob (float 128 112) "fp128")
    (frob (float 80 40) "x86_fp80")
    (frob (float 128 64) "ppc_fp128"))
  (is (equal 'cg-llvm::x86-mmx (emit-lisp-repr (cg-llvm-parse 'x86-mmx "x86_mmx"))))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (emit-lisp-repr (cg-llvm-parse 'vector ,expr))))))
    (frob (vector (integer 32) 4) "<4 x i32>")
    (frob (vector (float 32 16) 8) "<8 x float>")
    (frob (vector (integer 64) 2) "<2 x i64>")
    (frob (vector (cg-llvm::pointer (integer 64)) 4) "<4 x i64*>"))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (emit-lisp-repr (cg-llvm-parse 'array ,expr))))))
    (frob (array (array (integer 32) 4) 3) "[3 x [4 x i32]]")
    (frob (array (array (float 32 16) 10) 12) "[12 x [10 x float]]")
    (frob (array (array (array (integer 16) 4) 3) 2) "[2 x [3 x [4 x i16]]]"))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (emit-lisp-repr (cg-llvm-parse 'struct ,expr))))))
    (frob (cg-llvm::struct ((integer 32) (integer 16) (integer 8)) :packed-p nil)
	  "{ i32, i16, i8 }")
    (frob (cg-llvm::struct ((integer 32) (cg-llvm::pointer (float 32 16))) :packed-p nil)
	  "{ i32, float * }")
    (frob (cg-llvm::struct ((integer 8) (integer 32)) :packed-p t)
	  "<{ i8, i32 }>"))
  (is (equal 'cg-llvm::opaque (emit-lisp-repr (cg-llvm-parse 'struct "opaque")))))
    

(test more-complicated
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (emit-lisp-repr (cg-llvm-parse 'llvm-type ,expr))))))
    (frob (cg-llvm::pointer (integer 32) 5) #?"i32\naddrspace(5)*")
    (frob (cg-llvm::pointer (function (integer 32) ((cg-llvm::pointer (integer 32))) :vararg-p nil))
	  "i32 (i32 *) *")
    (frob (function (integer 32) ((integer 32)) :vararg-p nil)
	  "i32 (i32)")
    (frob (function (integer 1) ((integer 2) (integer 3) (integer 4)) :vararg-p nil)
	  "i1 (i2, i3, i4)")
    (frob (function (integer 32) ((cg-llvm::pointer (integer 8))) :vararg-p t)
	  "i32 (i8*, ...)")
    (frob (function (cg-llvm::struct ((integer 32) (integer 32)) :packed-p nil) ((integer 32)) :vararg-p nil)
	  "{i32, i32} (i32)")))


(test parsing-of-s-exps
  (macrolet ((frob (x)
	       `(is (equal ',x (emit-lisp-repr (parse-lisp-repr ',x))))))
    (frob (integer 32))
    (frob (cg-llvm::pointer (integer 32) 5))
    (frob (cg-llvm::pointer (function (integer 32) ((cg-llvm::pointer (integer 32))) :vararg-p nil)))
    (frob (function (integer 32) ((integer 32)) :vararg-p nil))
    (frob (function (integer 1) ((integer 2) (integer 3) (integer 4)) :vararg-p nil))
    (frob (function (integer 32) ((cg-llvm::pointer (integer 8))) :vararg-p t))
    (frob (function (cg-llvm::struct ((integer 32) (integer 32)) :packed-p nil) ((integer 32)) :vararg-p nil))
    (frob cg-llvm::void)
    (frob (float 64 32))
    (frob cg-llvm::x86-mmx)
    (frob (vector (integer 32) 4))
    (frob (vector (cg-llvm::pointer (integer 64)) 4))
    (frob (array (array (array (integer 16) 4) 3) 2))
    (frob (cg-llvm::struct ((integer 32) (integer 16) (integer 8)) :packed-p nil))
    (frob (cg-llvm::struct ((integer 32) (cg-llvm::pointer (float 32 16))) :packed-p nil))
    (frob (cg-llvm::struct ((integer 8) (integer 32)) :packed-p t))
    (frob cg-llvm::opaque)))


(test emitting-of-text
  (macrolet ((frob (x &optional y)
	       `(is (equal ,x (emit-text-repr (cg-llvm-parse 'llvm-type ,(or y x)))))))
    (frob "i32") (frob "void")
    (frob "double") (frob "half") (frob "float") (frob "fp128") (frob "x86_fp80") (frob "ppc_fp128")
    (frob "x86_mmx")
    (frob "<4 x i32>") (frob "<8 x float>") (frob "<2 x i64>") (frob "<4 x i64*>")
    (frob "[3 x [4 x i32]]") (frob "[12 x [10 x float]]") (frob "[2 x [3 x [4 x i16]]]")
    (frob "{i32, i16, i8}" "{ i32, i16, i8 }") (frob "{i32, float*}" "{ i32, float * }")
    (frob "<{i8, i32}>" "<{ i8, i32 }>")
    (frob "i32 addrspace(5)*" #?"i32\naddrspace(5)*") (frob "i32 (i32)") (frob "i1 (i2, i3, i4)")
    (frob "i32 (i8*, ...)") (frob "{i32, i32} (i32)")
    ))



;; terminating instructions

(test terminating-instructions
  (macrolet ((frob (x y)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (llvm-return ,y)))))))
    (frob "ret void" :void)
    (frob "ret i14 42" '(42 (integer 14)))
    (frob "ret i14 42" '(42 "i14")))
  (macrolet ((frob (x y)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (unconditional-branch ,y)))))))
    (frob "br label asdf" 'asdf)
    (frob "br label %Asdf" '+%asdf)
    (frob "br label ASDF" '*asdf))
  (macrolet ((frob (x y z w)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (conditional-branch ,y ,z ,w)))))))
    (frob "br i1 %cond, label %IfEqual, label %IfUnequal" '(%cond "i1") '+%if-equal '+%if-unequal))
  (macrolet ((frob (x &rest args)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (switch ,@args)))))))
    (frob "switch i32 %Val, label %truedest [i32 0, label %falsedest]"
	  '(+%val  "i32") '%truedest 0 '%falsedest)
    (frob "switch i32 0, label %dest []"
	  0 '%dest)
    (frob "switch i32 %val, label %otherwise [i32 0, label %onzero i32 1, label %onone i32 2, label %ontwo]"
	  '(%val "i32") '%otherwise 0 '%onzero 1 '%onone 2 '%ontwo))
  (macrolet ((frob (x y &rest args)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (indirect-branch ,y ,@args)))))))
    (frob "indirectbr i8* %Addr, [label %bb1, label %bb2, label %bb3]"
	  '(+%addr "i8*") '%bb1 '%bb2 '%bb3))
  (macrolet ((frob (x y)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (resume ,y)))))))
    (frob "resume {i8*, i32} %exn" '(%exn "{i8*, i32}")))
  (is (equal "unreachable" (with-output-to-string (*standard-output*) (unreachable))))
  (macrolet ((frob (x &rest args)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (invoke ,@args)))))))
    (frob "invoke i32 @Test(i32 15) to label %Continue unwind label %TestCleanup"
	  '(+@test "i32 (i32)") '(15) '+%continue '+%test-cleanup)
    (frob "invoke i32 @Test(i32 15) to label %Continue unwind label %TestCleanup"
	  '(+@test "i32") '(15) '+%continue '+%test-cleanup)
    (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
	  '(+%testfnptr "i32 (i32)") '(15) '+%continue '+%test-cleanup
	  :call-conv 'coldcc)
    (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
	  '(+%testfnptr "i32") '(15) '+%continue '+%test-cleanup
	  :call-conv 'coldcc)
    (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
	  '(+%testfnptr "i32(i32)*") '(15) '+%continue '+%test-cleanup
	  :call-conv 'coldcc)))
    
    
(test binary-operators
  (macrolet ((frob (x y)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(let ((cg-llvm::*context* :function))
				  (cg-llvm::reset-tmp-var-counts)
				  ,y))))))
    (frob (cg-llvm::join "~%"
			 "%tmpadd1 = add i32 1, 2"
			 "%tmpadd2 = add i32 3, 4"
			 "%tmpadd3 = add i32 %tmpadd1, %tmpadd2"
			 "ret i32 %tmpadd3")
	  (llvm-return (add (add 1 2) (add 3 4))))))

(test vector-operations
  (macrolet ((frob (x y)
	       `(is (equal ,x (with-output-to-string (*standard-output*)
				(cg-llvm::reset-tmp-var-counts)
				,y)))))
    (frob #?"%tmpexelt1 = extractelement <8 x i8> %tmpvec, i32 3\n"
	  (extractelement (mk-typed-value '(vector (integer 8) 8) '%tmpvec) 3))
    (frob #?"%tmpinselt1 = insertelement <8 x i8> %tmpvec, i8 %tmpint, i32 0\n"
	  (insertelement (mk-typed-value '(vector (integer 8) 8) '%tmpvec)
			 (mk-typed-value '(integer 8) '%tmpint)
			 0))
    (frob #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 4, i32 1, i32 5>\n"
	  (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
			 (mk-typed-value '(vector (integer 32) 4) '%v2)
			 ;; TODO : I don't know yet how to write vectors is lisp-notation
			 ;; so I just write text rep here
			 (mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 4, i32 1, i32 5>")))
    (frob #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>\n"
	  (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
			 (mk-typed-value '(vector (integer 32) 4) 'undef)
			 (mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 1, i32 2, i32 3>")))
    (frob (join "~%"
		(join " " #?"%tmpshufvec1 = shufflevector <8 x i32> %v1, <8 x i32> undef,"
		      "<4 x i32> <i32 0, i32 1, i32 2, i32 3>")
		"ret <4 x i32> %tmpshufvec1")
	  (let ((cg-llvm::*context* :function))
	    (llvm-return (shufflevector (mk-typed-value '(vector (integer 32) 8) '%v1)
					(mk-typed-value '(vector (integer 32) 8) 'undef)
					(mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 1, i32 2, i32 3>")))))
    (frob (join "~%"
		(join " " #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> %v2,"
		      "<8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")
		"ret <8 x i32> %tmpshufvec1")
	  (let ((cg-llvm::*context* :function))
	    (llvm-return
	     (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
			    (mk-typed-value '(vector (integer 32) 4) '%v2)
			    (mk-typed-value '(vector (integer 32) 8)
					    "<i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")))))))
  
	  

	  