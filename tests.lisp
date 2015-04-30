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

(test wildcard-equal
  (macrolet ((frob (x y z)
	       `(is (equal ,x (wildcard-equal ',y ',z)))))
    (frob t a a)
    (frob nil a b)
    (frob t *** a)
    (frob t *** nil)
    (frob t (pointer (integer 32) ***) (pointer (integer 32)))
    (frob t (pointer (integer 32) ***) (pointer (integer 32) 2))))
    

;; ;; terminating instructions

;; (test terminating-instructions
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (llvm-return ,y)))))))
;;     (frob "ret void" :void)
;;     (frob "ret i14 42" '(42 (integer 14)))
;;     (frob "ret i14 42" '(42 "i14")))
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (unconditional-branch ,y)))))))
;;     (frob "br label asdf" 'asdf)
;;     (frob "br label %Asdf" '+%asdf)
;;     (frob "br label ASDF" '*asdf))
;;   (macrolet ((frob (x y z w)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (conditional-branch ,y ,z ,w)))))))
;;     (frob "br i1 %cond, label %IfEqual, label %IfUnequal" '(%cond "i1") '+%if-equal '+%if-unequal))
;;   (macrolet ((frob (x &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (switch ,@args)))))))
;;     (frob "switch i32 %Val, label %truedest [i32 0, label %falsedest]"
;; 	  '(+%val  "i32") '%truedest 0 '%falsedest)
;;     (frob "switch i32 0, label %dest []"
;; 	  0 '%dest)
;;     (frob "switch i32 %val, label %otherwise [i32 0, label %onzero i32 1, label %onone i32 2, label %ontwo]"
;; 	  '(%val "i32") '%otherwise 0 '%onzero 1 '%onone 2 '%ontwo))
;;   (macrolet ((frob (x y &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (indirect-branch ,y ,@args)))))))
;;     (frob "indirectbr i8* %Addr, [label %bb1, label %bb2, label %bb3]"
;; 	  '(+%addr "i8*") '%bb1 '%bb2 '%bb3))
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (resume ,y)))))))
;;     (frob "resume {i8*, i32} %exn" '(%exn "{i8*, i32}")))
;;   (is (equal "unreachable" (with-output-to-string (*standard-output*) (unreachable))))
;;   (macrolet ((frob (x &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (invoke ,@args)))))))
;;     (frob "invoke i32 @Test(i32 15) to label %Continue unwind label %TestCleanup"
;; 	  '(+@test "i32 (i32)") '(15) '+%continue '+%test-cleanup)
;;     (frob "invoke i32 @Test(i32 15) to label %Continue unwind label %TestCleanup"
;; 	  '(+@test "i32") '(15) '+%continue '+%test-cleanup)
;;     (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
;; 	  '(+%testfnptr "i32 (i32)") '(15) '+%continue '+%test-cleanup
;; 	  :call-conv 'coldcc)
;;     (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
;; 	  '(+%testfnptr "i32") '(15) '+%continue '+%test-cleanup
;; 	  :call-conv 'coldcc)
;;     (frob "invoke coldcc i32 %Testfnptr(i32 15) to label %Continue unwind label %TestCleanup"
;; 	  '(+%testfnptr "i32(i32)*") '(15) '+%continue '+%test-cleanup
;; 	  :call-conv 'coldcc)))
    
    
;; (test binary-operators
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((cg-llvm::*context* :function))
;; 				  (cg-llvm::reset-tmp-var-counts)
;; 				  ,y))))))
;;     (frob (cg-llvm::join "~%"
;; 			 "%tmpadd1 = add i32 1, 2"
;; 			 "%tmpadd2 = add i32 3, 4"
;; 			 "%tmpadd3 = add i32 %tmpadd1, %tmpadd2"
;; 			 "ret i32 %tmpadd3")
;; 	  (llvm-return (add (add 1 2) (add 3 4))))))

;; (test vector-operations
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(cg-llvm::reset-tmp-var-counts)
;; 				,y)))))
;;     (frob #?"%tmpexelt1 = extractelement <8 x i8> %tmpvec, i32 3\n"
;; 	  (extractelement (mk-typed-value '(vector (integer 8) 8) '%tmpvec) 3))
;;     (frob #?"%tmpinselt1 = insertelement <8 x i8> %tmpvec, i8 %tmpint, i32 0\n"
;; 	  (insertelement (mk-typed-value '(vector (integer 8) 8) '%tmpvec)
;; 			 (mk-typed-value '(integer 8) '%tmpint)
;; 			 0))
;;     (frob #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 4, i32 1, i32 5>\n"
;; 	  (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
;; 			 (mk-typed-value '(vector (integer 32) 4) '%v2)
;; 			 ;; TODO : I don't know yet how to write vectors is lisp-notation
;; 			 ;; so I just write text rep here
;; 			 (mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 4, i32 1, i32 5>")))
;;     (frob #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>\n"
;; 	  (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
;; 			 (mk-typed-value '(vector (integer 32) 4) 'undef)
;; 			 (mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 1, i32 2, i32 3>")))
;;     (frob (join "~%"
;; 		(join " " #?"%tmpshufvec1 = shufflevector <8 x i32> %v1, <8 x i32> undef,"
;; 		      "<4 x i32> <i32 0, i32 1, i32 2, i32 3>")
;; 		"ret <4 x i32> %tmpshufvec1")
;; 	  (let ((cg-llvm::*context* :function))
;; 	    (llvm-return (shufflevector (mk-typed-value '(vector (integer 32) 8) '%v1)
;; 					(mk-typed-value '(vector (integer 32) 8) 'undef)
;; 					(mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 1, i32 2, i32 3>")))))
;;     (frob (join "~%"
;; 		(join " " #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> %v2,"
;; 		      "<8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")
;; 		"ret <8 x i32> %tmpshufvec1")
;; 	  (let ((cg-llvm::*context* :function))
;; 	    (llvm-return
;; 	     (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
;; 			    (mk-typed-value '(vector (integer 32) 4) '%v2)
;; 			    (mk-typed-value '(vector (integer 32) 8)
;; 					    "<i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")))))))
  
	  
;; (defmacro frob-context (x y)
;;   `(is (equal ,x (with-output-to-string (*standard-output*)
;; 		   (cg-llvm::reset-tmp-var-counts)
;; 		   (let ((cg-llvm::*context* :function))
;; 		     ,y)))))


;; (test aggregate-operations
;;   (frob-context (join "~%"
;; 		      #?"%tmpexval1 = extractvalue {i32, float} %agg, 0"
;; 		      "ret i32 %tmpexval1")
;; 		(llvm-return (extractvalue (mk-typed-value "{i32, float}" '%agg) 0)))
;;   (frob-context (join "~%"
;; 		      #?"%tmpexval1 = extractvalue {i32, float} %agg, 1"
;; 		      "ret float %tmpexval1")
;; 		(llvm-return (extractvalue (mk-typed-value "{i32, float}" '%agg) 1)))
;;   (frob-context (join "~%"
;; 		      "%tmpinsval1 = insertvalue {i32, float} undef, i32 1, 0"
;; 		      "%tmpinsval2 = insertvalue {i32, float} %tmpinsval1, float %val, 1"
;; 		      #?"%tmpinsval3 = insertvalue {i32, {float}} undef, float %val, 1, 0\n")
;; 		(progn (insertvalue (insertvalue (mk-typed-value "{i32, float}" 'undef) 1 0)
;; 				    (mk-typed-value "float" '%val) 1)
;; 		       (insertvalue (mk-typed-value "{i32, {float}}" 'undef)
;; 				    (mk-typed-value "float" '%val)
;; 				    1 0))))

;; (test memory-operations
;;   (frob-context #?"%tmpptr1 = alloca i32\n"
;; 		(alloca "i32"))
;;   (frob-context #?"%tmpptr1 = alloca i32, i32 4\n"
;; 		(alloca "i32" :num-elts 4))
;;   (frob-context #?"%tmpptr1 = alloca i32, i32 4, align 1024\n"
;; 		(alloca "i32" :num-elts 4 :align 1024))
;;   (frob-context #?"%tmpptr1 = alloca i32, align 1024\n"
;; 		(alloca "i32" :align 1024)))
  

;; (test conversion-operations
;;   (let ((my-fav-vector (mk-typed-value '(vector (integer 16) 2)
;; 				       "<i16 8, i16 7>")))
;;     (frob-context #?"%tmptrunc1 = trunc i32 4 to i1\n" (trunc 4 "i1"))
;;     (frob-context #?"%tmptrunc1 = trunc <2 x i16> <i16 8, i16 7> to <2 x i8>\n"
;; 		  (trunc my-fav-vector "<2 x i8>"))
;;     (frob-context #?"%tmpzext1 = zext i32 257 to i64\n" (zext 257 "i64"))
;;     (frob-context #?"%tmpzext1 = zext <2 x i16> <i16 8, i16 7> to <2 x i32>\n"
;; 		  (zext my-fav-vector "<2 x i32>"))
;;     (frob-context #?"%tmpsext1 = sext i8 -1 to i16\n" (sext (mk-typed-value "i8" -1) "i16"))
;;     (frob-context #?"%tmpsext1 = sext i1 true to i32\n" (sext (mk-typed-value "i1" 'true) "i32"))
;;     (frob-context #?"%tmpsext1 = sext <2 x i16> <i16 8, i16 7> to <2 x i32>\n"
;; 		  (sext my-fav-vector "<2 x i32>"))
;;     (frob-context #?"%tmpfptrunc1 = fptrunc double 123.0 to float\n"
;; 		  (fptrunc (mk-typed-value "double" 123.0) "float"))))
  

;; (test misc-operations
;;   (frob-context #?"%tmpphi1 = phi i32 [ 1, %x ], [ 2, %y ]\n"
;; 		(phi '(1 %x) '(2 %y)))
;;   (frob-context #?"%tmpsel1 = select i1 true, i32 17, i32 42\n"
;; 		(select 'true 17 42)))

(test complex-constants
  (is (equal '((array (integer 8) 3) (((integer 8) 1) ((integer 8) 2) ((integer 8) 3)))
	     (cg-llvm-parse 'array-constant "[ 3 x i8 ] [ i8 1, i8 2, i8 3 ]")))
  (is (equal '((array (integer 8) 3) :zero-initializer)
	     (cg-llvm-parse 'zero-init-constant "[ 3 x i8 ] zeroinitializer")))
  (is (equal '((array (integer 8) 3) (((integer 8) 97) ((integer 8) 115) ((integer 8) 100) ((integer 8) 102)))
	     (cg-llvm-parse 'string-constant "[ 3 x i8 ] c\"asdf\""))))
  
(test llvm-constants
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'llvm-constant ,y)))))
    (frob '((integer 8) 1) "i8 1")
    (frob '((integer 32) 42) "i32 42")
    (frob '((array (pointer (integer 32)) 2)
	    (((pointer (integer 32)) *@x)
	     ((pointer (integer 32)) *@y)))
	  "[2 x i32*] [ i32* @X, i32* @Y ]")
    ))
  
(test metadata-constants
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'metadata-node ,y)))))
    (frob '(:metadata-ref 24) "!24")
    (frob '(:metadata (:metadata-ref 4) (:metadata-ref 3)) "!{!4, !3}")
    (frob '(:metadata (:metadata #?"test\0") ((integer 32) 10)) "!{ !\"test\\00\", i32 10}")
    (frob '(:metadata (:metadata-ref 0) (:metadata (:metadata-ref 2) (:metadata-ref 0)) (:metadata "test"))
	  "!{!0, !{!2, !0}, !\"test\"}")
    (frob '(:metadata (:metadata-ref 0) ((integer 32) 0) ((pointer (integer 8)) @global)
	    ((pointer (function (integer 64) ((integer 64)) :vararg-p nil)) @function)
	    (:metadata "str"))
	  "!{!0, i32 0, i8* @global, i64 (i64)* @function, !\"str\"}")
    ))
	  
  

(test llvm-identifier
  (is (string= "@FOO" (string (cg-llvm-parse 'llvm-identifier "@foo"))))
  (is (string= "foo" (string (cg-llvm-parse 'llvm-identifier "\"foo\""))))
  (is (string= (concatenate 'string (string (code-char 1)) "foo")
	       (string (cg-llvm-parse 'llvm-identifier "\"\\01foo\"")))))

(test function-declaration
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'function-declaration ,y)))))
    (frob '(declare @foo "(...)" :return-type-placeholder) "declare i32 @foo (...)")
    (frob '(declare @foo "(...)"
	    (:linkage :private)
	    (:unnamed-addr t)
	    :return-type-placeholder)
	  "declare private unnamed_addr i32 @foo (...)")))

(test thread-local
  (is (equal '(:thread-local t) (cg-llvm-parse 'thread-local "thread_local")))
  (is (equal '(:thread-local :initialexec) (cg-llvm-parse 'thread-local "thread_local(initialexec)")))
  (signals error (cg-llvm-parse 'thread-local "thread_local(adsf)")))

(test aliases
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'alias ,y)))))
    (frob '(cg-llvm::alias @foo @bar (integer 8)) "@foo = alias i8 @bar")
    (frob '(cg-llvm::alias @foo @bar (integer 16)
	    (:linkage :private)
	    (:visibility :default)
	    (:dll-storage-class :dllimport)
	    (:thread-local :localexec)
	    (:unnamed-addr t))
	  "@foo = private default dllimport thread_local(localexec) unnamed_addr alias i16 @bar")))

  


(test comdat-toplevel
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'comdat-toplevel ,y)))))
    (frob '(cg-llvm::comdat "foo" :any) "$foo = comdat any")
    (frob '(cg-llvm::comdat "bar" :largest) "$bar = comdat  largest")))

(test inline-assembly
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'inline-assembly ,y)))))
    (frob '(cg-llvm::asm "inline asm goes here") "module asm \"inline asm goes here\"")
    (frob '(cg-llvm::asm "more can go here") "module asm \"more can go here\"")))
  
(test target-triple
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'target-triple ,y)))))
    (frob '(cg-llvm::target-triple "x86_64" "apple" "macosx10.7.0")
	  "target triple = \"x86_64-apple-macosx10.7.0\"")))


(test parsing-terminating-instructions
  (macrolet ((frob (x y)
	       `(is (equal ,x (cg-llvm-parse 'terminator-instruction ,y)))))
    (frob '(cg-llvm::ret ((integer 32) 5)) "ret i32 5")
    (frob '(cg-llvm::ret :void) "ret void")
    (frob '(cg-llvm::ret ((cg-llvm::struct ((integer 32) (integer 8)) :packed-p nil)
			  (((integer 32) 4) ((integer 8) 2))))
	  "ret { i32, i8 } { i32 4, i8 2 }")
    (frob '(cg-llvm::br ((integer 1) %cond) (cg-llvm::label +%-if-equal) (cg-llvm::label +%-if-unequal))
	  "br i1 %cond, label %IfEqual, label %IfUnequal")
    (frob '(cg-llvm::resume ((CG-LLVM::STRUCT ((POINTER (INTEGER 8)) (INTEGER 32)) :PACKED-P NIL)
			     %EXN))
	  "resume { i8*, i32 } %exn")
    (frob '(cg-llvm::unreachable) "unreachable")
    ))

(test parsing-binop-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (cg-llvm-parse ',y ,z)))))
    (frob (add (integer 32) 4 %var (:nuw t) (:nsw t)) add-instruction "add nuw nsw i32 4, %var")
    (frob (add (integer 32) 4 %var (:nuw t) (:nsw t)) add-instruction "add nsw nuw i32 4, %var")
    (frob (add (integer 32) 4 %var) add-instruction "add i32 4, %var")
    (frob (sub (integer 32) 4 %var) sub-instruction "sub i32 4, %var")
    (frob (sub (integer 32) 0 %val) sub-instruction "sub i32 0, %val")
    (frob (mul (integer 32) 4 %var) mul-instruction "mul i32 4, %var")
    (frob (fadd (float 32 16) 4.0 %var) fadd-instruction "fadd float 4.0, %var")
    (frob (fadd (float 32 16) 4.0 %var (:ninf t) (:arcp t))
	  fadd-instruction "fadd arcp ninf float 4.0, %var")
    (frob (shl (vector (integer 32) 2) (((integer 32) 1) ((integer 32) 1)) (((integer 32) 1) ((integer 32) 2)))
	  shl-instruction "shl <2 x i32> < i32 1, i32 1>, < i32 1, i32 2>")
    ))
  
(test aggregate-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (cg-llvm-parse ',y ,z)))))
    (frob (extractelement ((vector (integer 32) 4) %vec) ((integer 32) 0))
	  extractelement-instruction "extractelement <4 x i32> %vec, i32 0")
    (frob (insertelement ((vector (integer 32) 4) %vec) ((integer 32) 1) ((integer 32) 0))
	  insertelement-instruction "insertelement <4 x i32> %vec, i32 1, i32 0")
    (frob (shufflevector ((vector (integer 32) 4) %v1)
			 ((vector (integer 32) 4) %v2)
			 ((vector (integer 32) 4)
			  (((integer 32) 0) ((integer 32) 4) ((integer 32) 1) ((integer 32) 5))))
	  shufflevector-instruction
	  "shufflevector <4 x i32> %v1, <4 x i32> %v2,
                         <4 x i32> <i32 0, i32 4, i32 1, i32 5>")
    (frob (extractvalue ((cg-llvm::struct ((integer 32) (float 32 16)) :packed-p nil) %agg) 0)
	  extractvalue-instruction
	  "extractvalue {i32, float} %agg, 0")
    (frob (insertvalue ((cg-llvm::struct ((integer 32) (float 32 16)) :packed-p nil) :undef)
		       ((integer 32) 1) 0)
	  insertvalue-instruction
	  "insertvalue {i32, float} undef, i32 1, 0")
    (frob (insertvalue ((cg-llvm::struct ((integer 32) (float 32 16)) :packed-p nil) %agg1)
		       ((float 32 16) %val) 1)
	  insertvalue-instruction
	  "insertvalue {i32, float} %agg1, float %val, 1")
    (frob (insertvalue ((cg-llvm::struct ((integer 32)
					  (cg-llvm::struct ((float 32 16)) :packed-p nil))
					 :packed-p nil)
			:undef)
		       ((float 32 16) %val) 1 0)
	  insertvalue-instruction
	  "insertvalue {i32, {float}} undef, float %val, 1, 0")
    ))

(test memory-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (cg-llvm-parse ',y ,z)))))
    (frob (alloca (integer 32)) alloca-instruction "alloca i32")
    (frob (alloca (integer 32) (:nelts ((integer 32) 4)))
	  alloca-instruction "alloca i32, i32 4")
    (frob (alloca (integer 32) (:nelts ((integer 32) 4)) (:align 1024))
	  alloca-instruction "alloca i32, i32 4, align 1024")
    (frob (alloca (integer 32) (:align 1024))
	  alloca-instruction "alloca i32, align 1024")
    (frob (load (:atomic t) (integer 32) ((pointer (integer 32)) %x) (:ordering :unordered) (:align 256))
	  load-instruction "load atomic i32, i32* %x unordered, align 256")
    (frob (load (integer 32) ((pointer (integer 32)) %ptr))
	  load-instruction "load i32, i32* %ptr")
    (frob (store ((integer 32) 3) ((pointer (integer 32)) %ptr))
    	  store-instruction "store i32 3, i32* %ptr")
    (frob (fence (:ordering :acquire))
	  fence-instruction "fence acquire")
    (frob (fence (:singlethread t) (:ordering :seq-cst))
	  fence-instruction "fence singlethread seq_cst")
    (frob (cmpxchg ((pointer (integer 32)) %ptr)
		   ((integer 32) %cmp)
		   ((integer 32) %squared)
		   (:success-ord :acq-rel) (:failure-ord :monotonic))
    	  cmpxchg-instruction "cmpxchg i32* %ptr, i32 %cmp, i32 %squared acq_rel monotonic")
    ))
  
(test conversion-instructions
  (macrolet ((frob (x y)
	       `(is (equal ',x (cg-llvm-parse 'conversion-instruction ,y)))))
    (frob (trunc 257 (integer 32) (integer 8)) "trunc i32 257 to i8")
    (frob (trunc 123 (integer 32) (integer 1)) "trunc i32 123 to i1")
    (frob (trunc 122 (integer 32) (integer 1)) "trunc i32 122 to i1")
    (frob (trunc (((integer 16) 8) ((integer 16) 7))
		 (vector (integer 16) 2)
		 (vector (integer 8) 2))
	  "trunc <2 x i16> <i16 8, i16 7> to <2 x i8>")
    (frob (bitcast 255 (integer 8) (integer 8)) "bitcast i8 255 to i8")
    (frob (bitcast %x (pointer (integer 32)) (pointer (integer 32)))
	  "bitcast i32* %x to i32*")
    (frob (bitcast *%v (vector (integer 32) 2) (integer 64))
	  "bitcast <2 x i32> %V to i64")
    (frob (bitcast *%v (vector (pointer (integer 32)) 2) (vector (pointer (integer 64)) 2))
	  "bitcast <2 x i32*> %V to <2 x i64*>")
    ))
  

    
(test misc-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (cg-llvm-parse ',y ,z)))))
    (frob (phi (integer 32) (0 +%-loop-header)) phi-instruction "phi i32 [ 0, %LoopHeader ]")
    (frob (phi (integer 32) (0 +%-loop-header) (%nextindvar +%-loop))
	  phi-instruction "phi i32 [ 0, %LoopHeader ], [ %nextindvar, %Loop ]")
    (frob (select ((integer 1) "true") ((integer 8) 17) ((integer 8) 42))
	  select-instruction "select i1 true, i8 17, i8 42")
    (frob (va-arg ((pointer (integer 8)) %ap2) (integer 32))
	  va-arg-instruction "va_arg i8* %ap2, i32")
    (frob (icmp :eq (integer 32) 4 5) icmp-instruction "icmp eq i32 4, 5")
    (frob (icmp :ne (pointer (float 32 16)) *%x *%x) icmp-instruction "icmp ne float* %X, %X")
    (frob (fcmp :oeq (float 32 16) 4.0 5.0) fcmp-instruction "fcmp oeq float 4.0, 5.0")
    ))


(test basic-blocks
  (macrolet ((frob (x y)
	       `(is (equal ',x (cg-llvm-parse 'basic-block ,y)))))
    (frob (block (cg-llvm::ret ((integer 32) 3))) "ret i32 3")
    (frob (block (:label "end") (cg-llvm::ret ((integer 32) 3))) "end: ret i32 3")))