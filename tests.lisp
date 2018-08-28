(in-package :cl-user)

(defpackage :cg-llvm-tests
  (:use :cl :cg-llvm :fiveam :iterate ;;:cl-read-macro-tokens :defmacro-enhance
	:cg-llvm-symbols)
  (:shadowing-import-from #:cg-llvm #:join)
  (:export #:run-tests))

(in-package :cg-llvm-tests)

(defun pairs (lst)
  (assert (evenp (length lst)))
  (let (tmp res)
    (iter (for i from 0)
	  (for elt in lst)
	  (push elt tmp)
	  (when (oddp i)
	    (push (nreverse tmp) res)
	    (setf tmp nil)))
    (nreverse res)))

(cl-interpol:enable-interpol-syntax)

(in-suite* cg-llvm)

(defun parse-fun-for-test (exp text)
  (emit-lisp-repr
   (cg-llvm:cg-llvm-parse
    (find-symbol (string exp)
		 (find-package "CG-LLVM"))
    text)))

(defun noop (n)
  n)

(defun run-tests ()
  (let ((results (run 'cg-llvm)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(defmacro simple-tests (name &rest cases)
  `(progn ,@(mapcar (lambda (x)
		      `(is (equal ',(car x) (parse-fun-for-test ',name ,(cadr x)))))
		    (pairs cases))))

(defmacro elt-test (name &rest cases)
  (destructuring-bind (test-name name) (if (consp name)
					   name
					   (list name (intern (subseq (string name)
								      0
								      (1- (length (string name)))))))
    `(test ,test-name
       (simple-tests ,name
		     ,@cases))))

(test basic
  (is (equal 'void (noop (parse-fun-for-test 'llvm-type "void"))))
  ;;FIXME::use real layout, not fragile hack with "second"
  (is (equal 24 (second (parse-fun-for-test 'integer-type "i24"))))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (noop (parse-fun-for-test 'float-type ,expr))))))
    (frob (float 64 32)  "double")
    (frob (float 16 8) "half")
    (frob (float 32 16) "float")
    (frob (float 128 112) "fp128")
    (frob (float 80 40) "x86_fp80")
    (frob (float 128 64) "ppc_fp128"))
  (is (equal 'x86-mmx (noop (parse-fun-for-test 'x86-mmx "x86_mmx"))))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (noop (parse-fun-for-test 'vector ,expr))))))
    (frob (vector (integer 32) 4) "<4 x i32>")
    (frob (vector (float 32 16) 8) "<8 x float>")
    (frob (vector (integer 64) 2) "<2 x i64>")
    (frob (vector (pointer (integer 64)) 4) "<4 x i64*>"))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (noop (parse-fun-for-test 'array ,expr))))))
    (frob (array (array (integer 32) 4) 3) "[3 x [4 x i32]]")
    (frob (array (array (float 32 16) 10) 12) "[12 x [10 x float]]")
    (frob (array (array (array (integer 16) 4) 3) 2) "[2 x [3 x [4 x i16]]]"))
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (noop (parse-fun-for-test 'struct ,expr))))))
    (frob (struct ((integer 32) (integer 16) (integer 8)) :packed-p nil)
	  "{ i32, i16, i8 }")
    (frob (struct ((integer 32) (pointer (float 32 16))) :packed-p nil)
	  "{ i32, float * }")
    (frob (struct ((integer 8) (integer 32)) :packed-p t)
	  "<{ i8, i32 }>"))
  (is (equal 'opaque (noop (parse-fun-for-test 'struct "opaque"))))
  (is (equal '(named "%struct.ST")
	     (noop (parse-fun-for-test 'llvm-type "%struct.ST"))))
  (is (equal '(pointer (named "%struct.ST"))
	     (noop (parse-fun-for-test 'llvm-type "%struct.ST*")))))
    
(test more-complicated
  (macrolet ((frob (theor expr)
	       `(is (equal ',theor (noop (parse-fun-for-test 'llvm-type ,expr))))))
    (frob (pointer (integer 32) 5) #?"i32\naddrspace(5)*")
    (frob (pointer (function (integer 32) ((pointer (integer 32))) :vararg-p nil))
	  "i32 (i32 *) *")
    (frob (function (integer 32) ((integer 32)) :vararg-p nil)
	  "i32 (i32)")
    (frob (function (integer 1) ((integer 2) (integer 3) (integer 4)) :vararg-p nil)
	  "i1 (i2, i3, i4)")
    (frob (function (integer 32) ((pointer (integer 8))) :vararg-p t)
	  "i32 (i8*, ...)")
    (frob (function (struct ((integer 32) (integer 32)) :packed-p nil) ((integer 32)) :vararg-p nil)
	  "{i32, i32} (i32)")))

(test parsing-of-s-exps
  (macrolet ((frob (x)
	       `(is (equal ',x (emit-lisp-repr (parse-lisp-repr ',x))))))
    (frob (integer 32))
    (frob (pointer (integer 32) 5))
    (frob (pointer (function (integer 32) ((pointer (integer 32))) :vararg-p nil)))
    (frob (function (integer 32) ((integer 32)) :vararg-p nil))
    (frob (function (integer 1) ((integer 2) (integer 3) (integer 4)) :vararg-p nil))
    (frob (function (integer 32) ((pointer (integer 8))) :vararg-p t))
    (frob (function (struct ((integer 32) (integer 32)) :packed-p nil) ((integer 32)) :vararg-p nil))
    (frob void)
    (frob (float 64 32))
    (frob x86-mmx)
    (frob (vector (integer 32) 4))
    (frob (vector (pointer (integer 64)) 4))
    (frob (array (array (array (integer 16) 4) 3) 2))
    (frob (struct ((integer 32) (integer 16) (integer 8)) :packed-p nil))
    (frob (struct ((integer 32) (pointer (float 32 16))) :packed-p nil))
    (frob (struct ((integer 8) (integer 32)) :packed-p t))
    (frob opaque)))

#+nil
(test emitting-of-text
  (macrolet ((frob (x &optional y)
	       `(is (equal ,x (emit-text-repr (parse-fun-for-test 'llvm-type ,(or y x)))))))
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

#+nil
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
;; 				(let ((*context* :function))
;; 				  (llvm-return ,y)))))))
;;     (frob "ret void" :void)
;;     (frob "ret i14 42" '(42 (integer 14)))
;;     (frob "ret i14 42" '(42 "i14")))
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
;; 				  (unconditional-branch ,y)))))))
;;     (frob "br label asdf" 'asdf)
;;     (frob "br label %Asdf" '+%asdf)
;;     (frob "br label ASDF" '*asdf))
;;   (macrolet ((frob (x y z w)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
;; 				  (conditional-branch ,y ,z ,w)))))))
;;     (frob "br i1 %cond, label %IfEqual, label %IfUnequal" '(%cond "i1") '+%if-equal '+%if-unequal))
;;   (macrolet ((frob (x &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
;; 				  (switch ,@args)))))))
;;     (frob "switch i32 %Val, label %truedest [i32 0, label %falsedest]"
;; 	  '(+%val  "i32") '%truedest 0 '%falsedest)
;;     (frob "switch i32 0, label %dest []"
;; 	  0 '%dest)
;;     (frob "switch i32 %val, label %otherwise [i32 0, label %onzero i32 1, label %onone i32 2, label %ontwo]"
;; 	  '(%val "i32") '%otherwise 0 '%onzero 1 '%onone 2 '%ontwo))
;;   (macrolet ((frob (x y &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
;; 				  (indirect-branch ,y ,@args)))))))
;;     (frob "indirectbr i8* %Addr, [label %bb1, label %bb2, label %bb3]"
;; 	  '(+%addr "i8*") '%bb1 '%bb2 '%bb3))
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
;; 				  (resume ,y)))))))
;;     (frob "resume {i8*, i32} %exn" '(%exn "{i8*, i32}")))
;;   (is (equal "unreachable" (with-output-to-string (*standard-output*) (unreachable))))
;;   (macrolet ((frob (x &rest args)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(let ((*context* :function))
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
;; 				(let ((*context* :function))
;; 				  (reset-tmp-var-counts)
;; 				  ,y))))))
;;     (frob (join "~%"
;; 			 "%tmpadd1 = add i32 1, 2"
;; 			 "%tmpadd2 = add i32 3, 4"
;; 			 "%tmpadd3 = add i32 %tmpadd1, %tmpadd2"
;; 			 "ret i32 %tmpadd3")
;; 	  (llvm-return (add (add 1 2) (add 3 4))))))

;; (test vector-operations
;;   (macrolet ((frob (x y)
;; 	       `(is (equal ,x (with-output-to-string (*standard-output*)
;; 				(reset-tmp-var-counts)
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
;; 	  (let ((*context* :function))
;; 	    (llvm-return (shufflevector (mk-typed-value '(vector (integer 32) 8) '%v1)
;; 					(mk-typed-value '(vector (integer 32) 8) 'undef)
;; 					(mk-typed-value '(vector (integer 32) 4) "<i32 0, i32 1, i32 2, i32 3>")))))
;;     (frob (join "~%"
;; 		(join " " #?"%tmpshufvec1 = shufflevector <4 x i32> %v1, <4 x i32> %v2,"
;; 		      "<8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")
;; 		"ret <8 x i32> %tmpshufvec1")
;; 	  (let ((*context* :function))
;; 	    (llvm-return
;; 	     (shufflevector (mk-typed-value '(vector (integer 32) 4) '%v1)
;; 			    (mk-typed-value '(vector (integer 32) 4) '%v2)
;; 			    (mk-typed-value '(vector (integer 32) 8)
;; 					    "<i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>")))))))
  
	  
;; (defmacro frob-context (x y)
;;   `(is (equal ,x (with-output-to-string (*standard-output*)
;; 		   (reset-tmp-var-counts)
;; 		   (let ((*context* :function))
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
	     (parse-fun-for-test 'array-constant "[ 3 x i8 ] [ i8 1, i8 2, i8 3 ]")))
  (is (equal '((array (integer 8) 3) :zero-initializer)
	     (parse-fun-for-test 'zero-init-constant "[ 3 x i8 ] zeroinitializer")))
  (is (equal '((array (integer 8) 3) (((integer 8) 97) ((integer 8) 115) ((integer 8) 100) ((integer 8) 102)))
	     (parse-fun-for-test 'string-constant "[ 3 x i8 ] c\"asdf\""))))

(elt-test llvm-constants
	  ((integer 8) 1) "i8 1"
	  ((integer 32) 42) "i32 42"
	  ((array (pointer (integer 32)) 2)
	   (((pointer (integer 32)) "@X")
	    ((pointer (integer 32)) "@Y")))
	  "[2 x i32*] [ i32* @X, i32* @Y ]"
	  ((pointer (array (integer 8) 14)) "@.str") "[14 x i8]* @.str"
	  )

(elt-test metadata-constants
	  (meta-id 24) "!24"
	  (meta-node (meta-id 4) (meta-id 3)) "!{!4, !3}"
	  (meta-node (meta-str #?"test\0") ((integer 32) 10)) "!{ !\"test\\00\", i32 10}"
	  (meta-node (meta-id 0) (meta-node (meta-id 2) (meta-id 0)) (meta-str "test"))
	  "!{!0, !{!2, !0}, !\"test\"}"
	  (meta-node (meta-id 0) ((integer 32) 0) ((pointer (integer 8)) "@global")
		     ((pointer (function (integer 64) ((integer 64)) :vararg-p nil)) "@function")
		     (meta-str "str"))
	  "!{!0, i32 0, i8* @global, i64 (i64)* @function, !\"str\"}")

(elt-test metadata-entrys
	  (= (meta-id 0) (meta-node (meta-str #?"test\0") ((integer 32) 10)) (:distinct t))
	  "!0 = distinct !{!\"test\\00\", i32 10}")

(test llvm-identifier
  (is (equal '"@foo" (parse-fun-for-test 'llvm-identifier "@foo")))
  (is (equal '"@foo" (parse-fun-for-test 'llvm-identifier "@\"foo\"")))
  (is (equal (concatenate 'string "%" (string (code-char 1)) "foo")
	     (string (parse-fun-for-test 'llvm-identifier "%\"\\01foo\"")))))

(elt-test function-declarations
	  (declare "@foo" (((integer 32) "%a") ((pointer (integer 8)) "%b")) ((integer 32)))
	  "declare i32 @foo (i32 %a , i8* %b)"
	  (declare "@foo" (((integer 32) "%a" (:attrs (:zeroext))) ((pointer (integer 8)) "%b"))
		   ((integer 32))
		   (:linkage :private)
		   (:unnamed-addr t))
	  "declare private unnamed_addr i32 @foo (i32 zeroext %a , i8* %b)")


(test thread-local
  (is (equal '(:thread_local t) (parse-fun-for-test 'thread-local "thread_local")))
  (is (equal '(:thread_local :initialexec) (parse-fun-for-test 'thread-local "thread_local(initialexec)")))
  (signals error (parse-fun-for-test 'thread-local "thread_local(adsf)")))

(test aliases
  (macrolet ((frob (x y)
	       `(is (equal ,x (parse-fun-for-test 'alias ,y)))))
    (frob '(alias "@foo" "@bar" (integer 8)) "@foo = alias i8 @bar")
    (frob '(alias "@foo" "@bar" (integer 16)
	    (:linkage :private)
	    (:visibility :default)
	    (:dll-storage-class :dllimport)
	    (:thread_local :localexec)
	    (:unnamed-addr t))
	  "@foo = private default dllimport thread_local(localexec) unnamed_addr alias i16 @bar")))

  
(elt-test comdat-toplevels
  (comdat "foo" :any) "$foo = comdat any"
  (comdat "bar" :largest) "$bar = comdat  largest")

(elt-test inline-assemblys
  (asm "inline asm goes here") "module asm \"inline asm goes here\""
  (asm "more can go here") "module asm \"more can go here\"")

(elt-test terminator-instructions
  (ret ((integer 32) 5)) "ret i32 5"
  (ret :void) "ret void"
  (ret ((struct ((integer 32) (integer 8)) :packed-p nil)
	(((integer 32) 4) ((integer 8) 2))))
  "ret { i32, i8 } { i32 4, i8 2 }"
  (br ((integer 1) "%cond") (label "%IfEqual") (label "%IfUnequal"))
  "br i1 %cond, label %IfEqual, label %IfUnequal"
  (resume ((STRUCT ((POINTER (INTEGER 8)) (INTEGER 32)) :PACKED-P NIL)
	   "%exn"))
  "resume { i8*, i32 } %exn"
  (unreachable) "unreachable")

(test parsing-binop-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (parse-fun-for-test ',y ,z)))))
    (frob (add (integer 32) 4 "%var" (:nuw t) (:nsw t)) add-instruction "add nuw nsw i32 4, %var")
    (frob (add (integer 32) 4 "%var" (:nuw t) (:nsw t)) add-instruction "add nsw nuw i32 4, %var")
    (frob (add (integer 32) 4 "%var") add-instruction "add i32 4, %var")
    (frob (sub (integer 32) 4 "%var") sub-instruction "sub i32 4, %var")
    (frob (sub (integer 32) 0 "%val") sub-instruction "sub i32 0, %val")
    (frob (mul (integer 32) 4 "%var") mul-instruction "mul i32 4, %var")
    (frob (fadd (float 32 16) 4.0 "%var") fadd-instruction "fadd float 4.0, %var")
    (frob (fadd (float 32 16) 4.0 "%var" (:ninf t) (:arcp t))
	  fadd-instruction "fadd arcp ninf float 4.0, %var")
    (frob (shl (vector (integer 32) 2) (((integer 32) 1) ((integer 32) 1)) (((integer 32) 1) ((integer 32) 2)))
	  shl-instruction "shl <2 x i32> < i32 1, i32 1>, < i32 1, i32 2>")
    ))

(test aggregate-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (parse-fun-for-test ',y ,z)))))
    (frob (extractelement ((vector (integer 32) 4) "%vec") ((integer 32) 0))
	  extractelement-instruction "extractelement <4 x i32> %vec, i32 0")
    (frob (insertelement ((vector (integer 32) 4) "%vec") ((integer 32) 1) ((integer 32) 0))
	  insertelement-instruction "insertelement <4 x i32> %vec, i32 1, i32 0")
    (frob (shufflevector ((vector (integer 32) 4) "%v1")
			 ((vector (integer 32) 4) "%v2")
			 ((vector (integer 32) 4)
			  (((integer 32) 0) ((integer 32) 4) ((integer 32) 1) ((integer 32) 5))))
	  shufflevector-instruction
	  "shufflevector <4 x i32> %v1, <4 x i32> %v2,
                         <4 x i32> <i32 0, i32 4, i32 1, i32 5>")
    (frob (extractvalue ((struct ((integer 32) (float 32 16)) :packed-p nil) "%agg") 0)
	  extractvalue-instruction
	  "extractvalue {i32, float} %agg, 0")
    (frob (insertvalue ((struct ((integer 32) (float 32 16)) :packed-p nil) :undef)
		       ((integer 32) 1) 0)
	  insertvalue-instruction
	  "insertvalue {i32, float} undef, i32 1, 0")
    (frob (insertvalue ((struct ((integer 32) (float 32 16)) :packed-p nil) "%agg1")
		       ((float 32 16) "%val") 1)
	  insertvalue-instruction
	  "insertvalue {i32, float} %agg1, float %val, 1")
    (frob (insertvalue ((struct ((integer 32)
					  (struct ((float 32 16)) :packed-p nil))
					 :packed-p nil)
			:undef)
		       ((float 32 16) "%val") 1 0)
	  insertvalue-instruction
	  "insertvalue {i32, {float}} undef, float %val, 1, 0")
    ))

(defmacro with-frob1 (name &body body)
  `(macrolet ((frob1 (x y)
		`(frob (,',name ,@x)
		       ,',(intern #?"$((string name))-INSTRUCTION")
		       ,(concatenate 'string ,(cg-llvm::%stringify-symbol name) " " y))))
     ,@body))

(test simple-memory-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (parse-fun-for-test ',y ,z)))))
    (with-frob1 alloca
      (frob1 ((integer 32)) "i32")
      (frob1 ((integer 32) (:nelts ((integer 32) 4))) "i32, i32 4")
      (frob1 ((integer 32) (:nelts ((integer 32) 4)) (:align 1024)) "i32, i32 4, align 1024")
      (frob1 ((integer 32) (:align 1024)) "i32, align 1024"))
    (with-frob1 load
      (frob1 ((:atomic t) (integer 32) ((pointer (integer 32)) "%x")
	      (:ordering :unordered) (:align 256))
	     "atomic i32, i32* %x unordered, align 256")
      (frob1 ((integer 32) ((pointer (integer 32)) "%ptr")) "i32, i32* %ptr"))
    (with-frob1 store
      (frob1 (((integer 32) 3) ((pointer (integer 32)) "%ptr")) "i32 3, i32* %ptr"))
    (with-frob1 fence
      (frob1 ((:ordering :acquire)) "acquire")
      (frob1 ((:singlethread t) (:ordering :seq_cst)) "singlethread seq_cst"))
    (with-frob1 cmpxchg
      (frob1 (((pointer (integer 32)) "%ptr")
	      ((integer 32) "%cmp")
	      ((integer 32) "%squared")
	      (:success-ord :acq_rel) (:failure-ord :monotonic))
	     "i32* %ptr, i32 %cmp, i32 %squared acq_rel monotonic"))
    (with-frob1 atomicrmw
      (frob1 (:add ((pointer (integer 32)) "%ptr") ((integer 32) 1) (:ordering :acquire))
	     "add i32* %ptr, i32 1 acquire"))))

;; (test complex-memory-instructions
;;   (macrolet ((frob (x y z)
;; 	       `(is (equal ',x (parse-fun-for-test ',y ,z)))))
;;     (with-frob1 getelementptr
;;       (frob1 nil "inbounds %struct.ST, %struct.ST* %s, i64 1, i32 2, i32 1, i64 5, i64 13"))))

    ;;   (frob1 nil "%struct.ST, %struct.ST* %s, i32 1")
    ;;   (frob1 nil "%struct.ST, %struct.ST* %t1, i32 0, i32 2")
    ;;   (frob1 nil "%struct.RT, %struct.RT* %t2, i32 0, i32 1")
    ;;   (frob1 nil "[10 x [20 x i32]], [10 x [20 x i32]]* %t3, i32 0, i32 5")
    ;;   (frob1 nil "[20 x i32], [20 x i32]* %t4, i32 0, i32 13")
    ;;   (frob1 nil "{i32, [12 x i8]}, {i32, [12 x i8]}* %saptr, i64 0, i32 1")
    ;;   (frob1 nil "{i32, <2 x i8>}, {i32, <2 x i8>}* %svptr, i64 0, i32 1, i32 1")
    ;;   (frob1 nil "[12 x i8], [12 x i8]* %aptr, i64 0, i32 1")
    ;;   (frob1 nil "[10 x i32], [10 x i32]* @arr, i16 0, i16 0")
    ;;   (frob1 nil "i8, <4 x i8*> %ptrs, <4 x i64> %offsets"))
    ;; ))

(test conversion-instructions
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'conversion-instruction ,y)))))
    (frob (trunc 257 (integer 32) (integer 8)) "trunc i32 257 to i8")
    (frob (trunc 123 (integer 32) (integer 1)) "trunc i32 123 to i1")
    (frob (trunc 122 (integer 32) (integer 1)) "trunc i32 122 to i1")
    (frob (trunc (((integer 16) 8) ((integer 16) 7))
		 (vector (integer 16) 2)
		 (vector (integer 8) 2))
	  "trunc <2 x i16> <i16 8, i16 7> to <2 x i8>")
    (frob (bitcast 255 (integer 8) (integer 8)) "bitcast i8 255 to i8")
    (frob (bitcast "%x" (pointer (integer 32)) (pointer (integer 32)))
	  "bitcast i32* %x to i32*")
    (frob (bitcast "%V" (vector (integer 32) 2) (integer 64))
	  "bitcast <2 x i32> %V to i64")
    (frob (bitcast "%V" (vector (pointer (integer 32)) 2) (vector (pointer (integer 64)) 2))
	  "bitcast <2 x i32*> %V to <2 x i64*>")
    ))

(test misc-instructions
  (macrolet ((frob (x y z)
	       `(is (equal ',x (parse-fun-for-test ',y ,z)))))
    (frob (phi (integer 32) (0 "%LoopHeader")) phi-instruction "phi i32 [ 0, %LoopHeader ]")
    (frob (phi (integer 32) (0 "%LoopHeader") ("%nextindvar" "%Loop"))
	  phi-instruction "phi i32 [ 0, %LoopHeader ], [ %nextindvar, %Loop ]")
    (frob (select ((integer 1) "true") ((integer 8) 17) ((integer 8) 42))
	  select-instruction "select i1 true, i8 17, i8 42")
    (frob (va_arg ((pointer (integer 8)) "%ap2") (integer 32))
	  va_arg-instruction "va_arg i8* %ap2, i32")
    (frob (icmp :eq (integer 32) 4 5) icmp-instruction "icmp eq i32 4, 5")
    (frob (icmp :ne (pointer (float 32 16)) "%X" "%X") icmp-instruction "icmp ne float* %X, %X")
    (frob (fcmp :oeq (float 32 16) 4.0 5.0) fcmp-instruction "fcmp oeq float 4.0, 5.0")
    (frob (call (integer 32) "@test" (((integer 32) "%argc")))
	  call-instruction "call i32 @test(i32 %argc)")
    (frob (call (pointer (function (integer 32)
				   ((pointer (integer 8)))
				   :vararg-p t))
		"@printf"
		(((integer 32) 0) ((integer 32) 0)))
	  call-instruction "call i32 (i8*, ...)* @printf(i32 0, i32 0)")
    (frob (call (pointer (function (integer 32)
				   ((pointer (integer 8)))
				   :vararg-p t))
		"@printf"
		(((pointer (integer 8)) "%msg") ((integer 32) 12) ((integer 8) 42)))
	  call-instruction "call i32 (i8*, ...)* @printf(i8* %msg, i32 12, i8 42)")
    (frob (call (pointer (function (integer 32) ((pointer (integer 8))) :vararg-p t)) "@printf"
		(((pointer (integer 8))
		  (getelementptr ((pointer (array (integer 8) 14)) "@.str")
				 ((integer 32) 0) ((integer 32) 0) (:inbounds t)))))
	  call-instruction "call i32 (i8*, ...)* @printf(i8* getelementptr inbounds
                                                       ([14 x i8]* @.str, i32 0, i32 0))")
    (frob (call (integer 32) "@foo" nil (:tail :tail))
	  call-instruction "tail call i32 @foo()")
    (frob (call (integer 32) "@foo" nil (:cconv :fastcc) (:tail :tail))
	  call-instruction "tail call fastcc i32 @foo()")
    (frob (call void "%foo" (((integer 8) 97 (:attrs :signext))))
	  call-instruction "call void %foo(i8 97 signext)")
    ;; (frob nil ; types can be specified through local variables, that makes parsing
    ;; 	  ; context-sensitive
    ;; 	  call-instruction "call %struct.A @foo()")

    (frob (call void "@foo" nil (:fun-attrs (:noreturn)))
	  call-instruction "call void @foo() noreturn")
    (frob (call (integer 32) "@bar" nil (:return-attrs (:zeroext)))
	  call-instruction "call zeroext i32 @bar()"))
  )

(test block-labels
  (is (equal '"%entry" (parse-fun-for-test 'block-label "entry:")))
  (is (equal '"%entry" (parse-fun-for-test 'block-label "\"entry\":"))))

(test basic-blocks
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'basic-block ,y)))))
    (frob (block (ret ((integer 32) 3))) "ret i32 3")
    (frob (block (:label "%end") (ret ((integer 32) 3))) "end: ret i32 3")
    (frob (block (:label "%entry")
	    (= "%addtmp" (fadd (float 64 32) 4.0 5.0))
	    (ret ((float 64 32) "%addtmp")))
	  #?"entry:
  %addtmp = fadd double 4.000000e+00, 5.000000e+00
  ret double %addtmp")
    (frob (block (:label "%entry")
	    (= "%multmp" (fmul (float 64 32) "%a" "%a"))
	    (= "%multmp1" (fmul (float 64 32) 2.0 "%a"))
	    (= "%multmp2" (fmul (float 64 32) "%multmp1" "%b"))
	    (= "%addtmp" (fadd (float 64 32) "%multmp" "%multmp2"))
	    (= "%multmp3" (fmul (float 64 32) "%b" "%b"))
	    (= "%addtmp4" (fadd (float 64 32) "%addtmp" "%multmp3"))
	    (ret ((float 64 32) "%addtmp4")))
	  #?"entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double 2.000000e+00, %a
  %multmp2 = fmul double %multmp1, %b
  %addtmp = fadd double %multmp, %multmp2
  %multmp3 = fmul double %b, %b
  %addtmp4 = fadd double %addtmp, %multmp3
  ret double %addtmp4")
    (frob (block (:label "%entry")
	    (= "%calltmp" (call (float 64 32) "@foo" (((float 64 32) "%a") ((float 64 32) 4.0))))
	    (= "%calltmp1" (call (float 64 32) "@bar" (((float 64 32) 31337.0))))
	    (= "%addtmp" (fadd (float 64 32) "%calltmp" "%calltmp1"))
	    (ret ((float 64 32) "%addtmp")))
	  #?"entry:
  %calltmp = call double @foo(double %a, double 4.000000e+00)
  %calltmp1 = call double @bar(double 3.133700e+04)
  %addtmp = fadd double %calltmp, %calltmp1
  ret double %addtmp")
    (frob (block (:label "%entry")
	    (= "%calltmp" (call (float 64 32) "@cos" (((float 64 32) 1.234))))
	    (ret ((float 64 32) "%calltmp")))
	  #?"entry:
  %calltmp = call double @cos(double 1.234000e+00)
  ret double %calltmp")
    ))

(test function-definitions
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'function-definition ,y)))))
    (frob (define (float 64 32) "@" nil
	    (:body ((block (:label "%entry")
		      (= "%addtmp" (fadd (float 64 32) 4.0 5.0))
		      (ret ((float 64 32) "%addtmp"))))))
	  "define double @\"\"() {
entry:
        %addtmp = fadd double 4.000000e+00, 5.000000e+00
        ret double %addtmp
}")
    (frob (define (float 64 32) "@" nil
	    (:metadata (((meta-id "dbg") (meta-id 0))
			((meta-id "asdf") (meta-id 100))))
	    (:body ((block (:label "%entry")
		      (= "%addtmp" (fadd (float 64 32) 4.0 5.0))
		      (ret ((float 64 32) "%addtmp"))))))
	  "define double @\"\"() !dbg !0 !asdf !100 {
entry:
        %addtmp = fadd double 4.000000e+00, 5.000000e+00
        ret double %addtmp
}")
    (frob (define (integer 32) "@main" nil (:fun-attrs ((:group 0)))
		  (:body
		   ((block
			(= "%1"
			   (call (pointer (function (integer 32) ((pointer (integer 8))) :vararg-p t))
				 "@printf"
				 (((pointer (integer 8))
				   (getelementptr ((pointer (array (integer 8) 14))
						   "@.str") ((integer 32) 0) ((integer 32) 0)
						   (:inbounds t))))))
		      (ret ((integer 32) 0))))))
	  "define i32 @main() #0 {
             %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str, i32 0, i32 0))
             ret i32 0
           }")
    ))



(test target-datalayout
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'target-datalayout ,y)))))
    (frob (target-datalayout (:endianness :little) (:mangling :elf) (:integer 64 (:abi 64))
	    (:float 80 (:abi 128)) (:native-integers 8 16 32 64) (:stack 128))
	  "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"")))

(elt-test target-triples
  (target-triple (:ARCH "x86_64") (:VENDOR "pc") (:SYSTEM "linux") (:ENV "gnu"))
  "target triple = \"x86_64-pc-linux-gnu\""
  (target-triple (:ARCH "x86_64") (:VENDOR "pc") (:SYSTEM "linux"))
  "target triple = \"x86_64-pc-linux\"")
    
(test global-variable-definition
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'global-variable-definition ,y)))))
    (frob (:global-var "@G" (integer 32) nil (:linkage :external))
	  "@G = external global i32")
    (frob (:global-var "@G" (integer 32) 0 (:thread_local :initialexec) (:align 4))
	  "@G = thread_local(initialexec) global i32 0, align 4")
    (frob (:global-var "@G" (integer 32) 0 (:thread_local t) (:align 4))
	  "@G = thread_local global i32 0, align 4")
    (frob (:global-var "@G" (float 32 16) 1.0 (:addrspace 5) (:constant t) (:section "foo") (:align 4))
	  "@G = addrspace(5) constant float 1.0, section \"foo\", align 4")
    (frob (:global-var "@.str" (array (integer 8) 14)
		       (((integer 8) 72) ((integer 8) 101) ((integer 8) 108) ((integer 8) 108)
			((integer 8) 111) ((integer 8) 32) ((integer 8) 119) ((integer 8) 111)
			((integer 8) 114) ((integer 8) 108) ((integer 8) 100) ((integer 8) 33)
			((integer 8) 10) ((integer 8) 0))
		       (:linkage :private) (:unnamed-addr t) (:constant t) (:align 1))
	  "@.str = private unnamed_addr constant [14 x i8] c\"Hello world!\\0A\\00\", align 1")
    ))
	  
(test attribute-groups
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'attribute-group ,y)))))
    (frob (attributes 0 :alwaysinline (:alignstack 4))
	  "attributes #0 = { alwaysinline alignstack(4) }")
    (frob (attributes 1 "no-sse")
	  "attributes #1 = { \"no-sse\" }")
    (frob (ATTRIBUTES 0 :UWTABLE ("less-precise-fpmad" "false") ("no-frame-pointer-elim" "true")
			       "no-frame-pointer-elim-non-leaf" ("no-infs-fp-math" "false")
			       ("no-nans-fp-math" "false") ("stack-protector-buffer-size" "8")
			       ("unsafe-fp-math" "false") ("use-soft-float" "false"))
	  "attributes #0 = { uwtable \"less-precise-fpmad\"=\"false\"
 \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\"
 \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\"
 \"stack-protector-buffer-size\"=\"8\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }")
    (frob (ATTRIBUTES 1 ("less-precise-fpmad" "false") ("no-frame-pointer-elim" "true")
			       "no-frame-pointer-elim-non-leaf" ("no-infs-fp-math" "false")
			       ("no-nans-fp-math" "false") ("stack-protector-buffer-size" "8")
			       ("unsafe-fp-math" "false") ("use-soft-float" "false"))
	  "attributes #1 = { \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\"
 \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\"
 \"stack-protector-buffer-size\"=\"8\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }")
    ))

(test more-function-declarations
  (is (equal '(((pointer (integer 8))) :vararg) (parse-fun-for-test 'declfun-args "(i8*, ...)")))
  (is (equal '((:group 1)) (parse-fun-for-test 'parameter-attrs "#1")))
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'function-declaration ,y)))))
    (frob (declare "@printf"
		   (((pointer (integer 8))) :vararg)
		   ((integer 32))
		   (:fun-attrs ((:group 1))))
	  "declare i32 @printf(i8*, ...) #1")
    (frob (declare "@printf"
		   (((pointer (integer 8)) (:attrs (:noalias :nocapture))) :vararg)
		   ((integer 32)))
	  "declare i32 @printf(i8* noalias nocapture, ...)")
    (frob (declare "@atoi"
		   (((integer 8) (:attrs (:zeroext))))
		   ((integer 32)))
	  "declare i32 @atoi(i8 zeroext)")
    (frob (declare "@returns_signed_char"
		   nil
		   ((integer 8) (:attrs (:signext))))
	  "declare signext i8 @returns_signed_char()")
    (frob (declare "@puts"
		   (((pointer (integer 8)) (:attrs (:nocapture))))
		   ((integer 32))
		   (:fun-attrs (:nounwind)))
	  "declare i32 @puts(i8* nocapture) nounwind")
    (frob (declare "@foo"
		   (((pointer (integer 8))))
		   (void))
	  "declare void @foo(i8*)")
    (frob (declare "@getPointer"
		   (((pointer (integer 8))))
		   ((pointer (integer 8))))
	  "declare i8* @getPointer(i8*)")
    (frob (declare "@llvm.invariant.group.barrier"
		   (((pointer (integer 8))))
		   ((pointer (integer 8))))
	  "declare i8* @llvm.invariant.group.barrier(i8*)")
    ))

(test constant-expressions
  (macrolet ((frob (x y)
	       `(is (equal ',x (parse-fun-for-test 'constant-expression-value ,y)))))
    (frob (getelementptr ((pointer (array (integer 8) 14)) "@.str")
			 ((integer 32) 0) ((integer 32) 0)
			 (:inbounds t))
	  "getelementptr inbounds ([14 x i8]* @.str, i32 0, i32 0)")))

(elt-test funcall-args
	  (meta-id 0) "metadata !0"
	  (meta-node (meta-id 0) (meta-id 1) (meta-id 2)) "metadata !{!0, !1, !2}")

(elt-test fundef-metadatas
	  (((meta-id "dbg") (meta-id 0)) ((meta-id "asdf") (meta-id 100)))
	  "!dbg !0 !asdf !100")

(elt-test blockaddresss
	  (blockaddress "@foo" "%bar") "blockaddress(@foo, %bar )")

(elt-test (instructions-with-metadata nonfinal-statement)
	  (= "%indvar.next" (add (integer 64) "%indvar" 1 (:metadata (((meta-id "dbg")
								       (meta-id 21))))))
	  "%indvar.next = add i64 %indvar, 1, !dbg !21")

(elt-test llvm-comments
	  #\space ";   asdf")

(elt-test llvm-modules
	  (module
	   (target-datalayout (:endianness :little) (:mangling :elf) (:integer 64 (:abi 64))
			      (:float 80 (:abi 128)) (:native-integers 8 16 32 64) (:stack 128))
	   (target-triple (:arch "x86_64") (:vendor "unknown") (:system "linux") (:env "gnu"))
	   (:global-var "@.str" (array (integer 8) 13)
			(((integer 8) 72) ((integer 8) 101) ((integer 8) 108) ((integer 8) 108)
			 ((integer 8) 111) ((integer 8) 32) ((integer 8) 119) ((integer 8) 111)
			 ((integer 8) 114) ((integer 8) 108) ((integer 8) 100) ((integer 8) 33) ((integer 8) 0))
			(:linkage :private) (:unnamed-addr t) (:constant t) (:align 1))
	   (define (integer 32) "@main" nil (:fun-attrs ((:group 0)))
		   (:body
		    ((block (:label "%entry")
		       (= "%retval" (alloca (integer 32) (:align 4)))
		       (store ((integer 32) 0) ((pointer (integer 32)) "%retval"))
		       (= "%call"
			  (call (pointer (function (integer 32) ((pointer (integer 8))) :vararg-p t)) "@printf"
				(((pointer (integer 8)) (getelementptr ((pointer (array (integer 8) 13)) "@.str")
								       ((integer 32) 0) ((integer 32) 0)
								       (:inbounds t))))))
		       (ret ((integer 32) 0))))))
	   (declare "@printf"
		    (((pointer (integer 8))) :vararg)
		    ((integer 32))
		    (:fun-attrs ((:group 1))))
	   (attributes 0 :nounwind :uwtable ("less-precise-fpmad" "false") ("no-frame-pointer-elim" "true")
		       "no-frame-pointer-elim-non-leaf" ("no-infs-fp-math" "false")
		       ("no-nans-fp-math" "false") ("stack-protector-buffer-size" "8")
		       ("unsafe-fp-math" "false") ("use-soft-float" "false"))
	   (attributes 1 ("less-precise-fpmad" "false") ("no-frame-pointer-elim" "true")
		       "no-frame-pointer-elim-non-leaf" ("no-infs-fp-math" "false")
		       ("no-nans-fp-math" "false") ("stack-protector-buffer-size" "8")
		       ("unsafe-fp-math" "false") ("use-soft-float" "false"))
	   (= (meta-id "llvm.ident") (meta-node (meta-id 0)))
	   (= (meta-id 0) (meta-node (meta-str "clang version 3.6.2 (branches/release_36 255700)"))))
	  "; ModuleID = 'hello-world.c'
target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"
target triple = \"x86_64-unknown-linux-gnu\"

@.str = private unnamed_addr constant [13 x i8] c\"Hello world!\\00\", align 1

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str, i32 0, i32 0))
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind uwtable \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }
attributes #1 = { \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }

!llvm.ident = !{!0}

!0 = !{!\"clang version 3.6.2 (branches/release_36 255700)\"}
")
