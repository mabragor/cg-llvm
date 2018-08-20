
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(quasiquote-2.0:enable-quasiquote-2.0)


(defmacro define-simple-instruction-rule (name (&rest args) &body body)
  `(define-instruction-rule ,name
     (,@(if args
	    `(let* (,(if (symbolp (car args))
			 `(,(car args) (wh instr-arg))
			 `(,(caar args) (fail-parse-if-not ,(cadar args) (wh instr-arg))))
		    ,@(mapcar (lambda (x)
				(if (symbolp x)
				    `(,x (progn (v white-comma)
						(v instr-arg)))
				    `(,(car x) (fail-parse-if-not ,(cadr x)
								  (progn (v white-comma)
									 (v instr-arg))))))
			      (cdr args))))
	    `(progn))
	,@(or body
	      `(`(,,@(mapcar (lambda (x)
			       ``(quasiquote-2.0:inject ,(if (symbolp x) x (car x))))
			     args)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mash-sym-names (&rest syms)
    (intern (joinl "-" (mapcar #'string syms))))
  (defun append-instruction-to-sym (sym)
    (mash-sym-names sym 'instruction)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun any-of-kwds (kwd-var)
    `(|| ,@(mapcar (lambda (x)
		     `(progn (descend-with-rule 'string ,(stringify-symbol x))
			     ,(intern (string x) "KEYWORD")))
		   kwd-var))))

(defmacro define-instruction-alternative (name &body alternatives)
  (if (symbolp name)
      (let ((rule-name (append-instruction-to-sym name))
	    (alt-names (mapcar #'append-instruction-to-sym alternatives)))
	`(define-cg-llvm-rule ,rule-name () (|| ,@alt-names)))
      (destructuring-bind (name appendix) name
	(let ((rule-name (mash-sym-names name appendix))
	      (alt-names (mapcar #'(lambda (x)
				     (mash-sym-names x appendix))
				 alternatives)))
	  `(define-cg-llvm-rule ,rule-name () (|| ,@alt-names))))))
	

(defmacro define-lvalue-instruction-alternative (name)
  `(define-instruction-alternative ,name ,(mash-sym-names 'lvalue name) ,(mash-sym-names 'nolvalue name)))


(define-instruction-alternative llvm
  terminator binary bitwise-binary aggregate
  memory conversion other)


(define-instruction-alternative lvalue-terminator invoke)

(define-instruction-alternative nolvalue-terminator
  ret br switch
  indirectbr resume
  unreachable)

(define-lvalue-instruction-alternative terminator)

(define-simple-instruction-rule (valued-return ret) ((x (firstclass-type-p (car it)))))

(define-instruction-rule (nonvalued-return ret)
  (wh "void")
  (list :void))

(define-cg-llvm-rule ret-instruction ()
  (|| valued-return
      nonvalued-return))

(define-simple-instruction-rule (conditional-branch br)
    ((x (llvm-typep 'label (car it)))))

(define-simple-instruction-rule (unconditional-branch br)
    ((cond (llvm-typep '(integer 1) (car it)))
     (iftrue (llvm-typep 'label (car it)))
     (iffalse (llvm-typep 'label (car it)))))


;;;;FIXME::wtf is going on here?
(define-simple-instruction-rule switch
    ((value (llvm-typep '(integer *) (car it)))
     (defaultdest (llvm-typep 'label (car it))))
  (v some-custom-magic-???))


(define-simple-instruction-rule indirectbr ((address (llvm-typep '(pointer ***) (car it))))
  (v some-magic-similar-to-switch-???))

(define-cg-llvm-rule br-instruction ()
  (|| conditional-branch
      unconditional-branch))


;;;;FIXME::wtf is going on here?
(define-instruction-rule invoke
  (let* ((cconv (?wh cconv))
	 (return-attrs (?wh (whitelist-kwd-expr '(:zeroext :signext :inreg)
						(v parameter-attrs))))
	 (function-type (fail-parse-if-not (ptr-to-function-type-p it)
					   (wh llvm-type))))
    ;; TODO : this is not correct -- there are more possible things to be INVOKED
    (let* ((function-val (wh llvm-variable))
	   (args (wh? funcall-args))
	   (fun-attrs (?wh (whitelist-kwd-expr '(:noreturn :nounwind :readonly :readnone)
					       (v fun-attrs)))))
      (wh "to")
      (let ((normal-label (fail-parse-if-not (llvm-typep 'label it)
					     (wh llvm-variable))))
	(wh "unwind")
	(let ((exception-label (fail-parse-if-not (llvm-typep 'label it)
						  (wh llvm-variable))))
	  `((,(emit-lisp-repr function-type) ,function-val)
	    ,normal-label ,exception-label
	    (:args ,@args)
	    ,!m(inject-kwd-if-nonnil cconv)
	    ,!m(inject-kwd-if-nonnil return-attrs)
	    ,!m(inject-kwd-if-nonnil fun-attrs)))))))
		     
;; The check for correct type of resume instruction is at semantic level -- the whole body
;; of the function should be parsed for that
(define-simple-instruction-rule resume (x))
	
(define-simple-instruction-rule unreachable ())

(define-instruction-alternative lvalue-binary
  add fadd sub fsub mul fmul
  udiv sdiv fdiv
  urem srem frem)

(define-cg-llvm-rule nolvalue-binary-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative binary)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binop-instruction-body (name type)
    `((let ((type (emit-lisp-repr (wh llvm-type))))
	(if (not (or (llvm-typep '(,type ***) type)
		     (llvm-typep '(vector (,type ***) *) type)))
	    (fail-parse-format ,#?"$((string name)) instruction expects <type> to be $((string type)) \
                                      or VECTOR OF $((string type))S, but got: ~a"
			       type))
	(macrolet ((parse-arg ()
		     `(|| (if (llvm-typep '(,,type ***) type)
			      (descend-with-rule ',,(intern #?"$((string type))-CONSTANT-VALUE") type)
			      (descend-with-rule 'vector-constant-value type))
			  llvm-identifier)))
	  (let ((arg1 (wh (parse-arg))))
	    (v white-comma)
	    (let ((arg2 (parse-arg)))
	      `(,type ,arg1 ,arg2 ,@prefix-kwds)
	      ))))))
  (defun binop-constexpr-body (name type)
    `((let* ((val1 (progn-v wh? #\( wh? llvm-constant))
	     (val2 (prog1-v (progn-v wh? #\, wh? llvm-constant) wh? #\))))
	(if (not (or (llvm-typep '(,type ***) (car val1))
		     (llvm-typep '(vector (,type ***) *) (car val1))))
	    (fail-parse-format ,#?"$((string name)) constexpr expects <type> to be $((string type)) \
                                      or VECTOR OF $((string type))S, but got: ~a"
			       (car val1)))
	(if (not (llvm-same-typep (car val1) (car val2)))
	    (fail-parse-format ,#?"$((string name)) constexpr expects types of its arguments to coincide \
                                      but got: ~a and ~a"
			       (car val1) (car val2)))
	`(,val1 ,val2 ,@prefix-kwds)))))

(defmacro unordered-simple-keywords (&rest kwds)
  `(let ((kwds (times (wh (|| ,@(mapcar (lambda (x)
					  `(progn (descend-with-rule 'string ,(stringify-symbol x))
						  ,(intern (string x) "KEYWORD")))
					kwds)))
		      :upto ,(length kwds))))
     ;; (format t "kwds are: ~a~%" kwds)
     (mapcar (lambda (x)
	       (if (find x kwds :test #'eq)
		   `(,x t)))
	     (list ,@(mapcar (lambda (x)
			       (intern (string x) "KEYWORD"))
			     kwds)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-fast-math-flags '(nnan ninf nsz arcp fast)))

(defmacro with-fast-math-flags-prefix (&body body)
  `(let ((prefix-kwds (remove-if-not #'identity (unordered-simple-keywords ,@known-fast-math-flags))))
     ,@body))

(defmacro define-integer-binop-definer (name &optional prefix-code)
  `(defmacro ,name (name)
     (let ((constexpr-name (intern #?"$((string name))-CONSTEXPR")))
       (with-rule-names (name)
	 `(progn (define-instruction-rule ,name
		   (let ((prefix-kwds ,,prefix-code))
		     ,@(binop-instruction-body instr-name 'integer)))
		 (define-op-rule (,constexpr-name ,instr-name)
		   (let ((prefix-kwds ,,prefix-code))
		     ,@(binop-constexpr-body instr-name 'integer))))))))

(define-integer-binop-definer define-integer-binop-rule
    ;;integer-overflow
    (destructuring-bind (nuw nsw) (unordered-simple-keywords nuw nsw)
      `(,!m(inject-if-nonnil nuw) ,!m(inject-if-nonnil nsw))))
(define-integer-binop-definer define-exact-integer-binop-rule
    ;;exact
    (remove-if-not #'identity (unordered-simple-keywords exact)))
(define-integer-binop-definer define-simple-integer-binop-rule)

(defmacro define-float-binop-rule (name &key (type 'float))
  (let ((constexpr-name (intern (format nil #?"~a-CONSTEXPR" name))))
    (with-rule-names (name)
      `(progn (define-instruction-rule ,name
		(with-fast-math-flags-prefix
		  ,@(binop-instruction-body instr-name type)))
	      (define-op-rule (,constexpr-name ,instr-name)
		(with-fast-math-flags-prefix
		  ,@(binop-instruction-body instr-name type)))))))

(define-integer-binop-rule add)
(define-integer-binop-rule sub)
(define-integer-binop-rule mul)

(define-float-binop-rule fadd)
(define-float-binop-rule fsub)
(define-float-binop-rule fmul)
(define-float-binop-rule frem)
(define-float-binop-rule fdiv)

(define-exact-integer-binop-rule udiv)
(define-exact-integer-binop-rule sdiv)

(define-simple-integer-binop-rule urem)
(define-simple-integer-binop-rule srem)

(define-instruction-alternative lvalue-bitwise-binary
  shl lshr ashr and or xor)

(define-cg-llvm-rule nolvalue-bitwise-binary-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative bitwise-binary)

(define-integer-binop-rule shl)
(define-exact-integer-binop-rule lshr)
(define-exact-integer-binop-rule ashr)

(define-simple-integer-binop-rule and)
(define-simple-integer-binop-rule or)
(define-simple-integer-binop-rule xor)

(define-instruction-alternative lvalue-aggregate
  extractelement insertelement shufflevector
  extractvalue insertvalue)

(define-cg-llvm-rule nolvalue-aggregate-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative aggregate)

(define-simple-instruction-rule extractelement ((val (llvm-typep '(vector ***) (car it)))
					 (idx (llvm-typep '(integer ***) (car it)))))
(define-simple-instruction-rule insertelement ((val (llvm-typep '(vector ***) (car it)))
					elt
					(idx (llvm-typep '(integer ***) (car it))))
  (if (not (llvm-same-typep (car elt) (cadar val)))
      (fail-parse "ELT must be same type as subtype of VAL"))
  `(,val ,elt ,idx))

(define-simple-instruction-rule shufflevector ((v1 (llvm-typep '(vector ***) (car it)))
					(v2 (llvm-typep '(vector ***) (car it)))
					(mask (llvm-typep '(vector (integer 32) *) (car it))))
  (if (not (llvm-same-typep (cadar v1) (cadar v2)))
      (fail-parse "V1 and V2 must have same subtype"))
  `(,v1 ,v2 ,mask))

(define-cg-llvm-rule indices ()
  (cons (descend-with-rule 'integer-constant-value nil)
	(times (progn (v white-comma)
		      (descend-with-rule 'integer-constant-value nil)))))

(define-simple-instruction-rule extractvalue ((val (or (llvm-typep '(struct ***) (car it))
						       (llvm-typep '(array ***) (car it)))))
  (v white-comma)
  `(,val ,@(v indices)))

(define-simple-instruction-rule insertvalue ((val (or (llvm-typep '(struct ***) (car it))
						      (llvm-typep '(array ***) (car it))))
					     elt)
  (v white-comma)
  (let ((indices (v indices)))
    ;; TODO : check that elt has same type as elt of val
    `(,val ,elt ,@indices)))


(define-instruction-alternative lvalue-memory
  alloca
  load
  cmpxchg
  atomicrmw
  getelementptr)

(define-instruction-alternative nolvalue-memory
  store
  fence)

(define-lvalue-instruction-alternative memory)

(define-instruction-rule alloca
  (let ((inalloca (? (wh (progn (v "inalloca")
				t))))
	(type (emit-lisp-repr (wh llvm-type)))
	(nelts (? (progn (v white-comma)
			 (v integer-constant))))
	(align (? (progn (v white-comma)
			 (v "align")
			 (v whitespace)
			 (v integer)))))
    `(,type
      ,!m(inject-kwd-if-nonnil nelts)
      ,!m(inject-kwd-if-nonnil align)
      ,!m(inject-kwd-if-nonnil inalloca))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-orderings '(unordered monotonic acquire release acq-rel seq-cst)))

(define-kwd-rule ordering)

(defmacro define-load-store-instruction (name pre-body type-getter &body body)
  `(define-instruction-rule ,name
     ,@pre-body
     (let ((volatile (? (wh (progn (v "volatile")
				   t))))
	   (type ,type-getter)
	   (ptr (progn (v white-comma)
		       (fail-parse-if-not (llvm-typep '(pointer ***) (car it))
					  (v llvm-constant)))))
       ,@body)))

(defmacro define-atomic-load-store-instruction (name type-getter)
  (let ((rule-name (intern #?"ATOMIC-$((string name))-INSTRUCTION")))
    `(define-load-store-instruction (,rule-name ,name) ((wh "atomic"))
       ,type-getter
       (let ((singlethread (? (wh (progn (v "singlethread")
					 t))))
	     (ordering (wh (blacklist-kwd-expr '(:release :acq-rel)
					       (v ordering))))
	     (align (progn (v white-comma)
			   (v "align")
			   (v whitespace)
			   (v integer))))
	 `((:atomic t) ,type ,ptr
	   ,!m(inject-kwds-if-nonnil ordering
				     align
				     volatile
				     singlethread))))))

(defmacro define-non-atomic-load-store-instruction (name type-getter)
  (let ((rule-name (intern #?"NON-ATOMIC-$((string name))-INSTRUCTION")))
    `(define-load-store-instruction (,rule-name ,name) ()
	 ,type-getter
       (let ((align (? (progn (v white-comma)
			      (v "align")
			      (v whitespace)
			      (v integer)))))
	 `(,type ,ptr
		 ,!m(inject-kwds-if-nonnil align volatile))))))


(define-atomic-load-store-instruction load (emit-lisp-repr (wh llvm-type)))
(define-non-atomic-load-store-instruction load (emit-lisp-repr (wh llvm-type)))
(define-atomic-load-store-instruction store (wh instr-arg))
(define-non-atomic-load-store-instruction store (wh instr-arg))

(define-instruction-alternative load
  atomic-load non-atomic-load)

(define-instruction-alternative store
  atomic-store non-atomic-store)

(define-instruction-rule fence
  (let ((singlethread (? (wh (progn (v "singlethread")
				    t))))
	(ordering (wh (whitelist-kwd-expr '(:acquire :release :acq-rel :seq-cst)
					  (v ordering)))))
    `(,!m(inject-kwds-if-nonnil singlethread ordering))))

(define-instruction-rule cmpxchg
  (let* ((weak (? (wh (progn (v "weak")
			     t))))
	 (volatile (? (wh (progn (v "volatile")
				 t))))
	 (ptr (wh (fail-parse-if-not (and (llvm-typep '(pointer (integer ***) ***) (car it))
					  (>= (bit-length (cadar it)) 8))
				     (v instr-arg))))
    	 (cmp (progn (v white-comma)
		     (fail-parse-if-not (llvm-same-typep (car it) (cadar ptr))
					(v instr-arg))))
	 (new (progn (v white-comma)
		     (fail-parse-if-not (llvm-same-typep (car it) (car cmp))
					(v instr-arg))))
	 (singlethread (? (wh (progn (v "singlethread")
				     t))))
	 (success-ord (wh ordering))
	 (failure-ord (wh ordering)))
    ;; TODO : constraints on orderings
    `(,ptr ,cmp ,new ,!m(inject-kwds-if-nonnil success-ord failure-ord
					       weak volatile singlethread))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-atomicrmw-ops '(xchg add sub and nand or xor max min umax umin)))

(defmacro atomicrmw-kwds ()
  (any-of-kwds known-atomicrmw-ops))

(define-instruction-rule atomicrmw
  (let* ((volatile (?wh (progn (v "volatile")
			       t)))
	 (op (wh (atomicrmw-kwds)))
	 (ptr (wh (fail-parse-if-not (and (llvm-typep '(pointer (integer ***) ***) (car it))
					  (<= 8 (bit-length (cadar it))))
				     (v instr-arg))))
	 (val (progn (v white-comma)
		     (fail-parse-if-not
		      (llvm-same-typep (cadar ptr) (car it))
		      (v instr-arg))))
	 (singlethread (?wh (progn (v "singlethread")
				   t)))
	 (ordering (wh ordering)))
    `(,op ,ptr ,val ,!m(inject-kwds-if-nonnil ordering volatile singlethread))))

(define-cg-llvm-rule vector-getelementptr-body ()
  (let* ((ptrval (fail-parse-if-not (llvm-typep '(vector ***) (car it))
				    (v instr-arg)))
	 (idx (progn (v white-comma)
		     (fail-parse-if-not (llvm-typep '(vector ***) (car it))
					(v instr-arg)))))
    (if (not (equal (caddar ptrval)
		    (caddar idx)))
	(fail-parse "Sizes of vectors should be equal"))
    `(,ptrval ,idx)))

(define-cg-llvm-rule scalar-getelementptr-body ()
  (let ((ptrval (fail-parse-if-not (llvm-typep '(pointer ***)
					       (car it))
				   (v instr-arg))))
    (v white-comma)
    (let ((indices (v geteltptr-indices)))
      `(,ptrval ,@indices))))

;; TODO : we do not check correct types of indices of getelementptr
;; at this stage, as this would require keeping track of symbol table
;; (for named types) as we go along.
;; This, apparently, should be done at later stages of the transformation
(define-plural-rule geteltptr-indices geteltptr-index white-comma)

(define-cg-llvm-rule geteltptr-index ()
  (let ((type (emit-lisp-repr (v llvm-type))))
    (if (not (llvm-typep '(integer ***) type))
	(fail-parse "Geteltptr index is not an integer"))
    (let ((what (wh (|| integer llvm-identifier))))
      (list type what))))
		    
    

(define-instruction-rule getelementptr
  (let* ((inbounds (?wh (progn (v "inbounds")
			       t)))
	 (type (wh (prog1 (v llvm-type)
		     (v white-comma))))
	 (body (|| vector-getelementptr-body
		   scalar-getelementptr-body))
	 )
    `(,type ,@body ,!m(inject-kwd-if-nonnil inbounds))))

(define-instruction-alternative lvalue-conversion
  trunc-to zext-to sext-to fptrunc-to fpext-to fptoui-to fptosi-to
  uitofp-to sitofp-to ptrtoint-to inttoptr-to bitcast-to addrspacecast-to)

(define-cg-llvm-rule nolvalue-conversion-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative conversion)

(defun both-integers (type1 type2)
  (and (llvm-typep '(integer ***) type1)
       (llvm-typep '(integer ***) type2)))

(defun first-bitsize-larger (type1 type2)
  (> (cadr type1)
     (cadr type2)))

(defun first-bitsize-smaller (type1 type2)
  (< (cadr type1)
     (cadr type2)))

(defun both-vector-integers (type1 type2)
  (and (llvm-typep '(vector (integer ***) *)
		   type1)
       (llvm-typep '(vector (integer ***) *)
		   type2)))

(defun have-same-size (type1 type2)
  (equal (caddr type1)
	 (caddr type2)))

(defun have-different-addrspaces (type1 type2)
  (not (equal (if (equal 3 (length type1))
		  (caddr type1)
		  0)
	      (if (equal 3 (length type2))
		  (caddr type2)
		  0))))

(defmacro define-cast-instruction (name &body constraints)
  (let ((rule-name (intern #?"$((string name))-TO-INSTRUCTION")))
    `(define-instruction-rule (,rule-name ,name)
       (destructuring-bind (type1 value) (wh (v instr-arg))
	 (v whitespace)
	 (v "to")
	 (v whitespace)
	 (let ((type2 (emit-lisp-repr (v llvm-type))))
	   ,@constraints
	   `(,value ,type1 ,type2))))))


(defmacro define-simple-based (type1 type2)
  `(defmacro ,(intern #?"$((string type1))->$((string type2))-BASED") (&optional condition)
     `(or (and (llvm-typep '(,,type1 ***) type1)
	       (llvm-typep '(,,type2 ***) type1)
	       ,@(if condition `((,condition type1 type2))))
	  (and (llvm-typep '(vector (,,type1 ***) *) type1)
	       (llvm-typep '(vector (,,type2 *) *) type2)
	       (have-same-size type1 type2)
	       ,@(if condition `((,condition (cadr type1) (cadr type2))))))))


(define-simple-based integer integer)
(define-simple-based float float)

(define-cast-instruction trunc (integer->integer-based first-bitsize-larger))
(define-cast-instruction zext (integer->integer-based first-bitsize-smaller))
(define-cast-instruction sext (integer->integer-based first-bitsize-smaller))

(define-cast-instruction fptrunc (float->float-based first-bitsize-larger))
(define-cast-instruction fpext (float->float-based first-bitsize-smaller))

(define-simple-based float integer)
(define-simple-based integer float)
(define-simple-based pointer integer)
(define-simple-based integer pointer)

(define-cast-instruction fptoui (float->integer-based))
(define-cast-instruction fptosi (float->integer-based))
(define-cast-instruction uitofp (integer->float-based))
(define-cast-instruction sitofp (integer->float-based))
(define-cast-instruction ptrtoint (pointer->integer-based))
(define-cast-instruction inttoptr (integer->pointer-based))

(defun pointer-pointer-check (type1 type2)
  (if (not (llvm-typep '(pointer ***) type2))
      (fail-parse-format "Second type should also be pointer, but got ~a" type2)
      (if (have-different-addrspaces type1 type2)
	  (fail-parse-format "Both pointer types should have same addrspace"))))

(defun vector-check (type1 type2)
  (if (not (llvm-typep '(vector ***) type2))
      (fail-parse-format "Second type should also be a vector, but got ~a" type2)
      (if (not (equal (caddr type1) (caddr type2)))
	  (fail-parse-format "Lengths of vectors should match, but got ~a and ~a" (caddr type1) (caddr type2)))))

(defun rough-check-bitcast-type (label type)
  (let ((llvm-type (parse-lisp-repr type)))
    (if (or (not (and (firstclass-type-p llvm-type)
		  (not (aggregate-type-p llvm-type))))
	    (typep llvm-type 'llvm-label)
	    (typep llvm-type 'llvm-metadata))
	(fail-parse-format "~a type must be first-class, non-aggregate type, but got: ~a"
			   label
			   type))))

(defmacro bitcast-constraints ()
  `(progn (rough-check-bitcast-type "First" type1)
	  (rough-check-bitcast-type "Second" type2)
	  (if (not (and (firstclass-type-p type2) (not (aggregate-type-p type2))))
	      (fail-parse-format "Second type must be first-class non-aggregate type, but got: ~a" type2))
	  (cond ((llvm-typep '(pointer ***) type1)
		 (pointer-pointer-check type1 type2))
		((llvm-typep '(vector (pointer ***) ***) type1)
		 (vector-check type1 type2)
		 (pointer-pointer-check (cadr type1) (cadr type2)))
		(t (if (not (equal (bit-length type1) (bit-length type2)))
		       (fail-parse "Types of BITCAST should have identical bit sizes"))))))



(define-cast-instruction bitcast (bitcast-constraints))
	
	

(define-simple-based pointer pointer)

(define-cast-instruction addrspacecast (pointer->pointer-based have-different-addrspaces))

(define-instruction-alternative lvalue-other
  icmp fcmp phi select call va-arg landingpad)

(define-cg-llvm-rule nolvalue-other-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative other)

(define-cg-llvm-rule phi-arg (type)
  (let (c!-1)
    (v #\[)
    (v whitespace)
    (setf c!-1
	  (|| (descend-with-rule 'llvm-constant-value type)
	      llvm-identifier))
    (v white-comma)
    (cap b llvm-identifier)
    (v whitespace)
    (v #\])
    `(,c!-1 ,(recap b))))

(define-instruction-rule phi
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (if (not (firstclass-type-p type))
	(fail-parse-format "Type of phi instruction must be first-class, but got ~a" type))
    (v whitespace)
    (let ((first-arg (descend-with-rule 'phi-arg type)))
      (let ((rest-args (times (progn (v white-comma)
				     (descend-with-rule 'phi-arg type)))))
	`(,type ,first-arg ,@rest-args)))))


(define-simple-instruction-rule select ((cond (or (llvm-typep '(integer 1) (car it))
						  (llvm-typep '(vector (integer 1) *) (car it))))
					(val1 (firstclass-type-p (car it)))
					(val2 (firstclass-type-p (car it))))
  (if (llvm-typep '(integer *) (car cond))
      (if (not (llvm-same-typep (car val1)
				(car val2)))
	  (fail-parse-format "Different values of SELECT should be same type, but are: ~a ~a"
			     (car val1)
			     (car val2)))
      (let ((nelts (caddar cond)))
	(if (not (and (llvm-typep '(vector ***)
				  (car val1))
		      (llvm-typep '(vector ***)
				  (car val2))
		      (llvm-same-typep (cadar val1)
				       (cadar val2))
		      (equal nelts (caddar val1))
		      (equal nelts (caddar val2))))
	    (fail-parse-format "Different values of SELECT should be same type, but are: ~a ~a"
			       (car val1)
			       (car val2)))))
  `(,cond ,val1 ,val2))

(define-simple-instruction-rule va-arg ((va-list (llvm-typep '(pointer ***) (car it))))
  (v white-comma)
  (let ((type (emit-lisp-repr (v llvm-type))))
    `(,va-list ,type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-icmp-ops '(eq ne ugt uge ult ule sgt sge slt sle))
  (defparameter known-fcmp-ops '(false oeq ogt oge olt ole one ord
				 ueq ugt uge ult ule une uno true)))

(defmacro icmp-kwds ()
  (any-of-kwds known-icmp-ops))

(defmacro fcmp-kwds ()
  (any-of-kwds known-fcmp-ops))

(defmacro define-cmp-rule (name typecheck errinfo)
  (let ((macro-name (intern #?"$((string name))-KWDS")))
    `(define-instruction-rule ,name
       (let ((cond (wh (,macro-name)))
	     (type (emit-lisp-repr (wh llvm-type))))
	 (if (not ,typecheck)
	     (fail-parse-format ,@errinfo))
	 (let ((val1 (wh (v instr-arg-value type))))
	   (v white-comma)
	   (let ((val2 (v instr-arg-value type)))
	     `(,cond ,type ,val1 ,val2)))))))
	
(define-cmp-rule icmp
    (or (llvm-typep '(integer ***) type)
	(llvm-typep '(vector (integer ***) *) type)
	(llvm-typep '(pointer ***) type)
	(llvm-typep '(vector (pointer ***) *) type))
  ("Type of arguments of ICMP instruction should be INTEGER or POINTER or~
    VECTOR of INTEGERS or VECTOR of POINTERS"))

(define-cmp-rule fcmp
    (or (llvm-typep '(float ***) type)
	(llvm-typep '(vector (float ***) *) type))
  ("Type of arguments of FCMP instruction should be FLOAT~
    or VECTOR of FLOATS"))

(define-cg-llvm-rule cleanup-kwd ()
  (v "cleanup")
  '(:cleanup t))

(define-cg-llvm-rule catch-landingpad-clause ()
  (v "catch")
  (list :catch (wh llvm-constant)))

(define-cg-llvm-rule filter-landingpad-clause ()
  (v "filter")
  (list :filter (fail-parse-if-not (llvm-typep '(array ***) (car it))
				   (wh llvm-constant))))

(define-cg-llvm-rule landingpad-clause ()
  (|| catch-landingpad-clause
      filter-landingpad-clause))

(define-instruction-rule landingpad
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (wh "personality")
    (let ((pers (wh llvm-constant)))
      ;; check
      (let ((clauses (|| (cons (wh cleanup-kwd) (times (wh landingpad-clause)))
			 (postimes (wh landingpad-clause)))))
	`(,type ,pers ,@clauses)))))

(define-cg-llvm-rule call-instruction ()
  (let ((tail (? (|| (progn (prog1 (v "tail")
			      (v whitespace))
			    :tail)
		     (progn (prog1 (v "musttail")
			      (v whitespace))
			    :must-tail)))))
    (v "call")
    (let ((cconv (?wh cconv))
	  (return-attrs (?wh (mapcar (lambda (x)
				       (whitelist-kwd-expr '(:zeroext :signext :inreg) x))
				     (v parameter-attrs))))
	  (type (emit-lisp-repr (wh llvm-type)))
	  (ftype (?wh (fail-parse-if-not (llvm-typep '(pointer (function ***) ***) it)
					 (emit-lisp-repr (v llvm-type)))))
	  (fnptrval (wh llvm-identifier))
	  (args (wh? (progn (v #\()
			    (v wh?)
			    (cap thing (? funcall-args))
			    (v wh?)
			    (v #\))
			    (recap thing))))
	  (fun-attrs (?wh (mapcar (lambda (x)
				    (whitelist-kwd-expr '(:noreturn :nounwind :readonly :readnone) x))
				  (v fun-attrs)))))
      `(call ,type ,fnptrval ,args
	     ,!m(inject-kwds-if-nonnil cconv return-attrs ftype fun-attrs tail)))))

(define-cg-llvm-rule usual-funcall-arg ()
  (let ((instr-arg (v instr-arg))
	(attrs (?wh parameter-attrs)))
    `(,@instr-arg ,@(if attrs `((:attrs ,@attrs))))))

(define-cg-llvm-rule funcall-arg ()
  (|| usual-funcall-arg metadata-funcall-arg))

(define-plural-rule funcall-args funcall-arg white-comma)

(define-cg-llvm-rule defun-arg ()
  (|| vararg-sign
      (let* ((arg (v long-defun-arg))   ; arg in defun must have a name
	     (attrs (?wh parameter-attrs)))
	`(,@arg ,!m(inject-kwd-if-nonnil attrs)))))

(define-plural-rule %defun-args defun-arg white-comma)

(define-cg-llvm-rule defun-args ()
  (v #\()
  (? whitespace)
  (let ((args (? %defun-args)))
    ;; TODO : here we check for vararg special syntax
    (? whitespace)
    (v #\))
    args))

(define-cg-llvm-rule short-defun-arg ()
  `(,(emit-lisp-repr (v llvm-type))))
(define-cg-llvm-rule long-defun-arg ()
  `(,@(v short-defun-arg) ,(wh? local-identifier)))

(define-cg-llvm-rule vararg-sign ()
  (v "...")
  :vararg)

(define-cg-llvm-rule declfun-arg ()
  (|| vararg-sign
      (let* ((type (emit-lisp-repr (v llvm-type)))
	     (attrs (?wh (v parameter-attrs)))
	     (id (? (wh? local-identifier))))
	`(,type ,!m(inject-if-nonnil id) ,!m(inject-kwd-if-nonnil attrs)))))

(define-plural-rule %declfun-args declfun-arg white-comma)

(define-cg-llvm-rule declfun-args ()
  (v #\()
  (? whitespace)
  (let ((args (? %declfun-args)))
    ;; TODO : here we check for vararg special syntax
    (? whitespace)
    (v #\))
    args))

;;; Let's write something that is able to parse whole basic block of instructions

(define-instruction-alternative lvalue-nonterminator
  lvalue-other lvalue-conversion lvalue-memory lvalue-aggregate
  lvalue-bitwise-binary lvalue-binary)

(define-instruction-alternative nolvalue-nonterminator
  nolvalue-other nolvalue-conversion nolvalue-memory
  nolvalue-aggregate nolvalue-bitwise-binary nolvalue-binary)

(define-cg-llvm-rule block-label ()
  (destringify-symbol (text (list (literal-char #\%)
				  (prog1 (v identifier-body)
				    (v #\:))))))
;;;;FIXME::why discriminate between final and nonfinal statements?

(define-cg-llvm-rule lvalue-nonterminator-aux ()
  (|| lvalue-nonterminator-instruction
      `(= ,(v local-identifier)
	  ,(progn (v whitespace)
		  (v #\=)
		  (v  whitespace)
		  (v lvalue-nonterminator-instruction)))))
(define-cg-llvm-rule nonfinal-statement ()
  (|| nolvalue-nonterminator-instruction
      lvalue-nonterminator-aux))

(define-plural-rule nonfinal-statements nonfinal-statement whitespace)

(define-cg-llvm-rule final-statement ()
  (|| nolvalue-terminator-instruction
      lvalue-nonterminator-aux))
(define-cg-llvm-rule basic-block-body ()
  (let ((nonfinal-statements (? nonfinal-statements)))
    (if nonfinal-statements
	(v whitespace))
    ;; this way we first know that everything parsed well before we construct the list
    (let ((final-statement (v final-statement)))
      `(,@nonfinal-statements ,final-statement))))

#+nil
(define-plural-rule any-statements any-statement whitespace)

(define-cg-llvm-rule any-statement ()
  (|| nolvalue-terminator-instruction
      nolvalue-nonterminator-instruction
      lvalue-nonterminator-aux))
#+nil
(define-cg-llvm-rule basic-block-body ()
  (? any-statements))

(define-cg-llvm-rule basic-block ()
  (let ((label (? (prog1 (v block-label)
		    (v whitespace)))))
    (let ((basic-block-body (v basic-block-body)))
      `(block ,!m(inject-kwd-if-nonnil label)
	       ,@basic-block-body))))

(define-plural-rule basic-blocks basic-block whitespace)

(define-cg-llvm-rule function-body ()
  (progm (progn (v #\{)
		(? whitespace))
	 (? basic-blocks)
	 (progn (? whitespace)
		(v #\}))))

(define-cg-llvm-rule fundef-metadata-entry ()
  ;; TODO : in future the syntax of LLVM will likely become more flexible,
  ;;        and hence this place would have to be changed.
  (list (v  metadata-identifier)
	(progn (v whitespace)
	       (v metadata-identifier))))

(define-plural-rule fundef-metadata fundef-metadata-entry whitespace)

(define-op-rule (function-definition define)
  (let* ((linkage (?wh linkage-type))
	 (visibility (?wh visibility-style))
	 (dll-storage-class (?wh dll-storage-class))
	 (cconv (?wh cconv))
	 (unnamed-addr (?wh (progn (v "unnamed_addr")
				   t)))
	 (type (wh (emit-lisp-repr (v llvm-type))))
	 (return-attrs (?wh parameter-attrs))
	 (fname (wh llvm-identifier))
	 (args (wh? defun-args))
	 (fun-attrs (?wh fun-attrs))
	 (section (?wh section))
	 (align (?wh align))
	 (comdat (?wh comdat))
	 (gc (?wh gc-name))
	 (prefix (?wh prefix))
	 (prologue (?wh prologue))
	 (personality (?wh personality))
	 (metadata (?wh? fundef-metadata))
	 (body (wh? function-body)))
    `(,type ,fname ,args
	    ,!m(inject-kwds-if-nonnil linkage visibility dll-storage-class
				      cconv unnamed-addr return-attrs
				      fun-attrs section align comdat gc
				      prefix prologue personality metadata
				      body))))

(define-cg-llvm-rule global-variable-definition ()
  (let ((name (v global-identifier)))
    (v whitespace)
    (v #\=)
    (let* ((linkage (?wh linkage-type))
      	   (visibility (?wh visibility-style))
	   (dll-storage-class (?wh dll-storage-class))
	   (thread-local (?wh thread-local))
	   (unnamed-addr (?wh (progn (v "unnamed_addr")
				     t)))
	   (addrspace (?wh (let ((it (v addr-space)))
			     (if (equal 0 it)
				 nil
				 it))))
	   (externally-initialized (? (progn (? whitespace)
					     (v "externally_initialized")
					     t)))
	   (constant (progn (? whitespace)
			    (|| (progn (v "global")
				       nil)
				(progn (v "constant")
				       t))))
	   (type (emit-lisp-repr (wh? llvm-type)))
	   (value (if (eq :external linkage)
	   	      (? (?wh (descend-with-rule 'llvm-constant-value type)))
	   	      (?wh (descend-with-rule 'llvm-constant-value type))))
	   (section (? (progn (? whitespace)
			      (v #\,)
			      (? whitespace)
			      (v  section))))
	   (comdat (? (progn (? whitespace)
			     (v #\,)
			     (? whitespace)
			     (v comdat))))
	   (align (? (progn (? whitespace)
			    (v #\,)
			    (? whitespace)
			    (v align)))))
      `(:global-var ,name ,type ,value
		    ,!m(inject-kwds-if-nonnil linkage visibility dll-storage-class
					      unnamed-addr addrspace
					      externally-initialized
					      constant
					      section comdat)
		    ,!m(inject-stuff-if-nonnil thread-local align)
		    ))))

(define-cg-llvm-rule long-abstract-attr ()
  (cap name llvm-string)
  (? whitespace)
  (v #\=)
  (? whitespace)
  (cap value llvm-string)
  (list (recap name)
	(recap value)))

(define-cg-llvm-rule short-abstract-attr ()
  (v llvm-string))

(define-cg-llvm-rule abstract-attr ()
  (|| fun-attr
      parameter-attr
      long-abstract-attr
      short-abstract-attr))

(define-plural-rule abstract-attrs abstract-attr whitespace)

(define-op-rule (attribute-group attributes)
  (let* ((id (progn (? whitespace)
		    (v #\#)
		    (v pos-integer)))
	 (attrs (progm (progn (? whitespace)
			      (v #\=)
			      (? whitespace)
			      (v #\{)
			      (? whitespace))
		       abstract-attrs
		       (progn (? whitespace)
			      (v #\})))))
    `(,id ,@attrs)))

(define-op-rule (blockaddress blockaddress)
  (v wh?)
  (v #\()
  (cap a global-identifier)
  (v white-comma)
  (cap b local-identifier)
  (v wh?)
  (v #\))
  `(,(recap a) ,(recap b)))

