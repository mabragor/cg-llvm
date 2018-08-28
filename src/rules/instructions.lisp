(in-package #:cg-llvm)

(defmacro define-simple-instruction-rule (name (&rest args) &body body)
  `(define-instruction-rule ,name
     ,(append
       (if args
	   `(let* ((,(car args) 
		    (wh instr-arg))
		   ,@(mapcar (lambda (x)			       
			       `(,x 
				 (progn (v white-comma)
					(v instr-arg))))
			     (cdr args))))
	   `(progn))
       (or body
	   `((list
	      ,@args))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun any-of-kwds (kwd-var)
    `(|| ,@(mapcar (lambda (x)
		     `(progn (descend-with-rule 'string ,(%stringify-symbol x))
			     ,(keywordify x)))
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
  `(define-instruction-alternative
       ,name
     ,(mash-sym-names 'lvalue name)
     ,(mash-sym-names 'nolvalue name)))


(define-instruction-alternative llvm
  terminator
  binary
  bitwise-binary
  aggregate
  memory
  conversion
  other)

(define-instruction-alternative lvalue-terminator
  invoke)

(define-instruction-alternative nolvalue-terminator
  ret
  br
  switch
  indirectbr
  resume
  unreachable)

(define-lvalue-instruction-alternative terminator)

(define-simple-instruction-rule (valued-return ret)
    (x))

(define-instruction-rule (nonvalued-return ret)
  (wh "void")
  (list :void))

(define-cg-llvm-rule ret-instruction ()
  (|| valued-return
      nonvalued-return))

(define-simple-instruction-rule (unconditional-branch br)
    (x))

(define-simple-instruction-rule (conditional-branch br)
    (cond 
     iftrue 
     iffalse))


;;;;FIXME::wtf is going on here?
;;;switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]
;;;;is it a coincidence it has this pattern x[x*] ?
(define-cg-llvm-rule %switch-branch ()
  (nest
   (progn (v whitespace))
   (let- (case-value 
		     (v instr-arg)		 
		      ;;(wh integer-constant-type)
		      ;;(wh instr-arg-value)
		      ))
   (progn (v white-comma))
   (let-
    (dest 
	  (v instr-arg)))
   `(,case-value ,dest)))
(define-instruction-rule switch
  (nest
   (progn (v whitespace))
   (let- (value 
		(v instr-arg)		 
		 ;;(wh integer-constant-type)
		 ;;(wh instr-arg-value)
		 ))
   (progn (v white-comma))
   (let- (default-dest 
	     (v instr-arg)))
   (progn (? whitespace))
   (progn
     (white-[]
       (cap result
	    (times
	     %switch-branch
	     :from 0)))) 
   `((,value ,default-dest) ,@(recap? result))))

(define-simple-instruction-rule indirectbr (address)
  (v some-magic-similar-to-switch-???))

(define-cg-llvm-rule br-instruction ()
  (|| conditional-branch
      unconditional-branch))


;;;;FIXME::wtf is going on here?
(define-instruction-rule invoke
  (nest
   (progn (v wh?))
   (let- (cconv (? cconv)))
   (progn (v wh?))
   (let- (return-attrs (? (whitelist-kwd-expr
			   '(:zeroext :signext :inreg)
			   (v parameter-attrs)))))
   (progn (v whitespace))
   (let- (function-type
			(v llvm-type))
	 ;; TODO : this is not correct -- there are more possible things to be INVOKED
	 )
   (progn (v whitespace))
   (let- (function-val (v llvm-variable)))
   (progn (v wh?))
   (let- (args (? funcall-args)))
   (let- (fun-attrs (?wh (whitelist-kwd-expr '(:noreturn :nounwind :readonly :readnone)
					     (v fun-attrs)))))
   (progn (v whitespace))
   (progn (v "to"))
   (progn (v whitespace))
   
   (let- (normal-label 
		       (v llvm-variable)))
   (progn (v whitespace))
   (progn (v "unwind"))
   (progn (v whitespace))
   
   (let- (exception-label
			  (v llvm-variable)))
   
   `((function-type ,function-val)
     ,normal-label ,exception-label
     (:args ,@args)
     ,@(%%inject-kwd-if-nonnil cconv)
     ,@(%%inject-kwd-if-nonnil return-attrs)
     ,@(%%inject-kwd-if-nonnil fun-attrs))))

;; The check for correct type of resume instruction is at semantic level -- the whole body
;; of the function should be parsed for that
(define-simple-instruction-rule resume (x))

(define-simple-instruction-rule unreachable ())

(define-instruction-alternative lvalue-binary
  add
  fadd
  sub
  fsub
  mul
  fmul
  udiv
  sdiv
  fdiv
  urem
  srem
  frem)

(define-cg-llvm-rule nolvalue-binary-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative binary)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binop-instruction-body (name type)
    (declare (ignore name))
    (let ((constant-value-symbol (intern
				  ;;"$((string type))-CONSTANT-VALUE"
				  (interpol
				   (string type)
				   "-CONSTANT-VALUE"))))
      `((nest
	 (progn (v whitespace))
	 (let- (type (v llvm-type)))
	 (flet ((parse-arg ()
		  (|| (descend-with-rule ',constant-value-symbol type)
		      (descend-with-rule 'vector-constant-value type)
		      llvm-identifier))))
	 (progn (v whitespace))
	 (let- (arg1 (parse-arg)))
	 (progn (v white-comma))
	 (let- (arg2 (parse-arg)))
	 `(,type ,arg1 ,arg2 ,@prefix-kwds)))))
  (defun binop-constexpr-body (name type)
    (declare (ignorable name type))
    `((nest
       (progn (v wh?))
       (progn (v #\())
       (progn (v wh?))
       (let- (val1 (v llvm-constant)))
       (progn (v wh?))
       (progn (v #\,))
       (progn (v wh?)) 
       (let- (val2 (v llvm-constant)))
       (progn (v wh?))
       (progn (v #\)))
       `(,val1 ,val2 ,@prefix-kwds)))))

(defmacro unordered-simple-keywords (&rest kwds)
  `(let ((kwds (times (progn
			(v whitespace)
			(|| ,@(mapcar (lambda (x)
					`(progn (descend-with-rule 'string ,(%stringify-symbol x))
						,(keywordify x)))
				      kwds)))
		      :upto ,(length kwds))))
     ;; (format t "kwds are: ~a~%" kwds)
     (mapcar (lambda (x)
	       (if (find x kwds :test #'eq)
		   `(,x t)))
	     (list ,@(mapcar (lambda (x)
			       (keywordify x))
			     kwds)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-fast-math-flags '(nnan ninf nsz arcp fast)))

(defmacro with-fast-math-flags-prefix (&body body)
  `(let ((prefix-kwds (remove-if-not #'identity (unordered-simple-keywords ,@known-fast-math-flags))))
     ,@body))

(defmacro define-integer-binop-definer (name &optional prefix-code)
  `(defmacro ,name (name)
     (let ((constexpr-name (intern ;;#"$((string name))-CONSTEXPR"
			    (interpol
			     (string name)
			     "-CONSTEXPR"))))
       (with-rule-names (name)
	 `(progn (define-instruction-rule ,name
		   (let ((prefix-kwds ,',prefix-code))
		     ,@(binop-instruction-body instr-name 'integer)))
		 (define-op-rule (,constexpr-name ,instr-name)
		   (let ((prefix-kwds ,',prefix-code))
		     ,@(binop-constexpr-body instr-name 'integer))))))))

(define-integer-binop-definer define-integer-binop-rule
    ;;integer-overflow
    (destructuring-bind (nuw nsw) (unordered-simple-keywords nuw nsw)
      `(,@(%%inject-if-nonnil nuw) ,@(%%inject-if-nonnil nsw))))
(define-integer-binop-definer define-exact-integer-binop-rule
    ;;exact
    (remove-if-not #'identity (unordered-simple-keywords exact)))
(define-integer-binop-definer define-simple-integer-binop-rule)

(defmacro define-float-binop-rule (name &key (type 'float))
  (let ((constexpr-name (intern (format nil "~a-CONSTEXPR" name))))
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

(define-simple-instruction-rule extractelement
    (val
     idx))
(define-simple-instruction-rule insertelement
    (val
     elt
     idx)
  `(,val ,elt ,idx))

(define-simple-instruction-rule shufflevector
    (v1
     v2 
     mask)
  `(,v1 ,v2 ,mask))

(define-plural-rule indices integer white-comma)

(define-simple-instruction-rule extractvalue (val)
  (v white-comma)
  `(,val ,@(v indices)))

(define-simple-instruction-rule insertvalue (val
					     elt)
  (nest
   (progn (v white-comma))
   (let- (indices (v indices)))
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

;;;;<result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result
(define-instruction-rule alloca
  (let ((inalloca (? (wh (progn (v "inalloca")
				t))))
	(type (wh llvm-type))
	(nelts (? (progn (v white-comma)
			 (v integer-constant))))
	(align (? (progn (v white-comma)
			 (v "align")
			 (v whitespace)
			 (v integer)))))
    `(,type
      ,@(%%inject-kwd-if-nonnil nelts)
      ,@(%%inject-kwd-if-nonnil align)
      ,@(%%inject-kwd-if-nonnil inalloca))))

(defmacro define-load-store-instruction (name pre-body type-getter &body body)
  `(define-instruction-rule ,name
     ,@pre-body
     (let ((volatile (?wh-p "volatile"))
	   (type ,type-getter)
	   (ptr (progn (v white-comma)
		       (v llvm-constant))))
       ,@body)))

(defmacro define-atomic-load-store-instruction (name type-getter)
  (let ((rule-name (intern ;;#"ATOMIC-$((string name))-INSTRUCTION"
		    (interpol
		     "ATOMIC-"
		     (string name)
		     "-INSTRUCTION"))))
    `(define-load-store-instruction (,rule-name ,name) ((wh "atomic"))
	 ,type-getter
       (let ((singlethread (?wh-p "singlethread"))
	     (ordering (wh (blacklist-kwd-expr '(:release :acq_rel)
					       (v ordering))))
	     (align (progn (v white-comma)
			   (v "align")
			   (v whitespace)
			   (v integer))))
	 `((:atomic t) ,type ,ptr
	   ,@(%%inject-kwds-if-nonnil ordering
				      align
				      volatile
				      singlethread))))))

(defmacro define-non-atomic-load-store-instruction (name type-getter)
  (let ((rule-name (intern ;;#"NON-ATOMIC-$((string name))-INSTRUCTION"
		    (interpol
		     "NON-ATOMIC-"
		     (string name)
		     "-INSTRUCTION"))))
    `(define-load-store-instruction (,rule-name ,name) ()
	 ,type-getter
       (let ((align (? (progn (v white-comma)
			      (v "align")
			      (v whitespace)
			      (v integer)))))
	 `(,type ,ptr
		 ,@(%%inject-kwds-if-nonnil align volatile))))))


(define-atomic-load-store-instruction load (wh llvm-type))
(define-non-atomic-load-store-instruction load (wh llvm-type))
(define-atomic-load-store-instruction store (wh instr-arg))
(define-non-atomic-load-store-instruction store (wh instr-arg))

(define-instruction-alternative load
  atomic-load non-atomic-load)

(define-instruction-alternative store
  atomic-store non-atomic-store)

(define-instruction-rule fence
  (let ((singlethread (?wh-p "singlethread"))
	(ordering (wh (whitelist-kwd-expr '(:acquire :release :acq_rel :seq_cst)
					  (v ordering)))))
    `(,@(%%inject-kwds-if-nonnil singlethread ordering))))
;;;;cmpxchg [weak] [volatile] <ty>* <pointer>, <ty> <cmp>, <ty> <new> [syncscope("<target-scope>")] <success ordering> <failure ordering> ; yields  { ty, i1 }
(define-instruction-rule cmpxchg
  (let* ((weak (?wh-p "weak") )
	 (volatile (?wh-p "volatile"))
	 (ptr (wh 
		  (v instr-arg)))
    	 (cmp (progn (v white-comma)
		     (v instr-arg)))
	 (new (progn (v white-comma)
		     (v instr-arg)))
	 (singlethread (?wh-p "singlethread"))
	 (success-ord (wh ordering))
	 (failure-ord (wh ordering)))
    ;; TODO : constraints on orderings
    `(,ptr ,cmp ,new ,@(%%inject-kwds-if-nonnil success-ord failure-ord
						weak volatile singlethread))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-atomicrmw-ops '(xchg add sub and nand or xor max min umax umin)))

(defmacro atomicrmw-kwds ()
  (any-of-kwds known-atomicrmw-ops))

(define-instruction-rule atomicrmw
  (let* ((volatile (?wh-p "volatile"))
	 (op (wh (atomicrmw-kwds)))
	 (ptr (wh 
		  (v instr-arg)))
	 (val (progn (v white-comma)
		     (v instr-arg)))
	 (singlethread (?wh-p "singlethread"))
	 (ordering (wh ordering)))
    `(,op ,ptr ,val ,@(%%inject-kwds-if-nonnil ordering volatile singlethread))))

(define-cg-llvm-rule vector-getelementptr-body ()
  (let* ((ptrval (v instr-arg))
	 (idx (progn (v white-comma)
		     (v instr-arg))))
    `(,ptrval ,idx)))

(define-cg-llvm-rule scalar-getelementptr-body ()
  (let ((ptrval 
	  (v instr-arg)))
    (v white-comma)
    (let ((indices (v geteltptr-indices)))
      `(,ptrval ,@indices))))

(define-plural-rule geteltptr-indices geteltptr-index white-comma)

(define-cg-llvm-rule geteltptr-index ()
  (let ((type (v llvm-type)))
    (let ((what (wh (|| integer llvm-identifier))))
      (list type what))))

(define-instruction-rule getelementptr
  (let* ((inbounds (?wh-p "inbounds"))
	 (type (wh (prog1 (v llvm-type)
		     (v white-comma))))
	 (body (|| vector-getelementptr-body
		   scalar-getelementptr-body)))
    `(,type ,@body ,@(%%inject-kwd-if-nonnil inbounds))))

(define-instruction-alternative lvalue-conversion
  trunc-to zext-to sext-to fptrunc-to fpext-to fptoui-to fptosi-to
  uitofp-to sitofp-to ptrtoint-to inttoptr-to bitcast-to addrspacecast-to)

(define-cg-llvm-rule nolvalue-conversion-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative conversion)

(defmacro define-cast-instruction (name)
  (let ((rule-name (intern ;;#"$((string name))-TO-INSTRUCTION"
		    (interpol
		     (string name)
		     "-TO-INSTRUCTION"))))
    `(define-instruction-rule (,rule-name ,name)
       (destructuring-bind (type1 value) (wh (v instr-arg))
	 (v whitespace)
	 (v "to")
	 (v whitespace)
	 (let ((type2 (v llvm-type)))
	   `(,value ,type1 ,type2))))))

(define-cast-instruction trunc)
(define-cast-instruction zext)
(define-cast-instruction sext)

(define-cast-instruction fptrunc)
(define-cast-instruction fpext)

(define-cast-instruction fptoui)
(define-cast-instruction fptosi)
(define-cast-instruction uitofp)
(define-cast-instruction sitofp)
(define-cast-instruction ptrtoint)
(define-cast-instruction inttoptr)

(define-cast-instruction bitcast)

(define-cast-instruction addrspacecast)

(define-instruction-alternative lvalue-other
  icmp fcmp phi select call va_arg landingpad)

(define-cg-llvm-rule nolvalue-other-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative other)

(define-cg-llvm-rule phi-arg (type)
  (white-[]
    (cap a
	 (|| (descend-with-rule 'llvm-constant-value type)
	     llvm-identifier))
    (v white-comma)
    (cap b llvm-identifier))
  `(,(recap a)
     ,(recap b)))

(define-instruction-rule phi
  (let ((type (wh llvm-type)))
    (v whitespace)
    (let ((first-arg (descend-with-rule 'phi-arg type)))
      (let ((rest-args (times (progn (v white-comma)
				     (descend-with-rule 'phi-arg type)))))
	`(,type ,first-arg ,@rest-args)))))


(define-simple-instruction-rule select (cond 
					 val1 
					 val2 )
  `(,cond ,val1 ,val2))

(define-simple-instruction-rule va_arg (va-list)
  (v white-comma)
  `(,va-list ,(v llvm-type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-icmp-ops
    '(eq
      ne
      ugt
      uge
      ult
      ule
      sgt
      sge
      slt
      sle))
  (defparameter known-fcmp-ops
    '(false
      oeq
      ogt
      oge
      olt
      ole
      one
      ord
      ueq
      ugt
      uge
      ult
      ule
      une
      uno
      true)))

(defmacro icmp-kwds ()
  (any-of-kwds known-icmp-ops))

(defmacro fcmp-kwds ()
  (any-of-kwds known-fcmp-ops))

(defmacro define-cmp-rule (name)
  (let ((macro-name (intern ;;#"$((string name))-KWDS"
		     (interpol
		      (string name)
		      "-KWDS"))))
    `(define-instruction-rule ,name
       (let ((cond (wh (,macro-name)))
	     (type (wh llvm-type)))
	 (let ((val1 (wh (v instr-arg-value type))))
	   (v white-comma)
	   (let ((val2 (v instr-arg-value type)))
	     `(,cond ,type ,val1 ,val2)))))))

(define-cmp-rule icmp)

(define-cmp-rule fcmp)

(define-cg-llvm-rule cleanup-kwd ()
  (v "cleanup")
  '(:cleanup t))

(define-cg-llvm-rule catch-landingpad-clause ()
  (v "catch")
  (list :catch (wh llvm-constant)))

(define-cg-llvm-rule filter-landingpad-clause ()
  (v "filter")
  (list :filter 
	(wh llvm-constant)))

(define-cg-llvm-rule landingpad-clause ()
  (|| catch-landingpad-clause
      filter-landingpad-clause))

(define-instruction-rule landingpad
  (let ((type (wh llvm-type)))
    (wh "personality")
    (let ((pers (wh llvm-constant)))
      ;; check
      (let ((clauses (|| (cons (wh cleanup-kwd) (times (wh landingpad-clause)))
			 (postimes (wh landingpad-clause)))))
	`(,type ,pers ,@clauses)))))


;;;;<result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]

(define-cg-llvm-rule call-instruction ()
  (with-metadata
    (let ((tail (? (|| (progn (prog1 (v "tail")
				(v whitespace))
			      :tail)
		       (progn (prog1 (v "musttail")
				(v whitespace))
			      :must-tail)))))
      (v "call")
      (let ((cconv (?wh cconv))
	    (return-attrs (?wh 
			       (v parameter-attrs)))
	    (type (wh llvm-type))
	    (ftype (?wh (v llvm-type)))
	    (fnptrval (wh llvm-identifier))
	    (args (wh? (white-paren
			 (? funcall-args))))
	    (fun-attrs (?wh 
			    (v fun-attrs))))
	`(call ,type ,fnptrval ,args
	       ,@(%%inject-kwds-if-nonnil cconv return-attrs ftype fun-attrs tail))))))

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
	`(,@arg ,@(%%inject-kwd-if-nonnil attrs)))))

(define-plural-rule %defun-args defun-arg white-comma)

(define-cg-llvm-rule defun-args ()
  (white-paren
    (cap args (? %defun-args))
    ;; TODO : here we check for vararg special syntax
    )
  (recap? args))

(define-cg-llvm-rule short-defun-arg ()
  `(,(v llvm-type)))
(define-cg-llvm-rule long-defun-arg ()
  `(,@(v short-defun-arg) ,(wh? local-identifier)))

(define-cg-llvm-rule vararg-sign ()
  (v "...")
  :vararg)

(define-cg-llvm-rule declfun-arg ()
  (|| vararg-sign
      (let* ((type (v llvm-type))
	     (attrs (?wh (v parameter-attrs)))
	     (id (? (wh? local-identifier))))
	`(,type ,@(%%inject-if-nonnil id) ,@(%%inject-kwd-if-nonnil attrs)))))

(define-plural-rule %declfun-args declfun-arg white-comma)

(define-cg-llvm-rule declfun-args ()
  (white-paren
    (cap args (? %declfun-args))
    ;; TODO : here we check for vararg special syntax
    )
  (recap? args))

;;; Let's write something that is able to parse whole basic block of instructions

(define-instruction-alternative lvalue-nonterminator
  lvalue-other
  lvalue-conversion
  lvalue-memory
  lvalue-aggregate
  lvalue-bitwise-binary
  lvalue-binary)

(define-instruction-alternative nolvalue-nonterminator
  nolvalue-other
  nolvalue-conversion
  nolvalue-memory
  nolvalue-aggregate
  nolvalue-bitwise-binary
  nolvalue-binary)

(define-cg-llvm-rule block-label ()
  (text (list (literal-char #\%)
	      (prog1 (v identifier-body)
		(v #\:)))))
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

;;;;FIXME::put into a test file?
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
      `(block ,@(%%inject-kwd-if-nonnil label)
	 ,@basic-block-body))))

(define-plural-rule basic-blocks basic-block whitespace)

(define-cg-llvm-rule function-body ()
  (white-{}
    (? basic-blocks)))

(define-cg-llvm-rule fundef-metadata-entry ()
  ;; FIXME : in future the syntax of LLVM will likely become more flexible,
  ;;         and hence this place would have to be changed.
  (list (v  metadata-identifier)
	(progn (v whitespace)
	       (v metadata-identifier))))

(define-plural-rule fundef-metadata fundef-metadata-entry whitespace)

(define-op-rule (function-definition define)
  (let* ((linkage (?wh linkage-type))
	 (visibility (?wh visibility-style))
	 (dll-storage-class (?wh dll-storage-class))
	 (cconv (?wh cconv))
	 (unnamed-addr (?wh-p "unnamed_addr"))
	 (type (wh (v llvm-type)))
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
	    ,@(%%inject-kwds-if-nonnil linkage visibility dll-storage-class
				       cconv unnamed-addr return-attrs
				       fun-attrs section align comdat gc
				       prefix prologue personality metadata
				       body))))

;;;;@<GlobalVarName> = [Linkage] [PreemptionSpecifier] [Visibility]
;;;;                   [DLLStorageClass] [ThreadLocal]
;;;;                   [(unnamed_addr|local_unnamed_addr)] [AddrSpace]
;;;;                   [ExternallyInitialized]
;;;;                   <global | constant> <Type> [<InitializerConstant>]
;;;;                   [, section "name"] [, comdat [($name)]]
;;;;                   [, align <Alignment>] (, !name !N)*

;;;;FIXME:: add local_unnamed_addr and PreemptionSpecifier

(define-cg-llvm-rule llvm-constant-or-expression-value? (&optional type)
  (|| (v llvm-constant-value type)
      constant-expression-value))

(define-cg-llvm-rule global-variable-definition ()
  (let ((name (v global-identifier)))
    (v whitespace)
    (v #\=)
    (let* ((linkage (?wh linkage-type))
	   (preemption-specifier (?wh |PreemptionSpecifier|))
      	   (visibility (?wh visibility-style))
	   (dll-storage-class (?wh dll-storage-class))
	   (thread-local (?wh thread-local))
	   (unnamed-addr (?wh-p "unnamed_addr"))
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
	   (type (wh? llvm-type))
	   ;;FIXME:: llvm-constant-expression or llvm-constant-value?
	   (value (flet ((descend ()
			  (?wh (v llvm-constant-or-expression-value? type))))
		    (if (eq :external linkage)
			(? (descend))
			(descend))))
	   (section (? (progn-v
			white-comma
			section)))
	   (comdat (? (progn-v
		       white-comma
		       comdat)))
	   (align (? (progn-v
		      white-comma
		      align))))
      `(:global-var ,name ,type ,value
		    ,@(append
		       (%%inject-kwds-if-nonnil
			linkage
			preemption-specifier
			visibility dll-storage-class
			unnamed-addr addrspace
			externally-initialized
			constant
			section comdat)
		       (%%inject-stuff-if-nonnil thread-local align))
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
  (nest
   (progn (? whitespace))
   (progn (v #\#))
   (let- (id (v pos-integer)))
   (progn (? whitespace))
   (progn (v #\=))
   (progn (? whitespace))    
   (let- (attrs (white-{}
		   abstract-attrs)))
   `(,id ,@attrs)))

(define-op-rule (blockaddress blockaddress)
  (v wh?)
  (white-paren
    (cap a global-identifier)
    (v white-comma)
    (cap b local-identifier))
  `(,(recap a) ,(recap b)))

