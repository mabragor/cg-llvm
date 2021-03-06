
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)


(defmacro define-cast-constexpr (name &body constraints)
  (let ((rule-name (intern #?"$((string name))-TO-CONSTEXPR")))
    `(define-op-rule (,rule-name ,name)
       #\( (? whitespace)
       (destructuring-bind (type1 value) llvm-constant
	 whitespace "to" whitespace
	 (let ((type2 (emit-lisp-repr (prog1 llvm-type (? whitespace) #\)))))
	   ,@constraints
	   `(,value ,type1 ,type2))))))

(define-cast-constexpr trunc (integer->integer-based first-bitsize-larger))
(define-cast-constexpr zext (integer->integer-based first-bitsize-smaller))
(define-cast-constexpr sext (integer->integer-based first-bitsize-smaller))
(define-cast-constexpr fptrunc (float->float-based first-bitsize-larger))
(define-cast-constexpr fpext (float->float-based first-bitsize-smaller))
(define-cast-constexpr fptoui (float->integer-based))
(define-cast-constexpr fptosi (float->integer-based))
(define-cast-constexpr uitofp (integer->float-based))
(define-cast-constexpr sitofp (integer->float-based))
(define-cast-constexpr ptrtoint (pointer->integer-based))
(define-cast-constexpr inttoptr (integer->pointer-based))

(define-cast-constexpr bitcast (bitcast-constraints))
(define-cast-constexpr addrspacecast (pointer->pointer-based have-different-addrspaces))

(define-instruction-alternative (cast to-constexpr)
  trunc zext sext fptrunc fpext fptoui fptosi uitofp sitofp ptrtoint inttoptr
  bitcast addrspacecast)

(define-op-rule (select-constexpr select)
  (let* ((cond (?wh (progn #\( wh? (let ((it llvm-constant))
				     (if (not (or (llvm-typep '(integer 1) (car it))
						  (llvm-typep '(vector (integer 1) *) (car it))))
					 (fail-parse "SELECT constexpr 'cond' argument should be byte(???)")
					 it)))))
	 (val1 (progn white-comma llvm-constant))
	 (val2 (progn white-comma llvm-constant)))
    (if (llvm-typep '(integer *) (car cond))
	(if (not (llvm-same-typep (car val1) (car val2)))
	    (fail-parse-format "Different values of SELECT constexpr should be same type, but are: ~a ~a"
			       (car val1) (car val2)))
	(let ((nelts (caddar cond)))
	  (if (not (and (llvm-typep '(vector ***) (car val1))
			(llvm-typep '(vector ***) (car val2))
			(llvm-same-typep (cadar val1) (cadar val2))
			(equal nelts (caddar val1))
			(equal nelts (caddar val2))))
	      (fail-parse-format "Different values of SELECT constexpr should be same type, but are: ~a ~a"
				 (car val1) (car val2)))))
    `(,cond ,val1 ,val2)))




(defmacro define-cmp-constexpr (name typecheck &rest errinfo)
  (let ((macro-name (intern #?"$((string name))-KWDS"))
	(constexpr-name (intern #?"$((string name))-CONSTEXPR")))
    `(define-op-rule (,constexpr-name ,name)
       (let* ((cond (wh (,macro-name)))
	      (val1 (progn wh? #\( wh? llvm-constant))
	      (val2 (prog1 (progn white-comma llvm-constant) wh? #\))))
	 (if (not ,typecheck)
	     (fail-parse-format ,@errinfo))
	 `(,cond ,val1 ,val2)))))

(define-cmp-constexpr icmp
    (or (llvm-typep '(integer ***) (car val1))
	(llvm-typep '(vector (integer ***) *) (car val1))
	(llvm-typep '(pointer ***) (car val1))
	(llvm-typep '(vector (pointer ***) *) (car val1)))
  "Type of arguments of ICMP constexpr should be INTEGER or POINTER or~
    VECTOR of INTEGERS or VECTOR of POINTERS")

(define-cmp-constexpr fcmp
    (or (llvm-typep '(float ***) (car val1))
	(llvm-typep '(vector (float ***) *) (car val1)))
  "Type of arguments of FCMP constexpr should be FLOAT or~
    VECTOR of FLOATs")

(define-op-rule (insertvalue-constexpr insertvalue)
  (let ((val (progn wh? #\( wh? llvm-constant)))
    (if (not (or (llvm-typep '(struct ***) (car val))
		 (llvm-typep '(array ***) (car val))))
	(fail-parse-format "Type of INSERTVALUE constexpr should be aggregate, but got ~a" (car val)))
    white-comma
    (let* ((elt llvm-constant)
	   (indices (prog1 indices wh? #\))))
      ;; TODO : check that elt has same type as elt of val
      `(,val ,elt ,@indices))))

(define-op-rule (extractvalue-constexpr extractvalue)
  (let ((val (progn wh? #\( wh? llvm-constant)))
    (if (not (or (llvm-typep '(struct ***) (car val))
		 (llvm-typep '(array ***) (car val))))
	(fail-parse-format "Type of EXTRACTVALUE constexpr should be aggregate, but got ~a" (car val)))
    white-comma
    (let* ((indices (prog1 indices wh? #\))))
      ;; TODO : check that elt has same type as elt of val
      `(,val ,@indices))))

(define-op-rule (shufflevector-constexpr shufflevector)
  (let* ((v1 (progn wh? #\( wh? llvm-constant))
	 (v2 (progn white-comma llvm-constant))
	 (mask (prog1 (progn white-comma llvm-constant) wh? #\))))
    (if (not (and (llvm-typep '(vector ***) (car v1))
		  (llvm-typep '(vector ***) (car v2))
		  (llvm-typep '(vector (integer 32) *) (car mask))))
	(fail-parse "Shuffle vector accepts two vectors and one i32 mask"))
    (if (not (llvm-same-typep (cadar v1) (cadar v2)))
	(fail-parse "V1 and V2 must have same subtype"))
    `(,v1 ,v2 ,mask)))

(define-op-rule (extractelement-constexpr extractelement)
  (let* ((val (progn wh? #\( wh? llvm-constant))
	(idx (prog1 (progn white-comma llvm-constant) wh? #\))))
    (if (not (llvm-typep '(vector ***) (car val)))
	(fail-parse-format "EXTRACTELEMENT constexpr 1st arg is vector type, but got: ~a" (car val)))
    (if (not (llvm-typep '(integer ***) (car idx)))
	(fail-parse-format "EXTRACTELEMENT constexpr 2nd arg is integer type, but got: ~a" (car idx)))
    `(,val ,idx)))

(define-op-rule (insertelement-constexpr insertelement)
  (let* ((val (progn wh? #\( wh? llvm-constant))
	 (elt (progn white-comma llvm-constant))
	 (idx (prog1 (progn white-comma llvm-constant) wh? #\))))
    (if (not (llvm-typep '(vector ***) (car val)))
	(fail-parse-format "INSERTELEMENT constexpr 1st arg is vector type, but got: ~a" (car val)))
    (if (not (llvm-typep '(integer ***) (car idx)))
	(fail-parse-format "INSERTELEMENT constexpr 3rd arg is integer type, but got: ~a" (car idx)))
    (if (not (llvm-same-typep (car elt) (cadar val)))
	(fail-parse "ELT must be same type as subtype of VAL"))
    `(,val ,elt ,idx)))


(define-op-rule (getelementptr-constexpr getelementptr)
  (let* ((inbounds (?wh (progn "inbounds" t)))
	 (type (progn wh? #\( wh? llvm-constant))
	 (indices (progn white-comma (prog1 geteltptr-indices wh? #\)))))
    `(,type ,@indices ,!m(inject-kwd-if-nonnil inbounds))))


(define-instruction-alternative (special constexpr)
  getelementptr select icmp fcmp extractelement insertelement shufflevector
  extractvalue insertvalue opcode)

(define-instruction-alternative (opcode constexpr)
  add sub mul fadd fsub fmul frem fdiv udiv sdiv urem srem
  shl lshr ashr and or xor)

(define-cg-llvm-rule constant-expression-value ()
  (|| cast-to-constexpr
      special-constexpr))

(define-cg-llvm-rule constant-expression ()
  `(,(emit-lisp-repr llvm-type) ,(wh constant-expression-value)))

