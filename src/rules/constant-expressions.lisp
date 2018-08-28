(in-package #:cg-llvm)

(defmacro define-cast-constexpr (name)
  (let ((rule-name (intern ;;#"$((string name))-TO-CONSTEXPR"
		    (interpol
		     (string name)
		     "-TO-CONSTEXPR"
		     ))))
    `(define-op-rule (,rule-name ,name)
       (? whitespace)
       (v #\()
       (? whitespace)
       (destructuring-bind (type1 value) (v llvm-constant)
	 (v whitespace)
	 (v "to")
	 (v whitespace)
	 (let ((type2 (prog1 (v llvm-type)
			(? whitespace)
			(v #\)))))
	   `(,value ,type1 ,type2))))))

(define-cast-constexpr trunc)
(define-cast-constexpr zext)
(define-cast-constexpr sext)
(define-cast-constexpr fptrunc)
(define-cast-constexpr fpext)
(define-cast-constexpr fptoui)
(define-cast-constexpr fptosi)
(define-cast-constexpr uitofp)
(define-cast-constexpr sitofp)
(define-cast-constexpr ptrtoint)
(define-cast-constexpr inttoptr)

(define-cast-constexpr bitcast)
(define-cast-constexpr addrspacecast)

(define-instruction-alternative (cast to-constexpr)
  trunc zext sext fptrunc fpext fptoui fptosi uitofp sitofp ptrtoint inttoptr
  bitcast addrspacecast)

(define-op-rule (select-constexpr select)
  (let* ((cond (?wh (progn (v #\()
			   (v wh?)
			   (v llvm-constant))))
	 (val1 (progn (v white-comma)
		      (v llvm-constant)))
	 (val2 (progn (v white-comma)
		      (v llvm-constant))))
    `(,cond ,val1 ,val2)))

(defmacro define-cmp-constexpr (name)
  (let ((macro-name (intern ;;#"$((string name))-KWDS"
		     (interpol
		      (string name)
		      "-KWDS")))
	(constexpr-name (intern ;;#"$((string name))-CONSTEXPR"
			 (interpol
			  (string name)
			  "-CONSTEXPR"))))
    `(define-op-rule (,constexpr-name ,name)
       (let* ((cond (wh (,macro-name)))
	      (val1 (progn (v wh?)
			   (v #\()
			   (v wh?)
			   (v llvm-constant)))
	      (val2 (prog1 (progn (v white-comma)
				  (v llvm-constant))
		      (v wh?)
		      (v #\)))))
	 `(,cond ,val1 ,val2)))))

(define-cmp-constexpr icmp)
(define-cmp-constexpr fcmp)

(define-op-rule (insertvalue-constexpr insertvalue)
  (let ((val (progn (v wh?)
		    (v #\()
		    (v wh?)
		    (v llvm-constant))))
    (v white-comma)
    (let* ((elt (v llvm-constant))
	   (indices (prog1 (v indices)
		      (v wh?)
		      (v #\)))))
      ;; TODO : check that elt has same type as elt of val
      `(,val ,elt ,@indices))))

(define-op-rule (extractvalue-constexpr extractvalue)
  (let ((val (progn (v wh?)
		    (v #\()
		    (v wh?)
		    (v llvm-constant))))
    (v white-comma)
    (let* ((indices (prog1 (v indices)
		      (v wh?)
		      (v #\)))))
      ;; TODO : check that elt has same type as elt of val
      `(,val ,@indices))))

(define-op-rule (shufflevector-constexpr shufflevector)
  (let* ((v1 (progn (v wh?)
		    (v #\()
		    (v wh?)
		    (v llvm-constant)))
	 (v2 (progn (v white-comma)
		    (v llvm-constant)))
	 (mask (prog1 (progn (v white-comma)
			     (v llvm-constant))
		 (v wh?)
		 (v #\)))))
    `(,v1 ,v2 ,mask)))

(define-op-rule (extractelement-constexpr extractelement)
  (let* ((val (progn (v wh?)
		     (v #\()
		     (v wh?)
		     (v llvm-constant)))
	 (idx (prog1 (progn (v white-comma)
			    (v llvm-constant))
		(v wh?)
		(v #\)))))
    `(,val ,idx)))

(define-op-rule (insertelement-constexpr insertelement)
  (let* ((val (progn (v wh?)
		     (v #\()
		     (v wh?)
		     (v llvm-constant)))
	 (elt (progn (v white-comma)
		     (v llvm-constant)))
	 (idx (prog1 (progn (v white-comma)
			    (v llvm-constant))
		(v wh?)
		(v #\)))))
    `(,val ,elt ,idx)))


(define-op-rule (getelementptr-constexpr getelementptr)
  (let* ((inbounds (?wh (progn (v "inbounds")
			       t)))
	 (type (progn (v wh?)
		      (v #\()
		      (v wh?)
		      (v llvm-constant)))
	 (indices (progn (v white-comma)
			 (prog1 (v geteltptr-indices)
			   (v wh?)
			   (v #\))))))
    `(,type ,@indices ,@(%%inject-kwd-if-nonnil inbounds))))


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
  `(,(v llvm-type)
     ,(wh constant-expression-value)))

