
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

(defmacro define-constant-rules (name typecheck errinfo &body value-rule-body)
  (let ((type-rule-name (intern #?"$(name)-CONSTANT-TYPE"))
	(value-rule-name (intern #?"$(name)-CONSTANT-VALUE"))
	(rule-name (intern #?"$(name)-CONSTANT")))
    `(progn (define-cg-llvm-rule ,type-rule-name ()
	      (let ((type (emit-lisp-repr (v llvm-type))))
		(if (not ,typecheck)
		    (fail-parse-format ,@errinfo)
		    type)))
	    (define-cg-llvm-rule ,value-rule-name (type)
	      ,@value-rule-body)
	    (define-cg-llvm-rule ,rule-name ()
	      (let ((type (v ,type-rule-name)))
		(list type (wh (descend-with-rule ',value-rule-name type))))))))
	    

(defmacro define-typeguarding-constant-rules (name typecheck errinfo &body value-rule-body)
  `(define-constant-rules ,name ,typecheck ,errinfo
     (if (and type (not ,typecheck))
	 (fail-parse-format ,@errinfo))
     ,@value-rule-body))

(define-typeguarding-constant-rules boolean
    (llvm-typep '(integer 1) type)
    ((literal-string "Boolean must really be integer of 1 bit."))
    (text (|| "true" "false")))

(define-typeguarding-constant-rules integer
    (llvm-typep '(integer *) type)
    ((literal-string "Integer constant must be of integer type but got ~a.")
     type)
  (v integer))

(define-cg-llvm-rule sign ()
  (|| "+" "-"))

(define-cg-llvm-rule decimal-float ()
  (parse-number:parse-number
   (text (? sign)
	 (times ns-dec-digit)
	 (v #\.)
	 (times ns-dec-digit)
	 (? (list (v #\e)
		  (v sign)
		  (postimes ns-dec-digit))))))

;; TODO : hexadecimal float ???
(define-cg-llvm-rule hexadecimal-float ()
  (fail-parse "Hexadecimal float is not implemented yet."))

(define-cg-llvm-rule llvm-float ()
  (|| decimal-float
      hexadecimal-float))

(define-typeguarding-constant-rules float
    (llvm-typep '(float ***) type)
    ((literal-string "Float constant must be of float type but got ~a.") type)
  (v llvm-float))

(define-typeguarding-constant-rules null-ptr
    (llvm-typep '(pointer ***) type)
    ((literal-string "Null ptr constant must be of pointer type but got ~a.") type)
  (v "null"))

(define-typeguarding-constant-rules global-ident
    (llvm-typep '(pointer ***) type)
    ((literal-string "Global identifier must be of pointer type, but got ~a.") type)
  (v llvm-identifier))

(define-cg-llvm-rule global-ident-constant ()
  (let ((type (v global-ident-constant-type)))
    (list type
	  (wh (descend-with-rule 'global-ident-constant-value type)))))

(define-cg-llvm-rule simple-constant ()
  (|| boolean-constant
      integer-constant
      float-constant
      null-ptr-constant
      global-ident-constant))

(define-cg-llvm-rule simple-constant-value (type)
  (let ((ltype (emit-lisp-repr type)))
    (|| (descend-with-rule 'boolean-constant-value ltype)
	(descend-with-rule 'integer-constant-value ltype)
	(descend-with-rule 'float-constant-value ltype)
	(descend-with-rule 'null-ptr-constant-value ltype)
	(descend-with-rule 'global-ident-constant-value ltype))))


(define-cg-llvm-rule ordinary-constant ()
  (|| simple-constant
      complex-constant
      blockaddress
      constant-expression))

(define-cg-llvm-rule ordinary-constant-value (type)
  (|| (descend-with-rule 'simple-constant-value type)
      (descend-with-rule 'complex-constant-value type)))

(define-cg-llvm-rule llvm-constant ()
  (|| ordinary-constant
       metadata-constant))

(define-cg-llvm-rule llvm-constant-value (type)
  (|| (descend-with-rule 'ordinary-constant-value type)
      (descend-with-rule 'metadata-node-value type)))

(define-cg-llvm-rule llvm-variable-value ()
  (v llvm-identifier))

(define-cg-llvm-rule llvm-variable ()
  `(,(emit-lisp-repr (v llvm-type))
     ,(wh llvm-variable-value)))

(define-cg-llvm-rule llvm-undef-value ()
  (v "undef")
  :undef)
  
(define-cg-llvm-rule llvm-undef ()
  `(,(emit-lisp-repr (v llvm-type))
     ,(wh llvm-undef-value)))

(define-cg-llvm-rule instr-arg ()
  (|| llvm-variable
      llvm-undef
      llvm-constant))

(define-cg-llvm-rule instr-arg-value ()
  (|| llvm-variable-value
      llvm-undef-value
      llvm-constant-value))

(define-plural-rule llvm-constants llvm-constant (progn (? whitespace)
							(v #\,)
							(? whitespace)))

(defmacro define-complex-constant-rules (name lb rb typecheck errstr1 errstr2 contentcheck)
  (let ((errstr (join "" errstr1 " constant must be of "
		      errstr2 " type, but got ~a")))
    `(define-constant-rules ,name ,typecheck (,errstr type)
       (let ((content (progm (progn (descend-with-rule 'string ,lb)
				    (? whitespace))
			     llvm-constants
			     (progn (? whitespace)
				    (descend-with-rule 'string ,rb)))))
	 (if type
	     ,contentcheck)
	 content))))

(define-complex-constant-rules structure
    "{" "}" (llvm-typep '(struct ***) type) "Structure" "structure"
    (if (not (equal (length (cadr type)) (length content)))
	(fail-parse "Number of elements of type and content do not match.")
	(iter (for theor-subtype in (cadr type))
	      (for (expr-subtype nil) in content)
	      (if (not (llvm-same-typep theor-subtype expr-subtype))
		  (fail-parse "Type of structure field does not match declared one.")))))

(PROGN
 (DEFINE-CG-LLVM-RULE ARRAY-CONSTANT-TYPE
     NIL
   (LET ((TYPE (EMIT-LISP-REPR (V LLVM-TYPE))))
     (IF (NOT (LLVM-TYPEP '(ARRAY ***) TYPE))
	 (FAIL-PARSE-FORMAT "Array constant must be of array type, but got ~a" TYPE)
	 TYPE)))
 (DEFINE-CG-LLVM-RULE ARRAY-CONSTANT-VALUE
     (TYPE)
   (LET ((CONTENT
	  (PROGM (PROGN (DESCEND-WITH-RULE 'STRING "[")
			(? WHITESPACE))
		 (V LLVM-CONSTANTS)
		 (PROGN (? WHITESPACE)
			(DESCEND-WITH-RULE 'STRING "]")))))
     (IF TYPE
	 (IF (NOT (EQUAL (CADDR TYPE) (LENGTH CONTENT)))
	     (FAIL-PARSE "Number of elements of type and content do not match.")
	     (ITER
	       (FOR (EXPR-SUBTYPE NIL) IN CONTENT)
	       (IF (NOT (LLVM-SAME-TYPEP (CADR TYPE) EXPR-SUBTYPE))
		   (FAIL-PARSE "Type of array element does not match declared one.")))))
     CONTENT))
 (DEFINE-CG-LLVM-RULE ARRAY-CONSTANT
     NIL
   (LET ((TYPE (V ARRAY-CONSTANT-TYPE)))
     (LIST TYPE (WH (DESCEND-WITH-RULE 'ARRAY-CONSTANT-VALUE TYPE))))))
#+nil
(define-complex-constant-rules array
    "[" "]" (llvm-typep '(array ***) type) "Array" "array"
    (if (not (equal (caddr type) (length content)))
	(fail-parse "Number of elements of type and content do not match.")
	(iter (for (expr-subtype nil) in content)
	      (if (not (llvm-same-typep (cadr type) expr-subtype))
		  (fail-parse "Type of array element does not match declared one.")))))


(define-complex-constant-rules vector
    "<" ">" (llvm-typep '(vector ***) type) "Vector" "vector"
    (if (not (equal (caddr type) (length content)))
	(fail-parse "Number of elements of type and content do not match.")
	(iter (for (expr-subtype nil) in content)
	      (if (not (llvm-same-typep (cadr type) expr-subtype))
		  (fail-parse "Type of vector element does not match declared one.")))))


(define-constant-rules string
    (llvm-typep '(array (integer 8) *) type)
    ((literal-string "String constant must be of array-of-chars type, but got ~a") type)
  (v #\c)
  (mapcar (lambda (x)
	    `((integer 8) ,(char-code x)))
	  (coerce (v llvm-string) 'list)))

(define-constant-rules zero-init
    t
    ("")
  (v "zeroinitializer")
  :zero-initializer)

(define-cg-llvm-rule complex-constant ()
  (|| structure-constant
      array-constant
      string-constant ; just a special syntax for array of chars
      vector-constant
      zero-init-constant))

(define-cg-llvm-rule complex-constant-value (type)
  (|| (descend-with-rule 'structure-constant-value type)
      (descend-with-rule 'array-constant-value type)
      (descend-with-rule 'string-constant-value type) ; just a special syntax for array of chars
      (descend-with-rule 'vector-constant-value type)
      (descend-with-rule 'zero-init-constant-value type)))
