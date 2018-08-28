(in-package #:cg-llvm)

(defmacro define-constant-rules (name &body value-rule-body)
  (let ((type-rule-name
	 (intern
	  ;;#"$(name)-CONSTANT-TYPE"
	  (interpol       
	   name
	   "-CONSTANT-TYPE")))
	(value-rule-name
	 (intern
	  ;;#"$(name)-CONSTANT-VALUE"
	  (interpol
	   name
	   "-CONSTANT-VALUE")))
	(rule-name
	 (intern
	  ;;#"$(name)-CONSTANT"
	  (interpol
	   name
	   "-CONSTANT"))))
    `(progn (define-cg-llvm-rule ,type-rule-name ()
	      (v llvm-type))
	    (define-cg-llvm-rule ,value-rule-name (type)
	      ,@value-rule-body)
	    (define-cg-llvm-rule ,rule-name ()
	      (let ((type (v ,type-rule-name)))
		(list type (wh (descend-with-rule ',value-rule-name type))))))))


(define-constant-rules boolean
  (text (|| "true" "false")))

(define-constant-rules integer
  (v integer))

(define-constant-rules float
  (v llvm-float))

(define-constant-rules null-ptr
  (v "null"))

(define-constant-rules global-ident
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
  (let ((ltype type))
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
  `(,(v llvm-type)
     ,(wh llvm-variable-value)))

(define-cg-llvm-rule llvm-undef-value ()
  (v "undef")
  :undef)

(define-cg-llvm-rule llvm-undef ()
  `(,(v llvm-type)
     ,(wh llvm-undef-value)))

(define-cg-llvm-rule instr-arg ()
  (|| llvm-variable
      llvm-undef
      llvm-constant))

(define-cg-llvm-rule instr-arg-value (&optional type)
  (|| llvm-variable-value
      llvm-undef-value
      (descend-with-rule 'llvm-constant-value type)))

(define-plural-rule llvm-constants llvm-constant white-comma)

(defmacro define-complex-constant-rules (name parens)
  `(define-constant-rules ,name
     (,parens
      llvm-constants)))

;;;;FIXME::remove typechecking for structures, arrays, Vectors
;;;;postpone for later.
(define-complex-constant-rules structure
    white-{})

(define-complex-constant-rules array
    white-[])

(define-complex-constant-rules vector
    white-<>)

(define-constant-rules string
  (v #\c)
  (mapcar (lambda (x)
	    `((integer 8) ,(char-code x)))
	  (coerce (v llvm-string) 'list)))

(define-constant-rules zero-init
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
