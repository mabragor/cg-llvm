
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

;; OK, let's sketch the syntax for the function declaration and definition

;; (declare function-name (&rest args-with-optional-param-attrs ???)
;;   (:linkage :private | :internal | :available-externally | ...)
;;   (:visibility :default | :hidden | :protected)
;;   (:dll-storage-class :dllimport | :dllexport)
;;   ((:calling-convention | :cconv) :ccc | (:cc 10) | ...)
;;   (:unnamed-addr t)
;;   (:return-type ??? :parameter-attributes ???)
;;   (:align N)
;;   (:gc "foobar")
;;   (:prefix LLVMConstant)
;;   (:prologue LLVMConstant))


;; (define function-name (&rest args-with-optional-param-attrs ???)
;;   (:linkage :private | :internal | :available-externally | ...)
;;   (:visibility :default | :hidden | :protected)
;;   (:dll-storage-class :dllimport | :dllexport)
;;   ((:calling-convention | :cconv) :ccc | (:cc 10) | ...)
;;   (:unnamed-addr t)
;;   (:return-type ??? :parameter-attributes ???)
;;   (:attributes :alwaysinline :cold (:alignstack N) ...) ; this is listy slot
;;   (:section "name")
;;   (:align N)
;;   (:comdat "$name")
;;   (:gc "foobar")
;;   (:prefix LLVMConstant)
;;   (:prologue LLVMConstant)
;;   (:body list-of-basic blocks)


;; A few things come to mind:
;; * since all functions are global (no lambdas), the llvm-convention of adding @ can be automatic
;; * it seems feasible to really write the back-and-forth translation between lispy, text and clos
;;   representations of at least these function definitions and declarations, not to mention the whole LLVM
;; * as usual, the easiest thing is to start with ESRAP rules for parsing text representation.

(defmacro wh (x)
  `(progn whitespace ,x))

(defmacro wh? (x)
  `(progn (? whitespace) ,x))

(defmacro ?wh (x)
  `(? (progn whitespace ,x)))

(defmacro ?wh? (x)
  `(? (progn (? whitespace) ,x)))

(defmacro fail-parse-if-not (cond expr)
  `(let ((it ,expr))
     (if (not ,cond)
	 (fail-parse-format "Assertion is not satisfied: ~a" ',cond)
	 it)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-linkage-types '(private internal available-externally
				      linkonce weak common appending extern-weak
				      linkonce-odr weak-odr external)))

(defmacro define-kwd-rule (name &optional known-var)
  `(define-cg-llvm-rule ,name ()
     (destringify-symbol (|| ,@(mapcar (lambda (x)
					 `(descend-with-rule 'string ,(stringify-symbol x)))
				       (symbol-value (or known-var
							 (intern #?"KNOWN-$(name)S")))))
			 (literal-string "KEYWORD"))))

(define-kwd-rule linkage-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-visibility-styles '(default hidden protected)))

(define-kwd-rule visibility-style)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-dll-storage-classes '(dllimport dllexport)))

(define-kwd-rule dll-storage-class known-dll-storage-classes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-cconvs '(ccc fastccc coldcc webkit-jscc anyregcc preserve-mostcc preserve-allcc))
  (defparameter known-cons-cconvs '((cc pos-integer))))

(defmacro define-consy-kwd-rule (name known-var known-cons-var)
  (let ((g!-name (gensym (string name))))
    `(progn
       (define-kwd-rule ,g!-name ,known-var)
       (define-cg-llvm-rule ,name ()
	 (|| (descend-with-rule ',g!-name)
	     ,@(mapcar (lambda (x)
			 `(list (destringify-symbol (descend-with-rule 'string
								       ,(stringify-symbol (car x)))
						    (literal-string "KEYWORD"))
				(progn (descend-with-rule 'whitespace)
				       (descend-with-rule ',(cadr x)))))
		       (symbol-value known-cons-var)))))))

(define-consy-kwd-rule cconv known-cconvs known-cons-cconvs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-parameter-attrs '(zeroext signext inreg byval
					inalloca sret noalias nocapture
					nest returned nonull))
  (defparameter known-cons-parameter-attrs '((align pos-integer)))
  (defparameter known-algol-parameter-attrs '((dereferenceable pos-integer))))


(defmacro define-algol-consy-kwd-rule (name known-var known-cons-var known-algol-var)
  (let ((g!-name (gensym (string name))))
    `(progn
       (define-consy-kwd-rule ,g!-name ,known-var ,known-cons-var)
       (define-cg-llvm-rule ,name ()
	 (|| (descend-with-rule ',g!-name)
	     ,@(mapcar (lambda (x)
			 `(list (destringify-symbol (descend-with-rule 'string
								       ,(stringify-symbol (car x)))
						    (literal-string "KEYWORD"))
				(progm "(" (descend-with-rule ',(cadr x)) ")")))
		       (symbol-value known-algol-var)))))))


(define-algol-consy-kwd-rule parameter-attr
    known-parameter-attrs 
  known-cons-parameter-attrs
  known-algol-parameter-attrs)

(defmacro!! define-plural-rule (name single delim) ()
  `(define-cg-llvm-rule ,name ()
     (cons ,single
	   (times (progn ,delim ,single)))))


(define-plural-rule parameter-attrs parameter-attr whitespace)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-fun-attrs '(alwaysinline builtin cold inlinehint jumptable minsize
				  naked nobuiltin noduplicate noimplicitfloat noinline
				  nonlazybind noredzone noreturn nounwind optnone optsize
				  readnone readonly returns-twice sanitize-address
				  sanitize-memory sanitize-thread ssp sspreq sspstrong
				  thunk uwtable))
  (defparameter known-cons-fun-attrs '())
  (defparameter known-algol-fun-attrs '((alignstack pos-integer))))

(define-algol-consy-kwd-rule fun-attr known-fun-attrs known-cons-fun-attrs known-algol-fun-attrs)

(define-plural-rule fun-attrs fun-attr whitespace)

(define-cg-llvm-rule unnamed-addr ()
  "unnamed_addr"
  '(:unnamed-addr t))

(define-cg-llvm-rule alpha-char ()
  (character-ranges (#\a #\z) (#\A #\Z)))

(define-cg-llvm-rule alphanumeric-char ()
  (character-ranges (#\0 #\9) (#\a #\z) (#\A #\Z)))

(define-cg-llvm-rule alphanumeric-word ()
  (text (postimes alphanumeric-char)))

(define-cg-llvm-rule section ()
  "section" whitespace #\" c!-1-alphanumeric-word #\"
  `(:section ,c!-1))

(defmacro!! define-python-rule (name subrule)
    ()
  (destructuring-bind (rule-name cmd-name) (if (atom name) (list name name) name)
    `(define-cg-llvm-rule ,rule-name ()
       `(,,(intern (string cmd-name) (literal-string "KEYWORD"))
	   ,(progn (descend-with-rule 'string ,(stringify-symbol cmd-name))
		   whitespace ,subrule)))))

(defmacro!! define-algol-rule (name subrule)
    ()
  (destructuring-bind (rule-name cmd-name) (if (atom name) (list name name) name)
    `(define-cg-llvm-rule ,rule-name ()
       `(,,(intern (string cmd-name) (literal-string "KEYWORD"))
	   ,(progn (descend-with-rule 'string ,(stringify-symbol cmd-name))
		   "(" (prog1 ,subrule ")"))))))


(define-python-rule align pos-integer)
(define-algol-rule comdat (progn "$" alphanumeric-word))
(define-python-rule (gc-name gc) (progm #\" alphanumeric-word #\"))

(define-python-rule prefix llvm-constant)
(define-python-rule prologue llvm-constant)

;; TODO : attribute groups ?

(defmacro!! define-constant-rules (name typecheck errinfo &body value-rule-body) ()
  (let ((type-rule-name (intern #?"$(name)-CONSTANT-TYPE"))
	(value-rule-name (intern #?"$(name)-CONSTANT-VALUE"))
	(rule-name (intern #?"$(name)-CONSTANT")))
    `(progn (define-cg-llvm-rule ,type-rule-name ()
	      (let ((type (emit-lisp-repr llvm-type)))
		(if (not ,typecheck)
		    (fail-parse-format ,@errinfo)
		    type)))
	    (define-cg-llvm-rule ,value-rule-name (type)
	      ,@value-rule-body)
	    (define-cg-llvm-rule ,rule-name ()
	      (let ((type ,type-rule-name))
		(list type (wh (descend-with-rule ',value-rule-name type))))))))
	    

(defmacro!! define-typeguarding-constant-rules (name typecheck errinfo &body value-rule-body) ()
  `(define-constant-rules ,name ,typecheck ,errinfo
     (if (and type (not ,typecheck))
	 (fail-parse-format ,@errinfo))
     ,@value-rule-body))

(define-typeguarding-constant-rules boolean
    (llvm-typep '(integer 1) type) ((literal-string "Boolean must really be integer of 1 bit."))
    (text (|| "true" "false")))

(define-typeguarding-constant-rules integer
    (llvm-typep '(integer *) type) ((literal-string "Integer constant must be of integer type but got ~a.")
				    type)
  integer)

(define-cg-llvm-rule sign ()
  (|| "+" "-"))

(define-cg-llvm-rule decimal-float ()
  (parse-number:parse-number (text (? sign)
				   (times ns-dec-digit)
				   #\. (times ns-dec-digit)
				   (? (list #\e sign (postimes ns-dec-digit))))))

;; TODO : hexadecimal float ???
(define-cg-llvm-rule hexadecimal-float ()
  (fail-parse "Hexadecimal float is not implemented yet."))

(define-cg-llvm-rule llvm-float ()
  (|| decimal-float
      hexadecimal-float))

(define-typeguarding-constant-rules float
    (llvm-typep '(float ***) type) ((literal-string "Float constant must be of float type but got ~a.") type)
  llvm-float)

(define-typeguarding-constant-rules null-ptr
    (llvm-typep '(pointer ***) type)
    ((literal-string "Null ptr constant must be of pointer type but got ~a.") type)
  (text "null"))

(define-typeguarding-constant-rules global-ident
    (llvm-typep '(pointer ***) type)
    ((literal-string "Global identifier must be of pointer type, but got ~a.") type)
  llvm-identifier)

(define-cg-llvm-rule global-ident-constant ()
  (let ((type global-ident-constant-type))
    (list type (wh (descend-with-rule 'global-ident-constant-value type)))))

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
      complex-constant))

(define-cg-llvm-rule ordinary-constant-value (type)
  (|| (descend-with-rule 'simple-constant-value type)
      (descend-with-rule 'complex-constant-value type)))

(define-cg-llvm-rule llvm-constant ()
  (|| ordinary-constant
      metadata-node))

(define-cg-llvm-rule metadata-node-value (type)
  (if (and type (not (llvm-typep 'metadata type)))
      (fail-parse "METADATA value should have METADATA type"))
  (fail-parse "METADATA-NODE-VALUE is not implemented yet"))

(define-cg-llvm-rule llvm-constant-value (type)
  (|| (descend-with-rule 'ordinary-constant-value type)
      (descend-with-rule 'metadata-node-value type)))

(define-cg-llvm-rule llvm-variable-value ()
  llvm-identifier)

(define-cg-llvm-rule llvm-variable ()
  `(,(emit-lisp-repr llvm-type) ,(wh llvm-variable-value)))

(define-cg-llvm-rule llvm-undef-value ()
  "undef" :undef)
  
(define-cg-llvm-rule llvm-undef ()
  `(,(emit-lisp-repr llvm-type) ,(wh llvm-undef-value)))

(define-cg-llvm-rule instr-arg ()
  (|| llvm-variable
      llvm-undef
      llvm-constant))

(define-cg-llvm-rule instr-arg-value ()
  (|| llvm-variable-value
      llvm-undef-value
      llvm-constant-value))


(define-plural-rule llvm-constants llvm-constant (progn (? whitespace) #\, (? whitespace)))

(defmacro define-complex-constant-rules (name lb rb typecheck errstr1 errstr2 contentcheck)
  (let ((errstr (join "" errstr1 " constant must be of "
		      errstr2 " type, but got ~a")))
    `(define-constant-rules ,name ,typecheck (,errstr type)
       (let ((content (progm (progn (descend-with-rule 'string ,lb) (? whitespace))
			     llvm-constants
			     (progn (? whitespace) (descend-with-rule 'string ,rb)))))
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
  #\c
  (mapcar (lambda (x)
	    `((integer 8) ,(char-code x)))
	  (coerce llvm-string 'list)))

(define-constant-rules zero-init
    t ("")
  "zeroinitializer" :zero-initializer)

(define-cg-llvm-rule metadata-string ()
  #\! `(:metadata ,llvm-string))

(define-cg-llvm-rule named-metadata-identifier ()
  ;; TODO : I don't know precisely what is allowed as a name for named metadata
  #\! `(:metadata-ref ,alphanumeric-word))

(define-cg-llvm-rule unnamed-metadata-identifier ()
  ;; TODO : I don't know precisely what is allowed as a name for named metadata
  #\! `(:metadata-ref ,pos-integer))

(define-cg-llvm-rule metadata-structure ()
  #\! #\{ (? whitespace) c!-1-metadata-nodes #\}
  `(:metadata ,@c!-1))
  
(define-cg-llvm-rule specialized-metadata ()
  (fail-parse "Specialized metadata not implemented yet"))

(defmacro define-specialized-metadata (name &body field-specs)
  (declare (ignore name field-specs))
  ;; TODO : actually implement the thing
  nil)

(define-specialized-metadata m-d-compile-unit
    language file producer is-optimized flags
    runtime-version split-debug-filename emission-kind
    enums retained-types subprograms globals imports)

(define-specialized-metadata m-d-file
    filename directory)

(define-specialized-metadata m-d-basic-type
    name size align encoding tag)

(define-specialized-metadata m-d-subroutine-type
    types)

(define-specialized-metadata m-d-derived-type
    tag base-type size align offset)

(define-specialized-metadata m-d-composite-type
    tag name file line size align identifier elements)

(define-specialized-metadata m-d-subrange
    count lower-bound)

(define-specialized-metadata m-d-enumerator
    name value)

(define-specialized-metadata m-d-template-type-parameter
    name type)

(define-specialized-metadata m-d-template-value-parameter
    tag name type value)

(define-specialized-metadata m-d-namespace
    name scope file line)

(define-specialized-metadata m-d-global-variable
    name linkage-name scope file line type
    is-local is-definition variable declaration)

(define-specialized-metadata m-d-subprogram
    name linkage-name scope file line type is-local
    is-definition scope-line containing-type virtuality
    virtual-index flags is-optimized function template-params
    declaration variables)

(define-specialized-metadata m-d-lexical-block
    scope file line column)

(define-specialized-metadata m-d-lexical-block-file
    scope file line column discriminator)

(define-specialized-metadata m-d-location
    line column scope inlined-at)

(define-specialized-metadata m-d-local-variable
    tag name arg scope file line type flags)

;; The syntax of this one is really different from the rest ...
;; (define-specialized-metadata m-d-expression)
    
(define-specialized-metadata m-d-obj-c-property
    name file line setter getter attributes type)

(define-specialized-metadata m-d-imported-entity
    tag name scope entity line)


(define-cg-llvm-rule metadata-node ()
  (|| metadata-string
      unnamed-metadata-identifier
      named-metadata-identifier
      ordinary-constant
      metadata-structure
      specialized-metadata))

(define-plural-rule metadata-nodes metadata-node (progn (? whitespace) #\, (? whitespace)))

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


(define-cg-llvm-rule usual-identifier ()
  (let ((raw-text (text (list (|| #\@ #\%)
			      (|| alpha-char #\- #\$ #\. #\_)
			      (times (|| alphanumeric-char #\- #\$ #\. #\_))))))
    (handler-case (destringify-symbol raw-text)
      (error () raw-text))))

(define-cg-llvm-rule hex-digit ()
  (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))

(define-cg-llvm-rule double-hex-escaped-char ()
  #\\
  (code-char (parse-number:parse-number (text (times hex-digit :exactly 2))
					:radix 16)))

(define-cg-llvm-rule llvm-string ()
  (text (progm #\"
	       (times (progn (! #\")
			     (|| double-hex-escaped-char
				 (descend-with-rule 'character nil))))
	       #\")))
  

(define-cg-llvm-rule llvm-identifier ()
  (|| usual-identifier
      llvm-string))

(defmacro inject-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list (list ,(intern (string name) "KEYWORD")
		      ,name))))

(defmacro splice-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list ,(intern (string name) "KEYWORD")
		,name)))

(defmacro inject-if-nonnil (smth)
  ``,@(if ,smth
	  (list ,smth)))

(defun return-type-lisp-form (type attrs)
  (declare (ignore type attrs))
  :return-type-placeholder)

(define-cg-llvm-rule function-declaration ()
  "declare"
  (macrolet ((frob (x)
	       `(? (progn whitespace ,x))))
    (let* ((linkage (frob linkage-type))
	   (visibility (frob visibility-style))
	   (dll-storage-class (frob dll-storage-class))
	   (cconv (frob cconv))
	   (unnamed-addr (frob unnamed-addr))
	   (return-type (progn whitespace llvm-type))
	   (return-attrs (frob parameter-attrs))
	   (fname (progn whitespace llvm-identifier))
	   ;; TODO: arguments
	   (args (progn whitespace "(...)"))
	   (align (frob align))
	   (gc (frob gc-name))
	   (prefix (frob prefix))
	   ;; TODO: something needs to be done with whitespace here ...
	   (prologue (frob prologue)))
      `(declare ,fname ,args
		,!m(inject-kwd-if-nonnil linkage)
		,!m(inject-kwd-if-nonnil visibility)
		,!m(inject-kwd-if-nonnil dll-storage-class)
		,!m(inject-kwd-if-nonnil cconv)
		,!m(inject-if-nonnil unnamed-addr)
		,(return-type-lisp-form return-type return-attrs)
		,!m(inject-kwd-if-nonnil align)
		,!m(inject-kwd-if-nonnil gc)
		,!m(inject-kwd-if-nonnil prefix)
		,!m(inject-kwd-if-nonnil prologue)))))
		
;; Let's move to alias definitions
  
;; @<Name> = [Linkage] [Visibility] [DLLStorageClass] [ThreadLocal] [unnamed_addr] alias <AliaseeTy> @<Aliasee>

;; (alias new-name type old-name
;;   (:linkage linkage)
;;   (:visibility visibility)
;;   ...)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-tls-models '(localdynamic initialexec localexec generaldynamic)))
(define-kwd-rule tls-model known-tls-models)

(define-algol-rule (%thread-local thread-local) tls-model)

(define-cg-llvm-rule thread-local ()
  (|| %thread-local
      (progn "thread_local"
	     '(:thread-local t))))

(defun guard-kwd-expr (allowed-kwds expr)
  (if (find expr allowed-kwds :test #'eq)
      expr
      (fail-parse-format "Keyword ~a is not among allowed keywords." expr)))


(define-cg-llvm-rule alias ()
  (macrolet ((frob (x)
	       `(? (progn whitespace ,x))))
    (let ((name llvm-identifier))
      whitespace "="
      (let* ((linkage (frob (guard-kwd-expr '(:private :internal :linkoce :weak :linkonce-odr :weak-odr :external)
					    linkage-type)))
	     (visibility (frob visibility-style))
	     (dll-storage-class (frob dll-storage-class))
	     (thread-local (frob thread-local))
	     (unnamed-addr (frob unnamed-addr)))
	whitespace "alias"
	(let* ((type (progn whitespace llvm-type))
	       (old-name (progn whitespace llvm-identifier)))
	  `(alias ,name ,old-name ,(emit-lisp-repr type)
		  ,!m(inject-kwd-if-nonnil linkage)
		  ,!m(inject-kwd-if-nonnil visibility)
		  ,!m(inject-kwd-if-nonnil dll-storage-class)
		  ,!m(inject-if-nonnil thread-local)
		  ,!m(inject-if-nonnil unnamed-addr)))))))

;;; Let's move to comdats

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-selection-kinds '(any exactmatch largest noduplicates samesize)))
(define-kwd-rule selection-kind)

(define-cg-llvm-rule comdat-toplevel ()
  (let ((name (progn "$" alphanumeric-word)))
    whitespace "=" whitespace "comdat" whitespace
    (let ((kind selection-kind))
      `(comdat ,name ,kind))))

;;; Metadata is to scary to tackle right now ...


;;; Inline assembly
(define-cg-llvm-rule inline-assembly ()
  "module" whitespace "asm" whitespace
  (let ((it llvm-string))
    `(asm ,it)))

(define-cg-llvm-rule target-triple ()
  "target" whitespace "triple" whitespace "=" whitespace
  (let ((it llvm-string))
    `(target-triple ,@(cl-ppcre:split (literal-string "-") it))))

(define-cg-llvm-rule dl-big-endian ()
  "E" :big-endian)
(define-cg-llvm-rule dl-big-endian ()
  "e" :little-endian)

(define-cg-llvm-rule dl-stack-alignment ()
  "S"
  (let ((it pos-integer))
    (assert (equal 0 (mod it 8)))
    `(:stack-alignment ,it)))

(define-cg-llvm-rule dl-pointer-size ()
  "p"
  (let ((n (? pos-integer))
	(size (progn #\: pos-integer))
	(abi (progn #\: pos-integer))
	(pref (progn #\: pos-integer)))
    `(:pointer-size ,@(if n `((:addrspace ,n)))
		    (:size ,size) (:abi ,abi) (:pref ,pref))))
    


(define-cg-llvm-rule target-datalayout ()
  "target" whitespace "datalayout" whitespace "=" whitespace
  (let ((it (cl-ppcre:split (literal-string "-") llvm-string)))
    `(target-datalayout ,@(mapcar (lambda (x)
				    (cg-llvm-parse 'datalayout-spec x))
				  it))))





;;; Parsing of instructions

(define-cg-llvm-rule llvm-instruction ()
  (|| terminator-instruction
      binary-instruction
      bitwise-binary-instruction
      aggregate-instruction
      memory-instruction
      conversion-instruction
      other-instruction))

(define-cg-llvm-rule terminator-instruction ()
  (|| ret-instruction
      br-instruction
      switch-instruction
      indirectbr-instruction
      invoke-instruction
      resume-instruction
      unreachable-instruction))

(define-cg-llvm-rule white-comma ()
  (progm (? whitespace) #\, (? whitespace)))

(defmacro with-rule-names ((name-var) &body body)
  `(destructuring-bind (rule-name instr-name) (if (symbolp ,name-var)
						  (list (intern #?"$((string ,name-var))-INSTRUCTION")
							,name-var)
						  (list (car ,name-var) (cadr ,name-var)))
     ,@body))
  

(defmacro define-instruction-rule (name (&rest args) &body body)
  (with-rule-names (name)
    `(define-cg-llvm-rule ,rule-name ()
       (descend-with-rule 'string ,(stringify-symbol instr-name))
       (,@(if args
	      `(let* (,(if (symbolp (car args))
			   `(,(car args) (wh instr-arg))
			   `(,(caar args) (fail-parse-if-not ,(cadar args) (wh instr-arg))))
		      ,@(mapcar (lambda (x)
				  (if (symbolp x)
				      `(,x (progn white-comma instr-arg))
				      `(,(car x) (fail-parse-if-not ,(cadr x) (progn white-comma instr-arg)))))
				(cdr args))))
	      `(progn))
	 ,@(or body
	       `(`(,,instr-name ,,@(mapcar (lambda (x)
					     ``(quasiquote-2.0:inject ,(if (symbolp x) x (car x))))
					   args))))))))

(define-instruction-rule (valued-return ret) ((x (firstclass-type-p (car it)))))

(define-cg-llvm-rule nonvalued-return ()
  "ret" whitespace "void"
  '(ret :void))

(define-cg-llvm-rule ret-instruction ()
  (|| valued-return
      nonvalued-return))

(define-instruction-rule (conditional-branch br) ((x (llvm-typep 'label (car it)))))

(define-instruction-rule (unconditional-branch br) ((cond (llvm-typep '(integer 1) (car it)))
						    (iftrue (llvm-typep 'label (car it)))
						    (iffalse (llvm-typep 'label (car it)))))

(define-instruction-rule switch ((value (llvm-typep '(integer *) (car it)))
				 (defaultdest (llvm-typep 'label (car it))))
  some-custom-magic-???)


(define-instruction-rule indirectbr ((address (llvm-typep '(pointer ***) (car it))))
  some-magic-similar-to-switch-???)

(define-cg-llvm-rule br-instruction ()
  (|| conditional-branch
      unconditional-branch))


(define-cg-llvm-rule invoke-instruction ()
  "invoke"
  (let* ((cconv (?wh cconv))
	 (return-attrs (?wh (guard-kwd-expr '(:zeroext :signext :inreg)
					    parameter-attrs)))
	 (function-type (fail-parse-if-not (ptr-to-function-type-p it)
					   (wh llvm-type))))
    ;; TODO : this is not correct -- there are more possible things to be INVOKED
    (let* ((function-val (wh llvm-variable))
	   (args (wh? funcall-args))
	   (fun-attrs (?wh (guard-kwd-expr '(:noreturn :nounwind :readonly :readnone)
					   fun-attrs))))
      (wh "to")
      (let ((normal-label (fail-parse-if-not (llvm-typep 'label it)
					     (wh llvm-variable))))
	(wh "unwind")
	(let ((exception-label (fail-parse-if-not (llvm-typep 'label it)
						  (wh llvm-variable))))
	  `(invoke (,(emit-lisp-repr function-type) ,function-val)
		   ,normal-label ,exception-label
		   (:args ,@args)
		   ,m!(inject-kwd-if-nonnil cconv)
		   ,m!(inject-kwd-if-nonnil return-attrs)
		   ,m!(inject-kwd-if-nonnil fun-attrs)))))))
		     
;; The check for correct type of resume instruction is at semantic level -- the whole body
;; of the function should be parsed for that
(define-instruction-rule resume (x))
	
(define-instruction-rule unreachable ())

(define-cg-llvm-rule binary-instruction ()
  (|| add-instruction
      fadd-instruction
      sub-instruction
      fsub-instruction
      mul-instruction
      fmul-instruction
      udiv-instruction
      sdiv-instruction
      fdiv-instruction
      urem-instruction
      srem-instruction
      frem-instruction))

  ;; (let 
  ;;   (setf nuw (?wh "nuw"))
  ;;   (if nuw
  ;; 	(setf nsw (?wh "nsw"))
  ;; 	(progn (setf nsw (?wh "nsw"))
  ;; 	       (setf nuw (?wh "nuw"))))


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

;; Let's first write version which does not use macro, and then abstract-out the macro part...
;; (define-cg-llvm-rule add-instruction ()
;;   "add"
;;   (destructuring-bind (nuw nsw) (unordered-simple-keywords nuw nsw)
;;     (let ((type (emit-lisp-repr (wh llvm-type))))
;;       (if (not (or (llvm-typep '(integer *) type)
;; 		   (llvm-typep '(vector (integer *) *) type)))
;; 	  (fail-parse-format "ADD instruction expects <type> to be INTEGER or VECTOR-OF-INTEGERS, but got: ~a"
;; 			     type))
;;       (macrolet ((parse-arg ()
;; 		   `(|| (if (llvm-typep '(integer ***) type)
;; 			    (descend-with-rule 'integer-constant-value type)
;; 			    (descend-with-rule 'vector-constant-value type))
;; 			(descend-with-rule 'global-ident-constant-value type))))
;; 	(let ((arg1 (wh (parse-arg))))
;; 	  white-comma
;; 	  (let ((arg2 (parse-arg)))
;; 	    `(add ,type ,arg1 ,arg2
;; 		  ,!m(inject-if-nonnil nuw)
;; 		  ,!m(inject-if-nonnil nsw))
;; 	    ))))))


(defmacro with-integer-overflow-prefix (&body body)
  `(let ((prefix-kwds (destructuring-bind (nuw nsw) (unordered-simple-keywords nuw nsw)
			`(,!m(inject-if-nonnil nuw) ,!m(inject-if-nonnil nsw)))))
     ,@body))

(defmacro with-exact-prefix (&body body)
  `(let ((prefix-kwds (remove-if-not #'identity (unordered-simple-keywords exact))))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-fast-math-flags '(nnan ninf nsz arcp fast)))

(defmacro with-fast-math-flags-prefix (&body body)
  `(let ((prefix-kwds (remove-if-not #'identity (unordered-simple-keywords ,@known-fast-math-flags))))
     ,@body))

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
	    white-comma
	    (let ((arg2 (parse-arg)))
	      `(,,name ,type ,arg1 ,arg2 ,@prefix-kwds)
	      )))))))

(defmacro define-integer-binop-definer (name &optional prefix)
  `(defmacro ,name (name)
     (with-rule-names (name)
       `(define-cg-llvm-rule ,rule-name ()
	  (descend-with-rule 'string ,(stringify-symbol instr-name))
	  (,,(if prefix
		 (intern #?"WITH-$((string prefix))-PREFIX")
		 'progn)
	     ,@(binop-instruction-body instr-name 'integer))))))


(define-integer-binop-definer define-integer-binop-rule integer-overflow)
(define-integer-binop-definer define-exact-integer-binop-rule exact)
(define-integer-binop-definer define-simple-integer-binop-rule)

(defmacro define-float-binop-rule (name &key (type 'float))
  (with-rule-names (name)
    `(define-cg-llvm-rule ,rule-name ()
       (descend-with-rule 'string ,(stringify-symbol instr-name))
       (with-fast-math-flags-prefix
	 ,@(binop-instruction-body instr-name type)))))

(define-integer-binop-rule add)
(define-integer-binop-rule sub)
(define-integer-binop-rule mul)

(define-float-binop-rule fadd)
(define-float-binop-rule fsub)
(define-float-binop-rule fmul)
(define-float-binop-rule frem)

(define-exact-integer-binop-rule udiv)
(define-exact-integer-binop-rule sdiv)

(define-simple-integer-binop-rule urem)
(define-simple-integer-binop-rule srem)

(define-cg-llvm-rule bitwise-binary-instruction ()
  (|| shl-instruction
      lshr-instruction
      ashr-instruction
      and-instruction
      or-instruction
      xor-instruction))

(define-integer-binop-rule shl)
(define-exact-integer-binop-rule lshr)
(define-exact-integer-binop-rule ashr)
(define-simple-integer-binop-rule and)
(define-simple-integer-binop-rule or)
(define-simple-integer-binop-rule xor)

(define-cg-llvm-rule aggregate-instruction ()
  (|| extractelement-instruction
      insertelement-instruction
      shufflevector-instruction
      extractvalue-instruction
      insertvalue-instruction))

(define-instruction-rule extractelement ((val (llvm-typep '(vector ***) (car it)))
					 (idx (llvm-typep '(integer ***) (car it)))))
(define-instruction-rule insertelement ((val (llvm-typep '(vector ***) (car it)))
					elt
					(idx (llvm-typep '(integer ***) (car it))))
  (if (not (llvm-same-typep (car elt) (cadar val)))
      (fail-parse "ELT must be same type as subtype of VAL"))
  `(insertelement ,val ,elt ,idx))

(define-instruction-rule shufflevector ((v1 (llvm-typep '(vector ***) (car it)))
					(v2 (llvm-typep '(vector ***) (car it)))
					(mask (llvm-typep '(vector (integer 32) *) (car it))))
  (if (not (llvm-same-typep (cadar v1) (cadar v2)))
      (fail-parse "V1 and V2 must have same subtype"))
  `(shufflevector ,v1 ,v2 ,mask))

(define-cg-llvm-rule indices ()
  (cons (descend-with-rule 'integer-constant-value nil)
	(times (progn white-comma (descend-with-rule 'integer-constant-value nil)))))

(define-instruction-rule extractvalue ((val (or (llvm-typep '(struct ***) (car it))
						(llvm-typep '(array ***) (car it)))))
  white-comma
  `(extractvalue ,val ,@indices))

(define-instruction-rule insertvalue ((val (or (llvm-typep '(struct ***) (car it))
					       (llvm-typep '(array ***) (car it))))
				      elt)
  white-comma
  (let ((indices indices))
    ;; TODO : check that elt has same type as elt of val
    `(insertvalue ,val ,elt ,@indices)))


(define-cg-llvm-rule memory-instruction ()
  (|| alloca-instruction
      load-instruction
      store-instruction
      fence-instruction
      cmpxchg-instruction
      atomicrmw-instruction
      getelementptr-instruction))

(define-cg-llvm-rule alloca-instruction ()
  "alloca"
  (let ((inalloca (? (wh (progn "inalloca" t))))
	(type (emit-lisp-repr (wh llvm-type)))
	(nelts (? (progn white-comma integer-constant)))
	(align (? (progn white-comma "align" whitespace integer))))
    `(alloca ,type
	     ,!m(inject-kwd-if-nonnil nelts)
	     ,!m(inject-kwd-if-nonnil align)
	     ,!m(inject-kwd-if-nonnil inalloca))))
    

(define-cg-llvm-rule conversion-instruction ()
  (|| trunc-to-instruction
      zext-to-instruction
      sext-to-instruction
      fptrunc-to-instruction
      fpext-to-instruction
      fptoui-to-instruction
      fptosi-to-instruction
      uitofp-to-instruction
      sitofp-to-instruction
      ptrtoint-to-instruction
      inttoptr-to-instruction
      bitcast-to-instruction
      addrspacecast-to-instruction))

(defun both-integers (type1 type2)
  (and (llvm-typep '(integer ***) type1) (llvm-typep '(integer ***) type2)))

(defun first-bitsize-larger (type1 type2)
  (> (cadr type1) (cadr type2)))

(defun first-bitsize-smaller (type1 type2)
  (< (cadr type1) (cadr type2)))

(defun both-vector-integers (type1 type2)
  (and (llvm-typep '(vector (integer ***) *) type1) (llvm-typep '(vector (integer ***) *) type2)))

(defun have-same-size (type1 type2)
  (equal (caddr type1) (caddr type2)))

(defun have-different-addrspaces (type1 type2)
  (not (equal (if (equal 3 (length type1))
		  (caddr type1)
		  0)
	      (if (equal 3 (length type2))
		  (caddr type2)
		  0))))

(defmacro define-cast-instruction (name &body constraints)
  (let ((rule-name (intern #?"$((string name))-TO-INSTRUCTION")))
  `(define-cg-llvm-rule ,rule-name ()
     (descend-with-rule 'string ,(stringify-symbol name))
     whitespace
     (destructuring-bind (type1 value) instr-arg
       whitespace "to" whitespace
       (let ((type2 (emit-lisp-repr llvm-type)))
	 ,@constraints
	 `(,,name ,value ,type1 ,type2))))))
	

(defmacro define-simple-based (type1 type2)
  `(defmacro ,(intern #?"$((string type1))->$((string type2))-BASED") (&optional condition)
     `(or (and (llvm-typep '(,,type1 ***) type1) (llvm-typep '(,,type2 ***) type1)
	       ,@(if condition `((,condition type1 type2))))
	  (and (llvm-typep '(vector (,,type1 ***) *) type1) (llvm-typep '(vector (,,type2 *) *) type2)
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

(define-cast-instruction bitcast
  (rough-check-bitcast-type "First" type1)
  (rough-check-bitcast-type "Second" type2)
  (if (not (and (firstclass-type-p type2) (not (aggregate-type-p type2))))
      (fail-parse-format "Second type must be first-class non-aggregate type, but got: ~a" type2))
  (cond ((llvm-typep '(pointer ***) type1)
	 (pointer-pointer-check type1 type2))
	((llvm-typep '(vector (pointer ***) ***) type1)
	 (vector-check type1 type2)
	 (pointer-pointer-check (cadr type1) (cadr type2)))
	(t (if (not (equal (bit-length type1) (bit-length type2)))
	       (fail-parse "Types of BITCAST should have identical bit sizes")))))
	
	

(define-simple-based pointer pointer)

(define-cast-instruction addrspacecast (pointer->pointer-based have-different-addrspaces))

(define-cg-llvm-rule other-instruction ()
  (|| icmp-instruction
      fcmp-instruction
      phi-instruction
      select-instruction
      call-instruction
      va-arg-instruction
      landingpad-instruction))

(define-cg-llvm-rule phi-arg (type)
  (let (c!-1)
    #\[ whitespace
    (setf c!-1 (|| (descend-with-rule 'llvm-constant-value type)
		   llvm-identifier))
    white-comma c!-2-llvm-identifier whitespace #\]
    `(,c!-1 ,c!-2)))

(define-cg-llvm-rule phi-instruction ()
  "phi"
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (if (not (firstclass-type-p type))
	(fail-parse-format "Type of phi instruction must be first-class, but got ~a" type))
    whitespace
    (let ((first-arg (descend-with-rule 'phi-arg type)))
      (let ((rest-args (times (progn white-comma (descend-with-rule 'phi-arg type)))))
	`(phi ,type ,first-arg ,@rest-args)))))
    

(define-instruction-rule select ((cond (or (llvm-typep '(integer 1) (car it))
					   (llvm-typep '(vector (integer 1) *) (car it))))
				 (val1 (firstclass-type-p (car it)))
				 (val2 (firstclass-type-p (car it))))
  (if (llvm-typep '(integer *) (car cond))
      (if (not (llvm-same-typep (car val1) (car val2)))
	  (fail-parse-format "Different values of SELECT should be same type, but are: ~a ~a"
			     (car val1) (car val2)))
      (let ((nelts (caddar cond)))
	(if (not (and (llvm-typep '(vector ***) (car val1))
		      (llvm-typep '(vector ***) (car val2))
		      (llvm-same-typep (cadar val1) (cadar val2))
		      (equal nelts (caddar val1))
		      (equal nelts (caddar val2))))
	    (fail-parse-format "Different values of SELECT should be same type, but are: ~a ~a"
			       (car val1) (car val2)))))
  `(select ,cond ,val1 ,val2))

(define-instruction-rule va-arg ((va-list (llvm-typep '(pointer ***) (car it))))
  white-comma
  (let ((type (emit-lisp-repr llvm-type)))
    `(va-arg ,va-list ,type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-icmp-ops '(eq ne ugt uge ult ule sgt sge slt sle))
  (defparameter known-fcmp-ops '(false oeq ogt oge olt ole one ord
				 ueq ugt uge ult ule une uno true)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun any-of-kwds (kwd-var)
    `(|| ,@(mapcar (lambda (x)
		     `(progn (descend-with-rule 'string ,(stringify-symbol x))
			     ,(intern (string x) "KEYWORD")))
		   kwd-var))))

(defmacro icmp-kwds ()
  (any-of-kwds known-icmp-ops))

(defmacro fcmp-kwds ()
  (any-of-kwds known-fcmp-ops))

(defmacro define-cmp-rule (name typecheck errinfo)
  (let ((rule-name (intern #?"$((string name))-INSTRUCTION"))
	(macro-name (intern #?"$((string name))-KWDS")))
    `(define-cg-llvm-rule ,rule-name ()
       (descend-with-rule 'string ,(stringify-symbol name))
       (let ((cond (wh (,macro-name)))
	     (type (emit-lisp-repr (wh llvm-type))))
	 (if (not ,typecheck)
	     (fail-parse-format ,@errinfo))
	 (let ((val1 (wh (descend-with-rule 'llvm-constant-value type))))
	   white-comma
	   (let ((val2 (descend-with-rule 'llvm-constant-value type)))
	     `(,,name ,cond ,type ,val1 ,val2)))))))
	
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
  "cleanup" '(:cleanup t))

(define-cg-llvm-rule catch-landingpad-clause ()
  "catch"
  (list :catch (wh llvm-constant)))

(define-cg-llvm-rule filter-landingpad-clause ()
  "filter"
  (list :filter (fail-parse-if-not (llvm-typep '(array ***) (car it))
				   (wh llvm-constant))))

(define-cg-llvm-rule landingpad-clause ()
  (|| catch-landingpad-clause
      filter-landingpad-clause))

(define-cg-llvm-rule landingpad-instruction ()
  "landingpad"
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (wh "personality")
    (let ((pers (wh llvm-constant)))
      ;; check
      (let ((clauses (|| (cons (wh cleanup-kwd) (times (wh landingpad-clause)))
			 (postimes (wh landingpad-clause)))))
	`(landingpad ,type ,pers ,@clauses)))))

