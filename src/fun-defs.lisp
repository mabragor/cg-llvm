
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
	 (fail-parse-format "Assertion ~a is not satisfied by: ~a" ',cond it)
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
  (defparameter known-cconvs '(ccc fastcc coldcc webkit-jscc anyregcc preserve-mostcc preserve-allcc))
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
  c!-1)

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

(define-cg-llvm-rule usual-identifier-body ()
  (text (list (|| alpha-char #\- #\$ #\. #\_)
	      (times (|| alphanumeric-char #\- #\$ #\. #\_)))))

(defun try-destringify-symbol (str)
  (handler-case (destringify-symbol str)
    (error () str)))
  
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
  
(define-cg-llvm-rule identifier-body ()
  (|| usual-identifier-body
      llvm-string))

(define-cg-llvm-rule local-identifier ()
  (try-destringify-symbol (text (list #\% identifier-body))))
(define-cg-llvm-rule global-identifier ()
  (try-destringify-symbol (text (list #\@ identifier-body))))

(define-cg-llvm-rule llvm-identifier ()
  (|| local-identifier
      global-identifier))

(defmacro inject-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list (list ,(intern (string name) "KEYWORD")
		      ,name))))

(defmacro inject-kwds-if-nonnil (&rest names)
  ``,@`(,,@(mapcar (lambda (x)
		     ``,!m(inject-kwd-if-nonnil ,x))
		   names)))

(defmacro splice-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list ,(intern (string name) "KEYWORD")
		,name)))

(defmacro inject-if-nonnil (smth)
  ``,@(if ,smth
	  (list ,smth)))

(defmacro inject-stuff-if-nonnil (&rest stuff)
  ``,@`(,,@(mapcar (lambda (x)
		     ``,!m(inject-if-nonnil ,x))
		   stuff)))


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
	   (args (wh defun-args))
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

(defun whitelist-kwd-expr (allowed-kwds expr)
  (if (find expr allowed-kwds :test #'eq)
      expr
      (fail-parse-format "Keyword ~a is not among allowed keywords." expr)))

(defun blacklist-kwd-expr (disallowed-kwds expr)
  (if (find expr disallowed-kwds :test #'eq)
      (fail-parse-format "Keyword ~a is among disallowed keywords." expr)
      expr))


(define-cg-llvm-rule alias ()
  (macrolet ((frob (x)
	       `(? (progn whitespace ,x))))
    (let ((name llvm-identifier))
      whitespace "="
      (let* ((linkage (frob (whitelist-kwd-expr '(:private :internal :linkoce :weak :linkonce-odr :weak-odr :external)
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
    `(target-triple ,@(mapcar (lambda (x y)
				(list x y))
			      '(:arch :vendor :system :env)
			      (cl-ppcre:split (literal-string "-") it)))))

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
    
(define-cg-llvm-rule stack-layout ()
  (let ((it pos-integer))
    (if (not (equal 0 (mod it 8)))
	(fail-parse-format "Stack alignment should be multiple of 8, but got ~a" it))
    `(:stack ,it)))

(define-cg-llvm-rule pointer-layout ()
  (let ((n (? pos-integer)))
    (if (and n (not (and (< 0 n)
			 (> (expt 2 23) n))))
	(fail-parse-format "Pointer address space not in range: ~a" n))
    (let ((size (progm #\: pos-integer #\:))
	  (abi-pref abi-layout))
      `(:pointer ,size ,abi-pref ,@(if n `((:addrspace ,n)))))))

(define-cg-llvm-rule integer-layout ()
  (let ((size (prog1 pos-integer #\:))
	(abi-pref abi-layout))
    `(:integer ,size ,abi-pref)))
(define-cg-llvm-rule vector-layout ()
  (let ((size (prog1 pos-integer #\:))
	(abi-pref abi-layout))
    `(:vector ,size ,abi-pref)))
(define-cg-llvm-rule float-layout ()
  (let ((size (prog1 pos-integer #\:))
	(abi-pref abi-layout))
    `(:float ,size ,abi-pref)))
(define-cg-llvm-rule aggregate-layout ()
  `(:aggregate ,abi-layout))

(define-cg-llvm-rule abi-layout ()
  (let ((abi pos-integer)
	(pref (? (progn #\: pos-integer))))
    `(:abi ,abi ,@(if pref `(,pref)))))

(define-cg-llvm-rule mangling-layout ()
  #\:
  (list :mangling
	(|| (progn #\e :elf)
	    (progn #\m :mips)
	    (progn #\o :mach-o)
	    (progn #\w :windows-coff))))

(define-plural-rule integer-sizes pos-integer #\:)

(define-cg-llvm-rule native-integers-layout ()
  `(:native-integers ,@integer-sizes))

(define-cg-llvm-rule datalayout-spec ()
  (|| (progn #\E '(:endianness :big))
      (progn #\e '(:endianness :little))
      (progn #\S stack-layout)
      (progn #\p pointer-layout)
      (progn #\i integer-layout)
      (progn #\v vector-layout)
      (progn #\f float-layout)
      (progn #\a aggregate-layout)
      (progn #\m mangling-layout)
      (progn #\n native-integers-layout)))

(define-cg-llvm-rule target-datalayout ()
  "target" whitespace "datalayout" whitespace "=" whitespace
  (let ((it (cl-ppcre:split (literal-string "-") llvm-string)))
    ;; `(target-datalayout ,it)))
    `(target-datalayout ,@(mapcar (lambda (x)
				    (cg-llvm-parse 'datalayout-spec x))
				  it))))



;;; Parsing of instructions

(define-cg-llvm-rule white-comma ()
  (progm (? whitespace) #\, (? whitespace)))

(defmacro with-rule-names ((name-var) &body body)
  `(destructuring-bind (rule-name instr-name) (if (symbolp ,name-var)
						  (list (intern #?"$((string ,name-var))-INSTRUCTION")
							,name-var)
						  (list (car ,name-var) (cadr ,name-var)))
     ,@body))
  
(defmacro!! define-instruction-rule (name &body body) ()
  (with-rule-names (name)
    (let ((body-rule-name (if (symbolp name)
			      (intern #?"$((string name))-INSTRUCTION-BODY")
			      (intern #?"$((string (car name)))-BODY"))))
      `(progn (define-cg-llvm-rule ,body-rule-name ()
		,@body)
	      (define-cg-llvm-rule ,rule-name ()
		(descend-with-rule 'string ,(stringify-symbol instr-name))
		(cons ',instr-name ,body-rule-name))))))

(defmacro define-simple-instruction-rule (name (&rest args) &body body)
  `(define-instruction-rule ,name
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
  (let ((rule-name (append-instruction-to-sym name))
	(alt-names (mapcar #'append-instruction-to-sym alternatives)))
    `(define-cg-llvm-rule ,rule-name () (|| ,@alt-names))))

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
  (wh "void") (list :void))

(define-cg-llvm-rule ret-instruction ()
  (|| valued-return
      nonvalued-return))

(define-simple-instruction-rule (conditional-branch br) ((x (llvm-typep 'label (car it)))))

(define-simple-instruction-rule (unconditional-branch br) ((cond (llvm-typep '(integer 1) (car it)))
						    (iftrue (llvm-typep 'label (car it)))
						    (iffalse (llvm-typep 'label (car it)))))

(define-simple-instruction-rule switch ((value (llvm-typep '(integer *) (car it)))
				 (defaultdest (llvm-typep 'label (car it))))
  some-custom-magic-???)


(define-simple-instruction-rule indirectbr ((address (llvm-typep '(pointer ***) (car it))))
  some-magic-similar-to-switch-???)

(define-cg-llvm-rule br-instruction ()
  (|| conditional-branch
      unconditional-branch))


(define-instruction-rule invoke
  (let* ((cconv (?wh cconv))
	 (return-attrs (?wh (whitelist-kwd-expr '(:zeroext :signext :inreg)
					    parameter-attrs)))
	 (function-type (fail-parse-if-not (ptr-to-function-type-p it)
					   (wh llvm-type))))
    ;; TODO : this is not correct -- there are more possible things to be INVOKED
    (let* ((function-val (wh llvm-variable))
	   (args (wh? funcall-args))
	   (fun-attrs (?wh (whitelist-kwd-expr '(:noreturn :nounwind :readonly :readnone)
					   fun-attrs))))
      (wh "to")
      (let ((normal-label (fail-parse-if-not (llvm-typep 'label it)
					     (wh llvm-variable))))
	(wh "unwind")
	(let ((exception-label (fail-parse-if-not (llvm-typep 'label it)
						  (wh llvm-variable))))
	  `((,(emit-lisp-repr function-type) ,function-val)
	    ,normal-label ,exception-label
	    (:args ,@args)
	    ,m!(inject-kwd-if-nonnil cconv)
	    ,m!(inject-kwd-if-nonnil return-attrs)
	    ,m!(inject-kwd-if-nonnil fun-attrs)))))))
		     
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
	      `(,type ,arg1 ,arg2 ,@prefix-kwds)
	      )))))))

(defmacro define-integer-binop-definer (name &optional prefix)
  `(defmacro ,name (name)
     (with-rule-names (name)
       `(define-instruction-rule ,name
	  (,,(if prefix
		 (intern #?"WITH-$((string prefix))-PREFIX")
		 'progn)
	     ,@(binop-instruction-body instr-name 'integer))))))


(define-integer-binop-definer define-integer-binop-rule integer-overflow)
(define-integer-binop-definer define-exact-integer-binop-rule exact)
(define-integer-binop-definer define-simple-integer-binop-rule)

(defmacro define-float-binop-rule (name &key (type 'float))
  (with-rule-names (name)
    `(define-instruction-rule ,name
       (with-fast-math-flags-prefix
	 ,@(binop-instruction-body instr-name type)))))

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
	(times (progn white-comma (descend-with-rule 'integer-constant-value nil)))))

(define-simple-instruction-rule extractvalue ((val (or (llvm-typep '(struct ***) (car it))
						(llvm-typep '(array ***) (car it)))))
  white-comma
  `(,val ,@indices))

(define-simple-instruction-rule insertvalue ((val (or (llvm-typep '(struct ***) (car it))
					       (llvm-typep '(array ***) (car it))))
				      elt)
  white-comma
  (let ((indices indices))
    ;; TODO : check that elt has same type as elt of val
    `(,val ,elt ,@indices)))


(define-instruction-alternative lvalue-memory
  alloca load cmpxchg atomicrmw getelementptr)

(define-instruction-alternative nolvalue-memory
  store fence)

(define-lvalue-instruction-alternative memory)

(define-instruction-rule alloca
  (let ((inalloca (? (wh (progn "inalloca" t))))
	(type (emit-lisp-repr (wh llvm-type)))
	(nelts (? (progn white-comma integer-constant)))
	(align (? (progn white-comma "align" whitespace integer))))
    `(,type
      ,!m(inject-kwd-if-nonnil nelts)
      ,!m(inject-kwd-if-nonnil align)
      ,!m(inject-kwd-if-nonnil inalloca))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter known-orderings '(unordered monotonic acquire release acq-rel seq-cst)))

(define-kwd-rule ordering)

(defmacro!! define-load-store-instruction (name pre-body type-getter &body body) ()
  `(define-instruction-rule ,name
     ,@pre-body
     (let ((volatile (? (wh (progn "volatile" t))))
	   (type ,type-getter)
	   (ptr (progn white-comma (fail-parse-if-not (llvm-typep '(pointer ***) (car it))
						      llvm-constant))))
       ,@body)))

(defmacro!! define-atomic-load-store-instruction (name type-getter) ()
  (let ((rule-name (intern #?"ATOMIC-$((string name))-INSTRUCTION")))
    `(define-load-store-instruction (,rule-name ,name) ((wh "atomic"))
       ,type-getter
       (let ((singlethread (? (wh (progn "singlethread" t))))
	     (ordering (wh (blacklist-kwd-expr '(:release :acq-rel) ordering)))
	     (align (progn white-comma "align" whitespace integer)))
	 `((:atomic t) ,type ,ptr
	   ,!m(inject-kwds-if-nonnil ordering
				     align
				     volatile
				     singlethread))))))


(defmacro!! define-non-atomic-load-store-instruction (name type-getter) ()
  (let ((rule-name (intern #?"NON-ATOMIC-$((string name))-INSTRUCTION")))
    `(define-load-store-instruction (,rule-name ,name) ()
	 ,type-getter
       (let ((align (? (progn white-comma "align" whitespace integer))))
	 `(,type ,ptr
		 ,!m(inject-kwds-if-nonnil align volatile))))))


(define-atomic-load-store-instruction load (emit-lisp-repr (wh llvm-type)))
(define-non-atomic-load-store-instruction load (emit-lisp-repr (wh llvm-type)))
(define-atomic-load-store-instruction store (wh llvm-constant))
(define-non-atomic-load-store-instruction store (wh llvm-constant))
	
(define-instruction-alternative load
  atomic-load non-atomic-load)

(define-instruction-alternative store
  atomic-store non-atomic-store)

(define-instruction-rule fence
  (let ((singlethread (? (wh (progn "singlethread" t))))
	(ordering (wh (whitelist-kwd-expr '(:acquire :release :acq-rel :seq-cst) ordering))))
    `(,!m(inject-kwds-if-nonnil singlethread ordering))))

(define-instruction-rule cmpxchg
  (let* ((weak (? (wh (progn "weak" t))))
	 (volatile (? (wh (progn "volatile" t))))
	 (ptr (wh (fail-parse-if-not (and (llvm-typep '(pointer (integer ***) ***) (car it))
					  (>= (bit-length (cadar it)) 8))
				     instr-arg)))
    	 (cmp (progn white-comma (fail-parse-if-not (llvm-same-typep (car it) (cadar ptr))
    						    instr-arg)))
	 (new (progn white-comma (fail-parse-if-not (llvm-same-typep (car it) (car cmp))
						    instr-arg)))
	 (singlethread (? (wh (progn "singlethread" t))))
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
  (let* ((volatile (?wh (progn "volatile" t)))
	 (op (wh (atomicrmw-kwds)))
	 (ptr (wh (fail-parse-if-not (and (llvm-typep '(pointer (integer ***) ***) (car it))
					  (<= 8 (bit-length (cadar it))))
				     instr-arg)))
	 (val (progn white-comma (fail-parse-if-not (llvm-same-typep (cadar ptr) (car it))
						    instr-arg)))
	 (singlethread (?wh (progn "singlethread" t)))
	 (ordering (wh ordering)))
    `(,op ,ptr ,val ,!m(inject-kwds-if-nonnil ordering volatile singlethread))))

(define-cg-llvm-rule vector-getelementptr-body ()
  (let* ((ptrval (fail-parse-if-not (llvm-typep '(vector ***) (car it))
				    instr-arg))
	 (idx (progn white-comma (fail-parse-if-not (llvm-typep '(vector ***) (car it))
						    instr-arg))))
    (if (not (equal (caddar ptrval) (caddar idx)))
	(fail-parse "Sizes of vectors should be equal"))
    `(,ptrval ,idx)))

(define-cg-llvm-rule scalar-getelementptr-body ()
  (let* ((ptrval (fail-parse-if-not (llvm-typep '(pointer ***) (car it))
				    instr-arg))
	 (indices caboom!))
    `(,ptrval ,@indices)))

(define-instruction-rule getelementptr
  (let* ((inbounds (?wh (progn "inbounds" t)))
	 (type (wh (prog1 llvm-type white-comma)))
	 (body (|| vector-getelementptr-body
		   scalar-getelementptr-body)))
    `(,type ,@body ,!m(inject-kwd-if-nonnil inbounds))))

(define-instruction-alternative lvalue-conversion
  trunc-to zext-to sext-to fptrunc-to fpext-to fptoui-to fptosi-to
  uitofp-to sitofp-to ptrtoint-to inttoptr-to bitcast-to addrspacecast-to)

(define-cg-llvm-rule nolvalue-conversion-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative conversion)

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
    `(define-instruction-rule (,rule-name ,name)
       (destructuring-bind (type1 value) (wh instr-arg)
	 whitespace "to" whitespace
	 (let ((type2 (emit-lisp-repr llvm-type)))
	   ,@constraints
	   `(,value ,type1 ,type2))))))


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

(define-instruction-alternative lvalue-other
  icmp fcmp phi select call va-arg landingpad)

(define-cg-llvm-rule nolvalue-other-instruction ()
  (fail-parse "There are no such instructions"))

(define-lvalue-instruction-alternative other)

(define-cg-llvm-rule phi-arg (type)
  (let (c!-1)
    #\[ whitespace
    (setf c!-1 (|| (descend-with-rule 'llvm-constant-value type)
		   llvm-identifier))
    white-comma c!-2-llvm-identifier whitespace #\]
    `(,c!-1 ,c!-2)))

(define-instruction-rule phi
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (if (not (firstclass-type-p type))
	(fail-parse-format "Type of phi instruction must be first-class, but got ~a" type))
    whitespace
    (let ((first-arg (descend-with-rule 'phi-arg type)))
      (let ((rest-args (times (progn white-comma (descend-with-rule 'phi-arg type)))))
	`(,type ,first-arg ,@rest-args)))))
    

(define-simple-instruction-rule select ((cond (or (llvm-typep '(integer 1) (car it))
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
  `(,cond ,val1 ,val2))

(define-simple-instruction-rule va-arg ((va-list (llvm-typep '(pointer ***) (car it))))
  white-comma
  (let ((type (emit-lisp-repr llvm-type)))
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
	 (let ((val1 (wh (descend-with-rule 'llvm-constant-value type))))
	   white-comma
	   (let ((val2 (descend-with-rule 'llvm-constant-value type)))
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

(define-instruction-rule landingpad
  (let ((type (emit-lisp-repr (wh llvm-type))))
    (wh "personality")
    (let ((pers (wh llvm-constant)))
      ;; check
      (let ((clauses (|| (cons (wh cleanup-kwd) (times (wh landingpad-clause)))
			 (postimes (wh landingpad-clause)))))
	`(,type ,pers ,@clauses)))))

(define-cg-llvm-rule call-instruction ()
  (let ((tail (? (|| (progn (prog1 "tail" whitespace) :tail)
		     (progn (prog1 "musttail" whitespace) :must-tail)))))
    "call"
    (let ((cconv (?wh cconv))
	  (return-attrs (?wh (mapcar (lambda (x)
				       (whitelist-kwd-expr '(:zeroext :signext :inreg) x))
				     parameter-attrs)))
	  (type (emit-lisp-repr (wh llvm-type)))
	  (ftype (?wh (fail-parse-if-not (llvm-typep '(pointer (function ***) ***) it)
					 (emit-lisp-repr llvm-type))))
	  (fnptrval (wh llvm-identifier))
	  (args (wh? (progm #\( (? funcall-args) #\))))
	  (fun-attrs (?wh (mapcar (lambda (x)
				    (whitelist-kwd-expr '(:noreturn :nounwind :readonly :readnone) x))
				  fun-attrs))))
      `(call ,type ,fnptrval ,args
	     ,!m(inject-kwds-if-nonnil cconv return-attrs ftype fun-attrs tail)))))

(define-cg-llvm-rule funcall-arg ()
  (let ((instr-arg instr-arg)
	(attrs (?wh parameter-attrs)))
    `(,@instr-arg ,@(if attrs `((:attrs ,@attrs))))))

(define-plural-rule funcall-args funcall-arg white-comma)

(define-cg-llvm-rule defun-arg ()
  (fail-parse "Not implemented yet"))

(define-plural-rule defun-args defun-arg white-comma)

;;; Let's write something that is able to parse whole basic block of instructions

(define-instruction-alternative lvalue-nonterminator
  lvalue-other lvalue-conversion lvalue-memory lvalue-aggregate
  lvalue-bitwise-binary lvalue-binary)

(define-instruction-alternative nolvalue-nonterminator
  nolvalue-other nolvalue-conversion nolvalue-memory
  nolvalue-aggregate nolvalue-bitwise-binary nolvalue-binary)
      

(define-cg-llvm-rule block-label ()
  (destringify-symbol (text (list (literal-char #\%) (prog1 identifier-body #\:)))))

(define-cg-llvm-rule nonfinal-statement ()
  (|| nolvalue-nonterminator-instruction
      `(= ,local-identifier ,(progn whitespace #\= whitespace lvalue-nonterminator-instruction))))

(define-plural-rule nonfinal-statements nonfinal-statement whitespace)

(define-cg-llvm-rule final-statement ()
  (|| nolvalue-terminator-instruction
      `(= ,local-identifier (progn whitespace #\= whitespace ,lvalue-nonterminator-instruction))))

(define-cg-llvm-rule basic-block-body ()
  (let ((nonfinal-statements (? nonfinal-statements)))
    (if nonfinal-statements whitespace)
    ;; this way we first know that everything parsed well before we construct the list
    (let ((final-statement final-statement))
      `(,@nonfinal-statements ,final-statement))))

(define-cg-llvm-rule basic-block ()
  (let ((label (? (prog1 block-label whitespace))))
    (let ((basic-block-body basic-block-body))
      `(block ,!m(inject-kwd-if-nonnil label)
	       ,@basic-block-body))))

(define-plural-rule basic-blocks basic-block whitespace)

(define-cg-llvm-rule function-body ()
  (progm (progn #\{ (? whitespace))
	 (? basic-blocks)
	 (progn (? whitespace) #\})))

(define-instruction-rule (function-definition define)
  (let* ((linkage (?wh linkage-type))
	 (visibility (?wh visibility-style))
	 (dll-storage-class (?wh dll-storage-class))
	 (cconv (?wh cconv))
	 (unnamed-addr (?wh (progn "unnamed_addr" t)))
	 (type (wh (emit-lisp-repr llvm-type)))
	 (return-attrs (?wh parameter-attrs))
	 (fname (wh llvm-identifier))
	 (args (wh? (progm #\( (? defun-args) #\))))
	 (fun-attrs (?wh fun-attrs))
	 (section (?wh section))
	 (align (?wh align))
	 (comdat (?wh comdat))
	 (gc (?wh gc-name))
	 (prefix (?wh prefix))
	 (prologue (?wh prologue))
	 (body (wh? function-body)))
    `(,type ,fname ,args
	    ,!m(inject-kwds-if-nonnil linkage visibility dll-storage-class
				      cconv unnamed-addr return-attrs
				      fun-attrs section align comdat gc
				      prefix prologue body))))

(define-cg-llvm-rule global-variable-definition ()
  (let ((name global-identifier))
    whitespace #\=
    (let* ((linkage (?wh linkage-type))
      	   (visibility (?wh visibility-style))
	   (dll-storage-class (?wh dll-storage-class))
	   (thread-local (?wh thread-local))
	   (unnamed-addr (?wh (progn "unnamed_addr" t)))
	   (addrspace (?wh (let ((it addr-space))
			     (if (equal 0 it)
				 nil
				 it))))
	   (externally-initialized (? (progn (? whitespace) "externally_initialized" t)))
	   (constant (progn (? whitespace)
			    (|| (progn "global" nil)
				(progn "constant" t))))
	   (type (emit-lisp-repr (wh? llvm-type)))
	   (value (if (eq :external linkage)
	   	      (? (?wh (descend-with-rule 'llvm-constant-value type)))
	   	      (?wh (descend-with-rule 'llvm-constant-value type))))
	   (section (? (progn (? whitespace) #\, (? whitespace) section)))
	   (comdat (? (progn (? whitespace) #\, (? whitespace) comdat)))
	   (align (? (progn (? whitespace) #\, (? whitespace) align))))
      `(:global-var ,name ,type ,value
		    ,!m(inject-kwds-if-nonnil linkage visibility dll-storage-class
					      unnamed-addr addrspace
					      externally-initialized
					      constant
					      section comdat)
		    ,!m(inject-stuff-if-nonnil thread-local align)
		    ))))
	    
	 
