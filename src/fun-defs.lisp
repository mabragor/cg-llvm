
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
	    

(define-constant-rules boolean
    (llvm-typep '(integer 1) type) ((literal-string "Boolean must really be integer of 1 bit."))
    (text (|| "true" "false")))

(define-constant-rules integer
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

(define-cg-llvm-rule llvm-float ()
  (|| decimal-float
      hexadecimal-float))

(define-constant-rules float
    (llvm-typep '(float *) type) ((literal-string "Float constant must be of float type but got ~a.") type)
  llvm-float)

(define-constant-rules null-ptr
    (llvm-typep '(pointer ***) type)
    ((literal-string "Null ptr constant must be of pointer type but got ~a.") type)
  (text "null"))

(define-constant-rules global-ident
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

(define-cg-llvm-rule ordinary-constant ()
  (|| simple-constant
      complex-constant))

(define-cg-llvm-rule llvm-constant ()
  (|| ordinary-constant
      metadata-node))

(define-cg-llvm-rule llvm-variable ()
  `(,(emit-lisp-repr llvm-type) ,(progn whitespace llvm-identifier)))

(define-cg-llvm-rule instr-arg ()
  (|| llvm-variable
      llvm-constant))

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

(defmacro define-instruction-rule (name (&rest args) &body body)
  (destructuring-bind (rule-name instr-name) (if (symbolp name)
						 (list (intern #?"$((string name))-INSTRUCTION")
						       name)
						 (list (car name) (cadr name)))
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

(defmacro define-nsw-nuw-binop-rule (name &key (type 'integer))
  (destructuring-bind (rule-name instr-name) (if (symbolp name)
						 (list (intern #?"$((string name))-INSTRUCTION")
						       name)
						 name)
    `(define-cg-llvm-rule ,rule-name ()
       (descend-with-rule 'string ,(stringify-symbol instr-name))
       (destructuring-bind (nuw nsw) (unordered-simple-keywords nuw nsw)
	 (let ((type (emit-lisp-repr (wh llvm-type))))
	   (if (not (or (llvm-typep '(,type *) type)
			(llvm-typep '(vector (,type *) *) type)))
	       (fail-parse-format ,#?"ADD instruction expects <type> to be $((string type)) \
                                      or VECTOR OF $((string type))S, but got: ~a"
				  type))
	   (macrolet ((parse-arg ()
			`(|| (if (llvm-typep '(,,type ***) type)
				 (descend-with-rule ',,(intern #?"$((string type))-CONSTANT-VALUE") type)
				 (descend-with-rule 'vector-constant-value type))
			     (descend-with-rule 'global-ident-constant-value type))))
	     (let ((arg1 (wh (parse-arg))))
	       white-comma
	       (let ((arg2 (parse-arg)))
		 `(,,instr-name ,type ,arg1 ,arg2
			       ,!m(inject-if-nonnil nuw)
			       ,!m(inject-if-nonnil nsw))
		 ))))))))


(define-nsw-nuw-binop-rule add)

  

(define-cg-llvm-rule bitwise-binary-instruction ()
  (|| shl-instruction
      lshr-instruction
      ashr-instruction
      and-instruction
      or-instruction
      xor-instruction))
      
(define-cg-llvm-rule aggregate-instruction ()
  (|| extractelement-instruction
      insertelement-instruction
      shufflevector-instruction
      extractvalue-instruction
      insertvalue-instruction))

(define-cg-llvm-rule memory-instruction ()
  (|| alloca-instruction
      load-instruction
      store-instruction
      fence-instruction
      cmpxchg-instruction
      atomicrmw-instruction
      getelementptr-instruction))

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

(define-cg-llvm-rule other-instruction ()
  (|| icmp-instruction
      fcmp-instruction
      phi-instruction
      select-instruction
      call-instruction
      va-arg-instruction
      landingpad-instruction))
      
					 
      
      
      
