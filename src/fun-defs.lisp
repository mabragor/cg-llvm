
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

(define-cg-llvm-rule boolean-constant ()
  (let ((type llvm-type))
    (if (not (llvm-typep '(integer 1) type))
	(fail-parse "Boolean must really be integer of 1 bit."))
    whitespace
    (list type (text (|| "true" "false")))))

(define-cg-llvm-rule integer-constant ()
  (let ((type (emit-lisp-repr llvm-type)))
    (if (not (llvm-typep '(integer *) type))
	(fail-parse-format "Integer constant must be of integer type but got ~a." type))
    whitespace
    (list type integer)))

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

(define-cg-llvm-rule float-constant ()
  (let ((type (emit-lisp-repr llvm-type)))
    (if (not (llvm-typep '(float *) type))
	(fail-parse-format  "Float constant must be of float type but got ~a." type))
    whitespace
    (list type llvm-float)))

(define-cg-llvm-rule null-ptr-constant ()
  (let ((type (emit-lisp-repr llvm-type)))
    (if (not (or (llvm-typep '(pointer *) type)
		 (llvm-typep '(pointer * *) type)))
	(fail-parse-format "Null ptr constant must be of pointer type but got ~a." type))
    (list type (text "null"))))

(define-cg-llvm-rule global-ident-constant ()
  (let ((type (emit-lisp-repr llvm-type)))
    (if (not (or (llvm-typep '(pointer *) type)
		 (llvm-typep '(pointer * *) type)))
	(fail-parse-format "Global identifier must be of pointer type, but got ~a." type))
    (list type llvm-identifier)))

(define-cg-llvm-rule simple-constant ()
  (|| boolean-constant
      integer-constant
      float-constant
      null-ptr-constant
      global-ident-constant))

(define-cg-llvm-rule llvm-constant ()
  (|| simple-constant complex-constant))

(define-plural-rule llvm-constants llvm-constant (progn (? whitespace) #\, (? whitespace)))

(defmacro define-complex-constant-rule (name lb rb type-test content-test errstr1 errstr2)
  `(define-cg-llvm-rule ,name ()
     (let ((type (emit-lisp-repr llvm-type)))
       (if ,type-test
	   (fail-parse-format ,(join "" errstr1 " constant must be of " errstr2 " type, but got ~a") type))
       (? whitespace)
       (let ((content (progm (progn (descend-with-rule 'string ,lb) (? whitespace))
			     llvm-constants
			     (progn (? whitespace) (descend-with-rule 'string ,rb)))))
	 ,content-test
	 (list type content)))))

(define-complex-constant-rule structure-constant "{" "}"
  (not (llvm-typep '(struct ***) type))
  (if (not (equal (length (cadr type)) (length content)))
      (fail-parse "Number of elements of type and content do not match.")
      (iter (for theor-subtype in (cadr type))
	    (for (expr-subtype nil) in content)
	    (if (not (llvm-same-typep theor-subtype expr-subtype))
		(fail-parse "Type of structure field does not match declared one."))))
  "Structure" "structure" )

(define-complex-constant-rule array-constant "[" "]"
  (not (llvm-typep '(array ***) type))
  (if (not (equal (caddr type) (length content)))
      (fail-parse "Number of elements of type and content do not match.")
      (iter (for (expr-subtype nil) in content)
	    (if (not (llvm-same-typep (cadr type) expr-subtype))
		(fail-parse "Type of array element does not match declared one."))))
  "Array" "array")

(define-complex-constant-rule vector-constant "<" ">"
  (not (llvm-typep '(vector ***) type))
  (if (not (equal (caddr type) (length content)))
      (fail-parse "Number of elements of type and content do not match.")
      (iter (for (expr-subtype nil) in content)
	    (if (not (llvm-same-typep (cadr type) expr-subtype))
		(fail-parse "Type of vector element does not match declared one."))))
  "Vector" "vector")


(define-cg-llvm-rule string-constant ()
  (let ((type (emit-lisp-repr llvm-type)))
    (if (not (llvm-typep '(array (integer 8) *) type))
	(fail-parse-format "String constant must be of array-of-chars type, but got ~a" type))
    whitespace #\c
    (list type (mapcar (lambda (x)
			 `((integer 8) ,(char-code x)))
		       (coerce llvm-string 'list)))))

(define-cg-llvm-rule zero-init ()
  (let ((type (emit-lisp-repr llvm-type)))
    whitespace
    "zeroinitializer"
    `(,type :zero-initializer)))
	 
(define-cg-llvm-rule metadata-node ()
  (fail-parse "Metadata nodes are not implemented yet"))

(define-plural-rule metadata-nodes metadata-node (progn (? whitespace) #\, (? whitespace)))

(define-cg-llvm-rule complex-constant ()
  (|| structure-constant
      array-constant
      string-constant ; just a special syntax for array of chars
      vector-constant
      zero-init
      metadata-node))


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


