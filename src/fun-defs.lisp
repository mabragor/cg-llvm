
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

(define-cg-llvm-rule wh? ()
  (? whitespace) nil)

(defmacro ?wh (x)
  `(? (progn whitespace ,x)))

(defmacro ?wh? (x)
  `(? (progn (? whitespace) ,x)))


(define-cg-llvm-rule white-comma ()
  (progm (? whitespace) #\, (? whitespace)))

(defmacro fail-parse-if-not (cond expr)
  `(let ((it ,expr))
     (if (not ,cond)
	 (fail-parse-format "Assertion ~a is not satisfied by: ~a" ',cond it)
	 it)))

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


(defmacro!! define-plural-rule (name single delim) ()
  `(define-cg-llvm-rule ,name ()
     (cons ,single
	   (times (progn ,delim ,single)))))


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


(define-algol-consy-kwd-rule %parameter-attr
    known-parameter-attrs 
  known-cons-parameter-attrs
  known-algol-parameter-attrs)

(define-cg-llvm-rule parameter-attr ()
  (|| `(:group ,(progn #\# pos-integer))
      %parameter-attr))

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

(define-algol-consy-kwd-rule %fun-attr known-fun-attrs known-cons-fun-attrs known-algol-fun-attrs)

(define-cg-llvm-rule fun-attr ()
  (|| `(:group ,(progn #\# pos-integer))
      %fun-attr))

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
(define-python-rule personality llvm-constant)


(defun return-type-lisp-form (type attrs)
  `(,(emit-lisp-repr type) ,!m(inject-kwd-if-nonnil attrs)))

(defmacro with-rule-names ((name-var) &body body)
  `(destructuring-bind (rule-name instr-name) (if (symbolp ,name-var)
						  (list (intern #?"$((string ,name-var))-INSTRUCTION")
							,name-var)
						  (list (car ,name-var) (cadr ,name-var)))
     (declare (ignorable rule-name instr-name))
     ,@body))
  
(defmacro!! define-op-rule (name &body body) ()
  (with-rule-names (name)
    (let ((body-rule-name (if (symbolp name)
			      (intern #?"$((string name))-INSTRUCTION-BODY")
			      (intern #?"$((string (car name)))-BODY"))))
      `(progn (define-cg-llvm-rule ,body-rule-name ()
		,@body)
	      (define-cg-llvm-rule ,rule-name ()
		(descend-with-rule 'string ,(stringify-symbol instr-name))
		(cons ',instr-name ,body-rule-name))))))

(define-plural-rule instruction-metadata fundef-metadata-entry white-comma)

(defmacro!! define-instruction-rule (name &body body) ()
  `(define-op-rule ,name
     (let ((body (progn ,@body))
	   (metadata (? (progn white-comma instruction-metadata))))
       (if metadata
	   (append body (list (list :metadata metadata)))
	   body))))
	       

(define-op-rule (function-declaration declare)
  (let* ((linkage (?wh linkage-type))
	 (visibility (?wh visibility-style))
	 (dll-storage-class (?wh dll-storage-class))
	 (cconv (?wh cconv))
	 (unnamed-addr (?wh unnamed-addr))
	 (return-attrs (?wh parameter-attrs))
	 (return-type (progn whitespace llvm-type))
	 (fname (progn whitespace llvm-identifier))
	 (args (wh? declfun-args))
	 (fun-attrs (?wh fun-attrs))
	 (align (?wh align))
	 (gc (?wh gc-name))
	 (prefix (?wh prefix))
	 (prologue (?wh prologue)))
    `(,fname ,args
	     ,(return-type-lisp-form return-type return-attrs)
	     ,!m(inject-kwds-if-nonnil linkage visibility dll-storage-class
				       cconv)
	     ,!m(inject-if-nonnil unnamed-addr)
	     ,!m(inject-kwds-if-nonnil fun-attrs align gc prefix prologue))))
		
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

