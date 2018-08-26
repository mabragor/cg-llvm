(in-package #:cg-llvm)
;;;;FIXME:: Do these macros depend on the code-walking esrap-liquid to
;;;;expand? wtf?
(defmacro fail-parse-if-not (cond expr)
  `(let ((it ,expr))
     (if ,cond
	 it
	 (fail-parse-format "Assertion ~a is not satisfied by: ~a" ',cond it))))

(defmacro %%inject-kwd-if-nonnil (name)
  `(if ,name
       (list (list ,(keywordify name)
		   ,name))))

(defmacro %%inject-kwds-if-nonnil (&rest names)
  (cons 'append
	(mapcar (lambda (x)
		  `(%%inject-kwd-if-nonnil ,x))
		names)))

(defmacro %%inject-if-nonnil (smth)
  `(if ,smth
       (list ,smth)))

(defmacro %%inject-stuff-if-nonnil (&rest stuff)
  (cons 'append
	(mapcar (lambda (x)
		  `(%%inject-if-nonnil ,x))
		stuff)))


(defmacro define-plural-rule (name single delim)
  `(define-cg-llvm-rule ,name ()
     (cons (v ,single)
	   (times (progn-v ,delim ,single)))))


;;;;return a keyword after matching a particular string
(defmacro define-kwd-rule (name &optional known-words)
  `(define-cg-llvm-rule ,name ()
     (|| ,@(mapcar (lambda (x)
		     `(progn (descend-with-rule 'string ,(%stringify-symbol x))
			     ,(keywordify x)))
		   known-words))))

(define-kwd-rule linkage-type
    (private
     internal
     available-externally
     linkonce
     weak
     common
     appending
     extern-weak
     linkonce-odr
     weak-odr
     external))


(defmacro define-consy-kwd-rule (name known-var known-cons-var)
  (let ((g!-name (gensym (string name))))
    `(progn
       (define-kwd-rule ,g!-name ,known-var)
       (define-cg-llvm-rule ,name ()
	 (|| (descend-with-rule ',g!-name)
	     ,@(mapcar (lambda (x)
			 (let ((string (%stringify-symbol (car x))))
			   `(progn
			      (descend-with-rule 'string
						 ,string)
			      (list ,(keywordify (string-upcase string))
				    (progn (descend-with-rule 'whitespace)
					   (descend-with-rule ',(cadr x)))))))
		       known-cons-var))))))

(define-cg-llvm-rule cc_<n> ()
  (progm
   "<"
   pos-integer
   ">"))

;;;;C calling convention
(define-consy-kwd-rule cconv
    (ccc
     fastcc
     coldcc
     webkit_jscc
     anyregcc
     preserve_mostcc
     preserve_allcc

     cxx_fast_tlscc
     swiftcc)
  ((cc pos-integer)
   (cc cc_<n>)))

(define-kwd-rule visibility-style
    (default
     hidden
     protected))

(define-kwd-rule dll-storage-class
    (dllimport
     dllexport))

;;;;Thread Local Storage Models
(define-kwd-rule tls-model
    (localdynamic
     initialexec
     localexec
     generaldynamic))

(define-kwd-rule |PreemptionSpecifier|
    (dso_preemptable
     dso_local))

(define-kwd-rule ordering
    (unordered
     monotonic
     acquire
     release
     acq_rel
     seq_cst))

(defmacro define-algol-consy-kwd-rule (name known-var known-cons-var known-algol-var)
  (let ((g!-name (gensym (string name))))
    `(progn
       (define-consy-kwd-rule ,g!-name ,known-var ,known-cons-var)
       (define-cg-llvm-rule ,name ()
	 (|| (descend-with-rule ',g!-name)
	     ,@(mapcar (lambda (x)
			 (let ((string (%stringify-symbol (car x))))
			   `(progn
			      (descend-with-rule 'string
						 ,string)
			      (list ,(keywordify (string-upcase string))
				    (progm "(" (descend-with-rule ',(cadr x)) ")")))))
		       known-algol-var))))))

(define-algol-consy-kwd-rule %parameter-attr
    (zeroext
     signext
     inreg
     byval
     inalloca
     sret
     noalias
     nocapture
     nest
     returned
     nonull)
  ((align pos-integer))
  ((dereferenceable pos-integer)))

(define-cg-llvm-rule parameter-attr ()
  (|| `(:group ,(progn
		 (v #\#)
		 (v pos-integer)))
      %parameter-attr))

(define-plural-rule parameter-attrs parameter-attr whitespace)

(define-algol-consy-kwd-rule %fun-attr
    (alwaysinline
     builtin
     cold
     inlinehint
     jumptable
     minsize
     naked
     nobuiltin
     noduplicate
     noimplicitfloat
     noinline
     nonlazybind
     noredzone
     noreturn
     nounwind
     optnone
     optsize
     readnone
     readonly
     returns-twice
     sanitize-address
     sanitize-memory
     sanitize-thread
     ssp
     sspreq
     sspstrong
     thunk
     uwtable)
  ()
  ((alignstack pos-integer)))

(define-cg-llvm-rule fun-attr ()
  (|| `(:group ,(progn
		 (v #\#)
		 (v pos-integer)))
      %fun-attr))

(define-plural-rule fun-attrs fun-attr whitespace)

(define-cg-llvm-rule unnamed-addr ()
  (v "unnamed_addr")
  '(:unnamed-addr t))

(define-cg-llvm-rule section ()
  (progn-v
   "section"
   whitespace
   #\"
   (cap a alphanumeric-word)
   #\")
  (recap a))

;;;;Example: x y
(defmacro define-python-rule (name subrule)
  (destructuring-bind (rule-name cmd-name) (if (atom name) (list name name) name)
    `(define-cg-llvm-rule ,rule-name ()
       `(,',(keywordify cmd-name)
	    ,(progn-v (descend-with-rule 'string ,(%stringify-symbol cmd-name))
		      whitespace
		      ,subrule)))))

;;;;Example: x(y)
(defmacro define-algol-rule (name subrule)
  (destructuring-bind (rule-name cmd-name) (if (atom name) (list name name) name)
    `(define-cg-llvm-rule ,rule-name ()
       `(,',(keywordify cmd-name)
	    ,(progn-v (descend-with-rule 'string ,(%stringify-symbol cmd-name))
		      "("
		      (prog1-v ,subrule
			       ")"))))))


(define-python-rule align pos-integer)
(define-algol-rule comdat
    (progn-v
     "$"
     alphanumeric-word))
(define-python-rule (gc-name gc)
    (progm
     #\"
     alphanumeric-word
     #\"))

(define-python-rule prefix llvm-constant)
(define-python-rule prologue llvm-constant)
(define-python-rule personality llvm-constant)


(defun return-type-lisp-form (type attrs)
  `(,(emit-lisp-repr type) ,@(%%inject-kwd-if-nonnil attrs)))

(defmacro with-rule-names ((name-var) &body body)
  `(destructuring-bind (rule-name instr-name)
       (if (symbolp ,name-var)
	   (list
	    (intern (interpol ;#"$((string ,name-var))-INSTRUCTION"
		     (string ,name-var)
		     "-INSTRUCTION"))
	    ,name-var)
	   (list
	    (car ,name-var)
	    (cadr ,name-var)))
     (declare (ignorable rule-name instr-name))
     ,@body))

(defmacro define-op-rule (name &body body)
  (with-rule-names (name)
    (let ((body-rule-name (if (symbolp name)
			      (intern (interpol ;#"$((string name))-INSTRUCTION-BODY"
				       (string name)
				       "-INSTRUCTION-BODY"
				       ))
			      (intern (interpol ;#"$((string (car name)))-BODY"
				       (string (car name))
				       "-BODY"
				       )))))
      `(progn (define-cg-llvm-rule ,body-rule-name ()
		,@body)
	      (define-cg-llvm-rule ,rule-name ()
		(descend-with-rule 'string ,(%stringify-symbol instr-name))
		(cons ',instr-name
		      (v ,body-rule-name)))))))

(define-plural-rule instruction-metadata fundef-metadata-entry white-comma)

(defmacro with-metadata (&body body)
  `(let ((body (progn-v ,@body))
	 (metadata (? (progn
			(v white-comma)
			(v instruction-metadata)))))
     (if metadata
	 (append body (list (list :metadata metadata)))
	 body)))

(defmacro define-instruction-rule (name &body body)
  `(define-op-rule ,name
     (with-metadata ,@body)))


(define-op-rule (function-declaration declare)
  (let* ((linkage (?wh linkage-type))
	 (visibility (?wh visibility-style))
	 (dll-storage-class (?wh dll-storage-class))
	 (cconv (?wh cconv))
	 (unnamed-addr (?wh unnamed-addr))
	 (return-attrs (?wh parameter-attrs))
	 (return-type (progn (v whitespace)
			     (v llvm-type)))
	 (fname (progn (v whitespace)
		       (v llvm-identifier)))
	 (args (wh? declfun-args))
	 (fun-attrs (?wh fun-attrs))
	 (align (?wh align))
	 (gc (?wh gc-name))
	 (prefix (?wh prefix))
	 (prologue (?wh prologue)))
    `(,fname ,args
	     ,(return-type-lisp-form return-type return-attrs)
	     ,@(append (%%inject-kwds-if-nonnil
			linkage visibility dll-storage-class
			cconv)
		       (%%inject-if-nonnil unnamed-addr)
		       (%%inject-kwds-if-nonnil fun-attrs align gc prefix prologue)))))

;; Let's move to alias definitions

;; @<Name> = [Linkage] [Visibility] [DLLStorageClass] [ThreadLocal] [unnamed_addr] alias <AliaseeTy> @<Aliasee>

;; (alias new-name type old-name
;;   (:linkage linkage)
;;   (:visibility visibility)
;;   ...)

(define-algol-rule (%thread-local thread_local) tls-model)

(define-cg-llvm-rule thread-local ()
  (|| %thread-local
      (progn (v "thread_local")
	     '(:thread_local t))))

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
	       `(? (progn-v whitespace ,x))))
    (let ((name (v llvm-identifier)))
      (v whitespace)
      (v "=")
      (let* ((linkage (frob (whitelist-kwd-expr
			     '(:private
			       :internal
			       :linkoce
			       :weak
			       :linkonce-odr
			       :weak-odr
			       :external)
			     (v linkage-type))))
	     (visibility (frob visibility-style))
	     (dll-storage-class (frob dll-storage-class))
	     (thread-local (frob thread-local))
	     (unnamed-addr (frob unnamed-addr)))
	(v whitespace)
	(v "alias")
	(let* ((type (progn (v whitespace)
			    (v llvm-type)))
	       (old-name (progn (v whitespace)
				(v llvm-identifier))))
	  `(alias ,name ,old-name ,(emit-lisp-repr type)
		  ,@(%%inject-kwd-if-nonnil linkage)
		  ,@(%%inject-kwd-if-nonnil visibility)
		  ,@(%%inject-kwd-if-nonnil dll-storage-class)
		  ,@(%%inject-if-nonnil thread-local)
		  ,@(%%inject-if-nonnil unnamed-addr)))))))

;;; Let's move to comdats

(define-kwd-rule selection-kind
    (any
     exactmatch
     largest
     noduplicates
     samesize))

(define-cg-llvm-rule comdat-toplevel ()
  (let ((name (progn (v "$")
		     (v alphanumeric-word))))
    (v whitespace)
    (v "=")
    (v whitespace)
    (v "comdat")
    (v whitespace)
    (let ((kind (v selection-kind)))
      `(comdat ,name ,kind))))

;;; Metadata is to scary to tackle right now ...


;;; Inline assembly
(define-cg-llvm-rule inline-assembly ()
  (v "module")
  (v whitespace)
  (v "asm")
  (v whitespace)
  (let ((it (v llvm-string)))
    `(asm ,it)))

(define-cg-llvm-rule target-triple ()
  (v "target")
  (v whitespace)
  (v "triple")
  (v whitespace)
  (v "=")
  (v whitespace)
  (let ((it (v llvm-string)))
    `(target-triple ,@(mapcar (lambda (x y)
				(list x y))
			      '(:arch :vendor :system :env)
			      (cl-ppcre:split (literal-string "-") it)))))

(define-cg-llvm-rule dl-big-endian ()
  (v "E")
  :big-endian)
(define-cg-llvm-rule dl-big-endian ()
  (v "e")
  :little-endian)

(define-cg-llvm-rule dl-stack-alignment ()
  (v "S")
  (let ((it (v pos-integer)))
    (assert (equal 0 (mod it 8)))
    `(:stack-alignment ,it)))

(define-cg-llvm-rule dl-pointer-size ()
  (v "p")
  (let ((n (? pos-integer))
	(size (progn (v #\:)
		     (v pos-integer)))
	(abi (progn (v #\:)
		    (v pos-integer)))
	(pref (progn (v #\:)
		     (v pos-integer))))
    `(:pointer-size ,@(if n `((:addrspace ,n)))
		    (:size ,size)
		    (:abi ,abi)
		    (:pref ,pref))))

(define-cg-llvm-rule stack-layout ()
  (let ((it (v pos-integer)))
    (if (not (equal 0 (mod it 8)))
	(fail-parse-format "Stack alignment should be multiple of 8, but got ~a" it))
    `(:stack ,it)))

(define-cg-llvm-rule pointer-layout ()
  (let ((n (? pos-integer)))
    (if (and n (not (and (< 0 n)
			 (> (expt 2 23) n))))
	(fail-parse-format "Pointer address space not in range: ~a" n))
    (let ((size (progm #\: pos-integer #\:))
	  (abi-pref (v abi-layout)))
      `(:pointer ,size ,abi-pref ,@(if n `((:addrspace ,n)))))))

(define-cg-llvm-rule integer-layout ()
  (let ((size (prog1 (v pos-integer)
		(v #\:)))
	(abi-pref (v abi-layout)))
    `(:integer ,size ,abi-pref)))
(define-cg-llvm-rule vector-layout ()
  (let ((size (prog1 (v pos-integer)
		(v #\:)))
	(abi-pref (v abi-layout)))
    `(:vector ,size ,abi-pref)))
(define-cg-llvm-rule float-layout ()
  (let ((size (prog1 (v pos-integer)
		(v #\:)))
	(abi-pref (v abi-layout)))
    `(:float ,size ,abi-pref)))
(define-cg-llvm-rule aggregate-layout ()
  `(:aggregate ,(v abi-layout)))

(define-cg-llvm-rule abi-layout ()
  (let ((abi (v pos-integer))
	(pref (? (progn (v #\:)
			(v pos-integer)))))
    `(:abi ,abi ,@(if pref `(,pref)))))

(define-cg-llvm-rule mangling-layout ()
  (v #\:)
  (list :mangling
	(|| (progn (v #\e)
		   :elf)
	    (progn (v #\m)
		   :mips)
	    (progn (v #\o)
		   :mach-o)
	    (progn (v #\w)
		   :windows-coff))))

(define-plural-rule integer-sizes pos-integer #\:)

(define-cg-llvm-rule native-integers-layout ()
  `(:native-integers ,@(v integer-sizes)))

(define-cg-llvm-rule datalayout-spec ()
  (etouq
    `(||
      ,@(mapcar
	 (lambda (x)
	   `(progn-v ,@x))
	 '((#\E '(:endianness :big))
	   (#\e '(:endianness :little))
	   (#\S stack-layout)
	   (#\p pointer-layout)
	   (#\i integer-layout)
	   (#\v vector-layout)
	   (#\f float-layout)
	   (#\a aggregate-layout)
	   (#\m mangling-layout)
	   (#\n native-integers-layout))))))

(define-cg-llvm-rule target-datalayout ()
  (v "target")
  (v whitespace)
  (v "datalayout")
  (v whitespace)
  (v "=")
  (v whitespace)
  (let ((it (cl-ppcre:split (literal-string "-")
			    (v llvm-string))))
    ;; `(target-datalayout ,it)))
    `(target-datalayout ,@(mapcar (lambda (x)
				    (cg-llvm-parse 'datalayout-spec x))
				  it))))

