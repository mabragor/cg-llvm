
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

(defmacro define-plural-rule (name single delim)
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
	(fail-parse-format """Null ptr constant must be of pointer type but got ~a.""" type))
    (list type (text "null"))))

(define-cg-llvm-rule simple-constant ()
  (|| boolean-constant
      integer-constant
      float-constant
      null-ptr-constant))

(define-cg-llvm-rule llvm-constant ()
  (|| simple-constant complex-constant))

(define-plural-rule llvm-constants llvm-constant (progn (? whitespace) #\, (? whitespace)))

(defmacro define-complex-constant-rule (name lb rb type-test content-test errstr1 errstr2)
  `(define-cg-llvm-rule ,name ()
     (let ((type (emit-lisp-repr llvm-type)))
       (if ,type-test
	   (fail-parse-format ,(join "" errstr1 " constant must be of " errstr2 " type, but got ~a") type))
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


;; (define-cg-llvm-rule string-constant ()
;;   (let ((type (emit-lisp-repr llvm-type)))
;;     (if (not (llvm-typep '(array (integer 8) *) type))
;; 	(fail-parse-format "String constant must be of array-of-chars type, but got ~a") type)
;;     (let ((content (progm (progn (descend-with-rule 'string ,lb) (? whitespace))
;; 			  llvm-constants
;; 			  (progn (? whitespace) (descend-with-rule 'string ,rb)))))
;;       ,content-test
;;       (list type content))))



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

(define-cg-llvm-rule escaped-identifier ()
  (text (progm #\"
	       (times (progn (! #\")
			     (|| double-hex-escaped-char
				 (descend-with-rule 'character nil))))
	       #\")))
  

(define-cg-llvm-rule llvm-identifier ()
  (|| usual-identifier
      escaped-identifier))

;; (define-cg-llvm-rule function-declaration ()
;;   "define" whitespace
;;   (macrolet ((frob (x)
;; 	       `(? (prog1 ,x whitespace))))
;;     (let* ((linkage (frob linkage-type))
;; 	   (visibility (frob visibility-style))
;; 	   (dll-class (frob dll-storage-class))
;; 	   (cconv (frob cconv))
;; 	   (unnamed-addr (frob unnamed-addr))
;; 	   (return-type (prog1 llvm-type whitespace))
;; 	   (return-attrs (frob parameter-attrs))
;; 	   (setf fname alphanumeric-word) whitespace ; should be different
;;   ;; TODO: arguments
;;   "args" whitespace 
;;   (setf alignment (? align)) whitespace
;;   (setf gc (? gc-name)) whitespace
;;   (setf prefix (? prefix)) whitespace
;;   (setf prologue (? prologue)))
  
