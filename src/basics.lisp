;;;; cg-llvm.lisp

(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

;; TODO: extra sugar like autounderscoring symbols or assembling lists into something sensible?
;; Does this make sense for LLVM IR at all?

(defun ensure-cons (x)
  (if (atom x)
      (list x)
      x))

(defparameter *context* :toplevel)

(defmethod emit-text-repr ((obj t))
  (format nil "~a" obj))

(defmethod emit-text-repr ((obj symbol))
  (stringify-symbol obj))

(defclass typed-value ()
  ((type :initform (error "You should specify the LLVM type of the value")
	 :initarg :type)
   (value :initform (error "You should specify the value")
	  :initarg :value)))

(defun mk-typed-value (type value)
  (make-instance 'typed-value :type type :value value))
  

(defclass llvm-no-value (llvm-type)
  ())

(defmethod emit-text-repr ((typed-value typed-value))
  (with-slots (type value) typed-value
    (cond ((typep type 'llvm-no-value)
	   (error "Attempt to emit the no-value (terminating instructions used as input ?)"))
	  ((typep type 'llvm-void-type)
	   "void")
	  (t #?"$((emit-text-repr type)) $((emit-text-repr value))"))))
	   

(defun mk-novalue ()
  (make-instance 'typed-value :type (make-instance 'llvm-no-value) :value nil))

;;; Terminating instructions
(defun %llvm-return (typed-value)
  (if (not (eq :function *context*))
      (error "Return should only be used in function context, but context is ~a" *context*))
  (emit-cmd "ret" typed-value)
  (mk-novalue))

(defun imply-type (smth)
  (cond ((typep smth 'typed-value) smth)
	((eq :void smth)
	 (make-instance 'typed-value :type (make-instance 'llvm-void-type) :value :void))
	((floatp smth)
	 (make-instance 'typed-value
			:type (cg-llvm-parse 'llvm-type "float")
			:value smth))
	((integerp smth)
	 (make-instance 'typed-value
			:type (cg-llvm-parse 'llvm-type "i32")
			:value smth))
	((symbolp smth)
	 (make-instance 'typed-value
			:type (cg-llvm-parse 'llvm-type "label")
			:value (stringify-symbol smth)))
	((listp smth)
	 (destructuring-bind (value type) smth
	   (make-instance 'typed-value
			  :type (cond ((typep type 'llvm-type) type)
				      ((stringp type) (cg-llvm-parse 'llvm-type type))
				      ((consp type) (parse-lisp-repr type))
				      (t "Can't guessed type from this notation: ~a" type))
			  :value value)))
	(t (error "Failed to imply type for this: ~a" smth))))

(defun llvm-return (value &optional type)
  (%llvm-return (if type
		    (make-instance 'typed-value :type type :value value)
		    (imply-type value))))


(defun %print-typevalue (typevalue)
  "Typevalue is supposed to be the LIST of the form (VALUE TYPE). VALUE and TYPE are supposed to be atoms."
  (assert (equal 2 (length typevalue)))
  #?"$((cadr typevalue) (car typevalue))")

(defun emit-cmd (name &rest args)
  (format t "~a ~{~a~^, ~}" name (mapcar #'emit-text-repr args)))

(defun unconditional-branch (label)
  (let ((it (imply-type label)))
    (with-slots (type value) it
      (assert (typep type 'llvm-label))
      (emit-cmd "br" it)
      (mk-novalue))))

(defun conditional-branch (test then else)
  (let ((ttest (imply-type test))
	(tthen (imply-type then))
	(telse (imply-type else)))
    (assert (llvm-typep '(integer 1) ttest))
    (assert (llvm-typep 'label tthen))
    (assert (llvm-typep 'label telse))
    (emit-cmd "br" ttest tthen telse)
    (mk-novalue)))

(defun pairs (lst)
  (assert (evenp (length lst)))
  (let (tmp res)
    (iter (for i from 0)
	  (for elt in lst)
	  (push elt tmp)
	  (when (oddp i)
	    (push (nreverse tmp) res)
	    (setf tmp nil)))
    (nreverse res)))
	
(defun switch (value default-dest &rest branch-specs)
  (let ((tvalue (imply-type value))
	(tdefault-dest (imply-type default-dest))
	(tbranch-specs (mapcar #'imply-type branch-specs)))
    (assert (llvm-typep '(integer *) tvalue))
    (assert (llvm-typep 'label tdefault-dest))
    (format t "switch ~a, ~a [~a]"
	    (emit-text-repr tvalue)
	    (emit-text-repr tdefault-dest)
	    (joinl " " (mapcar (lambda (x)
				 (destructuring-bind (condition label) x
				   (assert (llvm-typep '(integer *) condition))
				   (assert (llvm-typep 'label label))
				   (joinl ", " (mapcar #'emit-text-repr x))))
			       (pairs tbranch-specs))))
    (mk-novalue)))


(defun indirect-branch (address &rest destinations)
  (let ((taddress (imply-type address))
	(tdestinations (mapcar #'imply-type destinations)))
    (assert (llvm-typep '(pointer (integer 8)) taddress))
    (format t "indirectbr ~a, [~a]"
	    (emit-text-repr taddress)
	    (joinl ", " (mapcar (lambda (x)
				  (assert (llvm-typep 'label x))
				  (emit-text-repr x))
				tdestinations)))
    (mk-novalue)))

(defparameter known-cconvs '("ccc" "fastcc" "coldcc" "webkit_jscc" "anyregcc" "preserve_mostcc"
			     "preserve_allcc"))

(defun coerce-to-cconv (smth)
  (cond ((not smth) "")
	((stringp smth) #?"$(smth) ")
	((symbolp smth) (let ((txt (stringify-symbol smth)))
			  (assert (member txt known-cconvs :test #'string=))
			  #?"$(txt) "))
	((consp smth) (destructuring-bind (cc number) smth
			(assert (integerp number))
			(assert (symbolp cc))
			(let ((txt (stringify-symbol cc)))
			  (if (string= "cc" txt)
			      #?"$(txt) $(number)"
			      (error "Don't know the numbered cconv ~a" smth)))))
	(t (error "Don't know how to coerce this to calling convention: ~a" smth))))

(defparameter known-parameter-attrs '("zeroext" "signext" "inreg" "byval"
				      "inalloca" "sret" "noalias" "nocapture"
				      "nest" "returned" "nonull"))

	
(defun %coerce-param-attr (smth)
  (cond ((not smth) "")
	((stringp smth) #?"$(smth)")
	((symbolp smth) (let ((txt (stringify-symbol smth)))
			  (assert (member txt known-parameter-attrs :test #'string=))
			  txt))
	((consp smth) (destructuring-bind (param number) smth
			(assert (integerp number))
			(assert (symbolp param))
			(let ((txt (stringify-symbol param)))
			  ;; Hardcoded KLUDGE, but they really have different syntax, what can I do...
			  (cond ((string= "align" txt) #?"$(txt) $(number)")
				((string= "dereferenceable" txt) #?"$(txt)($(number))")
				(t (error "Don't know cons parameter attribute: ~a" txt))))))
	(t (error "Don't know how to coerce this to parameter attribute: ~a" smth))))

(defun coerce-param-attrs (attrs &rest allowed-attrs)
  (let ((coerced-attrs (mapcar #'%coerce-param-attr attrs))
	(allowed-attrs (mapcar #'stringify-symbol allowed-attrs)))
    (iter (for attr in coerced-attrs)
	  (if (not (find attr allowed-attrs :test #'string=))
	      (error "Param attribute ~a is not among allowed ones, which are (~{~a~^ ~})"
		     attr allowed-attrs)))
    (if (not coerced-attrs)
	""
	#?"$((joinl " " coerced-attrs)) ")))
	  
(defparameter known-fun-attrs '("alwaysinline" "builtin" "cold" "inlinehint"
				"jumptable" "minsize" "naked" "nobuiltin"
				"noduplicate" "noimplicitfloat" "noinline"
				"nonlazybind" "noredzone" "noreturn"
				"nounwind" "optnone" "optsize" "readnone"
				"readonly" "returns_twice" "sanitize_address"
				"sanitize_memory" "sanitize_thread" "ssp" "sspreq"
				"sspstrong" "thunk" "uwtable"))


;; OK, I know it's a duplication, but so many details needs to be transfered to
;; a metafunction to correctly tell all the differences, that the pros of meta-ing kind of fade

(defun %coerce-fun-attr (smth)
  (cond ((not smth) "")
	((stringp smth) #?"$(smth)")
	((symbolp smth) (let ((txt (stringify-symbol smth)))
			  (assert (member txt known-fun-attrs :test #'string=))
			  txt))
	((consp smth) (destructuring-bind (param number) smth
			(assert (integerp number))
			(assert (symbolp param))
			(let ((txt (stringify-symbol param)))
			  ;; Hardcoded KLUDGE, but they really have different syntax, what can I do...
			  (cond ((string= "alignstack" txt) #?"$(txt)($(number))")
				(t (error "Don't know cons function attribute: ~a" txt))))))
	(t (error "Don't know how to coerce this to function attribute: ~a" smth))))

(defun coerce-fun-attrs (attrs &rest allowed-attrs)
  (let ((coerced-attrs (mapcar #'%coerce-fun-attr attrs))
	(allowed-attrs (mapcar #'stringify-symbol allowed-attrs)))
    (iter (for attr in coerced-attrs)
	  (if (not (find attr allowed-attrs :test #'string=))
	      (error "Function attribute ~a is not among allowed ones, which are (~{~a~^ ~})"
		     attr allowed-attrs)))
    (if (not coerced-attrs)
	""
	#?"$((joinl " " coerced-attrs)) ")))

(defparameter known-fast-math-flags '("nnan" "ninf" "nsz" "arcp" "fast"))


(defun coerce-fast-math-flag (smth)
  (cond ((not smth) "")
	((stringp smth) #?"$(smth)")
	((symbolp smth) (let ((txt (stringify-symbol smth)))
			  (assert (member txt known-fast-math-flags :test #'string=))
			  txt))
	(t (error "Don't know how to coerce this to function attribute: ~a" smth))))
  



;; TODO: I'll write for now like that but clearly I meant something more smart here...
;; Probably, possibility to not duplicate types of arguments, if they can be implied
;; from type of function or vice versa.
(defmacro nimply-fun-and-args-types (fun-var args-var)
  `(progn (setf ,fun-var (imply-type ,fun-var))
	  (setf ,args-var (mapcar #'imply-type ,args-var))))

(defun invoke (fun args normal-label unwind-label
	       &key call-conv return-attrs fun-attrs)
  (let ((tfun (imply-type fun))
	(targs (mapcar #'imply-type args))
	(tnormal-label (imply-type normal-label))
	(tunwind-label (imply-type unwind-label))
	(tcall-conv (coerce-to-cconv call-conv))
	(treturn-attrs (coerce-param-attrs return-attrs :zeroext :signext :inreg))
	(tfun-attrs (coerce-fun-attrs fun-attrs :noreturn :nounwind :readonly :readnone)))
    (nimply-fun-and-args-types tfun targs)
    (format t "invoke ~a~a~a(~a) ~ato ~a unwind ~a"
	    tcall-conv
	    treturn-attrs
	    (emit-text-repr tfun)
	    (joinl ", " (mapcar #'emit-text-repr targs))
	    tfun-attrs
	    (emit-text-repr tnormal-label)
	    (emit-text-repr tunwind-label))
    ;; OK, this is clearly NOT right, but what to do here?
    (mk-novalue)))
				  
(defun resume (typevalue)
  (emit-cmd "resume" (imply-type typevalue))
  (mk-novalue))

(defun unreachable ()
  (format t "unreachable")
  (mk-novalue))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trim-llvm (str)
    (if (m~ "^llvm_(.*)" str)
	$1
	str)))

(let ((counts-hash (make-hash-table :test #'equal)))
  (defun make-tmp-var (sym type)
    (let* ((str (string sym))
	   (num (incf (gethash str counts-hash 0))))
      (mk-typed-value type (intern #?"%TMP$(str)$(num)"))))
  (defun reset-tmp-var-counts ()
    (clrhash counts-hash)))

(defmacro define-no-wrap-binop (name &body type-checks)
  `(defun ,name (op1 op2 &key no-signed-wrap no-unsigned-wrap)
     (let ((top1 (imply-type op1))
	   (top2 (imply-type op2)))
       (assert (llvm-same-typep top1 top2))
       ,@type-checks
       (let ((res-var (make-tmp-var ',name (slot-value top1 'type))))
	 (format t "~a = ~a~%"
		 (emit-text-repr (slot-value res-var 'value))
		 (joinl " "
			(remove-if-not #'identity
					 (list ,(trim-llvm (underscorize name))
					       (if no-unsigned-wrap "nuw")
					       (if no-signed-wrap "nsw")
					       (join ", "
						     (emit-text-repr top1)
						     (emit-text-repr (slot-value top2 'value)))))))
	 res-var))))

;; OK, I probably want to reuse the results returned by these binary operations
;; in C++ library functions just simply return this result, outputting the result of
;; code-generation as a side-effect somehow.

;; So, what is the really smart and concise way to do this?

(defmacro define-float-binop (name &body type-checks)
  `(defun ,name (type op1 op2 &rest flags)
     (let ((top1 (imply-type op1))
	   (top2 (imply-type op2)))
       (assert (llvm-same-typep top1 top2))
       ,@type-checks
       (let ((res-var (make-tmp-var ',name (slot-value top1 'type))))
	 (format t "~a = ~a~%"
		 (emit-text-repr (slot-value res-var 'value))
		 (joinl " "
			(remove-if-not #'identity
				       `(list ,',(trim-llvm (underscorize name))
					      ,@(mapcar #'coerce-to-fast-math-flag flags)
					      (join ", "
						    (emit-text-repr top1)
						    (emit-text-repr (slot-value top2 'value)))))))
	 res-var))))


(defmacro define-exactable-binop (name &body type-checks)
  `(defun ,name (type op1 op2 &rest flags)
     (let ((top1 (imply-type op1))
	   (top2 (imply-type op2)))
       (assert (llvm-same-typep top1 top2))
       ,@type-checks
       (let ((res-var (make-tmp-var ',name (slot-value top1 'type))))
	 (format t "~a = ~a~%"
		 (emit-text-repr (slot-value res-var 'value))
		 (joinl " "
			(remove-if-not #'identity
				       `(list ,',(trim-llvm (underscorize name))
					      ,(if exact "exact")
					      (join ", "
						    (emit-text-repr top1)
						    (emit-text-repr (slot-value top2 'value)))))))
	 res-var))))


(defmacro define-simple-binop (name &body type-checks)
  `(defun ,name (type op1 op2)
     (let ((top1 (imply-type op1))
	   (top2 (imply-type op2)))
       (assert (llvm-same-typep top1 top2))
       ,@type-checks
       (let ((res-var (make-tmp-var ',name (slot-value top1 'type))))
	 (format t "~a = ~a~%"
		 (emit-text-repr (slot-value res-var 'value))
		 (joinl " "
			(remove-if-not #'identity
				       `(list ,',(trim-llvm (underscorize name))
					      (join ", "
						    (emit-text-repr top1)
						    (emit-text-repr (slot-value top2 'value)))))))
	 res-var))))


;; TODO : assert INTEGER or VECTOR of INTEGERS type here
(define-no-wrap-binop add)
(define-no-wrap-binop sub)
(define-no-wrap-binop mul)
(define-no-wrap-binop shl)

(define-float-binop fadd)
(define-float-binop fsub)
(define-float-binop fmul)
(define-float-binop fdiv)
(define-float-binop frem)

(define-exactable-binop udiv)
(define-exactable-binop sdiv)
(define-exactable-binop lshr)
(define-exactable-binop ashr)

(define-simple-binop urem)
(define-simple-binop srem)
(define-simple-binop llvm-add)
(define-simple-binop llvm-or)
(define-simple-binop llvm-xor)

;; So, I have this TRIVIAL-TEMPLATE framework.
;; There, I have a template, and I can append text to any special label, which is in it.
;; Now, I want to do kinda the same here - and vere similar to what C++ framework is doing.
;; Also, I want to capture the last "explicitly specified" mark, to which I inserted something,
;; such that I can "change the insertion point" only sometimes.

;; Then, I also want some elementary value type propagation, such that the code:
;; (add (add 'i32 1 3) 4)
;; would be correctly expanded into the "full version"
;; (add 'i32 (add 'i32 1 3) 4)

;; So, when the argument of the function is not a string, symbol or number, but a 'typedvalue' object,
;; I want some type-propagation to be automatic.


