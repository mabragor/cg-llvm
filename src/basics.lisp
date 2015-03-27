;;;; cg-llvm.lisp

(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

;; TODO: extra sugar like autounderscoring symbols or assembling lists into something sensible?
;; Does this make sense for LLVM IR at all?

(defmacro joining-with-comma-space (&body body)
  `(joinl ", " (mapcar #'emit-text-repr
		       (remove-if-not #'identity  `(,,@body)))))

(defmacro tsymstr (sym)
  `(if ,sym ,(string-downcase sym)))

(defun ensure-cons (x)
  (if (atom x)
      (list x)
      x))

(defmacro! emit-resulty (tmp-var-spec &body things)
  `(let ((,g!-tmp ,tmp-var-spec))
     (format t "~a = ~a~%"
	     (emit-text-repr (slot-value ,g!-tmp 'value))
	     (joinl " "
		    (remove-if-not #'identity
				   (list ,@things))))
     ,g!-tmp))

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
  (make-instance 'typed-value
		 :type (coerce-to-llvm-type type)
		 :value value))
  

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
	 (cond ((string= "TRUE" (string smth)) (mk-typed-value "i1" "true"))
	       ((string= "FALSE" (string smth)) (mk-typed-value "i1" "false"))
	       (t (make-instance 'typed-value
				 :type (cg-llvm-parse 'llvm-type "label")
				 :value (stringify-symbol smth)))))
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

(defmacro! define-coercer (name known-var)
  `(defun ,(intern #?"COERCE-TO-$((string name))") (smth)
     (if (not smth)
	 ""
	 (let ((txt (cond ((stringp smth) #?"$(smth)")
			  ((symbolp smth) (stringify-symbol smth))
			  (t (error ,#?"Don't know how to coerce this to $((stringify-symbol name)): ~a"
				    smth)))))
	   (if (not (member txt ,known-var :test #'string=))
	       (error ,#?"~s is not a known $((string-downcase name)), see variable $((string known-var))" txt))
	   txt))))

(defparameter known-fast-math-flags '("nnan" "ninf" "nsz" "arcp" "fast"))

(define-coercer fast-math-flag known-fast-math-flags)



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
	    (joining-with-comma-space ,@targs)
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

(defmacro define-binop-definer (name extra-args &body body)
  `(defmacro ,name (name &body type-checks)
     `(defun ,name (op1 op2 ,,@extra-args)
	(let ((top1 (imply-type op1))
	      (top2 (imply-type op2)))
	  (assert (llvm-same-typep top1 top2))
	  ,@type-checks
	  (emit-resulty (make-tmp-var ',name top1)
	    (joinl " "
		   (remove-if-not #'identity
				  `(,,(trim-llvm (underscorize name))
				      ,,,@body
				      (quasiquote-2.0:oinject
				       (joining-with-comma-space
					 ,top1
					 ,(slot-value top2 'value)))))))))))


(define-binop-definer define-no-wrap-binop (&key no-signed-wrap no-unsigned-wrap)
  ,(if no-unsigned-wrap "nuw")
  ,(if no-signed-wrap "nsw"))

(define-binop-definer define-float-binop (&rest flags)
  ,@(mapcar #'coerce-to-fast-math-flag flags))

(define-binop-definer define-exactable-binop (&key exact)
  ,(tsymstr exact))

(define-binop-definer define-simple-binop ())

;; OK, I probably want to reuse the results returned by these binary operations
;; in C++ library functions just simply return this result, outputting the result of
;; code-generation as a side-effect somehow.

;; So, what is the really smart and concise way to do this?

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


;;; Operations on vectors


(defun extractelement (vector index)
  (let ((tvector (imply-type vector))
	(tindex (imply-type index)))
    (assert (llvm-typep '(vector * *) tvector))
    (assert (llvm-typep '(integer *) tindex))
    (emit-resulty (make-tmp-var 'exelt (slot-value (slot-value tvector 'type) 'elt-type))
      "extractelement"
      (joining-with-comma-space ,tvector ,tindex))))


(defun insertelement (vector elt index)
  (let ((tvector (imply-type vector))
	(telt (imply-type elt))
	(tindex (imply-type index)))
    (assert (llvm-typep '(vector * *) tvector))
    (assert (llvm-typep '(integer *) tindex))
    (assert (llvm-same-typep telt (slot-value (slot-value tvector 'type) 'elt-type)))
    (emit-resulty (make-tmp-var 'inselt (slot-value tvector 'type))
      "insertelement"
      (joining-with-comma-space ,tvector ,telt ,tindex))))

(defun shufflevector (vec1 vec2 mask)
  (let ((tvec1 (imply-type vec1))
	(tvec2 (imply-type vec2))
	(tmask (imply-type mask)))
    (assert (llvm-typep '(vector * *) tvec1))
    (assert (llvm-typep '(vector * *) tvec2))
    (assert (llvm-typep '(vector (integer 32) *) tmask))
    (assert (llvm-same-typep (slot-value (slot-value tvec1 'type) 'elt-type)
			     (slot-value (slot-value tvec2 'type) 'elt-type)))
    (emit-resulty (make-tmp-var 'shufvec
				(llvm-vector (slot-value (slot-value tmask 'type) 'num-elts)
					     (slot-value (slot-value tvec1 'type) 'elt-type)))
      "shufflevector"
      (joining-with-comma-space ,tvec1 ,tvec2 ,tmask))))

;; This shows that assertion checks are made only in runtime
;; I dunno, whether I can persuade compiler somehow to do them in compile-time,
;; and even if it's a good thing, since everything is still compile time for LLVM code
(defun foo ()
  (extractelement 1 2))



;;; Aggregate operations

(defun extractvalue (aggregate &rest indices)
  (let ((taggregate (imply-type aggregate)))
    (assert (typep (slot-value taggregate 'type) 'llvm-aggregate-type))
    (assert indices)
    (let ((type (slot-value taggregate 'type)))
      (iter (for index in indices)
	    (assert (integerp index))
	    (setf type (cond ((typep type 'llvm-struct) (elt (slot-value type 'elt-types) index))
			     ((typep type 'llvm-array) (slot-value type 'elt-type))
			     (t (error "Attempt to take element of non-aggregate type: ~a" type)))))
      (emit-resulty (make-tmp-var 'exval type)
	"extractvalue"
	(joining-with-comma-space ,taggregate ,@indices)))))



(defun insertvalue (aggregate elt &rest indices)
  (let ((taggregate (imply-type aggregate))
	(telt (imply-type elt)))
    (assert (typep (slot-value taggregate 'type) 'llvm-aggregate-type))
    (assert indices)
    (let ((type (slot-value taggregate 'type)))
      (iter (for index in indices)
	    (assert (integerp index))
	    (setf type (cond ((typep type 'llvm-struct) (elt (slot-value type 'elt-types) index))
			     ((typep type 'llvm-array) (slot-value type 'elt-type))
			     (t (error "Attempt to insert into non-aggregate type: ~a" type)))))
      (assert (llvm-same-typep type telt))
      (emit-resulty (make-tmp-var 'insval (slot-value taggregate 'type))
	"insertvalue"
	(joining-with-comma-space ,taggregate ,telt ,@indices)))))


;;; Memory access and addressing operations

(defun alloca (type &key num-elts align inalloca)
  (let ((ttype (coerce-to-llvm-type type))
	(tnum-elts (if num-elts (imply-type num-elts))))
    (emit-resulty (make-tmp-var 'ptr (llvm-pointer ttype))
      "alloca"
      (tsymstr inalloca)
      (joining-with-comma-space
	,ttype
	,tnum-elts
	,(if align #?"align $((emit-text-repr align))")))))
    

;; The description of these operations is too large and scary for
;; me to attack them directly ...
(defun llvm-load ()
  nil)
(defun store ()
  nil)

(defparameter known-orderings '("unordered" "monotonic" "acquire" "release" "acq_rel" "seq_cst"))

(define-coercer ordering known-orderings)

(defun fence (ordering &key singlethread)
  (format t (joinl " "
		   (remove-if-not #'identity
				  (list "fence"
					(tsymstr singlethread)
					(coerce-to-ordering ordering))))))

(defun cmpxchg (ptr cmp new success-ord fail-ord &key weak volatile singlethread)
  (let ((tptr (imply-type ptr))
	(tcmp (imply-type cmp))
	(tnew (imply-type new)))
    (assert (llvm-same-typep tcmp tnew))
    (assert (llvm-same-typep (slot-value tptr 'pointee) tcmp))
    (emit-resulty (make-tmp-var 'xchg (llvm-struct (slot-value tcmp 'type) (coerce-to-llvm-type "i1")))
      "cmpxchg"
      (tsymstr weak)
      (tsymstr volatile)
      (joining-with-comma-space ,tptr ,tcmp ,tnew)
      (tsymstr singlethread)
      ;; TODO : checks for allowed orderings
      (coerce-to-ordering success-ord)
      (coerce-to-ordering fail-ord))))

(defparameter known-operations '("xchg" "add" "sub" "and" "nand" "or" "xor"
				 "max" "min" "umax" "umin"))

(define-coercer operation known-operations)


(defun atomicrmw (op ptr val ord &key volatile singlethread)
  (let ((tptr (imply-type ptr))
	(tval (imply-type val)))
    (assert (llvm-same-typep (slot-value tptr 'pointee) tval))
    (emit-resulty (make-tmp-var 'armw tval)
      "atomicrmw"
      (tsymstr volatile)
      (coerce-to-operation op)
      (joining-with-comma-space ,tptr ,tval)
      (tsymstr singlethread)
      ;; TODO : checks for allowed orderings
      (coerce-to-ordering ord))))


(defun getelementptr (type ptrval indices &key inbounds)
  (let ((ttype (coerce-to-llvm-type type))
	(tptrval (imply-type ptrval))
	(tindices (mapcar #'imply-type indices)))
    (emit-resulty (make-tmp-var 'getelptr (extract-getelementptr-type ttype tindices))
      "getelementptr"
      (tsymstr inbounds)
      ;; TODO : add those numerous checks about correct type of TPTRVAL and TINDICES
      (joining-with-comma-space ,ttype ,tptrval ,@tindices))))


;;; conversion operations

(defmacro define-conversion-op (name &body type-checks)
  (destructuring-bind (name opname) (if (atom name)
					(list name (string-downcase name))
					name)
    `(defun ,name (val type)
       (let ((tval (imply-type val))
	     (ttype (coerce-to-llvm-type type)))
	 ,@type-checks
	 (emit-resulty (make-tmp-var ',name ttype)
	   ,opname
	   (emit-text-repr tval)
	   "to"
	   (emit-text-repr ttype))))))

(define-conversion-op trunc)
(define-conversion-op zext)
(define-conversion-op sext)
(define-conversion-op fptrunc)
(define-conversion-op fpext)
(define-conversion-op fptoui)
(define-conversion-op fptosi)
(define-conversion-op uitofp)
(define-conversion-op sitofp)
(define-conversion-op ptrtoint)
(define-conversion-op inttoptr)
(define-conversion-op bitcast)
(define-conversion-op addrspacecast)

;;; Miscellaneous operations

(defparameter known-icmp-ops '("eq" "ne" "ugt" "uge" "ult" "ule" "sgt" "sge" "slt" "sle"))
(define-coercer icmp-op known-icmp-ops)

(defparameter known-fcmp-ops '("false" "oeq" "ogt" "oge" "olt" "ole" "one" "ord"
			       "ueq" "ugt" "uge" "ult" "ule" "une" "uno" "true"))
(define-coercer fcmp-op known-fcmp-ops)

(defun icmp (cmp-op val1 val2)
  (let ((tval1 (imply-type val1))
	(tval2 (imply-type val2)))
    ;; TODO : smart checks about vector types
    (emit-resulty (make-tmp-var 'icmp "i1")
      "icmp"
      (coerce-to-icmp-op cmp-op)
      tval1
      (slot-value tval2 'value))))

(defun fcmp (cmp-op val1 val2)
  (let ((tval1 (imply-type val1))
	(tval2 (imply-type val2)))
    ;; TODO : smart checks about vector types
    (emit-resulty (make-tmp-var 'fcmp "i1")
      "fcmp"
      (coerce-to-fcmp-op cmp-op)
      tval1
      (slot-value tval2 'value))))

(defun phi (&rest value-labels)
  (let ((tvalues (mapcar (lambda (x) (imply-type (car x))) value-labels))
	(tlabels (mapcar (lambda (x) (imply-type (cadr x))) value-labels)))
    (iter (for tvalue in (cdr tvalues))
	  (if (not (llvm-same-typep (car tvalues) tvalue))
	      (error "Not all incoming values of the phi node have same type.")))
    (iter (for tlabel in tlabels)
	  (assert (llvm-typep 'label tlabel)))
    (emit-resulty (make-tmp-var 'phi (car tvalues))
      "phi"
      (emit-text-repr (slot-value (car tvalues) 'type))
      (joining-with-comma-space ,@(mapcar (lambda (x y)
					    #?"[ $((slot-value x 'value)), $((slot-value y 'value)) ]")
					  tvalues tlabels)))))

(defun select (test val1 val2)
  (let ((ttest (imply-type test))
	(tval1 (imply-type val1))
	(tval2 (imply-type val2)))
    (assert (or (llvm-typep '(integer 1) ttest)
		(llvm-typep '(vector (integer 1) *) ttest)))
    ;; TODO : more type checks
    (emit-resulty (make-tmp-var 'sel tval1)
      "select"
      (joining-with-comma-space ,ttest ,tval1 ,tval2))))

