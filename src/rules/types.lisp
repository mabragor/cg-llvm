;;;; types.lisp
;;;; This file is part of CG-LLVM system,
;;;; see COPYING for details

(in-package #:cg-llvm)

;#+nil
(defgeneric emit-lisp-repr (obj)
  (:documentation "Emit cons-style representation of the object"))

(defmethod emit-lisp-repr ((obj t))
  obj)

#+nil
(defgeneric emit-text-repr (obj)
  (:documentation "Emit text-style representation of the object"))

(progn
  (defclass llvm-type ()
    ())
  (defclass llvm-void-type (llvm-type) ())
  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-void-type))
    'void)

  #+nil
  (defmethod emit-text-repr ((obj llvm-void-type))
    "void"))

(progn
  (defclass llvm-function-type (llvm-type)
    ((ret-type :initarg :ret-type)
     (param-types :initarg :param-types)
     (vararg-p :initarg :vararg-p)))

  (defmethod emit-lisp-repr ((obj llvm-function-type))
    (with-slots (ret-type param-types vararg-p) obj
      `(function ,(emit-lisp-repr ret-type) ,(mapcar #'emit-lisp-repr param-types)
		 :vararg-p ,vararg-p)))

  #+nil
  (defmethod emit-text-repr ((obj llvm-function-type))
    (with-slots (ret-type param-types vararg-p) obj
      (if (not vararg-p)
	  (format nil "~a (~{~a~^, ~})" (emit-text-repr ret-type) (mapcar #'emit-text-repr param-types))
	  (format nil "~a (~{~a, ~}...)" (emit-text-repr ret-type) (mapcar #'emit-text-repr param-types))))))


(progn
  (defclass llvm-named-type (llvm-type)
    ((name :initarg :name :initform "You should specify the name of the named type")))

  (defmethod emit-lisp-repr ((obj llvm-named-type))
    `(named ,(emit-lisp-repr (slot-value obj 'name)))))

(defclass typed-value ()
  ((type :initform (error "You should specify the LLVM type of the value")
	 :initarg :type)
   (value :initform (error "You should specify the value")
	  :initarg :value)))

(defclass llvm-no-value (llvm-type)
  ())

(defclass llvm-first-class-type (llvm-type) ())

#+nil
(defun firstclass-type-p (x)
  (cond ((typep x 'llvm-type) (typep x 'llvm-first-class-type))
	((stringp x) (handler-case (firstclass-type-p (cg-llvm-parse 'llvm-type x))
		       (error () nil)))
	((or (symbolp x) (consp x)) (firstclass-type-p (parse-lisp-repr x)))
	(t nil)))

#+nil
(defun aggregate-type-p (x)
  (cond ((typep x 'llvm-type) (typep x 'llvm-aggregate-type))
	((stringp x) (handler-case (aggregate-type-p (cg-llvm-parse 'llvm-type x))
		       (error () nil)))
	((or (symbolp x) (consp x)) (aggregate-type-p (parse-lisp-repr x)))
	(t nil)))

(progn
  (defclass llvm-integer (llvm-first-class-type)
    ((nbits :initarg :nbits)))
  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-integer))
    `(integer ,(emit-lisp-repr (slot-value obj 'nbits))))

  #+nil
  (defmethod emit-text-repr ((obj llvm-integer))
    ;;#"i$((slot-value obj 'nbits))"
    (interpol
     "i"
     (slot-value obj 'nbits)))
  
  (defvar max-nbits (1- (expt 2 23)))
  (defun llvm-integer (nbits)
    (if (or (not (integerp nbits))
	    (< nbits 1)
	    (> nbits max-nbits))
	(error "NBITS argument should be positive integer not greater than ~a ~a" max-nbits nbits))
    (make-instance 'llvm-integer :nbits nbits)))

(progn
  (progn
    (defclass llvm-float (llvm-first-class-type)
      ((nbits :initarg :nbits)
       (mantissa :initarg :mantissa :initform 0)))

    ;;#+nil
    (defmethod emit-lisp-repr ((obj llvm-float))
      `(float ,(emit-lisp-repr (slot-value obj 'nbits))
	      ,(emit-lisp-repr (slot-value obj 'mantissa))))

    #+nil
    (defmethod emit-text-repr ((obj llvm-float))
      (with-slots (nbits mantissa) obj
	(cond ((and (equal 16 nbits) (equal 8 mantissa)) "half")
	      ((and (equal 32 nbits) (equal 16 mantissa)) "float")
	      ((and (equal 64 nbits) (equal 32 mantissa)) "double")
	      ((and (equal 128 nbits) (equal 112 mantissa)) "fp128")
	      ((and (equal 80 nbits) (equal 40 mantissa)) "x86_fp80")
	      ((and (equal 128 nbits) (equal 64 mantissa)) "ppc_fp128")
	      (t (error "Don't know how to represent given float as text: ~a ~a" nbits mantissa))))))

  
  (defun llvm-float (str)
    (let ((str (string-downcase (string str)))) ;;;;FIXME:: symbol to string conversion is nasty
      (flet ((frob (nbits &optional (mantissa (/ nbits 2)))
	       (make-instance 'llvm-float :nbits nbits :mantissa mantissa)))
	(cond ((string= "half" str) (frob 16))
	      ((string= "float" str) (frob 32))
	      ((string= "double" str) (frob 64))
	      ((string= "fp128" str) (frob 128 112))
	      ((string= "x86_fp80" str) (frob 80))
	      ((string= "ppc_fp128" str) (frob 128))
	      (t (error "Unknown floating point specifier: ~s" str)))))))

(progn
  (defclass llvm-x86-mmx (llvm-first-class-type) ())

  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-x86-mmx))
    'x86-mmx)

  #+nil
  (defmethod emit-text-repr ((obj llvm-x86-mmx))
    "x86_mmx"))

(progn
  (progn
    (defclass llvm-pointer (llvm-first-class-type)
      ((pointee :initarg :pointee)
       (address-space :initform 0 :initarg :address-space)))

    ;;#+nil
    (defmethod emit-lisp-repr ((obj llvm-pointer))
      (with-slots (pointee address-space) obj
	`(pointer ,(emit-lisp-repr pointee)
		  ,@(if (not (equal 0 address-space))
			`(,address-space)))))

    #+nil
    (defmethod emit-text-repr ((obj llvm-pointer))
      (with-slots (pointee address-space) obj
	(if (equal 0 address-space)
	    ;;#"$((emit-text-repr pointee))*"
	    (interpol
	     (emit-text-repr pointee)
	     "*")
	    ;;#"$((emit-text-repr pointee)) addrspace($(address-space))*"
	    (interpol
	     (emit-text-repr pointee)
	     " addrspace("
	     address-space
	     ")*")))))

  (defun llvm-pointer (pointee &optional addrspace)
    (let ((pointee (coerce-to-llvm-type pointee)))
      (cond ((typep pointee 'llvm-void-type)
	     (progn
	       (warn "When declaring pointer type, VOID-typed pointee encountered, coercing to i8")
	       (setf pointee (llvm-integer 8))))
	    ((typep pointee 'llvm-label)
	     (progn
	       (warn "When declaring pointer type, LABEL-typed pointee encountered, coercing to i8")
	       (setf pointee (llvm-integer 8)))))
      (make-instance 'llvm-pointer :pointee pointee
		     :address-space (or addrspace 0)))))

(progn
  (progn
    (defclass llvm-vector (llvm-first-class-type)
      ((num-elts :initarg :num-elts)
       (elt-type :initarg :elt-type)))

    ;;#+nil
    (defmethod emit-lisp-repr ((obj llvm-vector))
      (with-slots (num-elts elt-type) obj
	`(vector ,(emit-lisp-repr elt-type)
		 ,(emit-lisp-repr num-elts))))

    #+nil
    (defmethod emit-text-repr ((obj llvm-vector))
      (with-slots (num-elts elt-type) obj
	;;#"<$(num-elts) x $((emit-text-repr elt-type))>"
	(interpol
	 "<"
	 num-elts
	 " x "
	 (emit-text-repr elt-type)
	 ">"))))
  (defun llvm-vector (n type)
    (if (or (not (integerp n))
	    (<= n 0))
	(error "Number of elements N should be integer greater than zero"))
    (let ((type (coerce-to-llvm-type type)))
      (if (not (or (typep type 'llvm-integer)
		   (typep type 'llvm-float)
		   (typep type 'llvm-pointer)))
	  (error "Element type of vector should be integer, float or pointer, but got/deduced ~a" type))
      (make-instance 'llvm-vector :num-elts n :elt-type type))))

(progn
  (defclass llvm-label (llvm-first-class-type) ())
  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-label))
    'label)

  #+nil
  (defmethod emit-text-repr ((obj llvm-label))
    "label"))

(progn
  (defclass llvm-metadata (llvm-first-class-type) ())
  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-metadata))
    'metadata)

  #+nil
  (defmethod emit-text-repr ((obj llvm-metadata))
    "metadata"))

(defclass llvm-aggregate-type (llvm-first-class-type) ())

(progn
  (defclass llvm-array (llvm-aggregate-type)
    ((num-elts :initarg :num-elts)
     (elt-type :initarg :elt-type)))

  ;;#+nil
  (defmethod emit-lisp-repr ((obj llvm-array))
    (with-slots (num-elts elt-type) obj
      `(array ,(emit-lisp-repr elt-type)
	      ,(emit-lisp-repr num-elts))))

  #+nil
  (defmethod emit-text-repr ((obj llvm-array))
    (with-slots (num-elts elt-type) obj
      ;;#"[$(num-elts) x $((emit-text-repr elt-type))]"
      (interpol
       "["
       num-elts
       " x "
       (emit-text-repr elt-type)
       "]")
      )))

(defun llvm-sizey-type-p (type)
  t)

(defun llvm-array (n type)
  (if (or (not (integerp n))
	  (< n 0))
      (error "Number of elements N should be non-negative integer"))
  (let ((type (coerce-to-llvm-type type)))
    (if (not (llvm-sizey-type-p type))
	(error "Array elt type should have a size, but got/deduced type ~a doesn't" type))
    (make-instance 'llvm-array :num-elts n :elt-type type)))

(progn
  (progn
    (defclass llvm-struct (llvm-aggregate-type)
      ((elt-types :initarg :elt-types)
       (packed-p :initarg :packed-p)))

    ;;#+nil
    (defmethod emit-lisp-repr ((obj llvm-struct))
      (with-slots (elt-types packed-p) obj
	`(struct ,(mapcar #'emit-lisp-repr elt-types)
		 :packed-p ,packed-p)))

    #+nil
    (defmethod emit-text-repr ((obj llvm-struct))
      (with-slots (elt-types packed-p) obj
	(if (not packed-p)
	    (format nil "{~{~a~^, ~}}" (mapcar #'emit-text-repr elt-types))
	    (format nil "<{~{~a~^, ~}}>" (mapcar #'emit-text-repr elt-types))))))

  (progn
    (defclass llvm-opaque-struct () ())

    ;;#+nil
    (defmethod emit-lisp-repr ((obj llvm-opaque-struct))
      'opaque)

    #+nil
    (defmethod emit-text-repr ((obj llvm-opaque-struct))
      "opaque"))

  (defun llvm-struct (&rest types)
    (destructuring-bind (packed-p opaque-p . types) (parse-out-keywords '(:packed-p :opaque-p) types)
      (if opaque-p
	  (progn (if (or packed-p types)
		     (error "Struct specified to be opaque, but also contains some other specifications"))
		 (make-instance 'llvm-opaque-struct))
	  (make-instance 'llvm-struct :packed-p packed-p
			 :elt-types
			 (iter (for type in types)
			       (let ((type (coerce-to-llvm-type type)))
				 (if (not (llvm-sizey-type-p type))
				     (error "Struct elt type should have a size,
                                           but got/deduced type ~a doesn't" type))
				 (collect type))))))))

(defun coerce-to-llvm-type (smth)
  (cond ((typep smth 'llvm-type) smth)
	((stringp smth) (cg-llvm-parse 'llvm-type smth))
	((or (consp smth) (symbolp smth)) (parse-lisp-repr smth))
	((typep smth 'typed-value) (slot-value smth 'type))
	(t (error "Don't know how to coerce this to LLVM type: ~a" smth))))

;;; parsing

(define-cg-llvm-rule pos-integer ()
  (parse-integer (text (postimes ns-dec-digit))))

(define-cg-llvm-rule integer ()
  (parse-integer (text (? "-") (postimes ns-dec-digit))))

(define-cg-llvm-rule integer-type ()
  (v #\i)
  (llvm-integer (v pos-integer)))

(define-cg-llvm-rule void-type ()
  (v "void")
  (make-instance 'llvm-void-type))

(defun parse-function-argtypes (lst)
  (if (eq '*** (car (last lst)))
      (values (butlast lst) t)
      (values lst nil)))

(define-cg-llvm-rule function-argtype ()
  (|| llvm-type
      (progn (v "...")
	     (quote ***) ;;return this
	     )))

(define-cg-llvm-rule function-argtypes ()
  (cons (v function-argtype)
	(times (progn (v white-comma)
		      (v function-argtype)))))

(define-cg-llvm-rule function-type ()
  (let ((ret-type (|| void-type
		      llvm-firstclass-type)))
    (if (or (typep ret-type 'llvm-label)
	    (typep ret-type 'llvm-metadata))
	(fail-parse "Got label or metadata type as function return type"))
    (? whitespace)
    
    (multiple-value-bind (param-types vararg-p)
	(parse-function-argtypes
	 (white-paren
	   function-argtypes))
      (make-instance 'llvm-function-type
		     :ret-type ret-type
		     :param-types param-types
		     :vararg-p vararg-p))))

(define-cg-llvm-rule float-type ()
  (llvm-float (cond-parse ((v "double") 'double)
			  ((v "half") 'half)
			  ((v "float") 'float)
			  ((v "fp128") 'fp128)
			  ((v "x86_fp80") 'x86_fp80)
			  ((v "ppc_fp128") 'ppc_fp128))))

(define-cg-llvm-rule x86-mmx ()
  (v "x86_mmx")
  (make-instance 'llvm-x86-mmx))

(define-cg-llvm-rule addr-space ()
  (|| (progn (v "addrspace")
	     (white-paren
	       simple-int))
      0))
;;;FIXME:: what Why is there a zero? what does it do?

(define-cg-llvm-rule elt-pointer ()
  (? whitespace)
  (cap a addr-space)
  (? whitespace)
  (v "*")
  (recap a))

;; TODO: smart pointer parsing

(define-cg-llvm-rule simple-int ()
  (parse-integer (text (postimes ns-dec-digit))))

(define-cg-llvm-rule white-x-white ()
  (with-whitespace #\x))

(define-cg-llvm-rule vector ()
  (white-<>
    (cap nelts simple-int)
    white-x-white
    (cap type llvm-type))
  (llvm-vector (recap nelts)
	       (recap type)))

(define-cg-llvm-rule label ()
  (v "label")
  (make-instance 'llvm-label))
(define-cg-llvm-rule metadata ()
  (v "metadata")
  (make-instance 'llvm-metadata))

(define-cg-llvm-rule array ()
  (white-[]
    (cap nelts simple-int)
    white-x-white
    (cap type llvm-type))
  
  (llvm-array (recap nelts)
	      (recap type)))


(define-cg-llvm-rule comma-separated-types ()
  (cap type llvm-type)
  (? whitespace)
  (? (progn (v #\,)
	    (? whitespace)
	    (cap rectypes comma-separated-types)))
  (cons (recap type)
	(recap? rectypes)))


(define-cg-llvm-rule nonpacked-literal-struct ()
  (apply #'llvm-struct
	 (white-{}
	   comma-separated-types)))

(define-cg-llvm-rule packed-literal-struct ()
  (apply #'llvm-struct
	 `(:packed-p t ,@(white-<>
			  (white-{}
			    comma-separated-types)))))

(define-cg-llvm-rule literal-struct ()
  (|| nonpacked-literal-struct
      packed-literal-struct))

(define-cg-llvm-rule opaque-struct ()
  (v "opaque")
  (make-instance 'llvm-opaque-struct))

(define-cg-llvm-rule struct ()
  (|| opaque-struct
      literal-struct))

;;;;FIXME::what is the proper name?
(define-cg-llvm-rule struct-declaration ()
  (cap a (v llvm-identifier))
  (v whitespace)
  (v "=")
  (v whitespace)
  (v "type")
  (v whitespace)
  (cap b struct)
  (list (recap a)
	(recap b)))

(define-cg-llvm-rule nonpointer-type ()
  (|| (|| void-type
	  function-type
	  integer-type
	  float-type
	  x86-mmx
	  vector
	  label
	  metadata
	  array
	  literal-struct)
      (make-instance 'llvm-named-type
		     :name (v local-identifier))))

(define-cg-llvm-rule nonpointer-firstclass-type ()
  (|| integer-type float-type x86-mmx vector label metadata array literal-struct))

(define-cg-llvm-rule pointer-stars ()
  (times elt-pointer))

(define-cg-llvm-rule llvm-type ()
  (let ((under-type (v nonpointer-type))
	(stars (v pointer-stars)))
    (iter (for addrspace in stars)
	  (setf under-type (llvm-pointer under-type addrspace)))
    under-type))

(define-cg-llvm-rule llvm-firstclass-type ()
  (let ((under-type (v nonpointer-firstclass-type))
	(stars (v pointer-stars)))
    (iter (for addrspace in stars)
	  (setf under-type (llvm-pointer under-type addrspace)))
    under-type))


;; parsing of s-exp grammar
;;;;FIXME:: add 'named and llvm-named-type?

(defun parse-lisp-repr (expr)
  (match expr
    ('void (make-instance 'llvm-void-type))
    ((list 'function ret-type arg-types :vararg-p vararg-p)
     (make-instance 'llvm-function-type
		    :ret-type (parse-lisp-repr ret-type)
		    :param-types (mapcar #'parse-lisp-repr arg-types)
		    :vararg-p vararg-p))
    ((list 'integer arity) (llvm-integer arity))
    ((list 'float nbits mantissa) (make-instance 'llvm-float :nbits nbits :mantissa mantissa))
    ('x86-mmx (make-instance 'llvm-x86-mmx))
    ((list 'pointer pointee) (llvm-pointer (parse-lisp-repr pointee)))
    ((list 'pointer pointee addrspace) (llvm-pointer (parse-lisp-repr pointee) addrspace))
    ((list 'vector elt-type num-elts) (llvm-vector num-elts (parse-lisp-repr elt-type)))
    ('label (make-instance 'llvm-label))
    ('metadata (make-instance 'llvm-metadata))
    ((list 'array elt-type num-elts) (llvm-array num-elts (parse-lisp-repr elt-type)))
    ((list 'struct elt-types :packed-p packed-p)
     (apply #'llvm-struct (append (list :packed-p packed-p)
				  (mapcar #'parse-lisp-repr elt-types))))
    ((list 'named local-identifier)
     (make-instance 'llvm-named-type
		    :name local-identifier))
    ('opaque (make-instance 'llvm-opaque-struct))
    (otherwise (error "Do not know how to parse form ~a" expr))))

(defmethod emit-lisp-repr ((smth cons))
  (cons (emit-lisp-repr (car smth))
	(emit-lisp-repr (cdr smth))))
(defmethod emit-lisp-repr ((smth symbol))
  smth)
