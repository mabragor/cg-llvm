;;;; types.lisp
;;;; This file is part of CG-LLVM system,
;;;; see COPYING for details

(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(defgeneric emit-lisp-repr (obj)
  (:documentation "Emit cons-style representation of the object"))

(defclass llvm-type ()
  ())
(defclass llvm-void-type (llvm-type) ())
(defclass llvm-function-type (llvm-type)
  ((ret-type :initarg :ret-type)
   (param-types :initarg :param-types)
   (vararg-p :initarg :vararg-p)))

(defmethod emit-lisp-repr ((obj llvm-void-type))
  'void)

(defmethod emit-lisp-repr ((obj llvm-function-type))
  (with-slots (ret-type param-types vararg-p) obj
    `(function ,(emit-lisp-repr ret-type) ,(mapcar #'emit-lisp-repr param-types)
	       :vararg-p ,vararg-p)))

(defclass llvm-first-class-type (llvm-type) ())

(defclass llvm-integer (llvm-first-class-type)
  ((nbits :initarg :nbits)))
(defvar max-nbits (1- (expt 2 23)))
(defun llvm-integer (nbits)
  (if (or (not (integerp nbits))
	  (< nbits 1)
	  (> nbits max-nbits))
      (error "NBITS argument should be positive integer not greater than ~a" max-nbits))
  (make-instance 'llvm-integer :nbits nbits))

(defmethod emit-lisp-repr ((obj llvm-integer))
  `(integer ,(slot-value obj 'nbits)))

(defclass llvm-float (llvm-first-class-type)
  ((nbits :initarg :nbits)
   (mantissa :initarg :mantissa :initform 0)))
(defun llvm-float (sym)
  (let ((str (underscorize sym)))
    (flet ((frob (nbits &optional (mantissa (/ nbits 2)))
	     (make-instance 'llvm-float :nbits nbits :mantissa mantissa)))
      (cond ((string= "half" str) (frob 16))
	    ((string= "float" str) (frob 32))
	    ((string= "double" str) (frob 64))
	    ((string= "fp128" str) (frob 128 112))
	    ((string= "x86_fp80" str) (frob 80))
	    ((string= "ppc_fp128" str) (frob 128))
	    (t (error "Unknown floating point specifier: ~a" sym))))))

(defmethod emit-lisp-repr ((obj llvm-float))
  `(float ,(slot-value obj 'nbits) ,(slot-value obj 'mantissa)))

(defclass llvm-x86-mmx (llvm-first-class-type) ())

(defmethod emit-lisp-repr ((obj llvm-x86-mmx))
  'x86-mmx)

(defclass llvm-pointer (llvm-first-class-type)
  ((pointee :initarg :pointee)
   (address-space :initform 0 :initarg :address-space)))

(defmethod emit-lisp-repr ((obj llvm-pointer))
  (with-slots (pointee address-space) obj
    `(pointer ,(emit-lisp-repr pointee)
	      ,@(if (not (equal 0 address-space))
		    `(,address-space)))))

(defun coerce-to-llvm-type (x)
  (cond ((typep x 'llvm-type) x)
	(t (error "Don't know how to coerce ~a to LLVM type" x))))

(defun llvm-pointer (pointee &optional addrspace)
  (let ((pointee (coerce-to-llvm-type pointee)))
    (cond ((typep pointee 'llvm-void-type)
	   (progn (warn "When declaring pointer type, VOID-typed pointee encountered, coercing to i8")
		  (setf pointee (llvm-integer 8))))
	  ((typep pointee 'llvm-label)
	   (progn (warn "When declaring pointer type, LABEL-typed pointee encountered, coercing to i8")
		  (setf pointee (llvm-integer 8)))))
    (make-instance 'llvm-pointer :pointee pointee
		   :address-space (or addrspace 0))))

(defclass llvm-vector (llvm-first-class-type)
  ((num-elts :initarg :num-elts)
   (elt-type :initarg :elt-type)))
(defun llvm-vector (n type)
  (if (or (not (integerp n))
	  (<= n 0))
      (error "Number of elements N should be integer greater than zero"))
  (let ((type (coerce-to-llvm-type type)))
    (if (not (or (typep type 'llvm-integer)
		 (typep type 'llvm-float)
		 (typep type 'llvm-pointer)))
	(error "Element type of vector should be integer, float or pointer, but got/deduced ~a" type))
    (make-instance 'llvm-vector :num-elts n :elt-type type)))

(defmethod emit-lisp-repr ((obj llvm-vector))
  (with-slots (num-elts elt-type) obj
    `(vector ,(emit-lisp-repr elt-type) ,num-elts)))

(defclass llvm-label (llvm-first-class-type) ())
(defclass llvm-metadata (llvm-first-class-type) ())

(defmethod emit-lisp-repr ((obj llvm-label))
  'label)
(defmethod emit-lisp-repr ((obj llvm-metadata))
  'metadata)

(defclass llvm-aggregate-type (llvm-first-class-type) ())

(defclass llvm-array (llvm-aggregate-type)
  ((num-elts :initarg :num-elts)
   (elt-type :initarg :elt-type)))

(defmethod emit-lisp-repr ((obj llvm-array))
  (with-slots (num-elts elt-type) obj
    `(array ,(emit-lisp-repr elt-type) ,num-elts)))

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
  
(defclass llvm-struct (llvm-aggregate-type)
  ((elt-types :initarg :elt-types)
   (packed-p :initarg :packed-p)))

(defmethod emit-lisp-repr ((obj llvm-struct))
  (with-slots (elt-types packed-p) obj
    `(struct ,(mapcar #'emit-lisp-repr elt-types)
	     :packed-p ,packed-p)))

(defclass llvm-opaque-struct () ())

(defmethod emit-lisp-repr ((obj llvm-opaque-struct))
  'opaque)

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
			       (collect type)))))))


;;; parsing
(define-cg-llvm-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return)))

(define-cg-llvm-rule ns-dec-digit ()
  (character-ranges (#\0 #\9)))

(define-cg-llvm-rule integer-type ()
  #\i (llvm-integer (parse-integer (text (postimes ns-dec-digit)))))

(define-cg-llvm-rule void-type ()
  "void"
  (make-instance 'llvm-void-type))

(defun parse-function-argtypes (lst)
  (if (eq '*** (car (last lst)))
      (values (butlast lst) t)
      (values lst nil)))

(define-cg-llvm-rule function-argtype ()
  (|| llvm-type
      (progn "..." '***)))

(define-cg-llvm-rule function-argtypes ()
  (cons function-argtype
	(times (progn (? whitespace) #\, (? whitespace)
		      function-argtype))))

(define-cg-llvm-rule function-type ()
  (let ((ret-type (|| void-type
		      llvm-firstclass-type)))
    (if (or (typep ret-type 'llvm-label)
	    (typep ret-type 'llvm-metadata))
	(fail-parse "Got label or metadata type as function return type"))
    (? whitespace) "(" c!-1-function-argtypes ")"
    (multiple-value-bind (param-types vararg-p) (parse-function-argtypes c!-1)
      (make-instance 'llvm-function-type
		     :ret-type ret-type
		     :param-types param-types
		     :vararg-p vararg-p))))

(define-cg-llvm-rule float-type ()
  (llvm-float (cond-parse ("double" 'double)
			  ("half" 'half)
			  ("float" 'float)
			  ("fp128" 'fp128)
			  ("x86_fp80" 'x86-fp80)
			  ("ppc_fp128" 'ppc-fp128))))

(define-cg-llvm-rule x86-mmx ()
  "x86_mmx"
  (make-instance 'llvm-x86-mmx))

(define-cg-llvm-rule addr-space ()
  (|| (progn "addrspace(" (? whitespace) c!-1-simple-int (? whitespace) ")" c!-1)
      0))

(define-cg-llvm-rule elt-pointer ()
  (? whitespace) c!-1-addr-space (? whitespace) "*" (? whitespace)
  c!-1)

;; TODO: smart pointer parsing

(define-cg-llvm-rule simple-int ()
  (parse-integer (text (postimes ns-dec-digit))))

(define-cg-llvm-rule vector ()
  #\< (? whitespace) c!-nelts-simple-int (? whitespace) #\x (? whitespace) c!-type-llvm-type (? whitespace) #\>
  (llvm-vector c!-nelts c!-type))

(define-cg-llvm-rule label ()
  "label" (make-instance 'llvm-label))
(define-cg-llvm-rule metadata ()
  "metadata" (make-instance 'llvm-metadata))

(define-cg-llvm-rule array ()
  #\[ (? whitespace) c!-nelts-simple-int (? whitespace) #\x (? whitespace) c!-type-llvm-type (? whitespace) #\]
  (llvm-array c!-nelts c!-type))


(define-cg-llvm-rule comma-separated-types ()
  c!-type-llvm-type (? whitespace) (? (progn #\, (? whitespace) c!-rectypes-comma-separated-types))
  (cons c!-type c!-rectypes))


(define-cg-llvm-rule nonpacked-literal-struct ()
  #\{ (? whitespace) c!-1-comma-separated-types (? whitespace) #\}
  (apply #'llvm-struct c!-1))

(define-cg-llvm-rule packed-literal-struct ()
  "<{" (? whitespace) c!-1-comma-separated-types (? whitespace) "}>"
  (apply #'llvm-struct `(:packed-p t ,@c!-1)))

(define-cg-llvm-rule literal-struct ()
  (|| nonpacked-literal-struct
      packed-literal-struct))

(define-cg-llvm-rule opaque-struct ()
  "opaque"
  (make-instance 'llvm-opaque-struct))

(define-cg-llvm-rule struct ()
  (|| opaque-struct
      literal-struct))

(define-cg-llvm-rule struct-declaration ()
  c!-1-llvm-ident whitespace "=" whitespace "type" whitespace c!-2-struct
  (list c!-1 c!-2))

(define-cg-llvm-rule nonpointer-type ()
  (|| void-type function-type integer-type float-type x86-mmx vector label metadata array literal-struct))

(define-cg-llvm-rule nonpointer-firstclass-type ()
  (|| integer-type float-type x86-mmx vector label metadata array literal-struct))

(define-cg-llvm-rule pointer-stars ()
  (times elt-pointer))

(define-cg-llvm-rule llvm-type ()
  (let ((under-type nonpointer-type)
	(stars pointer-stars))
    (iter (for addrspace in stars)
	  (setf under-type (llvm-pointer under-type addrspace)))
    under-type))

(define-cg-llvm-rule llvm-firstclass-type ()
  (let ((under-type nonpointer-firstclass-type)
	(stars pointer-stars))
    (iter (for addrspace in stars)
	  (setf under-type (llvm-pointer under-type addrspace)))
    under-type))


;; parsing of s-exp grammar

(defun parse-lisp-repr (expr)
  (match expr
    ('void (make-instance 'llvm-void-type))
    ;; ((cons 'function body) (parse-function-repr body))
    ((list 'integer arity) (llvm-integer arity))
    ((list 'float nbits mantissa) (make-instance 'llvm-float :nbits nbits :mantissa mantissa))
    (otherwise (error "Do not know how to parse form ~a" expr))))


(defun test-frob (expr)
  (match expr
    ('foo 1)
    (otherwise 'otherwise)))

