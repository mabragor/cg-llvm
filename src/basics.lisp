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

;;; Terminating instructions
(defun %llvm-return (type &optional value)
  (cond ((eq type :void)
	 (assert (not value))
	 "ret void")
	(t (assert value)
	   #?"ret $(type) $(value)")))

(defun llvm-return (value)
  (apply #'%llvm-return (reverse (ensure-cons value))))

(defun %print-typevalue (typevalue)
  "Typevalue is supposed to be the LIST of the form (VALUE TYPE). VALUE and TYPE are supposed to be atoms."
  (assert (equal 2 (length typevalue)))
  #?"$((cadr typevalue) (car typevalue))")

(defun unconditional-branch (label)
  #?"br label $(label)")
(defun conditional-branch (test then else)
  #?"br i1 $(test), label $(then), label $(else)")

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
	
(defun %print-label (label)
  #?"label $(label)")

(defun switch (value default-dest &rest branch-specs)
  (frnl "switch ~a, ~a [~a]"
	(%print-typevalue value)
	(%print-label default-dest)
	(joinl " " (mapcar (lambda (x)
			     #?"$((%print-typevalue (car x))), $((%print-label (cadr x)))")
			   (pairs branch-specs)))))


(defun indirect-branch (typevalue &rest labels)
  (frnl "indirectbr ~a, [ ~a ]"
	(progn (assert (equal 2 (length typevalue)))
	       #?"$((cadr typevalue))* $((car typevalue))")
	(joinl ", " (mapcar #'%print-label labels))))

(defun invoke (fun-type fun-val fun-args normal-label unwind-label
	       &key call-conv return-attrs fun-attrs)
  (joinl " " (remove-if-not #'identity
			    (list "invoke"
				  call-conv
				  return-attrs
				  fun-type
				  (frnl "~a(~a)" fun-val (joinl ", " fun-args))
				  fun-attrs
				  "to" (%print-label normal-label)
				  "unwind" (%print-label unwind-label)))))
				  
(defun resume (typevalue)
  #?"resume $((%print-typevalue typevalue))")

(defun unreachable ()
  "unreachable")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trim-llvm (str)
    (if (m~ "^llvm_(.*)" str)
	$1
	str)))

(defmacro define-no-wrap-binop (name)
  `(defun ,name (type op1 op2 &key no-signed-wrap no-unsigned-wrap)
     (joinl " " (remove-if-not #'identity
			       (list ,(trim-llvm (underscorize name))
				     (if no-signed-wrap "nsw")
				     (if no-unsigned-wrap "nuw")
				     type op1 op2)))))


;; OK, I probably want to reuse the results returned by these binary operations
;; in C++ library functions just simply return this result, outputting the result of
;; code-generation as a side-effect somehow.

;; So, what is the really smart and concise way to do this?

(defmacro define-float-binop (name)
  `(defun ,name (type op1 op2 &rest flags)
     (joinl " " (remove-if-not #'identity
			       `(,(trim-llvm (underscorize name)) ,@flags ,type ,op1 ,op2)))))

(defmacro define-exactable-binop (name)
  `(defun ,name (type op1 op2 &key exact)
     (joinl " " (remove-if-not #'identity
			       `(,(trim-llvm (underscorize name))
				  ,(if exact "exact")
				  ,type ,op1 ,op2)))))

(defmacro define-simple-binop (name)
  `(defun ,name (type op1 op2)
     (joinl " " (remove-if-not #'identity
			       `(,(trim-llvm (underscorize name)) ,type ,op1 ,op2)))))


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
(define-exactable-binop shr)
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


