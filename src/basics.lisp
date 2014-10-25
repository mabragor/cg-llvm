;;;; cg-llvm.lisp

(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)

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
	

(defun switch (value default-dest &rest branch-specs)
  (frnl "switch ~a, label ~a [~a]"
	(%print-typevalue value)
	default-dest
	(joinl " " (mapcar (lambda (x)
			     #?"$((%print-typevalue (car x))), label $((cadr x))")
			   (pairs branch-specs)))))

