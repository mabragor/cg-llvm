(in-package :cg-llvm)

(quasiquote-2.0:enable-quasiquote-2.0)

(defmacro define-simple-instruction-rule (name (&rest args) &body body)
  `(define-instruction-rule ,name
     (,@(if args
	    `(let* (,(if (symbolp (car args))
			 `(,(car args) (wh instr-arg))
			 `(,(caar args) (fail-parse-if-not ,(cadar args) (wh instr-arg))))
		    ,@(mapcar (lambda (x)
				(if (symbolp x)
				    `(,x (progn (v white-comma)
						(v instr-arg)))
				    `(,(car x) (fail-parse-if-not ,(cadr x)
								  (progn (v white-comma)
									 (v instr-arg))))))
			      (cdr args))))
	    `(progn))
	,@(or body
	      `(`(,,@(mapcar (lambda (x)
			       ``(quasiquote-2.0:inject ,(if (symbolp x) x (car x))))
			     args)))))))
