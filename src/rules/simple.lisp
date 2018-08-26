(in-package :cg-llvm)

;;;;individual characters

(define-cg-llvm-rule alpha-char ()
  (character-ranges
   (#\a #\z)
   (#\A #\Z)))

(define-cg-llvm-rule ns-dec-digit ()
  (character-ranges (#\0 #\9)))

(define-cg-llvm-rule alphanumeric-char ()
  (character-ranges
   (#\0 #\9)
   (#\a #\z)
   (#\A #\Z)))

(define-cg-llvm-rule hex-digit ()
  (character-ranges
   (#\0 #\9)
   (#\a #\f)
   (#\A #\F)))

(define-cg-llvm-rule whitespace ()
  (postimes
   (||
    #\space
    #\tab
    #\newline
    #\return
    llvm-comment)))

(define-cg-llvm-rule llvm-comment ()
  ;; we just totally ignore comments, replacing them with single space (C-style)
  (v #\;)
  (times (!! (|| #\newline #\return)))
  #\space)

(define-cg-llvm-rule alphanumeric-word ()
  (text (postimes alphanumeric-char)))

;;;;WHITESPACE

(progn
  (defmacro wh (x)
    `(progn-v whitespace ,x))
  (defmacro wh? (x)
    `(progn-v (? whitespace) ,x))
  (define-cg-llvm-rule wh? ()
    (? whitespace)
    nil)
  (defmacro ?wh (x)
    `(? (progn-v whitespace ,x)))
  (defmacro ?wh? (x)
    `(? (progn-v (? whitespace) ,x))))

(defmacro with-whitespace (&body body)
  `(progm (? whitespace)
	  (progn-v ,@body)
	  (? whitespace)))

(define-cg-llvm-rule white-comma ()
  (with-whitespace #\,))

;;;;Numbers
(progn
  (define-cg-llvm-rule sign ()
    (|| "+" "-"))
  (define-cg-llvm-rule decimal-float ()
    (let ((text (text (? sign)
		      (times ns-dec-digit)
		      (v #\.)
		      (times ns-dec-digit)
		      (? (list (v #\e)
			       (? sign)
			       (postimes ns-dec-digit))))))
      (handler-case 
	  (parse-number:parse-number text)
	(error () `(%decimal-float ,text)))))
  ;; TODO : hexadecimal float ???
  (define-cg-llvm-rule hexadecimal-float ()
    (let ((text (text (? sign)
		      (v #\0)
		      (v #\x)
		      (times hex-digit))))
      (handler-case 
	  (parse-integer text :radix 16)
	(error () `(%hexadecimal-float ,text))))
    #+nil
    (fail-parse "Hexadecimal float is not implemented yet."))
  (define-cg-llvm-rule llvm-float ()
    (|| decimal-float
	hexadecimal-float)))

(defmacro white-paren (&body body)
  `(progn (v "(")
	  (? whitespace)
	  (prog1-v (progn-v ,@body)
		   (? whitespace)
		   (v ")"))))

(defmacro white-[] (&body body)
  `(progn (v "[")
	  (? whitespace)
	  (prog1-v (progn-v ,@body)
		   (? whitespace)
		   (v "]"))))

(defmacro white-<> (&body body)
  `(progn (v "<")
	  (? whitespace)
	  (prog1-v (progn-v ,@body)
		   (? whitespace)
		   (v ">"))))

(defmacro white-{} (&body body)
  `(progn (v "{")
	  (? whitespace)
	  (prog1-v (progn-v ,@body)
		   (? whitespace)
		   (v "}"))))
