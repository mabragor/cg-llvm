(in-package :cg-llvm)

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

(define-cg-llvm-rule alphanumeric-word ()
  (text (postimes alphanumeric-char)))

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


;;;;FIXME:: WTF is going on with theses "wh" macros? chanaged progns to progn-v's
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
  `(? (progn-v (? whitespace) ,x)))

(define-cg-llvm-rule white-comma ()
  (progm (? whitespace)
	 (v #\,)
	 (? whitespace)))
