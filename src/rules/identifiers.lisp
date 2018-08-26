(in-package #:cg-llvm)

(define-cg-llvm-rule alphanumeric-word ()
  (text (postimes alphanumeric-char)))

(define-cg-llvm-rule ns-dec-digit ()
  (character-ranges (#\0 #\9)))

;;;;regex for name values ‘[%@][-a-zA-Z$._][-a-zA-Z$._0-9]*’
(define-cg-llvm-rule named-identifier-body ()
  (text (list (|| alpha-char #\- #\$ #\. #\_)
	      (times (|| alphanumeric-char #\- #\$ #\. #\_)))))

;;;;Unnamed values are represented as an unsigned numeric value with their prefix.
;;;;For example, %12, @2, %44.
(define-cg-llvm-rule unnamed-identifier-body ()
  (text (list (postimes (character-ranges (#\0 #\9))))))

(define-cg-llvm-rule hex-digit ()
  (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))

(define-cg-llvm-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return
		llvm-comment)))

(define-cg-llvm-rule llvm-comment ()
  ;; we just totally ignore comments, replacing them with single space (C-style)
  (v #\;)
  (times (!! (|| #\newline #\return)))
  #\space)

(define-cg-llvm-rule double-hex-escaped-char ()
  (v #\\)
  (code-char (parse-integer (text (times hex-digit :exactly 2))
			    :radix 16)))

(define-cg-llvm-rule llvm-string ()
  (text (progm #\"
	       (times (progn (! #\")
			     (|| double-hex-escaped-char
				 (descend-with-rule 'character nil))))
	       #\")))

(define-cg-llvm-rule identifier-body ()
  (|| named-identifier-body
      llvm-string
      unnamed-identifier-body))

(define-cg-llvm-rule local-identifier ()
  (text (list (v #\%)
	      (v identifier-body))))
(define-cg-llvm-rule global-identifier ()
  (text (list (v #\@)
	      (v identifier-body))))

(define-cg-llvm-rule llvm-identifier ()
  (|| local-identifier
      global-identifier))

;;;;FIXME:: destringify-symbol does not do anything
(defun try-destringify-symbol (str)
  (handler-case (destringify-symbol str)
    (error () str)))
