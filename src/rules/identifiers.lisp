
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(quasiquote-2.0:enable-quasiquote-2.0)

(define-cg-llvm-rule named-identifier-body ()
  (text (list (|| alpha-char #\- #\$ #\. #\_)
	      (times (|| alphanumeric-char #\- #\$ #\. #\_)))))

(define-cg-llvm-rule unnamed-identifier-body ()
  (text (list (postimes (character-ranges (#\0 #\9))))))

(defun try-destringify-symbol (str)
  (handler-case (destringify-symbol str)
    (error () str)))
  
(define-cg-llvm-rule hex-digit ()
  (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))

(define-cg-llvm-rule double-hex-escaped-char ()
  (v #\\)
  (code-char (parse-number:parse-number (text (times hex-digit :exactly 2))
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
  (try-destringify-symbol (text (list (v #\%)
				      (v identifier-body)))))
(define-cg-llvm-rule global-identifier ()
  (try-destringify-symbol (text (list (v #\@)
				      (v identifier-body)))))

(define-cg-llvm-rule llvm-identifier ()
  (|| local-identifier
      global-identifier))
