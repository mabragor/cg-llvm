(in-package :cg-llvm)

(define-cg-llvm-rule alpha-char ()
  (character-ranges
   (#\a #\z)
   (#\A #\Z)))

(define-cg-llvm-rule alphanumeric-char ()
  (character-ranges
   (#\0 #\9)
   (#\a #\z)
   (#\A #\Z)))
