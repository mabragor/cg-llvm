
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

(define-cg-llvm-rule symbol-table-entry ()
  (|| target-datalayout
      target-triple
      inline-assembly
      attribute-group
      metadata-entry
      (fail-parse "Not implemented")))

(define-cg-llvm-rule llvm-element ()
  (|| global-variable-definition
      function-declaration
      function-definition
      symbol-table-entry))

(define-plural-rule llvm-elements llvm-element (? whitespace))

(define-cg-llvm-rule llvm-module ()
  (? whitespace) `(module ,@llvm-elements))
