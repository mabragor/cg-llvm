(in-package #:cg-llvm)

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
      symbol-table-entry
      struct-declaration))

(define-plural-rule llvm-elements llvm-element (? whitespace))

(define-cg-llvm-rule llvm-module ()
  (progm (? whitespace)
	 `(module ,@(v llvm-elements))
	 (? whitespace)))
