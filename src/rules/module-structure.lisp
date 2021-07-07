(in-package #:cg-llvm)

;;;;LLVM programs are composed of Module’s, each of which is a translation unit of the input programs.
;;;;Each module consists of functions, global variables, and symbol table entries. 

(define-cg-llvm-rule symbol-table-entry ()
  (|| target-datalayout
      target-triple
      inline-assembly
      attribute-group
      metadata-entry

      ;;FIXME:: does this go here?
      struct-declaration 
      (fail-parse "Not implemented")))

(define-cg-llvm-rule llvm-element ()
  (|| global-variable-definition
      function-declaration
      function-definition
      symbol-table-entry))

(define-plural-rule llvm-elements llvm-element (? whitespace))

;;;;In general, a module is made up of a list of global values
;;;;(where both functions and global variables are global values). 
(define-cg-llvm-rule llvm-module ()
  (progm (? whitespace)
	 `(module ,@(v llvm-elements))
	 (? whitespace)))
