(in-package :cl-user)

(defpackage :cg-llvm-tests
  (:use :alexandria :cl :cg-llvm :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cg-llvm-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(def-suite cg-llvm)
(in-suite cg-llvm)

(defun run-tests ()
  (let ((results (run 'cg-llvm)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (equal 24 (slot-value (cg-llvm-parse 'integer-type "i24") 'cg-llvm::nbits))))
