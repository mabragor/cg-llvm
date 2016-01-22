;;;; cg-llvm.asd

(asdf:defsystem #:cg-llvm
  :description "Generate LLVM IR, without using fantastic C++ API."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-trivial-templates #:cl-ppcre #:cl-interpol #:iterate #:cg-common-ground #:lol-re
				      #:esrap-liquid #:optima #:defmacro-enhance
				      #:quasiquote-2.0 #:parse-number)
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "types-macros")
	       (:file "types")
	       (:file "fun-defs")
	       (:file "identifiers")
	       (:file "constants")
	       (:file "metadata")
	       (:file "instructions")
	       (:file "constant-expressions")
	       (:file "high-level-structure")
	       ;; (:file "basics")
	       ))



(defsystem :cg-llvm-tests
  :description "Tests for CG-LLVM."
  :licence "GPL"
  :depends-on (:cg-llvm :fiveam :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cg-llvm))))
  (load-system :cg-llvm)
  (funcall (intern "RUN-TESTS" :cg-llvm)))
