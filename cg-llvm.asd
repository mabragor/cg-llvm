;;;; cg-llvm.asd

(asdf:defsystem #:cg-llvm
  :description "Parse and generate LLVM IR, without using fantastic C++ API, purely in CL."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-trivial-templates #:cl-ppcre #:cl-interpol #:iterate #:cg-common-ground #:lol-re
				      #:esrap-liquid #:optima #:defmacro-enhance
				      #:quasiquote-2.0 #:parse-number

				      ;;;added by terminal625
				      #:alexandria)
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "types-macros")
	       (:module
		"rules"
		:serial t
		:components
		(
		 (:file "types")
		 (:file "fun-defs")
		 (:file "identifiers")
		 (:file "constants")
		 ;(:file "metadata")
		 ;(:file "instructions")
		 ;(:file "constant-expressions")
		 ;(:file "high-level-structure")
		 )
		)
	       
	       ;; (:file "basics")
	       ))



(defsystem :cg-llvm-tests
  :description "Tests for CG-LLVM."
  :licence "MIT"
  :depends-on (:cg-llvm :fiveam :cl-interpol)
  :serial t
  :components ((:file "package-post")
	       (:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cg-llvm))))
  (load-system :cg-llvm)
  (funcall (intern "RUN-TESTS" :cg-llvm)))
