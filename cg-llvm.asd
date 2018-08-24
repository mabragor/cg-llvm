;;;; cg-llvm.asd

(asdf:defsystem #:cg-llvm
  :description "Parse and generate LLVM IR, without using fantastic C++ API, purely in CL."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on
  (#:cl-trivial-templates
   #:cl-interpol
   #:iterate
   #:cg-common-ground
   #:esrap-liquid
   #:optima
   #:quasiquote-2.0
   #:parse-number
   )
  :pathname "src/"
  :serial t
  :components
  ((:file "package")
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
     (:file "metadata")
     (:file "quasiquote-2.0")
     (:file "instructions")
     (:file "constant-expressions")
     (:file "high-level-structure")
     ))))

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
