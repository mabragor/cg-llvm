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
