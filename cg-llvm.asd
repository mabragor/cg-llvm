;;;; cg-llvm.asd

(asdf:defsystem #:cg-llvm
  :serial t
  :description "Generate LLVM IR, without using fantastic C++ API."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-trivial-templates #:cl-ppcre #:cl-interpol #:iterate #:cg-common-ground)
  :pathname "src/"
  :components ((:file "package")
	       (:file "basics")))

