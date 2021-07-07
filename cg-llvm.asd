(asdf:defsystem #:cg-llvm
  :description "Parse and generate LLVM IR, without using fantastic C++ API, purely in CL."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on
  (#:cl-interpol
   #:iterate
   #:esrap-liquid
   #:optima
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
    ((:file "simple")
     (:file "types")
     (:file "fun-defs")
     (:file "identifiers")
     (:file "constants")
     (:file "metadata")
     (:file "instructions")
     (:file "constant-expressions")
     (:file "module-structure")
     ))))
