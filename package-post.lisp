
(in-package #:cl-user)

(defmacro proxy-package ((from-package to-package) &body symbols)
  `(defpackage ,from-package
     (:shadowing-import-from ,to-package ,@symbols)
     (:export ,@symbols)))

(proxy-package (#:cg-llvm-symbols #:cg-llvm)
  ;; terminator instructions
  #:ret #:br #:switch #:indirectbr #:invoke #:resume #:unreachable
  ;; #:catchswitch #:catchret #:cleanupret 
  ;; binary operations
  #:add #:fadd #:sub #:fsub #:mul #:fmul #:udiv #:sdiv #:fdiv #:urem #:srem #:frem
  ;; bitwise binary operations
  #:shl #:lshr #:ashr #:and #:or #:xor
  ;; vector operations
  #:extractelement #:insertelement #:shufflevector
  ;; aggregate operations
  #:extractvalue #:insertvalue
  ;; memory access and addressing operations
  #:alloca #:load #:store #:fence #:cmpxchg #:atomicrmw #:getelementptr
  ;; conversion operations
  #:trunc #:zext #:sext #:fptrunc #:fpext #:fptoui #:fptosi #:sitofp
  #:ptrtoint #:inttoptr #:bitcast #:addrspacecast
  ;; other operations
  #:icmp #:fcmp #:phi #:select #:call #:va_arg #:landingpad
  ;; #:catchpad #:cleanuppad
  ;; other symbols
  #:meta-str #:meta-node #:meta-id
  #:void #:x86-mmx #:nbits #:pointer #:struct #:opaque #:named
  #:alias #:comdat #:asm #:target-triple
  #:attributes #:target-datalayout  #:label #:blockaddress #:module
  #:define #:declare)


