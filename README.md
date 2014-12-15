CG-LLVM
-------

Generate LLVM IR, written purely in Common Lisp.

Type system
===========

To make writing of LLVM operations (mainly, binary) more convenient,
it is desirable to teach computer to understand LLVM type system
(and infer types of arguments, such that not all of them need to be specified explicitly)

For that, the following rough list of tasks should be done:
* (done, tested) parsing of llvm type grammar
* (done) emitting s-exp llvm type grammar from lisp-side types
* (done, tested) emitting text llvm type grammar from lisp-side types
* (should I really do this?) compile-time parsing of lisp-style llvm grammar
* (done, tested) runtime parsing of s-exp llvm type grammar
* (?) type-implication for llvm operations

