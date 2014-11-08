CG-LLVM
-------

Generate LLVM IR, written purely in Common Lisp.

Type system
===========

To make writing of LLVM operations (mainly, binary) more convenient,
it is desirable to teach computer to understand LLVM type system
(and infer types of arguments, such that not all of them need to be specified explicitly)

For that, the following rough list of tasks should be done:
* (done, untested) parsing of llvm type grammar
* (done) emitting llvm type grammar from lisp-side types
* compile-time parsing of lisp-style llvm grammar
* runtime parsing of lisp-style llvm grammar
* (?) type-implication for llvm operations

