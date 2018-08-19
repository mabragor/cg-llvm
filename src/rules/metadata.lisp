
(in-package #:cg-llvm)

(cl-interpol:enable-interpol-syntax)
;;;(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)


(define-cg-llvm-rule metadata-node-value (type)
  (if (and type (not (llvm-typep 'metadata type)))
      (fail-parse "METADATA value should have METADATA type"))
  (fail-parse "METADATA-NODE-VALUE is not implemented yet"))


(define-cg-llvm-rule metadata-string ()
  (v #\!)
  `(meta-str ,(v llvm-string)))

(define-cg-llvm-rule metadata-identifier ()
  (v #\!)
  `(meta-id
    ,(let ((it (v identifier-body)))
	  (handler-case (parse-integer it)
	    (error () (try-destringify-symbol it))))))

(define-cg-llvm-rule metadata-node ()
  (v #\!)
  (v #\{)
  (v wh?)
  (cap a metadata-node-operands)
  (v #\})
  `(meta-node ,@(recap a)))

(define-cg-llvm-rule specialized-metadata ()
  (fail-parse "Specialized metadata not implemented yet"))

(defmacro define-specialized-metadata (name &body field-specs)
  (declare (ignore name field-specs))
  ;; TODO : actually implement the thing
  nil)

(define-specialized-metadata m-d-compile-unit
    language file producer is-optimized flags
    runtime-version split-debug-filename emission-kind
    enums retained-types subprograms globals imports)

(define-specialized-metadata m-d-file
    filename directory)

(define-specialized-metadata m-d-basic-type
    name size align encoding tag)

(define-specialized-metadata m-d-subroutine-type
    types)

(define-specialized-metadata m-d-derived-type
    tag base-type size align offset)

(define-specialized-metadata m-d-composite-type
    tag name file line size align identifier elements)

(define-specialized-metadata m-d-subrange
    count lower-bound)

(define-specialized-metadata m-d-enumerator
    name value)

(define-specialized-metadata m-d-template-type-parameter
    name type)

(define-specialized-metadata m-d-template-value-parameter
    tag name type value)

(define-specialized-metadata m-d-namespace
    name scope file line)

(define-specialized-metadata m-d-global-variable
    name linkage-name scope file line type
    is-local is-definition variable declaration)

(define-specialized-metadata m-d-subprogram
    name linkage-name scope file line type is-local
    is-definition scope-line containing-type virtuality
    virtual-index flags is-optimized function template-params
    declaration variables)

(define-specialized-metadata m-d-lexical-block
    scope file line column)

(define-specialized-metadata m-d-lexical-block-file
    scope file line column discriminator)

(define-specialized-metadata m-d-location
    line column scope inlined-at)

(define-specialized-metadata m-d-local-variable
    tag name arg scope file line type flags)

;; The syntax of this one is really different from the rest ...
;; (define-specialized-metadata m-d-expression)
    
(define-specialized-metadata m-d-obj-c-property
    name file line setter getter attributes type)

(define-specialized-metadata m-d-imported-entity
    tag name scope entity line)

(define-cg-llvm-rule metadata-constant ()
  (|| metadata-string
      metadata-identifier
      metadata-node
      specialized-metadata))

(define-cg-llvm-rule metadata-funcall-arg ()
  (v "metadata")
  (v whitespace)
  (v metadata-constant))

(define-plural-rule metadata-node-operands metadata-node-operand white-comma)

(define-cg-llvm-rule metadata-node-operand ()
  ;; TODO : maybe this will turn out to be not exactly correct
  ;; but this is the best I can get for now from LLVM language reference manual.
  (v llvm-constant))

(define-cg-llvm-rule metadata-entry ()
  (let* ((id (v metadata-identifier)))
    (v wh?)
    (v #\=)
    (let* ((distinct (?wh? (progn (v "distinct")
				  t)))
	   (node (progn (v wh?)
			(v metadata-node))))
      `(= ,id ,node
	  ,!m(inject-kwd-if-nonnil distinct)))))
