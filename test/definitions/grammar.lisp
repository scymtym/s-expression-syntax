(cl:in-package #:syntax.test)

(def-suite* :syntax.grammar
  :in :syntax)

;;; Bindings

(test value-bindings
  "Smoke test for the `value-bindings' rule."

  (is-true (parser.packrat:parse '(syntax::value-bindings) '()))
  (is-true (parser.packrat:parse '(syntax::value-bindings) '(a)))
  (is-true (parser.packrat:parse '(syntax::value-bindings) '((a))))
  (is-true (parser.packrat:parse '(syntax::value-bindings) '((a 1)))))

;;; Types

(test type-specifier ()
  "Smoke test for the `type-specifier' rule."

  (is-false (parser.packrat:parse '(syntax::type-specifier) 5))
  (is-true (parser.packrat:parse '(syntax::type-specifier) 'boolean))
  (is-true (parser.packrat:parse '(syntax::type-specifier) '(unsigned-byte 32))))
