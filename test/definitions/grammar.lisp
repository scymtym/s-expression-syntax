(cl:in-package #:syntax.test)

(in-suite* :syntax.grammar
  :in :syntax)

;;; Basic rules

(test variable-name
  "Smoke test for the `variable-name' rule."

  (is-false (parser.packrat:parse '(syntax::variable-name) :foo)))

(test function-name
  "Smoke test for the `function-name' rule."

  (is-false (parser.packrat:parse '(syntax::function-name) 1))
  (is-true (parser.packrat:parse '(syntax::function-name) 'foo))
  (is-true (parser.packrat:parse '(syntax::function-name) '(setf foo))))

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
