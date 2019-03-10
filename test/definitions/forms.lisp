(cl:in-package #:syntax.test)

(in-suite :syntax)

(test body
  "Smoke test for the `body' rule."

  (rule-test-cases ((syntax::body syntax::special-operators))
    '(()                        t nil (()    ()))

    '((1)                       t nil (()    (1)))
    '((1 2)                     t nil (()    (1 2)))

    '(((declare 1))             t nil ((1)   ()))
    '(((declare 1) (declare 2)) t nil ((1 2) ()))

    '(((declare 1 2) 3 4)       t nil ((1 2) (3 4)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."

  (rule-test-cases ((syntax::docstring-body syntax::special-operators))
    '(()                  t nil (nil   () ()))

    '(("foo")             t nil (nil   () ("foo")))

    '(((declare 1))       t nil (nil   (1) ()))
    '(("foo" (declare 1)) t nil ("foo" (1) ()))
    '(((declare 1) "foo") t nil (nil   (1) ("foo")))

    '((1)                 t nil (nil   ()  (1)))
    '(("foo" 1)           t nil ("foo" ()  (1)))
    '((1 "foo")           t nil (nil   ()  (1 "foo")))))
