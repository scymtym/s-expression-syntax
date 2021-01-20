;;;; names.lisp --- Tests for name-related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.names
  :in :s-expression-syntax)

;;; Names

(test variable-name
  "Smoke test for the `variable-name' rule."

  (rule-test-cases ((syn::variable-name syn::names))
    '(1    nil 1    nil)
    '(:foo nil :foo nil)
    '(nil  nil nil  nil)
    '(t    nil t    nil)
    '(pi   nil pi   nil)
    '(a    t   a    a)))

(test variable-name!
  "Smoke test for the `variable-name!' rule."

  (rule-test-cases ((syn::variable-name! syn::names))
    '(1    :fatal 1    "variable name must be a symbol")
    '(:foo :fatal :foo "variable name must not be a keyword")
    '(nil  :fatal nil  "variable name must not designate a constant")
    '(t    :fatal t    "variable name must not designate a constant")
    '(pi   :fatal pi   "variable name must not designate a constant")
    '(a    t      a    a)))

(test function-name
  "Smoke test for the `function-name' rule."

  (rule-test-cases ((syn::function-name syn::names))
    '(1           nil    1          nil)
    '(nil         nil    nil        nil)
    '(foo         t      foo        foo)

    '((setf 1)    :fatal nil        "second element of SETF function name must be a symbol")
    '((setf foo)  t      (setf foo) (setf foo))))

;;; References

(test function-reference
  "Smoke test for `function-reference' rule."

  (rule-test-cases ((syn::function-reference syn::names))
    '((function 1)           :fatal nil                   "must be a function name")
    '((function nil)         :fatal nil                   "must be a function name")
    '((function foo)         t      (function foo)        (function foo))

    '((function (setf 1))    :fatal nil                   "second element of SETF function name must be a symbol")
    '((function (setf foo))  t      (function (setf foo)) (function (setf foo)))))
