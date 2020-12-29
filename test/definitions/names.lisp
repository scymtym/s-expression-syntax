;;;; names.lisp --- Tests for name-related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.names
  :in :syntax)

;;; Names

(test variable-name
  "Smoke test for the `variable-name' rule."

  (rule-test-cases ((syntax::variable-name syntax::names))
    '(:foo nil :foo nil)
    '(nil  nil nil nil)
    '(a    t   a   a)))

(test function-name
  "Smoke test for the `function-name' rule."

  (rule-test-cases ((syntax::function-name syntax::names))
    '(1           nil    1          nil)
    '(nil         nil    nil        nil)
    '(foo         t      foo        foo)

    '((setf 1)    :fatal nil        "second element of SETF function name must be a symbol")
    '((setf foo)  t      (setf foo) (setf foo))))

;;; References

(test function-reference
  "Smoke test for `function-reference' rule."

  (rule-test-cases ((syntax::function-reference syntax::names))
    '((function 1)           :fatal nil                   "must be a function name")
    '((function nil)         :fatal nil                   "must be a function name")
    '((function foo)         t      (function foo)        (function foo))

    '((function (setf 1))    :fatal nil                   "second element of SETF function name must be a symbol")
    '((function (setf foo))  t      (function (setf foo)) (function (setf foo)))))
