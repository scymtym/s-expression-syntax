;;;; bindings.lisp --- Tests for binding rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.bindings
  :in :syntax)

(test value-bindings
  "Smoke test for the `value-bindings' rule."

  (rule-test-cases ((syntax::value-bindings syntax::special-operators))
    '((1)       nil    nil nil)
    '(((1))     :fatal 1   "variable name must be a symbol")
    '(((a 1 2)) nil    nil nil)

    '(()        t      nil (()  ()))
    '(((a))     t      nil ((a) (nil)))
    '(((a 1))   t      nil ((a) (1)))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."

  (rule-test-cases ((syntax::symbol-macro-bindings syntax::special-operators))
    '((1)       nil    nil nil)
    '(((1))     :fatal 1   "variable name must be a symbol")
    '(((a))     nil    nil nil)
    '(((a 1 2)) nil    nil nil)

    '(()        t      nil (() ()))
    '(((a 1))   t      nil ((a) (1)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."

  (rule-test-cases ((syntax::function-bindings syntax::special-operators))
    '((1)           nil    nil nil)
    '(((1))         :fatal 1   "must be a function name")
    '(((a))         nil    nil nil)
    '(((a 1))       nil    nil nil)

    '(()            t      nil (()  ()))
    '(((a ()))      t      nil ((a) ((syntax::parsed-lambda (() () nil () nil) () () ()))))
    '(((a () 1))    t      nil ((a) ((syntax::parsed-lambda (() () nil () nil) () () (1)))))
    '(((a () "" 1)) t      nil ((a) ((syntax::parsed-lambda (() () nil () nil) "" () (1)))))))
