;;;; bindings.lisp --- Tests for binding rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.bindings
  :in :s-expression-syntax)

(test value-bindings
  "Smoke test for the `value-bindings' rule."

  (rule-test-cases ((syn::value-bindings syn::special-operators))
    '((1)       :fatal 1       "must be a binding of the form NAME, (NAME) or (NAME FORM)")
    '(((1))     :fatal 1       "variable name must be a symbol")
    '(((a 1 2)) :fatal (a 1 2) "must be a binding of the form NAME, (NAME) or (NAME FORM)")

    '(()        t      nil (()  ()))
    '(((a))     t      nil ((a) (nil)))
    '(((a 1))   t      nil ((a) (1)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."

  (rule-test-cases ((syn::function-bindings syn::special-operators))
    '((1)           :fatal 1     "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((1))         :fatal 1     "must be a function name")
    '(((a))         :fatal (a)   "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a 1))       :fatal (a 1) "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")

    '(()            t      nil (()  ()))
    '(((a ()))      t      nil ((a) ((syn::parsed-lambda (() () nil () nil ()) () () ()))))
    '(((a () 1))    t      nil ((a) ((syn::parsed-lambda (() () nil () nil ()) () () (1)))))
    '(((a () "" 1)) t      nil ((a) ((syn::parsed-lambda (() () nil () nil ()) "" () (1)))))))

(test macro-function-bindings
  "Smoke test for the `macro-function-bindings' rule."

  (rule-test-cases ((syn::macro-function-bindings syn::special-operators))
    '((1)                    :fatal 1     "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((1))                  :fatal 1     "must be a function name")
    '(((a))                  :fatal (a)   "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a 1))                :fatal (a 1) "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")

    '(()                     t      nil (()  ()))
    '(((a ()))               t      nil ((a) ((syn::parsed-lambda
                                               (:destructuring-lambda-list
                                                nil nil () () nil () nil () nil)
                                               () () ()))))
    '(((a (&whole w (a b)))) t      nil ((a) ((syn::parsed-lambda
                                               (:destructuring-lambda-list
                                                w nil
                                                ((:pattern nil (a b) () nil () nil () nil))
                                                () nil () nil () nil)
                                               () () ()))))
    '(((a () 1))             t      nil ((a) ((syn::parsed-lambda
                                               (:destructuring-lambda-list
                                                nil nil () () nil () nil () nil)
                                               () () (1)))))
    '(((a () "" 1))          t      nil ((a) ((syn::parsed-lambda
                                               (:destructuring-lambda-list
                                                nil nil () () nil () nil () nil)
                                               "" () (1)))))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."

  (rule-test-cases ((syn::symbol-macro-bindings syn::special-operators))
    '((1)       :fatal 1       "must be a binding of the form (NAME FORM)")
    '(((1))     :fatal 1       "variable name must be a symbol")
    '(((a))     :fatal (a)     "must be a binding of the form (NAME FORM)")
    '(((a 1 2)) :fatal (a 1 2) "must be a binding of the form (NAME FORM)")

    '(()        t      nil     (() ()))
    '(((a 1))   t      nil     ((a) (1)))))
