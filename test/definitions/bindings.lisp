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

    '(()        t      nil     (()  ()))
    '(((a))     t      nil     ((a) (nil)))
    '(((a 1))   t      nil     ((a) (1)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."

  (rule-test-cases ((syn::function-bindings syn::special-operators))
    '((1)     :fatal 1   "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((1))   :fatal 1   "must be a function name")
    '(((a))   :fatal (a) "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a 1)) :fatal 1   "must be an ordinary lambda list")

    '(()               t      nil (()  ()))
    '((#6=(a #7=()))   t      nil ((a) ((:local-function
                                         (:lambda-list (((:ordinary-lambda-list
                                                          ()
                                                          :source #7#))))
                                         :source #6#))))
    '((#8=(a #9=() 1))      t      nil ((a)
                                        ((:local-function
                                          (:lambda-list (((:ordinary-lambda-list
                                                           ()
                                                           :source #9#)))
                                           :forms       ((1)))
                                          :source #8#))))
    '((#10=(a #11=() "" 1)) t      nil ((a)
                                        ((:local-function
                                          (:lambda-list   (((:ordinary-lambda-list
                                                             ()
                                                             :source #11#)))
                                           :documentation ((""))
                                           :forms         ((1)))
                                          :source #10#))))))

(test macro-function-bindings
  "Smoke test for the `macro-function-bindings' rule."

  (rule-test-cases ((syn::macro-function-bindings syn::special-operators))
    '((1)     :fatal 1   "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((1))   :fatal 1   "must be a function name")
    '(((a))   :fatal (a) "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a 1)) :fatal 1   "must be a destructuring lambda list")

    '(() t nil (() ()))
    '((#6=(a #7=()))
      t nil ((a) ((:local-macro-function
                   (:lambda-list (((:destructuring-lambda-list
                                    ()
                                    :source #7#))))
                   :source #6#))))
    '((#8=(a #9=(&whole w #11=(a b))))
      t nil ((a) ((:local-macro-function
                   (:lambda-list (((:destructuring-lambda-list
                                    (:whole    ((w))
                                     :required (((:pattern
                                                  (:required ((a) (b)))
                                                  :source #11#))))
                                    :source #9#))))
                   :source #8#))))
    '((#14=(a #15=() #16=1))
      t nil ((a) ((:local-macro-function
                   (:lambda-list (((:destructuring-lambda-list () :source #15#)))
                    :forms       ((1)))
                   :source #14#))))
    '((#17=(a #18=() "" 1))
      t nil ((a) ((:local-macro-function
                   (:lambda-list   (((:destructuring-lambda-list () :source #18#)))
                    :documentation ((""))
                    :forms         ((1)))
                   :source #17#))))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."

  (rule-test-cases ((syn::symbol-macro-bindings syn::special-operators))
    '((1)       :fatal 1       "must be a binding of the form (NAME FORM)")
    '(((1))     :fatal 1       "variable name must be a symbol")
    '(((a))     :fatal (a)     "must be a binding of the form (NAME FORM)")
    '(((a 1 2)) :fatal (a 1 2) "must be a binding of the form (NAME FORM)")

    '(()        t      nil     (() ()))
    '(((a 1))   t      nil     ((a) (1)))))
