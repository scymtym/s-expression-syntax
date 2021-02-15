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

    '(()
      t nil (()  ()))
    '(#1=((#2=a))
      t #1# (((:variable-name () :name a :source #2#))
             (nil)))
    '(#3=((#4=a 1))
      t #3# (((:variable-name () :name a :source #4#))
             (1)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."

  (rule-test-cases ((syn::function-bindings syn::special-operators))
    '((1)     :fatal 1   "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((1))   :fatal 1   "must be a function name")
    '(((a))   :fatal (a) "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a 1)) :fatal 1   "must be an ordinary lambda list")

    '(()               t      nil (()  ()))
    '((#6=(#7=a #8=()))
      t #6# (((:function-name () :name a :source #7#))
             ((:local-function
               (:lambda-list (((:ordinary-lambda-list
                                ()
                                :source #8#))))
               :source #6#))))
    '((#9=(#10=a #11=() 1))
      t #9# (((:function-name () :name a :source #10#))
             ((:local-function
               (:lambda-list (((:ordinary-lambda-list
                                ()
                                :source #11#)))
                :forms       ((1)))
               :source #9#))))
    '((#12=(#13=a #14=() "" 1))
      t #12# (((:function-name () :name a :source #13#))
              ((:local-function
                (:lambda-list   (((:ordinary-lambda-list
                                   ()
                                   :source #14#)))
                 :documentation ((""))
                 :forms         ((1)))
                :source #12#))))))

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
    '((#8=(a #9=(&whole w #11=(#12=a #13=b))))
      t nil ((a) ((:local-macro-function
                   (:lambda-list (((:destructuring-lambda-list
                                    (:whole    ((w))
                                     :required (((:required-parameter
                                                  ((:name . 1) (((:pattern
                                                                  (:required (((:required-parameter
                                                                                ((:name . 1) ((a)))
                                                                                :source #12#))
                                                                              ((:required-parameter
                                                                                ((:name . 1) ((b)))
                                                                                :source #13#))))
                                                                  :source #11#))))
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
