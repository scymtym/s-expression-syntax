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
      t nil ())
    '(#1=(#2=(#3=a))
      t #1# ((:value-binding
              ((:name . 1) (((:variable-name () :name a :source #3#)
                             :evaluation :binding)))
              :source #2#)))
    '(#4=(#5=(#6=a 1))
      t #4# ((:value-binding
              ((:name  . 1) (((:variable-name () :name a :source #6#)
                              :evaluation :binding))
               (:value . 1) ((1 :evaluation t)))
              :source #5#)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."

  (rule-test-cases ((syn::function-bindings syn::special-operators))
    '((#1=1)     :fatal #1# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((#2=1))   :fatal #2# "must be a function name")
    '((#3=(a))   :fatal #3# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a #4=1)) :fatal #4# "must be an ordinary lambda list")
    ;; Valid
    '(()         t      nil ())
    '((#6=(#7=a #8=()))
      t #6# ((:local-function-binding
              ((:name        . 1) (((:function-name () :name a :source #7#)
                                    :evaluation :binding))
               (:lambda-list . 1) (((:ordinary-lambda-list
                                     ()
                                     :source #8#)
                                    :evaluation :compound)))
              :source #6#)))
    '((#10=(#11=a #12=() 1))
      t #10# ((:local-function-binding
               ((:name        . 1) (((:function-name () :name a :source #11#)
                                     :evaluation :binding))
                (:lambda-list . 1) (((:ordinary-lambda-list
                                      ()
                                      :source #12#)
                                     :evaluation :compound))
                (:form        . *) ((1 :evaluation t)))
               :source #10#)))
    '((#14=(#15=a #16=() "" 1))
      t #12# ((:local-function-binding
               ((:name          . 1) (((:function-name () :name a :source #15#)
                                       :evaluation :binding))
                (:lambda-list   . 1) (((:ordinary-lambda-list
                                        ()
                                        :source #16#)
                                       :evaluation :compound))
                (:documentation . 1) ((""))
                (:form          . *) ((1 :evaluation t)))
               :source #14#)))))

(test macro-function-bindings
  "Smoke test for the `macro-function-bindings' rule."

  (rule-test-cases ((syn::macro-function-bindings syn::special-operators))
    '((#1=1)     :fatal #1# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((#2=1))   :fatal #2# "must be a function name")
    '((#3=(a))   :fatal #3# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a #4=1)) :fatal #4# "must be a destructuring lambda list")
    ;; Valid
    '(() t nil ())
    '((#6=(#7=a #8=()))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #7#)
                                    :evaluation :binding))
               (:lambda-list . 1) (((:destructuring-lambda-list
                                     ()
                                     :source #8#)
                                    :evaluation :compound)))
              :source #6#)))
    '((#10=(#11=a #12=(&whole #13=w #14=(#15=a #16=b))))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #11#)
                                    :evaluation :binding))
               (:lambda-list . 1) (((:destructuring-lambda-list
                                     ((:whole    . 1) (((:variable-name () :name w :source #13#)))
                                      (:required . *) (((:required-parameter
                                                         ((:name . 1) (((:pattern
                                                                         ((:required . *) (((:required-parameter
                                                                                             ((:name . 1) (((:variable-name () :name a :source #15#)
                                                                                            :evaluation nil)))
                                                                         :source #15#)
                                                                        :evaluation :compound)
                                                                       ((:required-parameter
                                                                         ((:name . 1) (((:variable-name () :name b :source #16#)
                                                                                        :evaluation nil)))
                                                                         :source #16#)
                                                                        :evaluation :compound)))
                                                                         :source #14#)
                                                                        :evaluation :compound)))
                                                         :source #14#)
                                                        :evaluation :compound)))
                                     :source #12#)
                                    :evaluation :compound)))
              :source #10#)))
    '((#18=(#19=a #20=() #21=1))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #19#)
                                    :evaluation :binding))
               (:lambda-list . 1) (((:destructuring-lambda-list () :source #20#)
                                    :evaluation :compound))
               (:form        . *) ((1 :evaluation t)))
              :source #18#)))
    '((#23=(#24=a #25=() "" 1))
      t nil ((:local-macro-function-binding
              ((:name          . 1) (((:function-name () :name a :source #24#)
                                      :evaluation :binding))
               (:lambda-list   . 1) (((:destructuring-lambda-list () :source #25#)
                                      :evaluation :compound))
               (:documentation . 1) ((""))
               (:form          . *) ((1 :evaluation t)))
              :source #23#)))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."

  (rule-test-cases ((syn::symbol-macro-bindings syn::special-operators))
    '((#1=1)       :fatal #1# "must be a binding of the form (NAME FORM)")
    '(((#2=1))     :fatal #2# "variable name must be a symbol")
    '((#3=(a))     :fatal #3# "must be a binding of the form (NAME FORM)")
    '((#4=(a 1 2)) :fatal #4# "must be a binding of the form (NAME FORM)")
    ;; Valid
    '(()           t      nil ())
    '((#5=(#6=a 1))
      t nil ((:symbol-macro-binding
              ((:name      . 1) (((:variable-name () :name a :source #6#)
                                  :evaluation :binding))
               (:expansion . 1) ((1 :evaluation t)))
              :source #5#)))))
