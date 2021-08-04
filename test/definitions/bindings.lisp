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
    '((#1=1)     :fatal #1# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((#2=1))   :fatal #2# "must be a function name")
    '((#3=(a))   :fatal #3# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a #4=1)) :fatal #4# "must be an ordinary lambda list")
    ;; Valid
    '(()         t      nil (() ()))
    '((#6=(#7=a #8=()))
      t #6# ((#9=(:function-name () :name a :source #7#))
             ((:local-function
               ((:name        . 1) ((#9#))
                (:lambda-list . 1) (((:ordinary-lambda-list
                                      ()
                                      :source #8#)
                                     :evaluation :compound)))
               :source #6#))))
    '((#10=(#11=a #12=() 1))
      t #10# ((#13=(:function-name () :name a :source #11#))
              ((:local-function
                ((:name        . 1) ((#13#))
                 (:lambda-list . 1) (((:ordinary-lambda-list
                                       ()
                                       :source #12#)
                                      :evaluation :compound))
                 (:form        . *) ((1 :evaluation t)))
                :source #10#))))
    '((#14=(#15=a #16=() "" 1))
      t #12# ((#17=(:function-name () :name a :source #15#))
              ((:local-function
                ((:name          . 1) ((#17#))
                 (:lambda-list   . 1) (((:ordinary-lambda-list
                                         ()
                                         :source #16#)
                                        :evaluation :compound))
                 (:documentation . 1) ((""))
                 (:form          . *) ((1 :evaluation t)))
                :source #14#))))))

(test macro-function-bindings
  "Smoke test for the `macro-function-bindings' rule."

  (rule-test-cases ((syn::macro-function-bindings syn::special-operators))
    '((#1=1)     :fatal #1# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((#2=1))   :fatal #2# "must be a function name")
    '((#3=(a))   :fatal #3# "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)")
    '(((a #4=1)) :fatal #4# "must be a destructuring lambda list")
    ;; Valid
    '(() t nil (() ()))
    '((#6=(#7=a #8=()))
      t nil ((#9=(:function-name () :name a :source #7#))
             ((:local-macro-function
               ((:name        . 1) ((#9#))
                (:lambda-list . 1) (((:destructuring-lambda-list
                                      ()
                                      :source #8#)
                                     :evaluation :compound)))
               :source #6#))))
    '((#10=(#11=a #12=(&whole #13=w #14=(#15=a #16=b))))
      t nil ((#17=(:function-name () :name a :source #11#))
             ((:local-macro-function
               ((:name        . 1) ((#17#))
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
               :source #10#))))
    '((#18=(#19=a #20=() #21=1))
      t nil ((#22=(:function-name () :name a :source #19#))
             ((:local-macro-function
               ((:name        . 1) ((#22#))
                (:lambda-list . 1) (((:destructuring-lambda-list () :source #20#)
                                     :evaluation :compound))
                (:form        . *) ((1 :evaluation t)))
               :source #18#))))
    '((#23=(#24=a #25=() "" 1))
      t nil ((#26=(:function-name () :name a :source #24#))
             ((:local-macro-function
               ((:name          . 1) ((#26#))
                (:lambda-list   . 1) (((:destructuring-lambda-list () :source #25#)
                                       :evaluation :compound))
                (:documentation . 1) ((""))
                (:form          . *) ((1 :evaluation t)))
               :source #23#))))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."

  (rule-test-cases ((syn::symbol-macro-bindings syn::special-operators))
    '((#1=1)       :fatal #1# "must be a binding of the form (NAME FORM)")
    '(((#2=1))     :fatal #2# "variable name must be a symbol")
    '((#3=(a))     :fatal #3# "must be a binding of the form (NAME FORM)")
    '((#4=(a 1 2)) :fatal #4# "must be a binding of the form (NAME FORM)")
    ;; Valid
    '(()           t      nil (() ()))
    '(((#5=a 1))   t      nil (((:variable-name () :name a :source #5#))
                               (1)))))
