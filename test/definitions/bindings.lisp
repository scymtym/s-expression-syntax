;;;; bindings.lisp --- Tests for binding rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.bindings
  :in :s-expression-syntax)

(test value-bindings
  "Smoke test for the `value-bindings' rule."
  (rule-test-cases ((syn::value-bindings syn::special-operators))
    '((#1=1)
      :fatal #1# "must be a binding of the form NAME, (NAME) or (NAME FORM)")
    '(((#2=1))
      :fatal #2# "variable name must be a symbol")
    '((#3=(a 1 2))
      :fatal #3# "must be a binding of the form NAME, (NAME) or (NAME FORM)")

    '(()
      t nil ())
    '(#4=(#5=(#6=a))
      t #4# ((:value-binding
              ((:name . 1) (((:variable-name () :name a :source #6#)
                             :evaluation (:binding :namespace variable
                                                   :scope     :lexical))))
              :source #5#)))
    '(#7=(#8=(#9=a #10=1))
      t #7# ((:value-binding
              ((:name  . 1) (((:variable-name () :name a :source #9#)
                              :evaluation (:binding :namespace variable
                                                    :scope     :lexical)))
               (:value . 1) (((:unparsed
                               ()
                               :expression 1 :context :form :source #10#)
                              :evaluation t)))
              :source #8#)))))

(test function-bindings
  "Smoke test for the `function-bindings' rule."
  (rule-test-cases ((syn::function-bindings syn::special-operators))
    '((#1=1)
      :fatal #1# "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)")
    '(((#2=1))
      :fatal #2# "must be a function name")
    '((#3=(a))
      :fatal #3# "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)")
    '(((a #4=1))
      :fatal #4# "must be an ordinary lambda list")
    ;; Valid
    '(()
      t nil ())
    '((#5=(#6=a #7=()))
      t #5# ((:local-function-binding
              ((:name        . 1) (((:function-name () :name a :source #6#)
                                    :evaluation (:binding :namespace function
                                                          :scope     :lexical)))
               (:lambda-list . 1) (((:ordinary-lambda-list
                                     ()
                                     :source #7#)
                                    :evaluation :compound)))
              :source #5#)))
    '((#8=(#9=a #10=() #11=1))
      t #8# ((:local-function-binding
               ((:name        . 1) (((:function-name () :name a :source #9#)
                                     :evaluation (:binding :namespace function
                                                           :scope     :lexical)))
                (:lambda-list . 1) (((:ordinary-lambda-list
                                      ()
                                      :source #10#)
                                     :evaluation :compound))
                (:form        . *) (((:unparsed
                                      ()
                                      :expression 1
                                      :context    :form
                                      :source     #11#)
                                     :evaluation t)))
               :source #8#)))
     '((#12=(#13=a #14=() #15=1))
       t #12# ((:local-function-binding
                ((:name        . 1) (((:function-name () :name a :source #13#)
                                      :evaluation (:binding :namespace function
                                                            :scope     :lexical)))
                 (:lambda-list . 1) (((:ordinary-lambda-list
                                       ()
                                       :source #14#)
                                      :evaluation :compound))
                 (:form        . *) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source #15#)
                                      :evaluation t)))
                :source #12#)))
     '((#16=(#17=a #18=() #19="" #20=1))
       t #14# ((:local-function-binding
                ((:name          . 1) (((:function-name () :name a :source #17#)
                                        :evaluation (:binding :namespace function
                                                              :scope     :lexical)))
                 (:lambda-list   . 1) (((:ordinary-lambda-list
                                         ()
                                         :source #18#)
                                        :evaluation :compound))
                 (:documentation . 1) (((:documentation () :string "" :source #19#)))
                 (:form          . *) (((:unparsed
                                         ()
                                         :expression 1 :context :form :source #20#)
                                        :evaluation t)))
                :source #16#)))))

(test macro-function-bindings
  "Smoke test for the `macro-function-bindings' rule."
  (rule-test-cases ((syn::macro-function-bindings syn::special-operators))
    '((#1=1)
      :fatal #1# "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)")
    '(((#2=1))
      :fatal #2# "must be a function name")
    '((#3=(a))
      :fatal #3# "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)")
    '(((a #4=1))
      :fatal #4# "must be a destructuring lambda list")
    ;; Valid
    '(() t nil ())
    '((#5=(#6=a #7=()))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #6#)
                                    :evaluation (:binding :namespace function
                                                          :scope     :lexical)))
               (:lambda-list . 1) (((:destructuring-lambda-list
                                     ()
                                     :source #7#)
                                    :evaluation :compound)))
              :source #5#)))
    '((#8=(#9=a #10=(&whole #11=w #12=(#13=a #14=b))))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #9#)
                                    :evaluation (:binding :namespace function
                                                          :scope     :lexical)))
               (:lambda-list . 1) (((:destructuring-lambda-list
                                     ((:whole    . 1) (((:variable-name () :name w :source #11#)))
                                      (:required . *) (((:required-parameter
                                                         ((:name . 1) (((:pattern
                                                                         ((:required . *) (((:required-parameter
                                                                                             ((:name . 1) (((:variable-name () :name a :source #13#)
                                                                                                            :evaluation nil)))
                                                                                             :source #13#)
                                                                                            :evaluation :compound)
                                                                                           ((:required-parameter
                                                                                             ((:name . 1) (((:variable-name () :name b :source #14#)
                                                                                                            :evaluation nil)))
                                                                                             :source #14#)
                                                                                            :evaluation :compound)))
                                                                         :source #12#)
                                                                        :evaluation :compound)))
                                                         :source #12#)
                                                        :evaluation :compound)))
                                     :source #10#)
                                    :evaluation :compound)))
              :source #8#)))
    '((#15=(#16=a #17=() #18=1))
      t nil ((:local-macro-function-binding
              ((:name        . 1) (((:function-name () :name a :source #16#)
                                    :evaluation (:binding :namespace function
                                                          :scope     :lexical)))
               (:lambda-list . 1) (((:destructuring-lambda-list () :source #17#)
                                    :evaluation :compound))
               (:form        . *) (((:unparsed
                                     ()
                                     :expression #18# :context :form :source #18#)
                                    :evaluation t)))
              :source #15#)))
    '((#19=(#20=a #21=() #22="" #23=1))
      t nil ((:local-macro-function-binding
              ((:name          . 1) (((:function-name () :name a :source #20#)
                                      :evaluation (:binding :namespace function
                                                            :scope     :lexical)))
               (:lambda-list   . 1) (((:destructuring-lambda-list () :source #21#)
                                      :evaluation :compound))
               (:documentation . 1) (((:documentation () :string "" :source #22#)))
               (:form          . *) (((:unparsed
                                       ()
                                       :expression #23# :context :form :source #23#)
                                      :evaluation t)))
              :source #19#)))))

(test symbol-macro-bindings
  "Smoke test for the `symbol-macro-bindings' rule."
  (rule-test-cases ((syn::symbol-macro-bindings syn::special-operators))
    '((#1=1)
      :fatal #1# "must be a binding of the form (NAME FORM)")
    '(((#2=1))
      :fatal #2# "variable name must be a symbol")
    '((#3=(a))
      :fatal #3# "must be a binding of the form (NAME FORM)")
    '((#4=(a 1 2))
      :fatal #4# "must be a binding of the form (NAME FORM)")
    ;; Valid
    '(()
      t nil ())
    '((#5=(#6=a #7=1))
      t nil ((:symbol-macro-binding
              ((:name      . 1) (((:variable-name () :name a :source #6#)
                                  :evaluation (:binding :namespace variable
                                                        :scope     :lexical)))
               (:expansion . 1) (((:unparsed
                                   ()
                                   :expression 1 :context :form :source #7#)
                                  :evaluation t)))
              :source #5#)))))
