;;;; pseudo--operators.lisp --- Tests for pseudo operator rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.pseudo-operators
  :in :s-expression-syntax)

;;; Pseudo-operator variable reference

(define-syntax-test (syn::variable-reference)
  '(#1=:foo syn:invalid-syntax-error #1#)

  '(#2=foo (:variable-reference
            ((:name . 1) (((:variable-name () :name foo :source #2#))))
            :source #2#)))

;;; Pseudo-operator "application"

(define-syntax-test (syn::application)
  '((1)
    syn:invalid-syntax-error 1 "must be a symbol naming a function or a lambda expression")
  '(((lambda 1) 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '(((lambda (x #1=x)) 1)
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#2=(#3=foo)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #3#))))
     :source #2#))
  '(#4=(#5=foo #6=1)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #5#)))
      (:argument      . *) (((:unparsed () :expression 1 :context :form :source #6#)
                             :evaluation t)))
     :source #4#))
  '(#7=(#8=(lambda #9=(#10=x) #11=x) #12=1)
    (:application
     ((:function . 1) (((:lambda-expression
                         ((:lambda-list . 1) (((:ordinary-lambda-list
                                                ((:required . *) (((:required-parameter
                                                                    ((:name . 1) (((:variable-name
                                                                                    ()
                                                                                    :name x :source #10#)
                                                                                   :evaluation nil)))
                                                                    :source #10#))))
                                                :source #9#)
                                               :evaluation :compound))
                          (:form        . *) (((:unparsed
                                                ()
                                                :expression x
                                                :context    :form
                                                :source     #11#)
                                               :evaluation t)))
                         :source #8#)
                        :evaluation :compound))
      (:argument . *) (((:unparsed () :expression 1 :context :form :source #12#)
                        :evaluation t)))
     :source #7#)))

;;; Pseudo-operator "self evaluating"

(define-syntax-test (syn::self-evaluating)
  '(#1=1
    (:self-evaluating
     ((:value . 1) (((:unparsed
                      ()
                      :expression #1# :context :self-evaluating :source #1#))))
     :source #1#)))
