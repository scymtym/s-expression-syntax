;;;; pseudo--operators.lisp --- Tests for pseudo operator rules.
;;;;
;;;; Copyright (C) 2018-2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.pseudo-operators
  :in :s-expression-syntax)

;;; Pseudo-operator variable reference

(define-syntax-test (syn:variable-reference)
  ;; Invalid syntax
  '(#1=1
    syn:invalid-syntax-error #1#)
  ;; Valid syntax
  '(#2=foo
    (:variable-reference
     ((:name . 1) (((:variable-name () :name foo :source #2#)
                    :evaluation (:reference :namespace variable))))
     :source #2#))
  '(#3=:foo
    (:variable-reference
     ((:name . 1) (((:variable-name () :name :foo :source #3#)
                    :evaluation (:reference :namespace variable))))
     :source #3#))
  '(#4=nil
    (:variable-reference
     ((:name . 1) (((:variable-name () :name nil :source #4#)
                    :evaluation (:reference :namespace variable))))
     :source #4#)))

;;; Pseudo-operator "application"

(define-syntax-test (syn:application)
  '((1)
    syn:invalid-syntax-error 1 "must be a symbol naming a function or a lambda expression")
  '(((lambda 1) 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '(((lambda (#1=(a b))) 1) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #1# "variable name must be a symbol")
  '(((lambda (x #2=x)) 1)
    syn:invalid-syntax-error #2# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#3=(#4=foo)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #4#)
                             :evaluation (:reference :namespace function))))
     :source #3#))
  '(#5=(#6=foo #7=1)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #6#)
                             :evaluation (:reference :namespace function)))
      (:argument      . *) (((:unparsed () :expression 1 :context :form :source #7#)
                             :evaluation t)))
     :source #5#))
  '(#8=(#9=(lambda #10=(#11=x) #12=x) #13=1)
    (:application
     ((:function . 1) (((:lambda-expression
                         ((:lambda-list . 1) (((:ordinary-lambda-list
                                                ((:required-section . 1)
                                                 (((:required-section
                                                    ((:parameter . *) (((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name x :source #11#))))
                                                                         :source #11#))))))))
                                                :source #10#)
                                               :evaluation :compound))
                          (:form        . *) (((:unparsed
                                                ()
                                                :expression x
                                                :context    :form
                                                :source     #12#)
                                               :evaluation t)))
                         :source #9#)
                        :evaluation :compound))
      (:argument . *) (((:unparsed () :expression 1 :context :form :source #13#)
                        :evaluation t)))
     :source #8#)))

;;; Pseudo-operator "self evaluating"

(define-syntax-test (syn:self-evaluating)
  ;; Invalid syntax
  '(#1=:foo
    syn:invalid-syntax-error #1#)
  ;; Valid syntax
  '(#2=1
    (:self-evaluating
     ((:value . 1) (((:unparsed
                      ()
                      :expression 1 :context :self-evaluating :source #2#))))
     :source #2#)))
