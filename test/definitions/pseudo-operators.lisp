;;;; pseudo--operators.lisp --- Tests for pseudo operator rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.special-operators
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
  '(((lambda (x #3=x)) 1)
    syn:invalid-syntax-error #3# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#4=(#5=foo)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #5#))))
     :source #4#))
  '(#6=(#7=foo 1)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #7#)))
      (:argument      . *) ((1 :evaluation t)))
     :source #6#))
  '(#8=(#9=(lambda #10=(#11=x) x) 1)
    (:application
     ((:function . 1) (((:lambda-expression
                         ((:lambda-list . 1) (((:ordinary-lambda-list
                                                ((:required . *) (((:required-parameter
                                                                    ((:name . 1) (((:variable-name
                                                                                    ()
                                                                                    :name x :source #11#)
                                                                                   :evaluation nil)))
                                                                    :source #11#))))
                                                :source #10#)
                                               :evaluation :compound))
                          (:form        . *) ((x :evaluation t)))
                         :source #9#)
                        :evaluation :parsed))
      (:argument . *) ((1 :evaluation t)))
     :source #8#)))

;;; Pseudo-operator "self evaluating"

(define-syntax-test (syn::self-evaluating)
  '(#1=1 (:self-evaluating ((:value . 1) ((#1#))) :source #1#)))
