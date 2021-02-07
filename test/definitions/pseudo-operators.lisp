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

  '(#2=foo (:variable-reference (:name ((#2#))) :source #2#)))

;;; Pseudo-operator "application"

(define-syntax-test (syn::application)
  '((1)
    syn:invalid-syntax-error 1 "must be a symbol naming a function or a lambda expression")
  '(((lambda 1) 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '(((lambda (x #3=x)) 1)
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  ;; Valid syntax
  '(#4=(foo)
    (:application
     (:abstraction ((foo)))
     :source #4#))
  '(#5=(foo 1)
    (:application
     (:abstraction ((foo))
      :arguments   ((1)))
     :source #5#))
  '(#6=(#7=(lambda #8=(#9=x) x) 1)
    (:application
     (:abstraction (((:lambda-expression
                      (:lambda-list (((:ordinary-lambda-list
                                       (:required (((:required-parameter
                                                     ((:name . 1) ((x)))
                                                     :source #9#))))
                                       :source #8#)))
                       :form        ((x)))
                      :source #7#)))
      :arguments   ((1)))
     :source #6#)))

;;; Pseudo-operator "self evaluating"

(define-syntax-test (syn::self-evaluating)
  '(#1=1 (:self-evaluating (:value ((#1#))) :source #1#)))
