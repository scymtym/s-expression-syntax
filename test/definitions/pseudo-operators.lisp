;;;; pseudo--operators.lisp --- Tests for pseudo operator rules.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
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
  '(((lambda (x #1=x)) 1)
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#2=(#3=foo)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #3#))))
     :source #2#))
  '(#4=(#5=foo 1)
    (:application
     ((:function-name . 1) (((:function-name () :name foo :source #5#)))
      (:argument      . *) ((1 :evaluation t)))
     :source #4#))
  '(#6=(#7=(lambda #8=(#9=x) x) 1)
    (:application
     ((:function . 1) (((:lambda-expression
                         ((:lambda-list . 1) (((:ordinary-lambda-list
                                                ((:required . *) (((:required-parameter
                                                                    ((:name . 1) (((:variable-name
                                                                                    ()
                                                                                    :name x :source #9#)
                                                                                   :evaluation nil)))
                                                                    :source #9#))))
                                                :source #8#)
                                               :evaluation :compound))
                          (:form        . *) ((x :evaluation t)))
                         :source #7#)
                        :evaluation :parsed))
      (:argument . *) ((1 :evaluation t)))
     :source #6#)))

;;; Pseudo-operator "self evaluating"

(define-syntax-test (syn::self-evaluating)
  '(#1=1 (:self-evaluating ((:value . 1) ((#1#))) :source #1#)))
