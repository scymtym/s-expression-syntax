;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(in-suite :s-expression-syntax)

(test body
  "Smoke test for the `body' rule."

  (rule-test-cases ((syn::body syn::special-operators))
    '(()                   t      nil (() ()))
    ;; No declarations
    '((1)                  t      nil (() (1)))
    '((1 2)                t      nil (() (1 2)))
    ;; Invalid declarations
    '(((declare 1))        :fatal nil "must be a declaration")
    '(((declare ()))       :fatal nil "declaration kind must be a symbol")
    '(((declare (type 1))) :fatal nil "must be a type specifier")
    ;; Valid declarations
    '(((declare #7=(ignore a)))
      t nil (((:declaration (:argument ((a))) :kind ignore :source #7#))
             ()))
    '(((declare #8=(type bit)))
      t nil (((:declaration (:argument ((bit))) :kind type :source #8#))
             ()))
    '(((declare #9=(type bit a)))
      t nil (((:declaration (:argument ((bit) (a))) :kind type :source #9#))
             ()))
    '(((declare #10=(type bit a b)))
      t nil (((:declaration (:argument ((bit) (a) (b))) :kind type :source #10#))
             ()))
    ;; Multiple declarations
    '(((declare #11=(ignore a)) (declare #12=(ignore b)))
      t nil (((:declaration (:argument ((a))) :kind ignore :source #11#)
              (:declaration (:argument ((b))) :kind ignore :source #12#))
             ()))
    ;; Declarations and forms
    '(((declare #13=(ignore a)) 3 4)
      t nil (((:declaration (:argument ((a))) :kind ignore :source #13#))
             (3 4)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."

  (rule-test-cases ((syn::docstring-body syn::special-operators))
    ;; Empty
    '(()                              t nil (nil   () ()))
    ;; Only forms
    '(("foo")                         t nil (nil   () ("foo")))
    ;; Declarations and docstrings
    '(((declare #3=(ignore a)))       t nil (nil
                                             ((:declaration
                                               (:argument ((a)))
                                               :kind ignore :source #3#))
                                             ()))
    '(((declare #4=(ignore a)) "foo") t nil (nil
                                             ((:declaration
                                               (:argument ((a)))
                                               :kind ignore :source #4#))
                                             ("foo")))
    '(("foo" (declare #5=(ignore a))) t nil ("foo"
                                             ((:declaration
                                               (:argument ((a)))
                                               :kind ignore :source #5#))
                                             ()))
    ;; Forms and docstrings
    '((1)                             t nil (nil   ()  (1)))
    '(("foo" 1)                       t nil ("foo" ()  (1)))
    '((1 "foo")                       t nil (nil   ()  (1 "foo")))))
