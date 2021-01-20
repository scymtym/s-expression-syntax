;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(in-suite :s-expression-syntax)

(test body
  "Smoke test for the `body' rule."

  (rule-test-cases ((syn::body syn::special-operators))
    '(()                                          t      nil (() ()))
    ;; No declarations
    '((1)                                         t      nil (() (1)))
    '((1 2)                                       t      nil (() (1 2)))
    ;; Invalid declarations
    '(((declare 1))                               :fatal nil "must be a declaration")
    '(((declare ()))                              :fatal nil "declaration kind must be a symbol")
    '(((declare (type 1)))                        :fatal nil "must be a type specifier")
    ;; Valid declarations
    '(((declare (ignore a)))                      t      nil (((ignore (a))) ()))
    '(((declare (type bit)))                      t      nil (((type (bit))) ()))
    '(((declare (type bit a)))                    t      nil (((type (bit a))) ()))
    '(((declare (type bit a b)))                  t      nil (((type (bit a b))) ()))
    ;; Multiple declarations
    '(((declare (ignore a)) (declare (ignore b))) t      nil (((ignore (a)) (ignore (b))) ()))
    ;; Declarations and forms
    '(((declare (ignore a)) 3 4)                  t      nil (((ignore (a))) (3 4)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."

  (rule-test-cases ((syn::docstring-body syn::special-operators))
    ;; Empty
    '(()                           t nil (nil   () ()))
    ;; Only forms
    '(("foo")                      t nil (nil   () ("foo")))
    ;; Declarations and docstrings
    '(((declare (ignore a)))       t nil (nil   ((ignore (a))) ()))
    '(((declare (ignore a)) "foo") t nil (nil   ((ignore (a))) ("foo")))
    '(("foo" (declare (ignore a))) t nil ("foo" ((ignore (a))) ()))
    ;; Forms and docstrings
    '((1)                          t nil (nil   ()  (1)))
    '(("foo" 1)                    t nil ("foo" ()  (1)))
    '((1 "foo")                    t nil (nil   ()  (1 "foo")))))
