;;;; types.lisp --- Tests for type specifier rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.type-specifiers
  :in :s-expression-syntax)

(test type-specifier
  "Smoke test for the `type-specifier' rule."

  (rule-test-cases ((syn::type-specifier syn::type-specifiers))
    `(1                  nil    nil      nil)
    `((1)                nil    nil      nil)
    `(values             :fatal values   "the symbol VALUES is not a valid type specifier")
    `((values)           :fatal (values) "VALUES type is invalid in this context")

    '(bit                t   nil bit)
    '((vector t)         t   nil (vector t))
    '((unsigned-byte 32) t nil (unsigned-byte 32))))

(test values-type-specifier
  "Smoke test for the `values-type-specifier' rule."

  (rule-test-cases ((syn::values-type-specifier syn::type-specifiers))
    '(1                          nil nil nil)
    '(bit                        nil nil nil)

    '((values)                   t nil (() () nil nil))
    '((values bit)               t nil ((bit) () nil nil))
    '((values &optional bit)     t nil (() (bit) nil nil))
    '((values &rest bit)         t nil (() () bit nil))
    '((values &allow-other-keys) t nil (() () nil &allow-other-keys))))
