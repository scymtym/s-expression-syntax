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
    `(1                  nil nil nil)
    `((1)                nil nil nil)

    '(bit                t   nil bit)
    '((vector t)         t   nil (vector t))
    '((unsigned-byte 32) t nil (unsigned-byte 32))))
