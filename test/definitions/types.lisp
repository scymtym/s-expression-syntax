;;;; types.lisp --- Tests for type specifier rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.type-specifiers
  :in :syntax)

(test type-specifier
  "Smoke test for the `type-specifier' rule."

  (rule-test-cases ((syntax::type-specifier syntax::type-specifiers))
    `(1                  nil nil nil)
    `((1)                nil nil nil)

    '(bit                t   nil bit)
    '((vector t)         t   nil (vector t))
    '((unsigned-byte 32) t nil (unsigned-byte 32))))
