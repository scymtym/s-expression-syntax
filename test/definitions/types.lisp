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
    `(1                        nil    nil      nil)
    `((1)                      nil    nil      nil)
    `(values                   :fatal values   "the symbol VALUES is not a valid type specifier")
    `((values)                 :fatal (values) "VALUES type is invalid in this context")
    ;; Valid
    '(#1=bit                   t      nil (:atomic-type-specifier
                                           ((:name . 1) (((:type-name () :name bit :source #1#))))
                                           :source #1#))
    '(#2=(#3=vector #4=t)      t      nil (:compound-type-specifier
                                           ((:name     . 1) (((:type-name
                                                               ()
                                                               :name vector :source #3#)))
                                            (:argument . *) (((:atomic-type-specifier
                                                               ((:name . 1) (((:type-name
                                                                               ()
                                                                               :name t :source #4#))))
                                                               :source #4#))))
                                           :source #2#))
    '(#5=(#6=unsigned-byte 32) t      nil (:compound-type-specifier
                                           ((:name     . 1) (((:type-name
                                                               ()
                                                               :name unsigned-byte :source #6#)))
                                            (:argument . *) ((32)))
                                           :source #5#))))

(test values-type-specifier
  "Smoke test for the `values-type-specifier' rule."

  (rule-test-cases ((syn::values-type-specifier syn::type-specifiers))
    '(1                          nil nil nil)
    '(bit                        nil nil nil)
    ;; Valid
    '((values)                   t   nil   (() () nil nil))
    '((values #1=bit)            t   nil   (((:atomic-type-specifier
                                              ((:name . 1) (((:type-name () :name bit :source #1#))))
                                              :source #1#))
                                            () nil nil))
    '((values &optional #2=bit)  t   nil   (()
                                            ((:atomic-type-specifier
                                              ((:name . 1) (((:type-name () :name bit :source #2#))))
                                              :source #2#))
                                            nil nil))
    '((values &rest #3=bit)      t   nil   (() ()
                                            (:atomic-type-specifier
                                             ((:name . 1) (((:type-name () :name bit :source #3#))))
                                             :source #3#)
                                            nil))
    '((values &allow-other-keys) t   nil   (() () nil &allow-other-keys))))
