;;;; types.lisp --- Tests for type specifier rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.type-specifiers
  :in :s-expression-syntax)

(test compound-type-specifier
  "Smoke test for the `compound-type-specifier' rule."

  (rule-test-cases ((syn::compound-type-specifier syn::type-specifiers))
    `(1   nil    nil nil)
    `((1) :fatal 1   "must be a class name")))

(test function-type-specifier
  "Smoke test for the `function-type-specifier' rule."

  (rule-test-cases ((syn::function-type-specifier syn::type-specifiers))
    '(1                       nil nil nil)

    '(#3=(function #4=* #5=*) t #3# (:function-type-specifier
                                     ((:parameters . 1) (((:wildcard-type-specifier () :source #4#)))
                                      (:values     . 1) (((:wildcard-type-specifier () :source #5#))))
                                     :source #3#))))

(test values-type-specifier
  "Smoke test for the `values-type-specifier' rule."

  (rule-test-cases ((syn::values-type-specifier syn::type-specifiers))
    '(1                            nil nil nil)
    '(bit                          nil nil nil)
    '((values &allow-other-keys)   nil nil nil)
    ;; Valid
    '(#1=(values)                  t   nil (:values-type-specifier () :source #1#))
    '(#2=(values #3=bit)           t   nil (:values-type-specifier
                                            ((:required . *) (((:atomic-type-specifier
                                                                ((:name . 1) (((:type-name () :name bit :source #3#))))
                                                                :source #3#))))
                                            :source #2#))
    '(#4=(values &optional #5=bit) t   nil (:values-type-specifier
                                            ((:optional . *) (((:atomic-type-specifier
                                                                ((:name . 1) (((:type-name () :name bit :source #5#))))
                                                                :source #5#))))
                                            :source #4#))
    '(#6=(values &rest #7=bit)     t   nil (:values-type-specifier
                                            ((:rest . 1) (((:atomic-type-specifier
                                                            ((:name . 1) (((:type-name () :name bit :source #7#))))
                                                            :source #7#))))
                                            :source #6#))))


(test type-specifier
  "Smoke test for the `type-specifier' rule."

  (rule-test-cases ((syn::type-specifier syn::type-specifiers))
    `(#1=1        nil    #1# nil)
    `((#2=1)      :fatal #2# "must be a class name")
    `(#3=values   :fatal #3# "the symbol VALUES is not a valid type specifier")
    `(#4=(values) :fatal #4# "VALUES type is invalid in this context")
    ;; Valid
    '(#5=bit                    t nil (:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #5#))))
                                       :source #5#))
    '(#6=(#7=vector #8=t)       t nil (:compound-type-specifier
                                       ((:name     . 1) (((:type-name
                                                           ()
                                                           :name vector :source #7#)))
                                        (:argument . *) (((:atomic-type-specifier
                                                           ((:name . 1) (((:type-name
                                                                           ()
                                                                           :name t :source #8#))))
                                                           :source #8#))))
                                       :source #6#))
    '(#9=(#10=unsigned-byte 32) t nil (:compound-type-specifier
                                       ((:name     . 1) (((:type-name
                                                           ()
                                                           :name unsigned-byte :source #10#)))
                                        (:argument . *) ((32)))
                                       :source #9#))))
