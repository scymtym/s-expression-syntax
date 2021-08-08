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
    ;; Invalid syntax
    '(1 nil nil nil)
    '((function #20=1)
      :fatal #20# "must be of the form (REQUIRED* [&optional OPTIONAL*] [&rest REST] [&key KEY* [&allow-other-keys]])")
    '((function #1=(&rest))
      :fatal #1# "type specifier must follow &REST")
    ;; Valid syntax
    '(#2=function
      t #2# (:function-type-specifier () :source #2#))
    '(#3=(function #4=* #5=*)
      t #3# (:function-type-specifier
             ((:parameters . 1) (((:wildcard-type-specifier () :source #4#)))
              (:values     . 1) (((:wildcard-type-specifier () :source #5#))))
             :source #3#))
    '(#6=(function #7=(#8=bit #9=t))
      t #6# (:function-type-specifier
             ((:parameters . 1) (((:parameter-type-specifier
                                   ((:required . *) (((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name bit :source #8#))))
                                                       :source #8#))
                                                     ((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name t :source #9#))))
                                                       :source #9#))))
                                    :source #7#))))
              :source #6#))
    '(#10=(function #11=(&optional #12=bit #13=t))
      t #10# (:function-type-specifier
              ((:parameters . 1) (((:parameter-type-specifier
                                    ((:optional . *) (((:atomic-type-specifier
                                                        ((:name . 1) (((:type-name
                                                                        ()
                                                                        :name bit :source #12#))))
                                                        :source #12#))
                                                      ((:atomic-type-specifier
                                                        ((:name . 1) (((:type-name
                                                                        ()
                                                                        :name t :source #13#))))
                                                        :source #13#))))
                                    :source #11#))))
              :source #10#))
    '(#14=(function #15=(&rest #16=bit &key #17=(#18=a #19=t)))
      t #10# (:function-type-specifier
              ((:parameters . 1) (((:parameter-type-specifier
                                    ((:rest    . 1) (((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name bit :source #16#))))
                                                       :source #16#)))
                                     (:keyword . *) (((:keyword-parameter-type-specifier
                                                       ((:keyword . 1) (((:keyword
                                                                          ()
                                                                          :name a :source #18#)))
                                                        (:type    . 1) (((:atomic-type-specifier
                                                                          ((:name . 1) (((:type-name
                                                                                          ()
                                                                                          :name t :source #19#))))
                                                                          :source #19#))))
                                                       :source #17#))))
                                    :source #15#))))
              :source #14#))))

(test values-type-specifier
  "Smoke test for the `values-type-specifier' rule."

  (rule-test-cases ((syn::values-type-specifier syn::type-specifiers))
    ;; Invalid syntax
    '(1   nil nil nil)
    '(bit nil nil nil)
    '((values #8=*)
      :fatal #8# "the type specifier * is not allowed within a VALUES type specifier")
    '((values &optional #9=*)
      :fatal #9# "the type specifier * is not allowed within a VALUES type specifier")
    '((values &allow-other-keys)
      :fatal nil "must be of the form (values REQUIRED* [&optional OPTIONAL*] [&rest REST])")
    '((values &rest)
      :fatal nil "type specifier must follow &REST")
    '((values &rest bit bit)
      :fatal nil "must be of the form (values REQUIRED* [&optional OPTIONAL*] [&rest REST])")
    ;; Valid syntax
    '(#1=(values)                  t   nil (:values-type-specifier () :source #1#))
    '(#2=(values #3=bit)           t   nil (:values-type-specifier
                                            ((:required . *) (((:atomic-type-specifier
                                                                ((:name . 1) (((:type-name () :name bit :source #3#))))
                                                                :source #3#))))
                                            :source #2#))
    '(#10=(values #11=bit #12=t)   t   nil (:values-type-specifier
                                            ((:required . *) (((:atomic-type-specifier
                                                                ((:name . 1) (((:type-name () :name bit :source #11#))))
                                                                :source #11#))
                                                              ((:atomic-type-specifier
                                                                ((:name . 1) (((:type-name () :name t :source #12#))))
                                                                :source #12#))))
                                            :source #10#))
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
