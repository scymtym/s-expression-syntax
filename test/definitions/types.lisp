;;;; types.lisp --- Tests for type specifier rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.type-specifiers
  :in :s-expression-syntax)

(test compound-type-specifier
  "Smoke test for the `compound-type-specifier' rule."
  (rule-test-cases ((syn::compound-type-specifier syn::type-specifiers))
    '(#1=1   nil    #1# nil)
    '((#2=1) :fatal #2# "must be a class name")))

(test function-type-specifier
  "Smoke test for the `function-type-specifier' rule."
  (rule-test-cases ((syn::function-type-specifier syn::type-specifiers))
    ;; Invalid syntax
    '(#1=1 nil #1# nil)
    '((function #2=1)
      :fatal #2# "must be of the form (REQUIRED* [&optional OPTIONAL*] [&rest REST] [&key KEY* [&allow-other-keys]])")
    '((function (&rest))
      :fatal nil #| FIXME should be #3# |# "type specifier must follow &REST")
    ;; Valid syntax
    '(#3=function
      t #3# (:function-type-specifier () :source #3#))
    '(#4=(function #5=* #6=*)
      t #4# (:function-type-specifier
             ((:parameters . 1) (((:wildcard-type-specifier () :source #5#)))
              (:values     . 1) (((:wildcard-type-specifier () :source #6#))))
             :source #4#))
    '(#7=(function #8=(#9=bit #10=t))
      t #7# (:function-type-specifier
             ((:parameters . 1) (((:parameter-type-specifier
                                   ((:required . *) (((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name bit :source #9#))))
                                                       :source #9#))
                                                     ((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name t :source #10#))))
                                                       :source #10#))))
                                    :source #8#))))
              :source #7#))
    '(#11=(function #12=(&optional #13=bit #14=t))
      t #11# (:function-type-specifier
              ((:parameters . 1) (((:parameter-type-specifier
                                    ((:optional . *) (((:atomic-type-specifier
                                                        ((:name . 1) (((:type-name
                                                                        ()
                                                                        :name bit :source #13#))))
                                                        :source #13#))
                                                      ((:atomic-type-specifier
                                                        ((:name . 1) (((:type-name
                                                                        ()
                                                                        :name t :source #14#))))
                                                        :source #14#))))
                                    :source #12#))))
              :source #11#))
    '(#15=(function #16=(&rest #17=bit &key #18=(#19=a #20=t)))
      t #15# (:function-type-specifier
              ((:parameters . 1) (((:parameter-type-specifier
                                    ((:rest    . 1) (((:atomic-type-specifier
                                                       ((:name . 1) (((:type-name
                                                                       ()
                                                                       :name bit :source #17#))))
                                                       :source #17#)))
                                     (:keyword . *) (((:keyword-parameter-type-specifier
                                                       ((:keyword . 1) (((:keyword
                                                                          ()
                                                                          :name a :source #19#)))
                                                        (:type    . 1) (((:atomic-type-specifier
                                                                          ((:name . 1) (((:type-name
                                                                                          ()
                                                                                          :name t :source #20#))))
                                                                          :source #20#))))
                                                       :source #18#))))
                                    :source #16#))))
              :source #15#))))

(test values-type-specifier
  "Smoke test for the `values-type-specifier' rule."
  (rule-test-cases ((syn::values-type-specifier syn::type-specifiers))
    ;; Invalid syntax
    '(#1=1   nil #1# nil)
    '(#2=bit nil #2# nil)
    '((values #3=*)
      :fatal #3# "the type specifier * is not allowed within a VALUES type specifier")
    '((values &optional #4=*)
      :fatal #4# "the type specifier * is not allowed within a VALUES type specifier")
    '((values . #5=(&allow-other-keys))
      :fatal #5# "must be of the form (values REQUIRED* [&optional OPTIONAL*] [&rest REST])")
    '((values &rest)
      :fatal nil #| FIXME should be #6# |# "type specifier must follow &REST")
    '((values . #6=(&rest bit bit))
      :fatal #6# "must be of the form (values REQUIRED* [&optional OPTIONAL*] [&rest REST])")
    ;; Valid syntax
    '(#7=(values)
      t #7# (:values-type-specifier () :source #7#))
    '(#8=(values #9=bit)
      t #8# (:values-type-specifier
             ((:required . *) (((:atomic-type-specifier
                                 ((:name . 1) (((:type-name () :name bit :source #9#))))
                                 :source #9#))))
             :source #8#))
    '(#10=(values #11=bit #12=t)
      t #10# (:values-type-specifier
              ((:required . *) (((:atomic-type-specifier
                                  ((:name . 1) (((:type-name () :name bit :source #11#))))
                                  :source #11#))
                                ((:atomic-type-specifier
                                  ((:name . 1) (((:type-name () :name t :source #12#))))
                                  :source #12#))))
              :source #10#))
    '(#13=(values &optional #14=bit)
      t #13# (:values-type-specifier
              ((:optional . *) (((:atomic-type-specifier
                                  ((:name . 1) (((:type-name () :name bit :source #14#))))
                                  :source #14#))))
              :source #13#))
    '(#15=(values &rest #16=bit)
      t #15# (:values-type-specifier
              ((:rest . 1) (((:atomic-type-specifier
                              ((:name . 1) (((:type-name () :name bit :source #16#))))
                              :source #16#))))
              :source #15#))))

(test type-specifier
  "Smoke test for the `type-specifier' rule."
  (rule-test-cases ((syn::type-specifier syn::type-specifiers))
    '(#1=1        nil    #1# nil)
    '((#2=1)      :fatal #2# "must be a class name")
    '(#3=values   :fatal #3# "the symbol VALUES is not a valid type specifier")
    '(#4=(values) :fatal #4# "VALUES type is invalid in this context")
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
