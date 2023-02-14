;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
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
    '(((declare ()))       :fatal nil "must be a declaration")
    '(((declare (1)))      :fatal nil "declaration identifier must be a symbol")
    '(((declare (type 1))) :fatal nil "must be a type specifier")
    ;; Valid declarations
    '(((declare #1=(ignore #2=a)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #2#))))
               :kind ignore :source #1#))
             ()))
    '(((declare #3=(type #4=bit)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #4#))))
                                   :source #4#))))
               :kind type :source #3#))
             ()))
    '(((declare #5=(type #6=bit #7=a)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #6#))))
                                   :source #6#))
                                 ((:variable-name () :name a :source #7#))))
               :kind type :source #5#))
             ()))
    '(((declare #8=(type #9=bit #10=a #11=b)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #9#))))
                                   :source #9#))
                                 ((:variable-name () :name a :source #10#))
                                 ((:variable-name () :name b :source #11#))))
               :kind type :source #8#))
             ()))
    ;; Multiple declarations
    '(((declare #12=(ignore #13=a)) (declare #14=(ignore #15=b)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #13#))))
               :kind ignore :source #12#)
              (:declaration
               ((:argument . *) (((:variable-name () :name b :source #15#))))
               :kind ignore :source #14#))
             ()))
    ;; Declarations and forms
    '(((declare #16=(ignore #17=a)) 3 4)
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #17#))))
               :kind ignore :source #16#))
             (3 4)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."
  (rule-test-cases ((syn::docstring-body syn::special-operators))
    ;; Empty
    '(()                              t nil (nil   () ()))
    ;; Only forms
    '(("foo")                         t nil (nil   () ("foo")))
    ;; Declarations and docstrings
    '(#1=((declare #2=(ignore #3=a)))
      t #1# (nil
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #3#))))
               :kind ignore :source #2#))
             ()))
    '(#4=((declare #5=(ignore #6=a)) "foo")
      t #4# (nil
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #6#))))
               :kind ignore :source #5#))
             ("foo")))
    '(("foo" (declare #7=(ignore #8=a)))
      t nil ("foo"
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #8#))))
               :kind ignore :source #7#))
                                             ()))
    ;; Forms and docstrings
    '((1)                             t nil (nil   ()  (1)))
    '(("foo" 1)                       t nil ("foo" ()  (1)))
    '((1 "foo")                       t nil (nil   ()  (1 "foo")))))
