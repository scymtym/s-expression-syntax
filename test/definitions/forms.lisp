;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(in-suite :s-expression-syntax)

(test body
  "Smoke test for the `body' rule."
  (rule-test-cases ((syn::body syn::special-operators))
    '(()                      t      nil (() ()))
    ;; No declarations
    '((1)                     t      nil (() (1)))
    '((1 2)                   t      nil (() (1 2)))
    ;; Invalid declarations
    '(((declare #1=1))        :fatal #1# "must be a declaration")
    '(((declare #2=()))       :fatal #2# "must be a declaration")
    '(((declare (#3=1)))      :fatal #3# "declaration identifier must be a symbol")
    '(((declare (type #4=1))) :fatal #4# "must be a type specifier")
    ;; Valid declarations
    '(((declare #5=(ignore #6=a)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #6#))))
               :kind ignore :source #5#))
             ()))
    '(((declare #7=(type #8=bit)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #8#))))
                                   :source #8#))))
               :kind type :source #7#))
             ()))
    '(((declare #9=(type #10=bit #11=a)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #10#))))
                                   :source #10#))
                                 ((:variable-name () :name a :source #11#))))
               :kind type :source #9#))
             ()))
    '(((declare #12=(type #13=bit #14=a #15=b)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #13#))))
                                   :source #13#))
                                 ((:variable-name () :name a :source #14#))
                                 ((:variable-name () :name b :source #15#))))
               :kind type :source #12#))
             ()))
    ;; Multiple declarations
    '(((declare #16=(ignore #17=a)) (declare #18=(ignore #19=b)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #17#))))
               :kind ignore :source #16#)
              (:declaration
               ((:argument . *) (((:variable-name () :name b :source #19#))))
               :kind ignore :source #18#))
             ()))
    ;; Declarations and forms
    '(((declare #20=(ignore #21=a)) 3 4)
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #21#))))
               :kind ignore :source #20#))
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
