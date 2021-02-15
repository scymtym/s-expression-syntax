;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
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
    '(((declare ()))       :fatal nil "declaration kind must be a symbol")
    '(((declare (type 1))) :fatal nil "must be a type specifier")
    ;; Valid declarations
    '(((declare #7=(ignore #8=a)))
      t nil (((:declaration
               (:argument (((:variable-name () :name a :source #8#))))
               :kind ignore :source #7#))
             ()))
    '(((declare #9=(type bit)))
      t nil (((:declaration (:argument ((bit))) :kind type :source #9#))
             ()))
    '(((declare #10=(type bit #11=a)))
      t nil (((:declaration
               (:argument ((bit)
                           ((:variable-name () :name a :source #11#))))
               :kind type :source #10#))
             ()))
    '(((declare #12=(type bit #13=a #14=b)))
      t nil (((:declaration
               (:argument ((bit)
                           ((:variable-name () :name a :source #13#))
                           ((:variable-name () :name b :source #14#))))
               :kind type :source #12#))
             ()))
    ;; Multiple declarations
    '(((declare #15=(ignore #16=a)) (declare #17=(ignore #18=b)))
      t nil (((:declaration
               (:argument (((:variable-name () :name a :source #16#))))
               :kind ignore :source #15#)
              (:declaration
               (:argument (((:variable-name () :name b :source #18#))))
               :kind ignore :source #17#))
             ()))
    ;; Declarations and forms
    '(((declare #19=(ignore #20=a)) 3 4)
      t nil (((:declaration
               (:argument (((:variable-name () :name a :source #20#))))
               :kind ignore :source #19#))
             (3 4)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."

  (rule-test-cases ((syn::docstring-body syn::special-operators))
    ;; Empty
    '(()                              t nil (nil   () ()))
    ;; Only forms
    '(("foo")                         t nil (nil   () ("foo")))
    ;; Declarations and docstrings
    '(#3=((declare #4=(ignore #5=a)))
      t #3# (nil
             ((:declaration
               (:argument (((:variable-name () :name a :source #5#))))
               :kind ignore :source #4#))
             ()))
    '(#6=((declare #7=(ignore #8=a)) "foo")
      t #6# (nil
             ((:declaration
               (:argument (((:variable-name () :name a :source #8#))))
               :kind ignore :source #7#))
             ("foo")))
    '(#9=("foo" (declare #10=(ignore #11=a)))
      t nil ("foo"
             ((:declaration
               (:argument (((:variable-name () :name a :source #11#))))
               :kind ignore :source #10#))
                                             ()))
    ;; Forms and docstrings
    '((1)                             t nil (nil   ()  (1)))
    '(("foo" 1)                       t nil ("foo" ()  (1)))
    '((1 "foo")                       t nil (nil   ()  (1 "foo")))))
