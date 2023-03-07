;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(in-suite :s-expression-syntax)

(test body
  "Smoke test for the `body' rule."
  (rule-test-cases ((syn::body syn::forms))
    ;; Invalid declarations
    '(((declare #1=1))        :fatal #1# "must be a declaration")
    '(((declare #2=()))       :fatal #2# "must be a declaration")
    '(((declare (#3=1)))      :fatal #3# "declaration identifier must be a symbol")
    '(((declare (type #4=1))) :fatal #4# "must be a type specifier")
    ;; Empty
    '(() t nil (() ()))
    ;; No declarations
    '((#5=1)      t nil (() (#5#)))
    '((#6=1 #7=2) t nil (() (#6# #7#)))
    ;; Valid declarations
    '(((declare #8=(ignore #9=a)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #9#))))
               :kind ignore :source #8#))
             ()))
    '(((declare #10=(type #11=bit)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #11#))))
                                   :source #11#))))
               :kind type :source #10#))
             ()))
    '(((declare #12=(type #13=bit #14=a)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #13#))))
                                   :source #13#))
                                 ((:variable-name () :name a :source #14#))))
               :kind type :source #12#))
             ()))
    '(((declare #15=(type #16=bit #17=a #18=b)))
      t nil (((:declaration
               ((:argument . *) (((:atomic-type-specifier
                                   ((:name . 1) (((:type-name () :name bit :source #16#))))
                                   :source #16#))
                                 ((:variable-name () :name a :source #17#))
                                 ((:variable-name () :name b :source #18#))))
               :kind type :source #15#))
             ()))
    ;; Multiple declarations
    '(((declare #19=(ignore #20=a)) (declare #21=(ignore #22=b)))
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #20#))))
               :kind ignore :source #19#)
              (:declaration
               ((:argument . *) (((:variable-name () :name b :source #22#))))
               :kind ignore :source #21#))
             ()))
    ;; Declarations and forms
    '(((declare #23=(ignore #24=a)) #25=3 #26=4)
      t nil (((:declaration
               ((:argument . *) (((:variable-name () :name a :source #24#))))
               :kind ignore :source #23#))
             (#25# #26#)))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."
  (rule-test-cases ((syn::docstring-body syn::forms))
    ;; Empty
    '(() t nil (nil () ()))
    ;; Only forms
    '((#1="foo") t nil (nil () (#1#)))
    ;; Declarations and docstrings
    '(#2=((declare #3=(ignore #4=a)))
      t #2# (nil
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #4#))))
               :kind ignore :source #3#))
             ()))
    '(#5=((declare #6=(ignore #7=a)) #8="foo")
      t #5# (nil
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #7#))))
               :kind ignore :source #6#))
             (#8#)))
    '((#9="foo" (declare #10=(ignore #11=a)))
      t nil (#9#
             ((:declaration
               ((:argument . *) (((:variable-name () :name a :source #11#))))
               :kind ignore :source #10#))
             ()))
    ;; Forms and docstrings
    '((#12=1)           t nil (nil  () (#12#)))
    '((#13="foo" #14=1) t nil (#13# () (#14#)))
    '((#15=1 #16="foo") t nil (nil  () (#15# #16#)))))
