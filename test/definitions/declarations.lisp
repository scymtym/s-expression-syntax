;;;; declarations.lisp --- Tests for declaration rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.declarations
  :in :s-expression-syntax)

(test declaration
  "Smoke test for the `declaration' rule."
  (rule-test-cases ((declaration syn::special-operators))
    '((#1=1) :fatal #1# "declaration identifier must be a symbol")
    ;; `type' declaration
    '((type #2=1 a) :fatal #2# "must be a type specifier")
    '(#3=(type #4=bit #5=a)
      t #3# (:declaration
             ((:argument . *) (((:atomic-type-specifier
                                 ((:name . 1) (((:type-name () :name bit :source #4#))))
                                 :source #4#))
                               ((:variable-name () :name a :source #5#))))
             :kind type :source #3#))
    ;; `optimize' declaration
    '((optimize #6=1)         :fatal #6# "must be a quality name or a list (QUALITY {0,1,2,3})")
    '((optimize (speed #7=5)) :fatal #7# "must be an optimization value, that is 0, 1, 2 or 3")
    '(#8=(optimize #9=speed #10=debug)
      t #8# (:declaration
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #9# :value nil :source #9#))
                               ((:optimization-specification
                                 ()
                                 :quality #10# :value nil :source #10#))))
             :kind optimize :source #8#))
    '(#11=(optimize #12=(#13=speed #14=1))
      t #9# (:declaration
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #13# :value #14# :source #12#))))
             :kind optimize :source #11#))
    ;; `ignore' declaration
    '(#15=(ignore #16=a (function #17=b))
      t #15# (:declaration
             ((:argument . *) (((:variable-name () :name a :source #16#))
                               ((:function-name () :name b :source #17#))))
             :kind ignore :source #15#))))
