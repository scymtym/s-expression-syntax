;;;; declarations.lisp --- Tests for declaration rules.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.declarations
  :in :s-expression-syntax)

(test declaration
  "Smoke test for the `declaration' rule."
  (rule-test-cases ((declaration syn::special-operators))
    '((1) :fatal nil "declaration identifier must be a symbol")
    ;; `type' declaration
    '((type 1 a) :fatal nil "must be a type specifier")
    '(#1=(type #2=bit #3=a)
      t #1# (:declaration
             ((:argument . *) (((:atomic-type-specifier
                                 ((:name . 1) (((:type-name () :name bit :source #2#))))
                                 :source #2#))
                               ((:variable-name () :name a :source #3#))))
             :kind type :source #1#))
    ;; `optimize' declaration
    '((optimize 1)         :fatal nil "must be a quality name or a list (QUALITY {0,1,2,3})")
    '((optimize (speed 5)) :fatal nil "must be an optimization level, i.e. 0, 1, 2 or 3")
    '(#4=(optimize speed debug)
      t #4# (:declaration
             ((:argument . *) ((speed) (debug)))
             :kind optimize :source #4#))
    '(#5=(optimize (speed 1))
      t #5# (:declaration
             ((:argument . *) (((speed 1))))
             :kind optimize :source #5#))
    ;; `ignore' declaration
    '(#6=(ignore #7=a (function #8=b))
      t #6# (:declaration
             ((:argument . *) (((:variable-name () :name a :source #7#))
                               ((:function-name () :name b :source #8#))))
             :kind ignore :source #6#))))
