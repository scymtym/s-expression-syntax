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
    '((optimize (speed 5)) :fatal nil "must be an optimization value, that is 0, 1, 2 or 3")
    '(#4=(optimize #5=speed #6=debug)
      t #4# (:declaration
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #5# :value nil :source #5#))
                               ((:optimization-specification
                                 ()
                                 :quality #6# :value nil :source #6#))))
             :kind optimize :source #4#))
    '(#7=(optimize #8=(#9=speed #10=1))
      t #5# (:declaration
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #9# :value #10# :source #8#))))
             :kind optimize :source #7#))
    ;; `ignore' declaration
    '(#11=(ignore #12=a (function #13=b))
      t #11# (:declaration
             ((:argument . *) (((:variable-name () :name a :source #12#))
                               ((:function-name () :name b :source #13#))))
             :kind ignore :source #11#))))
