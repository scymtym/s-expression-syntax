;;;; declarations.lisp --- Tests for declaration rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.declarations
  :in :s-expression-syntax)

(test declaration-specifier
  "Smoke test for the `declaration-specifier' rule."
  (rule-test-cases ((syn::declaration-specifier syn::special-operators))
    ;; Invalid syntax
    '((#1=1) :fatal #1# "declaration identifier must be a symbol")
    ;; `type' declaration
    '((type . #2=())
      :fatal #2# "type specifier must follow TYPE declaration identifier")
    '((type #3=1 a)
      :fatal #3# "must be a type specifier")
    '(#4=(type #5=bit #6=a)
      t #4# (:declaration-specifier
             ((:argument . *) (((:atomic-type-specifier
                                 ((:name . 1) (((:type-name () :name bit :source #5#))))
                                 :source #5#))
                               ((:variable-name () :name a :source #6#))))
             :kind type :source #4#))
    ;; `ftype' declaration
    '((ftype . #7=())
      :fatal #7# "function type specifier must follow FTYPE declaration identifier")
    '((ftype #8=1 a)
      :fatal #8# "must be a function type specifier")
    ;; `optimize' declaration
    '((optimize #9=1)
      :fatal #9# "must be a quality name or a list (QUALITY {0,1,2,3})")
    '((optimize (speed #10=5))
      :fatal #10# "must be an optimization value, that is 0, 1, 2 or 3")
    '(#11=(optimize #12=speed #13=debug)
      t #11# (:declaration-specifier
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #12# :value nil :source #12#))
                               ((:optimization-specification
                                 ()
                                 :quality #13# :value nil :source #13#))))
             :kind optimize :source #11#))
    '(#14=(optimize #15=(#16=speed #17=1))
      t #12# (:declaration-specifier
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #16# :value #17# :source #15#))))
             :kind optimize :source #14#))
    ;; `ignore' declaration
    '(#18=(ignore #19=a (function #20=b))
      t #18# (:declaration-specifier
             ((:argument . *) (((:variable-name () :name a :source #19#))
                               ((:function-name () :name b :source #20#))))
             :kind ignore :source #18#))
    ;; `declaration' declaration
    '((declaration #21=1)
      :fatal #21# "declaration identifier must be a symbol")
    '(#22=(declaration)
      t #22# (:declaration-specifier () :kind declaration :source #22#))
    '(#23=(declaration #24=my-declaration)
      t #23# (:declaration-specifier
               ((:argument . *) (((:declaration-identifier
                                   ()
                                   :name my-declaration :source #24#))))
               :kind declaration :source #23#))))
