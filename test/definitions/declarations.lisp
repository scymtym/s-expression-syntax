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
    '(((#4=1) x)
      :fatal #4# "must be a type name")
    '(#5=(type #6=bit #7=a)
      t #5# (:declaration-specifier
             ((:argument . *) (((:atomic-type-specifier
                                 ((:name . 1) (((:type-name () :name bit :source #6#))))
                                 :source #6#))
                               ((:variable-name () :name a :source #7#))))
             :kind type :source #5#))
    '(#8=(#9=(#10=integer #11=1) #12=x)
      t #8# (:declaration-specifier
             ((:argument . *) (((:compound-type-specifier
                                 ((:name     . 1) (((:type-name () :name integer :source #10#)))
                                  (:argument . *) (((:subsidiary-item () :value 1 :source #11#))))
                                 :source #9#))
                               ((:variable-name () :name x :source #12#))))
             :kind type :source #8#))
    ;; `ftype' declaration
    '((ftype . #13=())
      :fatal #13# "function type specifier must follow FTYPE declaration identifier")
    '((ftype #14=1 a)
      :fatal #14# "must be a function type specifier")
    ;; `optimize' declaration
    '((optimize #15=1)
      :fatal #15# "must be a quality name or a list (QUALITY {0,1,2,3})")
    '((optimize (speed #16=5))
      :fatal #16# "must be an optimization value, that is 0, 1, 2 or 3")
    '(#17=(optimize #18=speed #19=debug)
      t #17# (:declaration-specifier
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #18# :value nil :source #18#))
                               ((:optimization-specification
                                 ()
                                 :quality #19# :value nil :source #19#))))
             :kind optimize :source #17#))
    '(#20=(optimize #21=(#22=speed #23=1))
      t #18# (:declaration-specifier
             ((:argument . *) (((:optimization-specification
                                 ()
                                 :quality #22# :value #23# :source #21#))))
             :kind optimize :source #20#))
    ;; `ignore' declaration
    '(#24=(ignore #25=a (function #26=b))
      t #24# (:declaration-specifier
             ((:argument . *) (((:variable-name () :name a :source #25#))
                               ((:function-name () :name b :source #26#))))
             :kind ignore :source #24#))
    ;; `declaration' declaration
    '((declaration #27=1)
      :fatal #27# "declaration identifier must be a symbol")
    '(#28=(declaration)
      t #28# (:declaration-specifier () :kind declaration :source #28#))
    '(#29=(declaration #30=my-declaration)
      t #29# (:declaration-specifier
               ((:argument . *) (((:declaration-identifier
                                   ()
                                   :name my-declaration :source #30#))))
               :kind declaration :source #29#))))
