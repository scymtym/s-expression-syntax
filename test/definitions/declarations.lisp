;;;; declarations.lisp --- Tests for declaration rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.declarations
  :in :s-expression-syntax)

(test declaration
  "Smoke test for the `declaration' rule."

  (rule-test-cases ((declaration syn::special-operators))
    '((1) :fatal nil "declaration kind must be a symbol")

    '((type 1 a) :fatal nil "must be a type specifier")
    '(#3=(type bit #4=a)
      t #3# (:declaration
             (:argument ((bit)
                         ((:variable-name () :name a :source #4#))))
             :kind type :source #3#))

    '((optimize 1)         :fatal nil "must be a quality name or a list (QUALITY {0,1,2,3})")
    '((optimize (speed 5)) :fatal nil "must be an optimization level, i.e. 0, 1, 2 or 3")
    '(#6=(optimize speed debug)
      t #6# (:declaration
             (:argument ((speed) (debug)))
             :kind optimize :source #6#))
    '(#7=(optimize (speed 1))
      t #7# (:declaration
             (:argument (((speed 1))))
             :kind optimize :source #7#))

    '(#8=(ignore #9=a #10=#'b)
      t #8# (:declaration
             (:argument (((:variable-name () :name a :source #9#))
                         ((:function-name () :name b :source #10#))))
             :kind ignore :source #8#))))
