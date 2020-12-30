;;;; declarations.lisp --- Tests for declaration rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.declarations
  :in :syntax)

(test declaration
  "Smoke test for the `declaration' rule."

  (rule-test-cases ((declaration syntax::special-operators))
    `((1)                    :fatal nil "declaration kind must be a symbol")

    `((type 1 a)             :fatal nil "must be a type specifier")
    `((type bit a)           t      nil (type (bit a)))

    `((optimize 1)           :fatal nil "must be a quality name or a list (QUALITY {0,1,2,3})")
    `((optimize (speed 5))   :fatal nil "must be an optimization level, i.e. 0, 1, 2 or 3")
    `((optimize speed debug) t      nil (optimize (speed debug)))
    `((optimize (speed 1))   t      nil (optimize ((speed 1))))

    `((ignore a #'b)         t      nil (ignore (a (function b))))))
