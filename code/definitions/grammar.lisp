;;;; grammar.lisp --- Grammar for special operators and standard macros.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar special-operators
  (:class eg::expression-grammar)
  (:use names
        lambda-lists
        destructuring-lambda-list
        deftype-lambda-list
        type-specifiers
        forms))
