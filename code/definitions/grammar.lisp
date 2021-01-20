;;;; grammar.lisp --- Grammar for special operator and standard macros.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; Grammar

(parser:defgrammar special-operators
  (:class eg::expression-grammar)
  (:use names
        lambda-lists
        destructuring-lambda-list
        deftype-lambda-list
        type-specifiers
        forms))

;;; Basic rules TODO separate file

(parser:in-grammar special-operators)

(defrule reference ()
    (guard symbolp)
  ; (bp:node* (:reference :name name))
  )

(defrule atom ()
    (guard atom)
  ; (bp:node* (:atom :value atom))
  )
