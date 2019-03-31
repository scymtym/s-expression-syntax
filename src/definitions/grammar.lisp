;;;; grammar.lisp --- Grammar for special operator and standard macros.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

;;; Grammar

(parser:defgrammar special-operators
  (:class syntax.expression-grammar::expression-grammar)
  (:use lambda-lists)) ; TODO not sure this is good

;;; Basic rules TODO separate file

(parser:in-grammar special-operators)

(parser:defrule reference ()
    (:guard symbolp)
  ; (bp:node* (:reference :name name))
  )

(parser:defrule atom ()
    (:guard atom)
  ; (bp:node* (:atom :value atom))
  )
