;;;; grammar.lisp --- TODO.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

;;; Grammar

(parser:defgrammar special-operators
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use lambda-lists)) ; TODO not sure this is good

;;; Basic rules TODO separate file

(parser:in-grammar special-operators)

(parser:defrule variable-name () ; TODO could be used in lambda list grammar
    (:guard (typep '(and symbol
                         (not keyword)
                         (not (member t nil)))))
  ; (bp:node* (:variable-name :name name))
  )

(parser:defrule function-name/symbol ()
    (:guard (typep '(and symbol (not (member t nil))))))

(parser:defrule function-name ()
    (or (function-name/symbol)
        (list 'setf (function-name/symbol))))

(parser:defrule reference ()
    (:guard symbolp)
  ; (bp:node* (:reference :name name))
  )

(parser:defrule atom ()
    (:guard atom)
  ; (bp:node* (:atom :value atom))
  )
