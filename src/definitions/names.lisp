;;;; names.lisp --- Rules for different kinds of names.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

(parser:defrule variable-name () ; TODO could be used in lambda list grammar
    (:guard (typep '(and symbol
                         (not keyword)
                         (not (member t nil))))))

(parser:defrule function-name/symbol ()
    (:guard (typep '(and symbol (not (member t nil))))))

(parser:defrule function-name ()
    (or (function-name/symbol)
        (list 'setf (function-name/symbol))))

(parser:defrule class-name ()           ; TODO call this type name?
    (and (:guard symbolp) (not (:guard null))))

(parser:defrule slot-name ()
    (:guard symbolp))
