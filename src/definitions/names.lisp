;;;; names.lisp --- Rules for different kinds of names.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:defgrammar names
  (:class syntax.expression-grammar::expression-grammar))

(parser:in-grammar names)

(defun constant? (name)
  (eq (sb-cltl2:variable-information name) :constant))

(parser:defrule variable-name/unchecked ()
    :any
    ;; (bp:node* (:variable-name :name name))
  )

(parser:defrule variable-name ()
    (and (guard symbolp)
         (not (guard keywordp))
         (not (guard constant?))
         (variable-name/unchecked)))

(parser:defrule variable-name! ()
    (and (:must (guard symbolp) "variable name must be a symbol")
         (:must (not (guard keywordp)) "variable name must not be a keyword")
         (:must (not (guard constant?)) "variable name must not designate a constant")
         (variable-name/unchecked)))

(parser:defrule function-name/symbol ()
    (:guard (typep '(and symbol (not (member t nil))))))

(parser:defrule function-name/symbol! ()
    (:must (function-name/symbol) "function name must be symbol"))

(parser:defrule function-name ()
  (or (function-name/symbol)
      (list 'setf (:must (function-name/symbol) "second element of SETF function name must be a symbol"))))

(parser:defrule function-name! ()
    (:must (function-name) "must be a function name"))

(parser:defrule class-name ()           ; TODO call this type name?
    (and (guard symbolp) (not (guard null))))

(parser:defrule class-name! ()
    (:must (class-name) "must be a class-name"))

(parser:defrule slot-name ()
    (variable-name))

(parser:defrule slot-name! ()
    (:must (slot-name) "slot name must be a symbol that is a valid variable name"))

;;; References

(parser:defrule function-reference ()
    (list 'function (function-name!)))

(parser:defrule function-reference! ()
    (:must (function-reference) "must be a function reference"))
