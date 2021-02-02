;;;; names.lisp --- Rules for different kinds of names.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar names
  (:class eg::expression-grammar))

(parser:in-grammar names)

(defun constant? (name)
  (eq (sb-cltl2:variable-information name) :constant))

(defrule variable-name/unchecked ()
    :any
    ;; (bp:node* (:variable-name :name name))
  )

(defrule variable-name ()
    (and (guard symbolp)
         (not (guard keywordp))
         (not (guard constant?))
         (variable-name/unchecked)))

(defrule variable-name! ()
    (and (must (guard symbolp) "variable name must be a symbol")
         (must (not (guard keywordp)) "variable name must not be a keyword")
         (must (not (guard constant?)) "variable name must not designate a constant")
         (variable-name/unchecked)))

(defrule function-name/symbol ()
    (:guard (typep '(and symbol (not (member t nil))))))

(defrule function-name/symbol! ()
    (must (function-name/symbol) "function name must be symbol"))

(defrule function-name ()
  (or (function-name/symbol)
      (list 'setf (:must (function-name/symbol) "second element of SETF function name must be a symbol"))))

(defrule function-name! ()
    (must (function-name) "must be a function name"))

(defrule class-name ()           ; TODO call this type name?
    (and (guard symbolp) (not (guard null))))

(defrule class-name! ()
    (must (class-name) "must be a class name"))

(defrule slot-name ()
    (variable-name))

(defrule slot-name! ()
    (must (slot-name) "slot name must be a symbol that is a valid variable name"))

;;; References

(defrule function-reference ()
    (list 'function (function-name!)))

(defrule function-reference! ()
    (must (function-reference) "must be a function reference"))