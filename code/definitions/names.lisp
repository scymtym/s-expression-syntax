;;;; names.lisp --- Rules for different kinds of names.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar names
  (:class eg::expression-grammar))

(parser:in-grammar names)

(defun constant? (name)
  (eq (sb-cltl2:variable-information name) :constant))

(defrule constant ()
    (structure 'symbol
               (symbol-name symbol-name)
               (symbol-package (structure 'package (package-name package-name))))
  (a:if-let ((package (find-package package-name)))
    (multiple-value-bind (symbol found?) (find-symbol symbol-name package)
      (unless (and found? (constant? symbol))
        (:fail)))
    (:fail)))

(defrule variable-name/unchecked ()
    (value (source)
      symbol)
  (bp:node* (:variable-name :name (eg::%naturalize symbol) :source source)))

(defrule variable-name ()
  (and (guard (typep 'symbol))
       (not (guard (typep 'keyword)))
       (not (constant))
       (variable-name/unchecked)))

(defrule variable-name! ()
  (and (must (guard (typep 'symbol)) "variable name must be a symbol")
       (must (not (guard (typep 'keyword))) "variable name must not be a keyword")
       (must (not (constant)) "variable name must not designate a constant")
       (variable-name/unchecked)))

(defrule function-name/symbol/raw ()
  (and (guard (typep 'symbol))
       (not 't) (not 'nil)))

(defrule function-name/symbol ()
    (value (source)
      (<- symbol (function-name/symbol/raw)))
  (bp:node* (:function-name :name (eg::%naturalize symbol) :source source)))

(defrule function-name/symbol! ()
  (must (function-name/symbol) "function name must be symbol"))

(defrule function-name/setf ()
    (value (source)
      (list 'setf (must (<- symbol (function-name/symbol/raw))
                        "second element of SETF function name must be a symbol")))
  (bp:node* (:function-name :name   `(setf ,(eg::%naturalize symbol))
                            :source source)))

(defrule function-name ()
  (or (function-name/symbol)
      (function-name/setf)))

(defrule function-name! ()
  (must (function-name) "must be a function name"))

(defrule class-name ()           ; TODO call this type name?
    (value (source)
      (and (guard (typep 'symbol)) (not 'nil) symbol))
  (bp:node* (:type-name :name (eg::%naturalize symbol) :source source)))

(defrule class-name! ()
  (must (class-name) "must be a class name"))

(defrule slot-name ()
  (variable-name))

(defrule slot-name! ()
  (must (slot-name) "slot name must be a symbol that is a valid variable name"))

;;; References

(defrule function-reference ()
    (list 'function (<- name (function-name!)))
  name)

(defrule function-reference! ()
  (must (function-reference) "must be a function reference"))
