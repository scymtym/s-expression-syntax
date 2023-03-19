;;;; pseudo-operators.lisp --- Syntax elements that are not special forms or macros.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; The syntax elements defined here handle most of the three cases
;;;; of form evaluation:
;;;; 1 Symbols as forms
;;;; 2 Conses as forms (excluding special forms and macros)
;;;; 3 Self-evaluating objects

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Pseudo-operator variable reference
;;;
;;; Non-keyword symbols semantically are some sort of variable
;;; reference. Whether the reference is to a special or lexical
;;; variable or whether the name is actually bound to a symbol macro
;;; cannot be determined syntactically.

(define-syntax variable-reference
    (and (guard (typep 'symbol))
         (<- name ((variable-name/unchecked names))))
  ((name 1)))

;;; Pseudo-operator "application"
;;;
;;; This handles the two cases
;;;
;;;   (FUNCTION-NAME ARGUMENT1 ARGUMENT2 ...)
;;;   ((lambda (LAMBDA-LIST) BODY) ARGUMENT1 ARGUMENT2 ...)

(define-syntax application
    (list (or (<- function-name ((function-name/symbol names)))
              (must (<- function (lambda-expression))
                    "must be a symbol naming a function or a lambda expression"))
          (* (<<- argument ((form! forms)))))
  ((function-name ?)
   (function      ? :evaluation :compound)
   (argument      * :evaluation t)))

;;; Pseudo-operator "self evaluating"
;;;
;;; Anything that is not a variable reference, a function or macro
;;; application or a special form is self-evaluating.

(define-syntax self-evaluating
    (<- value (and (not (guard (typep 'cons)))
                   (not (guard (typep 'symbol)))
                   ((unparsed-expression forms) :self-evaluating)))
  ((value 1)))
