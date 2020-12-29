;;;; declarations.lisp --- Rules for parsing declarations.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:defgrammar declarations
  (:class syntax.expression-grammar::expression-grammar)
  (:use names
        type-specifiers))

(parser:in-grammar declarations)

;;; `declaration'

(parser:defrule declaration-arguments () ; TODO
    (list (* (<<- arguments)))
  arguments #+later (bp:node* (:declaration-arguments :arguments arguments)))

(parser:defrule declaration ()
    (or (list (<- kind (or 'ignore 'ignorable 'dynamic-extent))
              (* (<<- arguments (bound-declaration-reference!))))

        (list (<- kind 'special)
              (* (<<- arguments (variable-name!))))

        (list (<- kind (or 'type 'ftype))
              (:must (<<- arguments (type-specifier!)) "must be a type specifier")
              (* (<<- arguments (variable-name!))))

        (list (<- kind 'optimize)
              (* (<<- arguments (:must (or (optimization-quality)
                                           (list (optimization-quality!)
                                                 (optimization-level!)))
                                       "must be a quality name or a list (QUALITY {0,1,2,3})"))))

        (list* (:must (guard kind symbolp) "declaration kind must be a symbol")
               (<- arguments (declaration-arguments))))
  (list kind (nreverse arguments))
  #+later (bp:node* (:declaration :kind kind)
    (1 :arguments arguments)))

(parser:defrule declaration! ()
    (:must (declaration) "must be a declaration"))

;;; Standard declarations

(parser:defrule bound-declaration-reference! ()
  (:must (or (variable-name) (function-reference))
         "must be a variable name or function name"))

(defrule optimization-quality ()
  (or 'speed 'debug 'safety 'space 'compilation-speed (guard symbolp)))

(defrule optimization-quality! ()
  (:must (optimization-quality) "must be an optimization quality name"))

(defrule optimization-level ()
  (or 0 1 2 3))

(defrule optimization-level! ()
  (:must (optimization-level) "must be an optimization level, i.e. 0, 1, 2 or 3"))
