;;;; declarations.lisp --- Rules for parsing declarations.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar declarations
  (:class eg::expression-grammar)
  (:use names
        type-specifiers))

(parser:in-grammar declarations)

;;; `declaration'

(defrule declaration-arguments () ; TODO
    (list (* (<<- arguments)))
  arguments #+later (bp:node* (:declaration-arguments :arguments arguments)))

(defrule declaration () ; TODO rename to declaration-specifier
    (value (source)
      (or (list (<- kind (or 'ignore 'ignorable 'dynamic-extent))
                (* (<<- arguments (bound-declaration-reference!))))

          (list (<- kind 'special)
                (* (<<- arguments ((variable-name! names)))))

          (list (<- kind 'type)
                (<<- arguments ((type-specifier! type-specifiers)))
                (* (<<- arguments ((variable-name! names)))))

          (list (<- kind 'ftype)
                (<<- arguments ((function-type-specifier! type-specifiers)))
                (* (<<- arguments ((function-name! names)))))

          (list (<- kind (or 'inline 'notinline))
                (* (<<- arguments ((function-name! names)))))

          (list (<- kind 'optimize)
                (* (<<- arguments (optimization-specification!))))

          (list (<- kind 'declaration)
                (* (<<- arguments ((declaration-identifier! names)))))

          (list* (<- kind ((declaration-identifier! names)))
                 (<- arguments (declaration-arguments)))))
  (let ((kind (eg::%naturalize kind)))
    (bp:node* (:declaration :kind kind :source source)
      (* (:argument . *) (nreverse arguments)))))

(defrule declaration! ()
    (must (declaration) "must be a declaration"))

;;; Standard declarations

(defrule bound-declaration-reference! ()
  (must (or (variable-name) (function-reference))
         "must be a variable name or function name"))

(defrule optimization-specification ()
  (or (optimization-quality)
      (list (optimization-quality!)
            (optimization-level!))))

(defrule optimization-specification! ()
  (must (optimization-specification)
         "must be a quality name or a list (QUALITY {0,1,2,3})"))

(defrule optimization-quality ()
  (or 'speed 'debug 'safety 'space 'compilation-speed (guard symbolp)))

(defrule optimization-quality! ()
  (must (optimization-quality) "must be an optimization quality name"))

(defrule optimization-level ()
  (or 0 1 2 3))

(defrule optimization-level! ()
  (must (optimization-level) "must be an optimization level, i.e. 0, 1, 2 or 3"))
