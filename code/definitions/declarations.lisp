;;;; declarations.lisp --- Rules for parsing declarations.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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

(defrule declaration-specifier ()
    (value (source)
      (or (list (<- identifier (or 'ignore 'ignorable 'dynamic-extent))
                (* (<<- arguments (bound-declaration-reference!))))

          (list (<- identifier 'special)
                (* (<<- arguments ((variable-name! names)))))

          (list (<- identifier 'type)
                (<<- arguments ((type-specifier! type-specifiers)))
                (* (<<- arguments ((variable-name! names)))))

          (list (<- identifier 'ftype)
                (<<- arguments ((function-type-specifier! type-specifiers)))
                (* (<<- arguments ((function-name! names)))))

          (list (<- identifier (or 'inline 'notinline))
                (* (<<- arguments ((function-name! names)))))

          (list (<- identifier 'optimize)
                (* (<<- arguments (optimization-specification!))))

          (list (<- identifier 'declaration)
                (* (<<- arguments ((declaration-identifier! names)))))

          (list* (<- identifier ((declaration-identifier! names)))
                 (<- arguments (declaration-arguments)))))
  (let ((identifier (eg::%naturalize identifier)))
    (bp:node* (:declaration-specifier :kind identifier :source source)
      (* (:argument . *) (nreverse arguments)))))

(defrule declaration-specifier! ()
  (must (declaration-specifier) "must be a declaration specifier"))

;;; Standard declarations

(defrule bound-declaration-reference! ()
  (must (or (variable-name) (function-reference))
        "must be a variable name or function name"))

(defrule optimization-specification ()
    (value (source)
      (or (<- quality (optimization-quality))
          (list (<- quality (optimization-quality!))
                (<- value   (optimization-value!)))))
  (bp:node* (:optimization-specification :quality quality
                                         :value   value
                                         :source  source)))

(defrule optimization-specification! ()
  (must (optimization-specification)
         "must be a quality name or a list (QUALITY {0,1,2,3})"))

(defrule optimization-quality ()
  (or 'speed 'debug 'safety 'space 'compilation-speed (guard symbolp)))

(defrule optimization-quality! ()
  (must (optimization-quality) "must be an optimization quality name"))

(defrule optimization-value ()
  (or 0 1 2 3))

(defrule optimization-value! ()
  (must (optimization-value) "must be an optimization value, that is 0, 1, 2 or 3"))
