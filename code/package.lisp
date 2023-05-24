;;;; package.lisp --- Package definition for the syntax system.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax
  (:use
   #:cl)

  (:local-nicknames
   (#:a  #:alexandria)

   (#:parser #:parser.packrat)
   (#:bp     #:architecture.builder-protocol)

   (#:eg #:s-expression-syntax.expression-grammar))

  (:import-from #:parser.packrat
   #:defrule)

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<- #:guard #:must
   #:value)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq #:? #:bounds) ; bounds is for format

  ;; Conditions
  (:export
   #:syntax-not-found-error
   #:name

   #:part-not-found-error
   #:syntax
   #:name

   #:invalid-syntax-error
   #:syntax
   #:expression
   #:message)

  ;; Name protocol
  (:export
   #:name)

  ;; Part protocol (extends name protocol)
  (:export
   #:cardinality
   #:evaluation)

  ;; Syntax description protocol (extends name protocol)
  (:export
   #:parts
   #:find-part)

  ;; Syntax lookup protocol
  (:export
   #:syntaxes
   #:syntaxes/alist

   #:find-syntax                    ; also `setf'
   #:ensure-syntax)

  ;; Parse protocol
  (:export
   #:classify
   #:parse)

  ;; Macros
  (:export
   #:define-syntax)

  ;; Lambda lists
  (:export
   #:required-parameter
   #:optional-parameter
   #:keyword-parameter
   #:aux-parameter

   #:ordinary-lambda-list
   #:generic-function-lambda-list
   #:specialized-parameter
   #:specialized-lambda-list
   #:pattern
   #:destructuring-lambda-list
   #:deftype-lambda-list)

  ;; Bindings
  (:export
   #:value-binding
   #:local-function-binding
   #:local-macro-function-binding
   #:symbol-macro-binding)

  ;; Pseudo operators
  (:export
   #:variable-reference
   #:application
   #:self-evaluating)

  ;; Special operators
  (:export
   #:lambda-expression)

  ;; Definition macros
  (:export
   #:slot-description

   #:slot-specifier

   #:condition-slot-specifier
   #:condition-report

   #:method-description

   #:import-from
   #:local-nicknames
   #:local-nickname)

  ;; Control macros
  (:export
   #:cond-clause

   #:case-normal-clause
   #:case-otherwise-clause

   #:typecase-normal-clause
   #:typecase-otherwise-clause

   #:handler-binding
   #:no-error-clause
   #:handler-clause

   #:restart-binding
   #:restart-clause))
