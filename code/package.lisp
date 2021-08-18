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

   #:component-not-found-error
   #:syntax
   #:name

   #:invalid-syntax-error
   #:syntax
   #:value ; TODO collides with symbol from grammar.base
   #:message)

  ;; Name protocol
  (:export
   #:name)

  ;; Component protocol (extends name protocol)
  (:export
   #:cardinality
   #:evaluation)

  ;; Syntax description protocol (extends name protocol)
  (:export
   #:components
   #:find-component)

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
   #:define-syntax))
