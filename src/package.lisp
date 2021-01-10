;;;; package.lisp --- Package definition for the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:syntax
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria))

  (:import-from #:parser.packrat
   #:defrule)

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<- #:guard #:must)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq #:? #:bounds) ; bounds is for format

  ;; Conditions
  (:export
   #:syntax-not-found-error
   #:name

   #:component-not-found
   #:syntax
   #:name

   #:invalid-syntax-error
   #:syntax
   #:value)

  ;; Component protocol
  (:export
   #:name
   #:cardinality
   #:evaluation)

  ;; Syntax protocol
  (:export
   #:parse

   #:find-component)

  ;; Syntax lookup protocol
  (:export
   #:syntaxes
   #:syntaxes/alist

   #:find-syntax                    ; also `setf'
   #:ensure-syntax)

  ;; Macros
  (:export
   #:define-syntax)

  (:local-nicknames
   (#:parser #:parser.packrat)
   (#:bp     #:architecture.builder-protocol)))
