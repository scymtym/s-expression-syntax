;;;; package.lisp --- Package definition for the format-control module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.format-control
  (:use
   #:cl)

  (:local-nicknames
   (#:a      #:alexandria)

   (#:parser #:parser.packrat)
   (#:bp     #:architecture.builder-protocol))

  (:import-from #:parser.packrat
                #:defrule)

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<- #:guard #:must)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq #:? #:bounds) ; bounds is for format

  ;; Grammar and rules
  (:export
   #:format-control

   #:at-and-colon
   #:numeric-argument
   #:character-argument

   #:directive-character
   #:directive-newline
   #:directive-fresh-line
   #:directive-page
   #:directive-tilde

   #:directive-conditional-newline
   #:directive-logical-block
   #:directive-indent
   #:directive-call-function
   #:directive-goto
   #:directive-conditional
   #:directive-iteration
   #:directive-recursive
   #:directive-case-conversion
   #:directive-clause-separator
   #:directive-escape
   #:directive-ignored-newline
   #:directive-aesthetic
   #:directive-standard
   #:directive-write

   #:format-control))
