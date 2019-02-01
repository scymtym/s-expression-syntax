;;;; package.lisp --- Package definition for the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:syntax
  (:use
   #:cl
   #:alexandria)

  (:import-from #:parser.packrat.bootstrap
   #:? #:<- #:<<-)

  ;; Conditions
  (:export
   #:syntax-not-found-error
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
