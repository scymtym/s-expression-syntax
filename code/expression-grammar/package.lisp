;;;; package.lisp --- Package definition for the expression-grammar module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.expression-grammar
  (:use
   #:cl
   #:let-plus)

  (:local-nicknames
   (#:a    #:alexandria)

   (#:env  #:parser.packrat.environment)

   (#:exp  #:parser.packrat.expression)

   (#:c    #:parser.packrat.compiler)

   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence)
   (#:sexp #:parser.packrat.grammar.sexp))

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<- #:guard #:must)

  (:export
   #:*client*

   #:natural?
   #:naturalize)

  ;; Symbol operations
  (:export
   #:symbol-name*
   #:symbol-package*
   #:package-name*)

  ;; List operations
  (:export
   #:listp*
   #:null*
   #:first*
   #:rest*)

  ;; Comparisons
  (:export
   #:typep*
   #:equal*
   #:eql*)

  ;; Grammar rule macros
  (:export
   #:once

   #:option  #:option*
   #:poption #:poption*))
