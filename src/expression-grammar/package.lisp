;;;; package.lisp --- Package definition for the expression-grammar module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:syntax.expression-grammar
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
   #:<- #:guard #:must)

  (:export
   #:*client*

   #:natural?
   #:naturalize

   #:listp*
   #:null*
   #:first*
   #:rest*

   #:equal*
   #:eql*))
