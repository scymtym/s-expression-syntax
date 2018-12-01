(cl:defpackage #:syntax.expression-grammar
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:env  #:parser.packrat.environment)

   (#:exp  #:parser.packrat.expression)

   (#:c    #:parser.packrat.compiler)

   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence))

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
