;;;; meta-grammar.lisp --- Meta grammar used by the expression-grammar module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.expression-grammar)

(parser.packrat:defgrammar meta-grammar
  (:class   sexp:sexp-grammar)
  (:cached? nil)
  (:use     sexp::meta-grammar))

(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule once (context)  ; TODO define-macro-rule
    (:compose (:transform (list 'once variable expression)
                (let ((temp (gensym)))
                  `(<- ,variable (guard (must (:transform
                                                 (<- ,temp ,expression)
                                               (if ,variable (:fail) ,temp))
                                              "option must not be repeated")
                                        identity))))
              (base::expression context)))

(parser.packrat:defrule base::expression (context)
  (or (once context)

      ((base::expression sexp::meta-grammar) context)))
