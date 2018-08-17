;;;; declarations.lisp --- Rules for parsing declarations.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

(parser:defrule declaration-arguments () ; TODO
    (<- arguments)
  (bp:node* (:declaration-arguments :arguments arguments)))

(parser:defrule declaration ()
    (list* (:guard kind symbolp) (<- arguments (declaration-arguments)))
  (bp:node* (:declaration :kind kind)
    (1 :arguments arguments)))
