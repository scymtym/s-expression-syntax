;;;; package.lisp --- Package definition for the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.concrete-syntax-tree
  (:use
   #:cl)

  (:local-nicknames
   (#:eg  #:s-expression-syntax.expression-grammar)))
