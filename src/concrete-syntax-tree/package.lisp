;;;; package.lisp --- Package definition for the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:syntax.concrete-syntax-tree
  (:use
   #:cl)

  (:local-nicknames
   (#:eg  #:syntax.expression-grammar)))
