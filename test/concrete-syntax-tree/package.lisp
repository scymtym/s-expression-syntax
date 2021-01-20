;;;; package.lisp --- Package definition for tests of the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.concrete-syntax-tree.test
  (:use
   #:cl

   #:fiveam)

  (:local-nicknames
   (#:syn #:s-expression-syntax)
   (#:eg  #:s-expression-syntax.expression-grammar))

  (:export
   #:run-tests))

(cl:in-package #:s-expression-syntax.concrete-syntax-tree.test)

(def-suite :s-expression-syntax.concrete-syntax-tree)

(defun run-tests ()
  (run! :s-expression-syntax.concrete-syntax-tree))
