;;;; package.lisp --- Package definition for tests of the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:syntax.concrete-syntax-tree.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:syntax.concrete-syntax-tree.test)

(def-suite :syntax.concrete-syntax-tree)

(defun run-tests ()
  (run! :syntax.concrete-syntax-tree))
