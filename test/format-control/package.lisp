;;;; package.lisp --- Package definition for tests of the s-expression-syntax.format system.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.format-control.test
  (:use
   #:cl

   #:fiveam

   #:s-expression-syntax.format-control)

  (:local-nicknames
   (#:a #:alexandria))

  (:export
   #:run-tests))

(cl:in-package #:s-expression-syntax.format-control.test)

(def-suite :s-expression-syntax.format-control)

(defun run-tests ()
  (run! :s-expression-syntax.format-control))

(defmacro rule-test-cases (((rule grammar) &rest arguments) &body cases)
  `(architecture.builder-protocol:with-builder ('list)
     (s-expression-syntax.test::rule-test-cases ((,rule ,grammar) ,@arguments)
       ,@cases)))
