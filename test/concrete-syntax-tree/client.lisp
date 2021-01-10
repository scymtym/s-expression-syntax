;;;; client.lisp --- Tests for the client class of the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.concrete-syntax-tree.test)

(in-suite :syntax.concrete-syntax-tree)

(test client.smoke
  "Smoke test for the `cst-client' class."

  (let ((syntax.expression-grammar:*client* syntax.concrete-syntax-tree::*client*))
    (is-true (parser.packrat:parse `((syntax::specialized-lambda-list syntax::lambda-lists)
                                     ,(make-hash-table :test #'eq))
                                   (cst:cstify `((a integer) b c &optional d &rest r &key e))))
    (is-false (parser.packrat:parse `((syntax::specialized-lambda-list syntax::lambda-lists)
                                      ,(make-hash-table :test #'eq))
                                    (cst:cstify `((a 1 2) b c &optional d &rest r &key e))))))
