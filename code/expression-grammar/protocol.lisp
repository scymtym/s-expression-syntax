;;;; protocol.lisp --- Protocol provided by the expression-grammar module.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

;;; TODO have the client return a bunch of functions at the beginning
;;;      of each rule and make them available lexically?

(defvar *client*)

(defgeneric natural? (client thing))

(defgeneric naturalize (client thing))

(defgeneric listp* (client maybe-list))

(defgeneric null* (client maybe-list))

(defgeneric first* (client maybe-list))

(defgeneric rest* (client maybe-list))

(defgeneric equal* (client left right))

(defgeneric eql* (client left right))

;;; verbatim client

(defclass verbatim-client ()
  ())

(defmethod natural? ((client verbatim-client) (thing t))
  t)

(defmethod listp* ((client verbatim-client) (maybe-list t))
  nil)

(defmethod null* ((client verbatim-client) (maybe-list t))
  nil)
