;;;; protocol.lisp --- Protocol provided by the expression-grammar module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

;;; TODO have the client return a bunch of functions at the beginning
;;;      of each rule and make them available lexically?

(defvar *client*)

(defgeneric natural? (client thing))

(defgeneric naturalize (client thing))

;;; List operations

(defgeneric listp* (client maybe-list))

(defgeneric null* (client maybe-list))

(defgeneric first* (client maybe-list))

(defgeneric rest* (client maybe-list))

;;; Symbol operations

(defgeneric symbol-name* (client symbol))

(defgeneric symbol-package* (client symbol))

(defgeneric package-name* (client package))

;;; Comparisons

(defgeneric typep* (client thing type))

(defgeneric equal* (client left right))

(defgeneric eql* (client left right))

;;; `verbatim-client'
;;;
;;; This client class implements the TODO protocol by treating every
;;; object as an atom.

(defclass verbatim-client ()
  ())

(defmethod natural? ((client verbatim-client) (thing t))
  t)

(defmethod listp* ((client verbatim-client) (maybe-list t))
  nil)

(defmethod null* ((client verbatim-client) (maybe-list t))
  nil)
