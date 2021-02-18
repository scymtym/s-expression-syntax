;;;; grammar.lisp --- A client for processing CSTs.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.concrete-syntax-tree)

;;; Client class

(defclass cst-client ()
  ())

(defvar *client* (make-instance 'cst-client))

;;; Natural

(defmethod eg:natural? ((client cst-client) (thing t))
  t)

(defmethod eg:natural? ((client cst-client) (thing cst:cst))
  nil)

(defmethod eg:naturalize ((client cst-client) (thing cst:cst))
  (cst:raw thing))

;;; Symbol operations

(defmethod eg:symbol-name* ((client cst-client) (symbol cst:atom-cst))
  (symbol-name (cst:raw symbol)))

(defmethod eg:symbol-package* ((client cst-client) (symbol cst:atom-cst))
  (symbol-package (cst:raw symbol)))

;;; List operations

(defmethod eg:listp* ((client cst-client) (maybe-list t))
  nil)

(defmethod eg:listp* ((client cst-client) (maybe-list cst:cons-cst))
  t)

(defmethod eg:listp* ((client cst-client) (maybe-list cst:cst))
  (cst:null maybe-list))

(defmethod eg:null* ((client cst-client) (maybe-list t))
  nil)

(defmethod eg:null* ((client cst-client) (maybe-list cst:cst))
  (cst:null maybe-list))

(defmethod eg:first* ((client cst-client) (maybe-list cst:cst))
  (cst:first maybe-list))

(defmethod eg:rest* ((client cst-client) (maybe-list cst:cst))
  (cst:rest maybe-list))

;;; Comparisons

(defmethod eg:typep* ((client cst-client) (thing cst:cst) (type t))
  (typep (cst:raw thing) type))

(defmethod eg:equal* ((client cst-client) (left cst:cst) (right t))
  (equal (cst:raw left) right))

(defmethod eg:equal* ((client cst-client) (left cst:cst) (right cst:cst))
  (equal (cst:raw left) (cst:raw right)))

(defmethod eg:eql* ((client cst-client) (left cst:cst) (right t))
  (eql (cst:raw left) right))
