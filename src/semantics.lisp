;;;; semantics.lisp --- Semantics supported by the syntax system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(defclass namespace-mixin ()
  ((namespace :initarg  :namespace
              :reader   namespace)))

(defmethod print-items:print-items append ((object namespace-mixin))
  `((:namespace ,(namespace object) "~A")))

(defclass binding-semantics (namespace-mixin
                             print-items:print-items-mixin)
  ((scope  :initarg  :scope
           :reader   scope)
   ;; TODO extent
   (order  :initarg  :order
           :reader   order
           :initform :parallel)
   (values :initarg  :values
           :reader   values*)))

(defclass reference-semantics (namespace-mixin)
  ((namespace :initarg :namespace)))
