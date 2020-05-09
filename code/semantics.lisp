;;;; semantics.lisp --- Semantics supported by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defclass namespace-mixin ()
  ((%namespace :initarg  :namespace
               :reader   namespace))
  (:default-initargs
   :namespace (a:required-argument :namespace)))

(defmethod print-items:print-items append ((object namespace-mixin))
  `((:namespace ,(namespace object) "~A")))

(defclass binding-semantics (namespace-mixin
                             print-items:print-items-mixin)
  ((%scope  :initarg  :scope
            :reader   scope)
   ;; TODO extent
   (%order  :initarg  :order
            :reader   order
            :initform :parallel)
   (%values :initarg  :values
            :reader   values*))
  (:default-initargs
   :scope  (a:required-argument :scope)
   :values (a:required-argument :values)))

(defclass reference-semantics (namespace-mixin
                               print-items:print-items-mixin)
  ())
