;;;; semantics.lisp --- Semantics supported by the syntax system.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defclass namespace-mixin ()
  ((%namespace :initarg  :namespace
               :reader   namespace))
  (:default-initargs
   :namespace (missing-required-initarg 'namespace-mixin :namespace)))

(defmethod print-items:print-items append ((object namespace-mixin))
  `((:namespace "~A" ,(namespace object))))

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
   :scope  (missing-required-initarg 'binding-semantics :scope)
   :values (missing-required-initarg 'binding-semantics :values)))

(defclass assignment-semantics (namespace-mixin
                                print-items:print-items-mixin)
  ())

(defclass reference-semantics (namespace-mixin)
  ())
