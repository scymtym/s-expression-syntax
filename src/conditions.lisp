;;;; conditions.lisp --- Conditions signaled by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(define-condition syntax-not-found-error (error)
  ((%name :initarg :name
          :reader  name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No syntax named ~S.~@:>" (name condition)))))

(define-condition invalid-syntax-error (error)
  ((%syntax :initarg :syntax
            :reader  syntax)
   (%value  :initarg :value
            :reader  value))
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid ~A syntax at ~S.~@:>"
             (name (syntax condition))
             (value condition)))))
