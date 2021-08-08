;;;; conditions.lisp --- Conditions signaled by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; Should be defined in protocol.lisp, but is needed here
(defgeneric name (thing))

;;;

(define-condition syntax-not-found-error (error)
  ((%name :initarg :name
          :reader  name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No syntax named ~S.~@:>" (name condition)))))

(define-condition part-not-found-error (error)
  ((%syntax :initarg :syntax
            :reader  syntax)
   (%name   :initarg :name
            :reader  name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No part named ~S in syntax ~A.~@:>"
             (name condition) (syntax condition)))))

;;;

(define-condition invalid-syntax-error (error)
  ((%syntax  :initarg  :syntax
             :reader   syntax)
   (%value   :initarg  :value
             :reader   value)
   (%message :initarg  :message
             :reader   message
             :initform nil))
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid ~A syntax at ~S~@[: ~A~].~@:>"
             (name (syntax condition))
             (value condition)
             (message condition)))))
