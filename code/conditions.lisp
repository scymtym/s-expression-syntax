;;;; conditions.lisp --- Conditions signaled by the syntax system.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; Utility for required initargs

(defun missing-required-initarg (class-name initarg)
  (error "~@<The required initarg ~S for class ~S has not been supplied.~@:>"
         initarg class-name))

;;; Should be defined in protocol.lisp, but is needed here
(defgeneric name (thing))

;;; Conditions related to syntax descriptions

(define-condition syntax-not-found-error (error)
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (missing-required-initarg 'syntax-not-found-error :name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No syntax named ~S.~@:>" (name condition))))
  (:documentation
   "This error is signaled if a specified syntax cannot be found."))

(define-condition part-not-found-error (error)
  ((%syntax :initarg :syntax
            :reader  syntax)
   (%name   :initarg :name
            :reader  name))
  (:default-initargs
   :syntax (missing-required-initarg 'part-not-found-error :syntax)
   :name   (missing-required-initarg 'part-not-found-error :name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No part named ~S in syntax ~A.~@:>"
             (name condition) (syntax condition))))
  (:documentation
   "This error is signaled if a specified part cannot be found in a given
syntax description."))

;;; Conditions related to parsing s-expression syntax

(define-condition invalid-syntax-error (error)
  ((%syntax     :initarg  :syntax
                :reader   syntax)
   (%expression :initarg  :expression
                :reader   expression)
   (%message    :initarg  :message
                :reader   message
                :initform nil))
  (:default-initargs
   :syntax     (missing-required-initarg 'invalid-syntax-error :syntax)
   :expression (missing-required-initarg 'invalid-syntax-error :expression))
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid ~A syntax at ~S~@[: ~A~].~@:>"
             (name (syntax condition))
             (expression condition)
             (message condition)))))

(defun invalid-syntax-error (syntax expression message)
  (error 'invalid-syntax-error :syntax     syntax
                               :expression expression
                               :message    message))
