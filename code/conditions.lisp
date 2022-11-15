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

(define-condition s-expression-syntax-condition (condition)
  ((%syntax     :initarg  :syntax
                :reader   syntax)
   (%expression :initarg  :expression
                :reader   expression))
  (:default-initargs
   :syntax     (missing-required-initarg 's-expression-syntax-condition :syntax)
   :expression (missing-required-initarg 's-expression-syntax-condition :expression)))

(define-condition s-expression-syntax-error (error
                                             s-expression-syntax-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid ~A syntax at ~S~@[: ~A~].~@:>"
             (name (syntax condition))
             (expression condition)
             (message condition)))))

(define-condition invalid-syntax-error (s-expression-syntax-error)
  ((%message :initarg  :message
             :reader   message
             :initform nil)))

(defun invalid-syntax-error (syntax expression &optional message)
  (error 'invalid-syntax-error :syntax     syntax
                               :expression expression
                               :message    message))

;;; Specific conditions

(define-condition invalid-block-name-error (invalid-syntax-error)
  ((%message :allocation :class
             :reader     message
             :initform   "block name must be a symbol")))
