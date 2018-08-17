;;;; conditions.lisp --- Conditions signaled by the syntax system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(define-condition syntax-not-found-error (error)
  ((name :initarg :name
         :reader  name))
  (:report
   (lambda (condition stream)
     (format stream "~@<No syntax named ~S.~@:>" (name condition)))))
