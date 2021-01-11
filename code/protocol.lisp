;;;; protocol.lisp --- Protocol functions provided by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(defgeneric parse (client syntax expression))

;;;

(defgeneric find-syntax (name &key if-does-not-exist))

(defgeneric (setf find-syntax) (new-value name &key if-does-not-exist))

(defgeneric components (container))

(defgeneric find-component (name container &key if-does-not-exist))

(defgeneric ensure-syntax (name class &rest initargs))

;;; Default behavior

(defmethod find-syntax :around ((name t) &key (if-does-not-exist #'error))
  (or (call-next-method)
      (typecase if-does-not-exist
        (function
         (funcall if-does-not-exist (make-condition 'syntax-not-found-error
                                                    :name name)))
        (t
         if-does-not-exist))))

(defmethod find-component ((name t) (container symbol) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (find-component name (find-syntax container)))

(defmethod find-component :around ((name t) (container t) &key if-does-not-exist) ; TODO should be outermost call only
  (or (call-next-method)
      (typecase if-does-not-exist
        (function
         (funcall if-does-not-exist (make-condition 'component-not-found-error
                                                    :syntax container
                                                    :name   name)))
        (t
         if-does-not-exist))))
