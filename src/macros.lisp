;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(defmacro define-syntax (name syntax &rest components)
  (let ((component-forms '())
        (value-forms     '()))
    (map nil (lambda (component)
               (destructuring-bind (name cardinality &key evaluation)
                   component
                 (push `(make-instance 'component
                                       :name        ',name
                                       :cardinality ',cardinality
                                       :evaluation  ,evaluation)
                       component-forms)
                 (push `',name value-forms)
                 (push (case cardinality
                         (* `(nreverse ,name))
                         (t name))
                       value-forms)))
         components)
    `(progn
       (parser.packrat:defrule (,name :grammar special-operators) ()
           ,syntax
         (list ,@(nreverse value-forms)))
       (ensure-syntax ',name 'special-operator
                      :components (list ,@(nreverse component-forms))
                      :rule       ',name))))

(defmacro define-special-operator (name syntax &rest components)
  (check-type  syntax (cons (member list list*)))
  `(define-syntax ,name
       (,(first syntax) ',name ,@(rest syntax))
     ,@components))
