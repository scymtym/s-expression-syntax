;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defmacro define-syntax (name syntax components &rest options)
  (let ((documentation   (second (find :documentation options :key #'first)))
        (component-forms '())
        (value-forms     '()))
    (map nil (lambda (component)
               (destructuring-bind (name cardinality &key evaluation)
                   component
                 (push `(make-instance 'component
                                       :name        ',name
                                       :cardinality ',(case cardinality
                                                        (*> '*)
                                                        (t  cardinality))
                                       :evaluation  ,evaluation)
                       component-forms)
                 (push `',name value-forms)
                 (push (ecase cardinality
                         (*     `(nreverse ,name))
                         (*>    name)
                         ((? 1) name))
                       value-forms)))
         components)
    `(progn
       (parser.packrat:defrule (,name :grammar special-operators) ()
           ,syntax
         (list ,@(nreverse value-forms)))
       (ensure-syntax ',name 'special-operator
                      :components (list ,@(nreverse component-forms))
                      :rule       ',name
                      ,@(when documentation
                          `(:documentation ,documentation))))))

(defmacro define-special-operator (name syntax &rest components)
  (check-type syntax (cons (member list list*)))
  `(define-syntax ,name
       (,(first syntax) ',name ,@(rest syntax))
     ,@components))

(defmacro define-macro (name syntax &rest components)
  (check-type syntax (cons (member list list*)))
  `(define-syntax ,name
       (,(first syntax) ',name ,@(rest syntax))
     ,@components))
