;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defmacro define-syntax (name-and-options syntax components &rest options)
  (destructuring-bind (name &key (grammar (parser.packrat.grammar:name
                                           parser.packrat::*grammar*))
                                 arguments
                                 (kind    (a:make-keyword name)))
      (a:ensure-list name-and-options)
    (let ((documentation   (second (find :documentation options :key #'first)))
          (component-forms '())
          (relations       '()))
      (map nil (lambda (component)
                 (destructuring-bind (name cardinality &key evaluation)
                     component
                   (let ((relation     (a:make-keyword name))
                         (cardinality* (case cardinality
                                         (*> '*)
                                         (?  'bp:?)
                                         (t  cardinality))))
                     (push `(make-instance 'component :name        ',name
                                                      :cardinality ',cardinality*
                                                      :evaluation  ,evaluation)
                           component-forms)
                     (push `(,cardinality*
                             ,(case cardinality
                                ((1 ?) `(,relation . 1))
                                ((* *>)`(,relation . *))
                                (t     `(,relation . ,cardinality)))
                             ,(ecase cardinality
                                (*     `(nreverse ,name))
                                (*>    name)
                                ((1 ?) name))
                             ,@(unless (null evaluation)
                                 `(:evaluation ',(case cardinality
                                                   ((1 ?) evaluation)
                                                   (t     (a:make-circular-list
                                                           1 :initial-element evaluation))))))
                           relations))))
           components)
      (a:with-unique-names (source)
        `(progn
           (parser.packrat:defrule (,name :grammar ,grammar)
               (,@(map 'list #'first arguments))
               (value (,source) ,syntax)
             (bp:node* (,kind :source ,source)
               ,@(nreverse relations)))
           (ensure-syntax ',name 'special-operator
                          :components (list ,@(nreverse component-forms))
                          :rule       ,(if arguments
                                           `'(,name ,@arguments)
                                           `',name)
                          :grammar    ',grammar
                          ,@(when documentation
                              `(:documentation ,documentation))))))))

(defmacro define-special-operator (name-and-options syntax &rest components)
  (check-type syntax (cons (member list list*)))
  (destructuring-bind (name &key (operator name))
      (a:ensure-list name-and-options)
    `(define-syntax ,name
         (,(first syntax) ',operator ,@(rest syntax))
       ,@components)))

(defmacro define-macro (name syntax &rest components)
  (check-type syntax (cons (member list list*)))
  `(define-syntax ,name
       (,(first syntax) ',name ,@(rest syntax))
     ,@components))
