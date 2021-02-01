;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defmacro define-syntax (name syntax components &rest options)
  (let ((kind            (a:make-keyword name))
        (documentation   (second (find :documentation options :key #'first)))
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
                   (push `(make-instance 'component
                                         :name        ',name
                                         :cardinality ',cardinality*
                                         :evaluation  ,evaluation)
                         component-forms)
                   (push `(,cardinality*
                           ,relation
                           ,(ecase cardinality
                              (*        `(nreverse ,name))
                              ((*> ? 1) name)))
                         relations))))
         components)
    (a:with-unique-names (source)
     `(progn
        (parser.packrat:defrule (,name :grammar special-operators) ()
            (value (,source) ,syntax)
          (bp:node* (,kind :source ,source)
            ,@(nreverse relations)))
        (ensure-syntax ',name 'special-operator
                       :components (list ,@(nreverse component-forms))
                       :rule       ',name
                       ,@(when documentation
                           `(:documentation ,documentation)))))))

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
