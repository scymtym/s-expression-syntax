;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defmacro define-syntax (name-and-options syntax unparser parts &rest options)
  (destructuring-bind (name &key (grammar  (parser.packrat.grammar:name
                                            parser.packrat::*grammar*))
                                 arguments
                                 (kind     (a:make-keyword name))
                                 (source   (gensym "SOURCE"))
                                 (class    'syntax-description))
      (a:ensure-list name-and-options)
    (let ((documentation (second (find :documentation options :key #'first)))
          (part-forms    '())
          (relations     '()))
      (map nil (lambda (part)
                 (destructuring-bind (name cardinality &key evaluation)
                     part
                   (let ((relation     (a:make-keyword name))
                         (cardinality* (case cardinality
                                         (*> '*)
                                         (?  'bp:?)
                                         (t  cardinality))))
                     (push `(make-instance 'part :name        ',name
                                                 :cardinality ',cardinality*
                                                 :evaluation  ,evaluation)
                           part-forms)
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
                                 `(:evaluation ,(case cardinality
                                                  ((1 ?) evaluation)
                                                  (t     `(load-time-value
                                                           (a:make-circular-list
                                                            1 :initial-element ,evaluation)))))))
                           relations))))
           parts)
      `(progn
         (parser.packrat:defrule (,name :grammar ,grammar)
             (,@(map 'list #'first arguments))
           (value (,source) ,syntax)
           (bp:node* (,kind :source ,source)
             ,@(nreverse relations)))
         (ensure-syntax ',name ',class
                        :parts   (list ,@(nreverse part-forms))
                        :rule    ,(if arguments
                                      `'(,name ,@arguments)
                                      `',name)
                        :grammar ',grammar
                        :unparser (lambda (initargs &key ,@(mapcar #'first parts))
                                    (declare (ignorable initargs))
                                    (flet ((? (thing &optional (value thing) &rest more-values)
                                             (when thing (list* value more-values))))
                                      (declare (ignorable #'?))
                                      ,unparser))
                        ,@(when documentation
                            `(:documentation ,documentation)))))))

(defmacro define-special-operator (name-and-options syntax unparser &rest parts)
  (check-type syntax (cons (member list list*)))
  (destructuring-bind (name &key (operator name))
      (a:ensure-list name-and-options)
    `(define-syntax (,name :class special-operator-syntax)
         (,(first syntax) ',operator ,@(rest syntax))
         (list* ',operator ,unparser)
       ,@parts)))

(defmacro define-macro (name syntax unparser &rest parts)
  (check-type syntax (cons (member list list*)))
  `(define-syntax (,name :class macro-syntax)
       (,(first syntax) ',name ,@(rest syntax))
       (list* ',name ,unparser)
     ,@parts))
