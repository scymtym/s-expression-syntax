;;;; macros.lisp --- Macros provided by the syntax system.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defun generate-relation-name (part-name)
  (a:make-keyword part-name))

(defun generate-relation-cardinality (cardinality)
  (case cardinality
    (*> '*)
    (?  'bp:?)
    (t  cardinality)))

(defun generate-relation-specifier (part-name cardinality)
  (let ((relation-name (generate-relation-name part-name)))
    (case cardinality
      ((1 ?) `(,relation-name . 1))
      ((* *>)`(,relation-name . *))
      (t     `(,relation-name . ,cardinality)))))

(defun generate-relation-value (part-name cardinality)
  (ecase cardinality
    (*     `(nreverse ,part-name))
    (*>    part-name)
    ((1 ?) part-name)))

(defun generate-evaluation-arguments (evaluation cardinality)
  (unless (null evaluation)
    `(:evaluation ,(case cardinality
                     ((1 ?) evaluation)
                     (t     `(load-time-value
                              (a:make-circular-list
                               1 :initial-element ,evaluation)))))))

(defun generate-node (kind initargs part-specifiers)
  ;; If elements of PART-SPECIFIERS contain an `:index' keyword
  ;; argument, the order at runtime of the generated `bp:relate' calls
  ;; must conform to those arguments. The value of each `:index'
  ;; keyword argument is a form that evaluates at runtime to the sort
  ;; key for the corresponding relation.
  (let ((must-sort? (find-if (lambda (part-specifier)
                               (getf (cddr part-specifier) :index))
                             part-specifiers)))
    (if must-sort?
        ;; Lexically bind variables which hold dynamic-extent "cells"
        ;; which hold sort keys and relations. Put cells into a
        ;; dynamic-extent vector, (destructively) sort that vector,
        ;; then (destructively) strip cells of their sort keys.
        (let ((cell-names '()))
          (flet ((one-part (part)
                   (destructuring-bind (name cardinality &key evaluation index)
                       part
                     (let ((cell-name     (gensym "CELL"))
                           (previous-cell (first cell-names)))
                       (push cell-name cell-names)
                       `(,cell-name
                         (let ((value ,(generate-relation-value
                                        name cardinality)))
                           (if (null value)
                               ,previous-cell
                               (list*
                                (cons ,index
                                      (list ',(generate-relation-cardinality
                                               cardinality)
                                            ',(generate-relation-specifier
                                               name cardinality)
                                            value
                                            ,@(generate-evaluation-arguments
                                               evaluation cardinality)))
                                ,previous-cell))))))))
            `(let* (,@(mapcar #'one-part part-specifiers)
                    (items     ,(first cell-names))
                    (sorted    (sort items #'< :key #'car))
                    (relations (map-into sorted #'cdr sorted)))
               (declare (dynamic-extent ,@cell-names))
               (bp:make+finish-node+relations*
                ',kind (list ,@initargs) relations))))
        ;; If there is no need to sort, just generate a plain
        ;; `bp:node*' form.
        (flet ((one-part (part)
                 (destructuring-bind (name cardinality &key evaluation) part
                   `(,(generate-relation-cardinality cardinality)
                     ,(generate-relation-specifier name cardinality)
                     ,(generate-relation-value name cardinality)
                     ,@(generate-evaluation-arguments
                        evaluation cardinality)))))
          `(bp:node* (,kind ,@initargs)
             ,@(mapcar #'one-part part-specifiers))))))

(defun generate-part (name cardinality evaluation)
  (let ((cardinality (generate-relation-cardinality cardinality)))
    `(make-instance 'part :name        ',name
                          :cardinality ',cardinality
                          :evaluation  ,evaluation)))

(defun generate-unparser (parts body)
  (let ((supplied?-variables '()))
    (flet ((part-parameter (part)
             (let* ((name               (first part))
                    (supplied?-variable (a:symbolicate name '#:-supplied?)))
               (push supplied?-variable supplied?-variables)
               `(,name nil ,supplied?-variable))))
      `(lambda (initargs &key ,@(mapcar #'part-parameter parts))
         (declare (ignorable initargs ,@supplied?-variables))
         (flet ((? (thing &optional (value thing) &rest more-values)
                  (when thing (list* value more-values))))
           (declare (ignorable #'?))
           ,body)))))

(defmacro define-syntax (name-and-options syntax unparser parts &rest options)
  (destructuring-bind (name &key (grammar  (parser.packrat.grammar:name
                                            parser.packrat::*grammar*))
                                 arguments
                                 (kind     (a:make-keyword name))
                                 (source   (gensym "SOURCE"))
                                 (class    'syntax-description))
      (a:ensure-list name-and-options)
    (let ((documentation (second (find :documentation options :key #'first))))
      (flet ((one-part (part-specifier)
               (destructuring-bind (name cardinality &key evaluation index)
                   part-specifier
                 (declare (ignore index))
                 (generate-part name cardinality evaluation))))
        `(progn
           (parser.packrat:defrule (,name :grammar ,grammar)
               (,@(map 'list #'first arguments))
               (value (,source) ,syntax)
               ,(generate-node kind `(:source ,source) parts))
           (ensure-syntax ',name ',class
                          :parts   (list ,@(mapcar #'one-part parts))
                          :rule    ,(if arguments
                                        `'(,name ,@arguments)
                                        `',name)
                          :grammar ',grammar
                          :unparser ,(generate-unparser parts unparser)
                          ,@(when documentation
                              `(:documentation ,documentation))))))))

(defmacro define-special-operator (name-and-options syntax unparser &rest parts)
  (check-type syntax (cons (member list list*)))
  (destructuring-bind (name &rest options
                            &key (operator name) &allow-other-keys)
      (a:ensure-list name-and-options)
    (let ((other-options (a:remove-from-plist options :operator)))
      `(define-syntax (,name :class special-operator-syntax ,@other-options)
           (,(first syntax) ',operator ,@(rest syntax))
           (list* ',operator ,unparser)
         ,@parts))))

(defmacro define-macro (name syntax unparser &rest parts)
  (check-type syntax (cons (member list list*)))
  `(define-syntax (,name :class macro-syntax)
       (,(first syntax) ',name ,@(rest syntax))
       (list* ',name ,unparser)
     ,@parts))
