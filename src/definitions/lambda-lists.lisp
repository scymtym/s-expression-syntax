;;;; lambda-lists.lisp --- Rules for parsing different kinds of lambda lists.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:defgrammar lambda-lists
  (:class syntax.expression-grammar::expression-grammar)
  (:use names
        forms))

(parser:in-grammar lambda-lists)

;;; Shared rules

;; TODO could use lambda-list-keywords
(defun lambda-list-keyword? (symbol)
  (member symbol '(&whole &optional &rest &key &aux &allow-other-keys) :test #'eq))

(parser:defrule (lambda-list-variable-name) ()
  (and (not (guard lambda-list-keyword?))
       ((variable-name names))))

(defrule unique-variable-name (seen)
    (<- name (lambda-list-variable-name))
  (cond ((not seen)
         name)
        ((not (gethash name seen))
         (setf (gethash name seen) t)
         name)
        (t
         (:fail))))

(defrule unique-variable-name! (seen)
    (:must (unique-variable-name seen) "must be a lambda list variable name"))

(defrule required-parameter (seen)
    (unique-variable-name seen))

(defrule required-parameter! (seen)
    (and (not (guard lambda-list-keyword?)) (unique-variable-name! seen)))

(defrule optional-parameter (seen)
    (or (list (<- name (unique-variable-name! seen))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not (guard lambda-list-keyword?))
                      (unique-variable-name! seen))))
  (list name default supplied))

(defrule rest-parameter (seen)
    (unique-variable-name! seen))

(defrule keyword-parameter (seen)
    (or (list (or (list (<- keyword (:must (guard keywordp) "must be a keyword"))
                        (<- name (unique-variable-name! seen)))
                  (<- name (unique-variable-name! seen)))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not (guard lambda-list-keyword?))
                      (unique-variable-name! seen))))
  (list (or keyword `(keyword ,name)) name default supplied))

(defrule aux-parameter (seen)
    (or (list (<- name (unique-variable-name! seen)) (? (<- value ((form! forms)))))
        (<- name (unique-variable-name! seen)))
  (list name value))

(defrule aux-parameter! (seen)
    (:must (aux-parameter seen) "must be an aux parameter"))

;;; Reusable parts of lambda lists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (aux-parameters :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&aux (* (<<- parameters (aux-parameter! seen))))
    (nreverse parameters)))

;;; 3.4.1 Ordinary Lambda Lists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (optional-rest-key-parameters :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq (? (seq '&optional (* (<<- optional (optional-parameter seen)))))
           (? (seq '&rest (<- rest (rest-parameter seen))))
           (? (seq '&key
                   (* (<<- keyword (keyword-parameter seen)))
                   (? (<- allow-other-keys? '&allow-other-keys)))))
    (list (nreverse optional) rest (nreverse keyword) allow-other-keys?))

  (defrule (optional-rest-key-aux-parameters :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq (<- optional-rest-key (optional-rest-key-parameters seen))
           (? (<- aux (aux-parameters seen))))
    (if aux
        (nconc optional-rest-key (list aux))
        optional-rest-key)))


(defrule ordinary-lambda-list (seen)
    (list (* (<<- required (required-parameter! seen)))
          (<- rest (optional-rest-key-aux-parameters seen)))
  (list* (nreverse required) rest))

;;; 3.4.2 Generic Function Lambda Lists

(defrule generic-function-lambda-list (seen)
    (list (* (<<- required (required-parameter! seen)))
          (<- rest (optional-rest-key-parameters seen)))
  (list* (nreverse required) rest))

;;; 3.4.3 Specialized Lambda Lists

(defrule specializer ()
    (or (list* 'eql (:must (list :any) "must be a single object"))
        ((class-name! names))))

(defrule specialized-parameter (seen)
    (or (list (<- name (required-parameter! seen))
              (<- specializer (specializer)))
        (<- name (required-parameter! seen)))
  (list name specializer))

(defrule specialized-lambda-list (seen)
    (list (* (<<- required (specialized-parameter seen)))
          (<- rest (optional-rest-key-parameters seen)))
  (list* (nreverse required) rest))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

(parser:defgrammar destructuring-lambda-list
  (:class syntax.expression-grammar::expression-grammar)
  (:use lambda-lists))

(parser:in-grammar destructuring-lambda-list)

(defrule unique-variable-name! (seen) ; TODO maybe not all unique-variable-names?
  #+no (unique-variable-name seen)
  #+no (:must (:guard symbolp) "must be a symbol")
  (parser.packrat.grammar.base::next-rule seen))

(defrule unique-variable-name (seen) ; TODO maybe not all unique-variable-names?
    (or '()
        (destructuring-lambda-list seen)
        (parser.packrat.grammar.base::next-rule seen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (whole-parameter :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&whole (<- name (unique-variable-name! seen)))
    name)

  (defrule (environment-parameter :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&environment (<- name (unique-variable-name! seen)))
    name))

(defrule destructuring-lambda-list (seen)
    (list (? (<- whole (whole-parameter seen)))
          (* (<<- required (required-parameter! seen) #+no (or (lambda-list-variable-name)
                                                                               (destructuring-lambda-list seen))))
          (or '()
              (<- (optional rest key allow-other-keys?)
                  (optional-rest-key-parameters seen))
                                        ; (<- cdr (destructuring-lambda-list seen))
              )
          (? (<- environment (environment-parameter seen)))
          (? (<- aux (aux-parameters seen))))
  (list :destructuring-lambda-list whole environment (nreverse required) optional rest key allow-other-keys? aux #+later cdr))

;;; 3.4.8 Deftype Lambda Lists
;;;
;;; A deftype lambda list differs from a macro lambda list only in
;;; that if no init-form is supplied for an optional parameter or
;;; keyword parameter in the lambda-list, the default value for that
;;; parameter is the symbol * (rather than nil).

(parser:defgrammar deftype-lambda-list
  (:class syntax.expression-grammar::expression-grammar)
  (:use destructuring-lambda-list))

(parser:in-grammar deftype-lambda-list)

(defrule deftype-lambda-list (seen)
  ((destructuring-lambda-list destructuring-lambda-list) seen))
