;;;; lambda-lists.lisp --- Rules for parsing different kinds of lambda lists.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
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

;;; Reusable sections of lambda lists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (required-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (+ (<<- parameters (required-parameter! seen)))
    (nreverse parameters))

  (defrule (optional-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&optional (* (<<- parameters (optional-parameter seen))))
    (nreverse parameters))

  (defrule (rest-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&rest (<- parameter (rest-parameter seen)))
    parameter)

  (defrule (keyword-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&key
           (* (<<- keyword-parameters (keyword-parameter seen)))
           (? (<- allow-other-keys? '&allow-other-keys)))
    (list (nreverse keyword-parameters) allow-other-keys?))

  (defrule (aux-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&aux (* (<<- parameters (aux-parameter! seen))))
    (nreverse parameters)))

;;; 3.4.1 Ordinary Lambda Lists

(defrule ordinary-lambda-list (seen)
    (list (? (<- required (required-section seen)))
          (? (<- optional (optional-section seen)))
          (? (<- rest (rest-section seen)))
          (? (<- (keyword allow-other-keys?) (keyword-section seen)))
          (? (<- aux (aux-section seen))))
  (list required optional rest keyword allow-other-keys? aux))

;;; 3.4.2 Generic Function Lambda Lists

(defrule generic-function-lambda-list (seen)
    (list (? (<- required (required-section seen)))
          (? (<- optional (optional-section seen))) ; TODO disallow defaults
          (? (<- rest (rest-section seen)))
          (? (<- (keyword allow-other-keys?) (keyword-section seen)))) ; TODO disallow defaults
  (list required optional rest keyword allow-other-keys?))

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
          (? (<- optional (optional-section seen)))
          (? (<- rest (rest-section seen)))
          (? (<- (keyword allow-other-keys?) (keyword-section seen)))
          (? (<- aux (aux-section seen))))
  (list (nreverse required) optional rest keyword allow-other-keys? aux))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

(parser:defgrammar destructuring-lambda-list
  (:class syntax.expression-grammar::expression-grammar)
  (:use lambda-lists))

(parser:in-grammar destructuring-lambda-list)

(defrule unique-variable-name (seen) ; TODO maybe not all unique-variable-names?
    (or '()
        (and (guard consp) (pattern seen))
        ((unique-variable-name lambda-lists) seen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (whole-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&whole (<- name (unique-variable-name! seen)))
    name)

  (defrule (environment-section :environment (make-instance 'syntax.expression-grammar::expression-environment)) (seen)
      (seq '&environment (<- name (unique-variable-name! seen)))
    name))

(defrule pattern (seen)
    (list* (? (<- whole    (whole-section seen)))
           (? (<- required (required-section seen)))
           (? (<- optional (optional-section seen)))
           (or (<- cdr ((unique-variable-name lambda-lists) seen)) ; TODO name!
               (list (? (<- rest                    (rest-section seen)))
                     (? (<- (key allow-other-keys?) (keyword-section seen)))
                     (? (<- aux                     (aux-section seen))))))
  (list :pattern whole required optional rest key allow-other-keys? aux cdr))

(defrule destructuring-lambda-list (seen)
    (list* (? (<- whole    (whole-section seen)))    (? (<- env (environment-section seen)))
           (? (<- required (required-section seen))) (? (<- env (environment-section seen)))
           (? (<- optional (optional-section seen))) (? (<- env (environment-section seen)))
           (or (<- cdr ((unique-variable-name lambda-lists) seen)) ; TODO name!
               (list (? (<- rest                    (rest-section seen)))    (? (<- env (environment-section seen)))
                     (? (<- (key allow-other-keys?) (keyword-section seen))) (? (<- env (environment-section seen)))
                     (? (<- aux                     (aux-section seen)))     (? (<- env (environment-section seen))))))
  (list :destructuring-lambda-list whole env required optional rest key allow-other-keys? aux cdr))

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
