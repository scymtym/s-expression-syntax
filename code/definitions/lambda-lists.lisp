;;;; lambda-lists.lisp --- Rules for parsing different kinds of lambda lists.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar lambda-lists
  (:class eg::expression-grammar)
  (:use names
        forms))

(parser:in-grammar lambda-lists)

;;; Shared rules

;; TODO could use lambda-list-keywords
(defun lambda-list-keyword? (symbol)
  (member symbol '(&whole &environment &optional &rest &key &aux &allow-other-keys) :test #'eq))

(defrule lambda-list-variable-name ()
  (and (not (guard lambda-list-keyword?))
       ((variable-name names))))

(defrule lambda-list-variable-name! ()
  (must (lambda-list-variable-name) "must be a lambda list variable name"))

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
    (must (unique-variable-name seen) "must be a lambda list variable name"))

(defrule required-parameter (seen)
    (value (source) (<- name (unique-variable-name seen)))
  (bp:node* (:required-parameter :source source)
    (1 :name name)))

(defrule required-parameter! (seen)
    (value (source)
      (and (not (guard lambda-list-keyword?))
           (<- name (unique-variable-name! seen))))
  (bp:node* (:required-parameter :source source)
    (1 :name name)))

(defrule optional-parameter (seen)
    (value (source)
      (or (list (<- name (unique-variable-name! seen))
                (? (seq (<- default ((form! forms)))
                        (? (<- supplied (unique-variable-name! seen))))))
          (<- name (and (not (guard lambda-list-keyword?))
                        (unique-variable-name! seen)))))
  (bp:node* (:optional-parameter :source source)
    (1    :name     name)
    (bp:? :default  default)
    (bp:? :supplied supplied)))

(defrule rest-parameter (seen)
    (unique-variable-name! seen))

(defrule keyword-parameter (seen)
    (value (source)
      (or (list (or (list (<- keyword (must (guard symbolp) "must be a symbol"))
                          (<- name (unique-variable-name! seen)))
                    (<- name (unique-variable-name! seen)))
                (? (seq (<- default ((form! forms)))
                        (? (<- supplied (unique-variable-name! seen))))))
          (<- name (and (not (guard lambda-list-keyword?))
                        (unique-variable-name! seen)))))
  (bp:node* (:keyword-parameter :source source)
    (1    :name     name)
    (bp:? :keyword  keyword)
    (bp:? :default  default)
    (bp:? :supplied supplied)))

(defrule aux-parameter (seen)
    (value (source)
      (or (list (<- name (unique-variable-name! seen))
                (? (<- value ((form! forms)))))
          (<- name (unique-variable-name! seen))))
  (bp:node* (:aux-parameter :source source)
    (1    :name  name)
    (bp:? :value value)))

(defrule aux-parameter! (seen)
    (must (aux-parameter seen) "must be an aux parameter"))

;;; Reusable sections of lambda lists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (required-section :environment (make-instance 'eg::expression-environment)) (seen)
      (+ (<<- parameters (required-parameter! seen)))
    (nreverse parameters))

  (defrule (optional-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&optional (* (<<- parameters (optional-parameter seen))))
    (nreverse parameters))

  (defrule (rest-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&rest (<- parameter (rest-parameter seen)))
    parameter)

  (defrule (keyword-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&key
           (* (<<- keyword-parameters (keyword-parameter seen)))
           (? (<- allow-other-keys? '&allow-other-keys)))
    (list (nreverse keyword-parameters) allow-other-keys?))

  (defrule (aux-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&aux (* (<<- parameters (aux-parameter! seen))))
    (nreverse parameters)))

;;; 3.4.1 Ordinary Lambda Lists

(defrule %ordinary-lambda-list (seen)
    (value (source)
      (list (? (<- required (required-section seen)))
            (? (<- optional (optional-section seen)))
            (? (<- rest (rest-section seen)))
            (? (<- (keyword allow-other-keys?) (keyword-section seen)))
            (? (<- aux (aux-section seen)))))
  (bp:node* (:ordinary-lambda-list :source source)
    (*    :required         required)
    (*    :optional         optional)
    (bp:? :rest             rest)
    (*    :keyword          keyword)
    (bp:? :allow-other-keys allow-other-keys?)
    (*    :aux              aux)))

(defrule ordinary-lambda-list ()
  (and (<- seen (:transform :any (make-hash-table :test #'eq)))
       (%ordinary-lambda-list seen)))

(defrule ordinary-lambda-list! ()
  (must (ordinary-lambda-list) "must be an ordinary lambda list"))

;;; 3.4.2 Generic Function Lambda Lists

(defrule %generic-function-lambda-list (seen)
    (value (source)
      (list (? (<- required (required-section seen)))
            (? (<- optional (optional-section seen))) ; TODO disallow defaults
            (? (<- rest (rest-section seen)))
            (? (<- (keyword allow-other-keys?) (keyword-section seen))))) ; TODO disallow defaults
  (bp:node* (:generic-function-lambda-list :source source)
    (*    :required         required)
    (*    :optional         optional)
    (bp:? :rest             rest)
    (*    :keyword          keyword)
    (bp:? :allow-other-keys allow-other-keys?)))

(defrule generic-function-lambda-list ()
  (and (<- seen (:transform :any (make-hash-table :test #'eq)))
       (%generic-function-lambda-list seen)))

(defrule generic-function-lambda-list! ()
  (must (generic-function-lambda-list)
        "must be a generic function lambda list"))

;;; 3.4.3 Specialized Lambda Lists

(defrule specializer ()
    (or (list* 'eql (must (list :any) "must be a single object"))
        ((class-name! names))))

(defrule specialized-parameter (seen)
    (value (source)
           (or (and (list* :any)
                    (must (list (<- name (unique-variable-name! seen))
                                (<- specializer (specializer)))
                          "must be of the form (NAME SPECIALIZER)"))
          (<- name (required-parameter! seen))))
  (bp:node* (:specialized-parameter :source source)
    (1    :name        name)
    (bp:? :specializer specializer)))

(defrule %specialized-lambda-list (seen)
    (value (source)
      (list (* (<<- required (specialized-parameter seen)))
            (? (<- optional (optional-section seen)))
            (? (<- rest (rest-section seen)))
            (? (<- (keyword allow-other-keys?) (keyword-section seen)))
            (? (<- aux (aux-section seen)))))
    (bp:node* (:specialized-lambda-list :source source)
      (*    :required         (nreverse required))
      (*    :optional         optional)
      (bp:? :rest             rest)
      (*    :keyword          keyword)
      (bp:? :allow-other-keys allow-other-keys?)
      (*    :aux              aux)))

(defrule specialized-lambda-list ()
  (and (<- seen (:transform :any (make-hash-table :test #'eq)))
       (%specialized-lambda-list seen)))

(defrule specialized-lambda-list! ()
  (must (specialized-lambda-list )
        "must be a specialized lambda list"))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

(parser:defgrammar destructuring-lambda-list
  (:class eg::expression-grammar)
  (:use lambda-lists))

(parser:in-grammar destructuring-lambda-list)

(defrule unique-variable-name (seen) ; TODO maybe not all unique-variable-names?
    (or '()
        (and (guard consp) (pattern seen))
        ((unique-variable-name lambda-lists) seen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (whole-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&whole (<- name (unique-variable-name! seen)))
    name)

  (defrule (environment-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&environment (<- name (unique-variable-name! seen)))
    name))

(defrule pattern (seen)
    (value (source)
      (list* (? (<- whole    (whole-section seen)))
             (? (<- required (required-section seen)))
             (? (<- optional (optional-section seen)))
             (or (<- cdr ((unique-variable-name lambda-lists) seen)) ; TODO name!
                 (list (? (<- rest                    (rest-section seen)))
                       (? (<- (key allow-other-keys?) (keyword-section seen)))
                       (? (<- aux                     (aux-section seen)))))))
  (bp:node* (:pattern :source source)
    (bp:? :whole            whole)
    (*    :required         required)
    (*    :optional         optional)
    (bp:? :rest             rest)
    (*    :keyword          key)
    (bp:? :allow-other-keys allow-other-keys?)
    (*    :aux              aux)
    (bp:? :cdr              cdr)))

(defrule %destructuring-lambda-list (seen)
    (value (source)
      (list* (? (<- whole    (whole-section seen)))
             (? (<- env      #1=(eg:once (environment-section seen)
                                         :flag env? :name &environment)))
             (? (<- required (required-section seen)))
             (? (<- env      #1#))
             (? (<- optional (optional-section seen)))
             (? (<- env      #1#))
             (or (<- cdr ((unique-variable-name lambda-lists) seen)) ; TODO name!
                 (list (? (<- rest                    (rest-section seen)))
                       (? (<- env                     #1#))
                       (? (<- (key allow-other-keys?) (keyword-section seen)))
                       (? (<- env                     #1#))
                       (? (<- aux                     (aux-section seen)))
                       (? (<- env                     #1#))))))
  (bp:node* (:destructuring-lambda-list :source source)
    (bp:? :whole            whole)
    (bp:? :env              env)
    (*    :required         required)
    (*    :optional         optional)
    (bp:? :rest             rest)
    (*    :keyword          key)
    (bp:? :allow-other-keys allow-other-keys?)
    (*    :aux              aux)
    (bp:? :cdr              cdr)))

(defrule destructuring-lambda-list ()
  (and (<- seen (:transform :any (make-hash-table :test #'eq)))
       (%destructuring-lambda-list seen)))

(defrule destructuring-lambda-list! ()
  (must (destructuring-lambda-list)
        "must be a destructuring lambda list"))

;;; 3.4.8 Deftype Lambda Lists
;;;
;;; A deftype lambda list differs from a macro lambda list only in
;;; that if no init-form is supplied for an optional parameter or
;;; keyword parameter in the lambda-list, the default value for that
;;; parameter is the symbol * (rather than nil).

(parser:defgrammar deftype-lambda-list
  (:class eg::expression-grammar)
  (:use destructuring-lambda-list))

(parser:in-grammar deftype-lambda-list)

(defrule %deftype-lambda-list (seen)
  ((%destructuring-lambda-list destructuring-lambda-list) seen))

(defrule deftype-lambda-list ()
  (and (<- seen (:transform :any (make-hash-table :test #'eq)))
       (%deftype-lambda-list seen)))

(defrule deftype-lambda-list! ()
  (must (deftype-lambda-list) "must be a DEFTYPE lambda list"))
