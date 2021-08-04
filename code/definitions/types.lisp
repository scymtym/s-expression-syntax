;;;; types.lisp --- Rules for parsing type specifiers.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar type-specifiers
  (:class eg::expression-grammar)
  (:use names))

(parser:in-grammar type-specifiers)

;;; General type specifier syntax

(defrule compound-type-specifier ()
    (value (source)
      (list (<- name ((class-name! names)))
            (* (<<- arguments (or (type-specifier)
                                  :any)))))
  (bp:node* (:compound-type-specifier :source source)
    (1 (:name     . 1) name)
    (* (:argument . *) (nreverse arguments))))

(defrule atomic-type-specifier ()
    (value (source)
      (guard name (typep 'symbol))) ; TODO control whether * is allowed, use class-name or type-name from names grammar
  (bp:node* (:atomic-type-specifier :source source)
    (1 (:name . 1) (bp:node* (:type-name :name (eg::%naturalize name) :source source)))))

;;; Well-known compound specifiers

(defrule wildcard-type-specifer ()
    (value (source) '*)
  (bp:node* (:wildcard-type-specifier :source source)))

(defrule keyword-parameter-type-specifier ()
    (value (source)
      (list (<- keyword ((parameter-keyword! names)))
            (<- type    (type-specifier!))))
  (bp:node* (:keyword-parameter-type-specifier :source source)
    (1 :keyword keyword)
    (1 :type    type)))

(defrule parameter-type-specifier ()
    (value (source)
      (list (* (<<- required (and :any
                                  (not (or '&optional '&rest '&key '&allow-other-keys))
                                  (type-specifier!))))
            (? (seq '&optional (* (<<- optional (type-specifier!)))))
            (? (seq '&rest     (<- rest (type-specifier!))))
            (? (seq '&key      (* (<<- keyword (keyword-parameter-type-specifier)))))))
  (bp:node* (:parameter-type-specifier :source source)
    (*    :required (nreverse required))
    (bp:? :rest     rest)
    (*    :keyword  keyword)))

;; TODO The symbol * may not be among the value-types.
(defrule values-type-specifier ()
    (value (source)
      (list 'values
            (* (<<- required (and :any
                                  (not (or '&optional '&rest '&allow-other-keys))
                                  (type-specifier!))))
            (? (seq '&optional (* (<<- optional (and :any
                                                     (not (or '&rest '&allow-other-keys))
                                                     (type-specifier!))))))
            (? (seq '&rest (<- rest (type-specifier!))))))
  (bp:node* (:values-type-specifier :source source)
    (*    (:required . *) required)
    (*    (:optional . *) optional)
    (bp:? (:rest     . 1) rest)))

(defrule function-type-specifier ()
    (value (source)
      (or 'function
          (list 'function
                (? (<- parameters (or (wildcard-type-specifer)
                                      (parameter-type-specifier))))
                (? (<- values     (or (wildcard-type-specifer)
                                      (values-type-specifier)))))))
  (bp:node* (:function-type-specifier :source source)
    (1 (:parameters . 1) parameters)
    (1 (:values     . 1) values)))

(defrule function-type-specifier! ()
  (must (function-type-specifier) "must be a function type specifier"))

;;;

(defrule type-specifier ()
  (and (must (not (list* 'values :any)) "VALUES type is invalid in this context")
       (must (not 'values) "the symbol VALUES is not a valid type specifier")
       (or (atomic-type-specifier)
           (function-type-specifier)
           (compound-type-specifier)
           ;; (list (guard (typep 'symbol)) (* :any))
           )))

(defrule type-specifier! ()
    (must (type-specifier) "must be a type specifier"))
