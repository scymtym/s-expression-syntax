;;;; types.lisp --- Rules for parsing type specifiers.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar type-specifiers
  (:class eg::expression-grammar))

(parser:in-grammar type-specifiers)

;;; General type specifier syntax

(defrule compound-type-specifier ()
    (value (source)
      (list (<- name ((class-name names)))
            (* (<<- arguments (or (type-specifier)
                                  :any)))))
  (bp:node* (:compound-type-specifier :source source)
    (1 :name     name)
    (* :argument (nreverse arguments))))

(defrule atomic-type-specifier ()
    (value (source)
      (guard name (typep 'symbol))) ; TODO control whether * is allowed, use class-name or type-name from names grammar
  (bp:node* (:atomic-type-specifier :source source)
    (1 :name (bp:node* (:type-name :name name :source source)))))

;;; Well-known compound specifiers

(defrule wildcard-type-specifer ()
    (value (source) '*)
  (bp:node* (:wildcard-type-specifier :source source)))

;; TODO The symbol * may not be among the value-types.
(defrule values-type-specifier ()
    (list 'values
          (* (<<- required (and :any
                                (not (or '&optional '&rest '&allow-other-keys))
                                (type-specifier!))))
          (? (seq '&optional (<<- optional (and :any
                                                (not (or '&rest '&allow-other-keys))
                                                (type-specifier!)))))
          (? (seq '&rest (<- rest (type-specifier!))))
          (? (<- allow-other-keys? '&allow-other-keys)))
  (list required optional rest allow-other-keys?))

(defrule function-type-specifier ()
    (value (source)
      (or 'function
          (list 'function
                (? (<- arguments '()))
                (? (<- values (or (wildcard-type-specifer)
                                  (values-type-specifier)))))))
  (bp:node* (:function-type-specifier :source source)
    (1 :values values)))

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
