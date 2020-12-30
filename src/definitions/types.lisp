;;;; types.lisp --- Rules for parsing type specifiers.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:defgrammar type-specifiers
  (:class syntax.expression-grammar::expression-grammar))

(parser:in-grammar type-specifiers)

;;; General type specifier syntax

#+TODO-maybe (parser:defrule compound-type-specifier ()
                 (list (guard symbolp) (* :any))
               (bp:node* (:compound-type-specifier :head )
                 (* :argument )))

(parser:defrule type-specifier ()
    (and (:must (not (list* 'values :any)) "VALUES type is invalid in this context")
         (or (guard symbolp) ; TODO control whether * is allowed
             (list (guard symbolp) (* :any)))))

(parser:defrule type-specifier! ()
    (:must (type-specifier) "must be a type specifier"))

;;; `values' type specifier

(parser:defrule values-type-specifier ()
    (list 'values
          (* (<<- required (type-specifier)))
          (? (seq '&optional (<<- optional (type-specifier))))
          (? (seq &rest (<- rest (type-specifier))))
          (? (<- allow-other-keys '&allow-other-keys))))
