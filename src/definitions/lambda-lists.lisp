;;;; lambda-lists.lisp --- Rules for parsing different kinds of lambda lists.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:defgrammar lambda-lists
  (:class syntax.expression-grammar::expression-grammar))

(parser:in-grammar lambda-lists)

;;; Shared rules

;; TODO could use lambda-list-keywords
(defun lambda-list-keyword? (symbol)
  (member symbol '(&optional &rest &key &aux &allow-other-keys) :test #'eq))

(defun constant? (name)
  (eq (sb-cltl2:variable-information name) :constant))

(parser:defrule (lambda-list-variable-name) ()
  (and (:guard symbolp)
       (not (:guard constant?))
       (not (:guard keywordp))
       (not (:guard lambda-list-keyword?))
       :any))

(parser:defrule (unique-variable-name) (seen)
    (<- name (lambda-list-variable-name))
  (cond ((not seen)
         name)
        ((not (gethash name seen))
         (setf (gethash name seen) t)
         name)
        (t
         (:fail))))

(parser:defrule (required-parameter) (seen)
    (unique-variable-name seen))

(parser:defrule (optional-parameter) (seen)
  (or (list (<- name (unique-variable-name seen))
            (? (:seq (<- default)
                     (? (<- supplied (unique-variable-name seen))))))
      (<- name (unique-variable-name seen)))
  (list name default supplied))

(parser:defrule (rest-parameter) (seen)
  (unique-variable-name seen))

(parser:defrule (keyword-parameter) (seen)
  (or (list (or (list (<- keyword (:guard keywordp))
                      (<- name (unique-variable-name seen)))
                (<- name (unique-variable-name seen)))
            (? (:seq (<- default)
                     (? (<- supplied (unique-variable-name seen))))))
      (<- name (unique-variable-name seen)))
  (list (or keyword (alexandria:make-keyword name)) name default supplied))

(parser:defrule (aux-parameter) (seen)
    (or (list (<- name (unique-variable-name seen)) (? value))
        (<- name (unique-variable-name seen)))
  (list name value))

;;; 3.4.1 Ordinary Lambda Lists

(parser:defrule ordinary-lambda-list (seen)
    (list* (* (<<- required (required-parameter seen)))
           (<- rest (optional-rest-key-parameters seen))
           ; (parser.packrat.bootstrap::next-rule)
           )
  (list* (nreverse required) rest))

;; TODO there should be sharing between the following two
(parser:defrule optional-rest-key-aux-parameters (seen)
    (list* (<- optional-rest-key (optional-rest-key-parameters seen))
           (list (? (:seq '&aux (* (<<- aux (aux-parameter seen)))))))
  (if aux
      (append optional-rest-key (list aux))
      optional-rest-key))

(parser:defrule optional-rest-key-parameters (seen)
    (list (? (:seq '&optional (* (<<- optional (optional-parameter seen)))))
          (? (:seq '&rest (<- rest (rest-parameter seen))))
          (? (:seq '&key
                   (* (<<- keyword (keyword-parameter seen)))
                   (? (<- allow-other-keys? '&allow-other-keys)))))
  (list optional rest keyword allow-other-keys?))

;;; 3.4.2 Generic Function Lambda Lists

(parser:defrule generic-function-lambda-list (seen)
    (list* (* (<<- required (required-parameter seen)))
           (<- rest (optional-rest-key-parameters seen)))
  (list* required rest))

;;; 3.4.3 Specialized Lambda Lists

(parser:defrule specialized-parameter (seen)
    (or (list (<- name (required-parameter seen)) specializer)
        (<- name (required-parameter seen)))
    (list name specializer))

(parser:defrule specialized-lambda-list (seen)
    (list* (* (<<- required (specialized-parameter seen)))
           (<- rest (optional-rest-key-parameters seen)))
  (list* required rest))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

#+later (defgrammar destructuring-lambda-list
  (:use
   ordinary-lambda-list))
#+later (in-grammar destructuring-lambda-list)
#+later (defrule required-parameter (seen)
  (or (next-rule seen)
      (destructuring-lambda-list seen)))

(parser:defrule destructuring-lambda-list (seen)
    (list* (? (:seq '&whole (<- whole (unique-variable-name seen))))
           (* (<<- required (or (lambda-list-variable-name)
                                (destructuring-lambda-list seen))))
           (or '()
               (:compose (optional-rest-key-parameters seen)
                         (list optional rest key allow-other-keys?))
               (<- cdr (destructuring-lambda-list seen))))
  (list whole (nreverse required) optional rest key allow-other-keys? cdr))

;;; 3.4.8 Deftype Lambda Lists
;;;
;;; A deftype lambda list differs from a macro lambda list only in
;;; that if no init-form is supplied for an optional parameter or
;;; keyword parameter in the lambda-list, the default value for that
;;; parameter is the symbol * (rather than nil).

(parser:defrule deftype-lambda-list (seen)
    (destructuring-lambda-list seen))
