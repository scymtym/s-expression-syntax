;;;; types.lisp --- Rules for parsing type specifiers.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar type-specifiers
  (:class eg::expression-grammar)
  (:use names))

(parser:in-grammar type-specifiers)

;;; General type specifier syntax

(defrule subsidiary-item ()
    (value (source) value)
  (let ((value (eg::%naturalize value)))
    (bp:node* (:subsidiary-item :value value :source source))))

(defrule compound-type-specifier (must?)
    (value (source)
      (list (or (and (:transform :any (unless must? (:fail)))
                     (<- name ((class-name! names))))
                (<- name ((class-name names))))
            (* (<<- arguments (or (%type-specifier 'nil)
                                  (subsidiary-item)
                                  )))))
  (bp:node* (:compound-type-specifier :source source)
    (1 (:name     . 1) name)
    (* (:argument . *) (nreverse arguments))))

(defrule atomic-type-specifier ()
    (value (source)
      (guard name (typep 'symbol))) ; TODO control whether * is allowed, use class-name or type-name from names grammar
  (let ((name (eg::%naturalize name)))
    (bp:node* (:atomic-type-specifier :source source)
      (1 (:name . 1) (bp:node* (:type-name :name name :source source))))))

;;; Well-known compound specifiers

(defrule wildcard-type-specifer ()
    (value (source) '*)
  (bp:node* (:wildcard-type-specifier :source source)))

(defrule keyword-parameter-type-specifier ()
    (value (source)
      (list (<- keyword ((parameter-keyword! names)))
            (<- type    (type-specifier!))))
  (bp:node* (:keyword-parameter-type-specifier :source source)
    (1 (:keyword . 1) keyword)
    (1 (:type    . 1) type)))

(defrule parameter-type-specifier ()
    (value (source)
      (list (* (<<- required (and :any
                                  (not (or '&optional '&rest '&key '&allow-other-keys))
                                  (type-specifier!))))
            (? (seq '&optional (* (<<- optional (type-specifier!)))))
            (? (seq '&rest     (must (<- rest (type-specifier!))
                                     "type specifier must follow &REST")))
            (? (seq '&key      (* (<<- keyword (keyword-parameter-type-specifier)))))))
  (bp:node* (:parameter-type-specifier :source source)
    (*    (:required . *) (nreverse required))
    (*    (:optional . *) (nreverse optional))
    (bp:? (:rest     . 1) rest)
    (*    (:keyword  . *) keyword)))

(defrule parameter-type-specifier! ()
  (must (parameter-type-specifier)
        "must be of the form (REQUIRED* [&optional OPTIONAL*] [&rest REST] [&key KEY* [&allow-other-keys]])"))

(defrule values-type-specifier ()
    (value (source)
      (list* 'values
            (must
             (list (* (<<- required (and :any
                                         (not (or '&optional '&rest '&allow-other-keys))
                                         (type-specifier/not-*! 'values))))
                   (? (seq '&optional (* (<<- optional (and :any
                                                            (not (or '&rest '&allow-other-keys))
                                                            (type-specifier/not-*! 'values))))))
                   (? (seq '&rest (must (<- rest (type-specifier/not-*! 'values))
                                        "type specifier must follow &REST"))))
             "must be of the form (values REQUIRED* [&optional OPTIONAL*] [&rest REST])")))
  (bp:node* (:values-type-specifier :source source)
    (*    (:required . *) (nreverse required))
    (*    (:optional . *) (nreverse optional))
    (bp:? (:rest     . 1) rest)))

(defrule function-type-specifier ()
    (value (source)
      (or 'function
          (list 'function
                (? (<- parameters (or (wildcard-type-specifer)
                                      (parameter-type-specifier!))))
                (? (<- values     (or (wildcard-type-specifer)
                                      (values-type-specifier)))))))
  (bp:node* (:function-type-specifier :source source)
    (bp:? (:parameters . 1) parameters)
    (bp:? (:values     . 1) values)))

(defrule function-type-specifier! ()
  (must (function-type-specifier) "must be a function type specifier"))

;;;

(defrule %type-specifier (list-must-be-compound?)
  (and (must (not (list* 'values :any)) "VALUES type is invalid in this context")
       (must (not 'values) "the symbol VALUES is not a valid type specifier")
       (or (atomic-type-specifier)
           (function-type-specifier)
           (compound-type-specifier list-must-be-compound?)
           ;; (list (guard (typep 'symbol)) (* :any))
           )))

(defrule type-specifier ()
  (%type-specifier 't))

(defrule type-specifier! ()
  (must (type-specifier) "must be a type specifier"))

(defrule type-specifier/not-*! (context)
  (or (:transform '* (:fatal (format nil "the type specifier * is not ~
                                          allowed within a ~A type specifier"
                                     context)))
      (type-specifier!)))
