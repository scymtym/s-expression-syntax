;;;; forms.lisp --- Rules for parsing forms and bodies.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar forms
  (:class eg::expression-grammar)
  (:use declarations))

(parser:in-grammar forms)

;;; Documentation strings

(defrule documentation-string ()
    (guard stringp))

(defrule documentation-string! ()
    (must (documentation-string) "must be a documentation string"))

;;; Forms

#+later (parser:defrule declaration ()
    (list 'declare (* (<<- declarations #+maybe ((declaration declarations)))))
  (nreverse declarations))

(defrule form ()
    ;; (cl:declare …) must not appear where a form is required.
    ;; CL:DECLAIM warns in SBCL
    (and (must (not (list* 'declare :any)) "declare is not allowed here")
         :any))

(defrule form! () ; TODO use this where appropriate
    (must (form) "must be a form"))

(defrule compound-form ()
    (and (list* :any :any) (form)))

(defrule compound-form! ()
    (must (compound-form) "must be a compound form"))

(defrule forms ()
    (list (* (<<- forms (form))))
  (nreverse forms))

;; TODO do we need forms! ?

(defrule body ()
    (list* (* (list 'declare (* (<<- declarations ((declaration! declarations))))))
           (<- forms (forms)))
  (list (nreverse declarations) forms))

(defrule docstring-body ()
    ;; 3.4.11 Syntactic Interaction of Documentation Strings and Declarations
    ;; If the first form in the body is a string, it is a
    ;; documentation string. Exception: if the body consists of only
    ;; one form, the form is not a documentation string.
    (or (list (<- body (:transform (guard form stringp)
                         (list () (list form)))))
        (list* (<- docstring (documentation-string)) (<- body (body)))
        (<- body (body)))
  (list* docstring body))
