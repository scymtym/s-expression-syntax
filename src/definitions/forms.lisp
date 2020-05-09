;;;; forms.lisp --- Rules for parsing forms and bodies.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

#+later (parser:defrule declaration ()
    (list 'declare (* (<<- declarations #+maybe (declaration))))
  (nreverse declarations))

(parser:defrule form ()
    ;; (cl:declare …) must not appear where a form is required.
    ;; CL:DECLAIM warns in SBCL
    (and (:must (not (list* 'declare :any)) "declare is not allowed here")
         :any))

(parser:defrule form! () ; TODO use this where appropriate
    (:must (form) "must be a form"))

(parser:defrule compound-form ()
    (and (list* :any :any) (form)))

(parser:defrule compound-form! ()
    (:must (compound-form) "must be a compound form"))

(parser:defrule forms ()
    (list (* (<<- forms (form))))
  (nreverse forms))

;; TODO do we need forms! ?

(parser:defrule body ()
  (list* (* (and (<<- declarations-raw) (list 'declare (* (<<- declarations (declaration!))))))
         (and (<- forms (forms)) forms-raw))
  (list (nreverse declarations) #+later declarations-raw forms #+later forms-raw))

(parser:defrule docstring-body ()
    ;; 3.4.11 Syntactic Interaction of Documentation Strings and Declarations
    ;; If the first form in the body is a string, it is a
    ;; documentation string. Exception: if the body consists of only
    ;; one form, the form is not a documentation string.
    (or (list (<- body (:transform (:guard form stringp)
                         (list nil #+later nil (list form) #+later form))))
        (list* (:guard docstring stringp) (<- body (body)))
        (<- body (body)))
  (list* docstring body))
