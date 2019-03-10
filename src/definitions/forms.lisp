;;;; forms.lisp --- Rules for parsing forms and bodies.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

(parser:defrule form ()
  ;; (cl:declare …) must not appear where a form is required.
  (and (not (list* 'declare :any)) :any))

(parser:defrule forms ()
    (list (* (<<- forms (form))))
  (nreverse forms))

(parser:defrule body ()
    (list* (* (list 'declare (* (<<- declarations #+maybe (declaration)))))
           (<- forms (forms)))
  (list (nreverse declarations) forms))

(parser:defrule docstring-body ()
    ;; 3.4.11 Syntactic Interaction of Documentation Strings and Declarations
    ;; If the first form in the body is a string, it is a
    ;; documentation string. Exception: if the body consists of only
    ;; one form, the form is not a documentation string.
    (or (list (<- body (:transform (:guard form stringp)
                                   (list nil (list form)))))
        (list* (:guard docstring stringp) (<- body (body)))
        (<- body (body)))
  (list* docstring body))
