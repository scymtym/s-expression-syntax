;;;; forms.lisp --- Rules for parsing forms and bodies.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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

(defrule forms ()
    (list (* (<<- forms (form))))
  (nreverse forms))

(defrule compound-form ()
  (and (list* :any :any) (form)))

(defrule compound-form! ()
  (must (compound-form) "must be a compound form"))

(defrule place ()
  (and (or (guard (typep 'cons))
           (and (guard (typep 'symbol))
                (not (guard (typep 'keyword)))
                (not (constant))))
       (form)))

(defrule place! ()
  (or (place)
      (:transform (guard (typep 'keyword))
        (:fatal "place must not be a keyword"))
      (:transform (constant)
        (:fatal "place must not be a constant variable"))
      (:transform :any
        (:fatal "place must be a cons or a variable name"))))

(defrule body ()
    (list* (* (list 'declare (* (<<- declarations ((declaration! declarations))))))
           (<- forms (forms)))
  (list (nreverse declarations) forms))

(defrule docstring-body ()
    ;; 3.4.11 Syntactic Interaction of Documentation Strings and Declarations
    ;; If the first form in the body is a string, it is a
    ;; documentation string. Exception: if the body consists of only
    ;; one form, the form is not a documentation string.
    (or (list (<- body (:transform (and (guard stringp) (<- form (form)))
                         (list () (list form)))))
        (list* (<- docstring (documentation-string)) (<- body (body)))
        (<- body (body)))
  (list* docstring body))
