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

;;; Unparsed expressions

(defrule unparsed-expression (context)
    (value (source) expression)
  (bp:node* (:unparsed :expression expression
                       :context    context
                       :source     source)))

;;; Documentation strings

(defrule documentation-string ()
    (value (source) (guard string stringp))
  (bp:node* (:documentation :string string :source source)))

(defrule documentation-string! ()
  (must (documentation-string) "must be a documentation string"))

;;; Forms

#+later (parser:defrule declaration ()
    (list 'declare (* (<<- declarations #+maybe ((declaration-specifier declarations)))))
  (nreverse declarations))

(defrule form ()
  ;; (cl:declare …) must not appear where a form is required.
  ;; CL:DECLAIM warns in SBCL
  (and (must (not (list* 'declare :any)) "declare is not allowed here")
       (unparsed-expression ':form)))

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
       ;; See `form' rule.
       (must (not (list* 'declare :any)) "declare is not allowed here")
       (unparsed-expression ':place)))

(defrule place! ()
  (or (place)
      (:transform (guard (typep 'keyword))
        (:fatal "place must not be a keyword"))
      (:transform (constant)
        (:fatal "place must not be a constant variable"))
      (:transform :any
        (:fatal "place must be a cons or a variable name"))))

(defrule body ()
    (list* (* (list 'declare
                    (* (<<- declarations
                            ((declaration-specifier! declarations))))))
           (<- forms (forms)))
  (list (nreverse declarations) forms))

(defrule docstring-body ()
    ;; 3.4.11 Syntactic Interaction of Documentation Strings and Declarations
    ;; If the first form in the body is a string, it is a
    ;; documentation string. Exception: if the body consists of only
    ;; one form, the form is not a documentation string.
    (or (list (and (guard stringp)
                   (<- body (:transform (<- form (form))
                              (list () (list form))))))
        (list* (<- docstring (documentation-string)) (<- body (body)))
        (<- body (body)))
  (list* docstring body))

(defrule (tagbody-segment :environment (make-instance 'eg::expression-environment)) (seen first?)
    (value (source)
      (seq (<- label (or (and (:transform :any (unless first? (:fail)))
                              (or (and ((integer-or-symbol names))
                                       ((unique-tag! names) seen))
                                  (seq)))
                         ((unique-tag! names) seen)))
           (* (<<- statements (and (not ((integer-or-symbol names)))
                                   ((form! forms)))))))
  (let ((statements (nreverse statements)))
    (bp:node* (:tagbody-segment :source source)
      (bp:? (:label     . 1) label      :evaluation (make-instance 'binding-semantics
                                                                   :namespace 'tag
                                                                   :scope     :lexical
                                                                   :values    nil))
      (*    (:statement . *) statements :evaluation (a:circular-list t)))))

(defrule tagbody-segments ()
    (list (? (and (<- seen (:transform :any (make-hash-table)))
                  (<<- segments (tagbody-segment seen 't))))
          (* (<<- segments (tagbody-segment seen 'nil))))
  (nreverse segments))

(defrule tagbody-body ()
    (list* (* (list 'declare (* (<<- declarations ((declaration-specifier! declarations))))))
           (<- segments (tagbody-segments)))
  (list (nreverse declarations) segments))
