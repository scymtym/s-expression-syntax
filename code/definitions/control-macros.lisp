;;;; control-macros.lisp --- Standard macros for control.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macros `and' and `or'

(macrolet ((define (name)
             `(define-macro ,name
                  (list (* (<<- form ((form! forms)))))
                ((form * :evaluation t)))))
  (define and)
  (define or))

;;; Standard macro `cond'

(define-syntax cond-clause
    (list* (<- test-form ((form! forms))) (<- form ((forms forms))))
  ((test-form 1  :evaluation t)
   (form      *> :evaluation t)))

(defrule cond-clause! ()
  (must (cond-clause) "must be a clause of the form (TEST-FORM FORM*)"))

(define-macro cond
    (list (* (<<- clause (cond-clause!))))
  ((clause * :evaluation :compound)))

;;; Standard macros `when' and `unless'

(macrolet ((define (name)
             `(define-macro ,name
                  (list (<- test ((form! forms)))
                        (* (<<- form ((form! forms)))))
                ((test 1 :evaluation t)
                 (form * :evaluation t)))))
  (define when)
  (define unless))

;;; Standard macros `[ec]case'

(defrule otherwise-key ()
  (or 'otherwise 't))

(defrule normal-key (allow-otherwise?)
  (and (or (:transform (otherwise-key)
             (unless allow-otherwise? (:fail)))
           (not (otherwise-key)))
       ((unparsed-expression forms) ':key)))

(define-syntax (case-normal-clause :arguments ((allow-otherwise? t)))
    (list (or (list (* (<<- key ((unparsed-expression forms) ':key))))
              (<<- key (normal-key allow-otherwise?)))
          (* (<<- form ((form! forms)))))
  ((key  *)
   (form * :evaluation t)))

(defrule case-normal-clause! (allow-otherwise?)
  (must (case-normal-clause allow-otherwise?)
        "must be a clause of the form (KEY-OR-KEYS FORM*)"))

(define-syntax case-otherwise-clause
    (list (otherwise-key) (* (<<- form ((form! forms)))))
  ((form * :evaluation t)))

(defrule case-clauses ()
    (list (* (<<- clauses
                  (eg:element
                   (or (eg:once (case-otherwise-clause)
                                :flag otherwise? :name "otherwise clause")
                       (:transform (<- clause (case-normal-clause 'nil))
                         (when otherwise?
                           (:fatal "normal clause must not follow otherwise clause"))
                                   clause)
                       (:transform :any
                         (:fatal "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")))))))
  (nreverse clauses))

(define-macro case
    (list* (<- keyform ((form! forms))) (<- clause (case-clauses)))
  ((keyform 1  :evaluation t)
   (clause  *> :evaluation :compound)))

(define-macro ccase
    (list (<- keyplace ((place! forms)))
          (* (<<- clause (case-normal-clause! 't))))
  ((keyplace 1 :evaluation t)
   (clause   * :evaluation :compound)))

(define-macro ecase
    (list (<- keyform ((form! forms)))
          (* (<<- clause (case-normal-clause! 't))))
  ((keyform 1 :evaluation t)
   (clause  * :evaluation :compound)))

;;; Standard macros `[ec]typecase'

(define-syntax typecase-normal-clause
    (list (or (eg:element
               (or (:transform 'otherwise
                     (:fatal "CL:OTHERWISE does not name a type"))
                   (:transform (list* 'otherwise :any)
                     (:fatal "CL:OTHERWISE does not name a compound type"))))
              (<- type ((type-specifier! type-specifiers))))
          (* (<<- form ((form! forms)))))
  ((type 1)
   (form * :evaluation t)))

(defrule typecase-normal-clause! ()
  (must (typecase-normal-clause)
        "must be a clause of the form (TYPE FORM*)"))

(define-syntax typecase-otherwise-clause
    (list 'otherwise (* (<<- form ((form! forms)))))
  ((form * :evaluation t)))

(defrule typecase-clauses ()
    (list (* (<<- clauses
                  (eg:element
                   (or (eg:once (typecase-otherwise-clause)
                                :flag otherwise? :name "otherwise clause")
                       (:transform (<- clause (typecase-normal-clause))
                         (when otherwise?
                           (:fatal "normal clause must not follow otherwise clause"))
                                   clause)
                       (:transform :any
                         (:fatal "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")))))))
  (nreverse clauses))

(define-macro typecase
    (list* (<- keyform ((form! forms))) (<- clause (typecase-clauses)))
  ((keyform 1  :evaluation t)
   (clause  *> :evaluation :compound)))

(define-macro ctypecase
    (list (<- keyplace ((place! forms)))
          (* (<<- clause (typecase-normal-clause!))))
  ((keyplace 1 :evaluation t)
   (clause   * :evaluation :compound)))

(define-macro etypecase
    (list (<- keyform ((form! forms)))
          (* (<<- clause (typecase-normal-clause!))))
  ((keyform 1 :evaluation t)
   (clause  * :evaluation :compound)))

;;; Standard macros `prog' and `prog*'

(macrolet ((define (name order)
             (declare (ignore order))
             `(define-macro ,name
                  (list* (<- binding (value-bindings!))
                         (<- (declaration segment) ((tagbody-body forms))))
                ((binding     *> :evaluation :compound) ; TODO :order order
                 (declaration *>)
                 (segment     *> :evaluation :compound)))))
  (define prog  :parallel)
  (define prog* :sequential))

;;; Standard macros `prog1' and `prog2'
;;;
;;; Note that `progn' is a special operator, not a macro and therefore
;;; not defined here.

(define-macro prog1
    (list (must (<- first ((form! forms)))
                "must be of the form (prog1 FIRST-FORM FORM*)")
          (* (<<- rest ((form! forms)))))
  ((first 1 :evaluation t)
   (rest  * :evaluation t)))

(define-macro prog2
    (list (must (seq (<- first ((form! forms)))
                     (<- second ((form! forms))))
                "must be of the form (prog2 FIRST-FORM SECOND-FORM FORM*)")
          (* (<<- rest ((form! forms)))))
  ((first  1 :evaluation t)
   (second 1 :evaluation t)
   (rest   * :evaluation t)))

;;; Standard macros `handler-bind' and `handler-case'

(define-syntax handler-binding
    (list (<- type ((type-specifier! type-specifiers)))
          (<- form ((form! forms))))
  ((type 1)
   (form 1 :evaluation t)))

(defrule handler-binding! ()
  (must (handler-binding) "must be of the form (TYPE HANDLER-FORM)"))

(define-macro handler-bind
    (list* (must (list (* (<<- binding (handler-binding!))))
                 "must be a list of handler bindings")
           (<- form (forms)))
  ((binding *  :evaluation :compound)
   (form    *> :evaluation t)))

(define-syntax handler-clause
    (list* (<- type ((type-specifier! type-specifiers)))
           (must (list (? (<- variable ((required-parameter! lambda-lists) '()))))
                 "must be a lambda list with zero or one required parameter")
           (<- (declaration form) ((body forms))))
  ((type        1)
   (variable    ?  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil))
   (declaration *>)
   (form        *> :evaluation t)))

(define-syntax no-error-clause
    (list* :no-error
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (declaration form) ((body forms))))
  ((lambda-list 1  :evaluation :compound)
   (declaration *>)
   (form        *> :evaluation t)))

(define-macro handler-case
    (list (<- form ((form! forms)))
          (* (or (and (list* (eg:once :no-error) :any)
                      (<- no-error-clause (no-error-clause)))
                 (<<- clause (handler-clause))))
          (must (not :any)
                "must be a list of handler clauses"))
  ((form            1 :evaluation t)
   (clause          * :evaluation :compound)
   (no-error-clause ? :evaluation :compound)))

;;; Standard macros `restart-bind' and `restart-case'

(define-syntax restart-binding
    (list (or 'nil (<- name ((variable-name! names))))
          (<- function ((form! forms)))
          (* (or (eg:poption :interactive-function
                             (<- interactive-function ((form! forms))))
                 (eg:poption :report-function
                             (<- report-function ((form! forms))))
                 (eg:poption :test-function
                             (<- test-function ((form! forms)))))))
  ((name                 1)
   (function             1 :evaluation t)
   (interactive-function ? :evaluation t)
   (report-function      ? :evaluation t)
   (test-function        ? :evaluation t)))

(defrule restart-binding! ()
  (must (restart-binding) "must be of the form (NAME FUNCTION [OPTIONS])"))

(define-macro restart-bind
    (list* (must (list (* (<<- binding (restart-binding!))))
                 "must be a list of restart bindings")
           (<- form ((forms forms))))
  ((binding *  :evaluation :compound)
   (form    *> :evaluation t)))

(define-syntax restart-clause
    (list* (or 'nil (<- name ((variable-name! names))))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (* (or (eg:poption :interactive (or (<- interactive-name   ((function-name/symbol names)))
                                               (<- interactive-lambda (lambda-expression))))
                  (eg:poption :report      (or (<- report-string      (and (guard (typep 'string))
                                                                           ((unparsed-expression forms)
                                                                            ':restart-report-string)))
                                               (<- report-name        ((function-name/symbol names)))
                                               (<- report-lambda      (lambda-expression))))
                  (eg:poption :test        (or (<- test-name          ((function-name/symbol forms)))
                                               (<- test-lambda        (lambda-expression))))))
           (:transform (<- (declaration form) ((body forms)))
             (when (not (or name report-string report-name report-lambda))
               (:fatal "for an unnamed restart, the :REPORT option must be supplied"))))
  ((name               1)
   (lambda-list        1)
   (interactive-name   ?)
   (interactive-lambda ?)
   (report-string      ?)
   (report-name        ?)
   (report-lambda      ?)
   (test-name          ?)
   (test-lambda        ?)
   (declaration        *>)
   (form               *> :evaluation t)))

(define-macro restart-case
    (list (<- form ((form forms)))
          (* (<<- clause (restart-clause)))
          (must (not :any) "must be a list of restart clauses"))
  ((form   1 :evaluation t)
   (clause * :evaluation :compound)))
