;;;; bindings.lisp --- Rules for binding constructs.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Value bindings

(define-syntax value-binding
    (or (:transform
         (or (<- name ((variable-name names))) ; TODO repeated variable names
             (list (<- name ((variable-name! names)))))
         (setf value nil))
        (list (<- name ((variable-name! names)))
              (<- value ((form! forms)))))
  ((name  1 :evaluation (make-instance 'binding-semantics
                                       :namespace 'variable
                                       :scope     :lexical
                                       :order     :parallel
                                       :values    'value))
   (value ? :evaluation t)))

(defrule value-binding! ()
  (must (value-binding) "must be a binding of the form NAME, (NAME) or (NAME FORM)"))

(defrule value-bindings ()
    (list (* (<<- bindings (value-binding!))))
  (nreverse bindings))

(defrule value-bindings! ()
  (must (value-bindings) "must be a list of bindings"))

;;; Function bindings

(define-syntax local-function-binding
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1 :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    'functions))
   (lambda-list   1 :evaluation :compound)
   (documentation ?)
   (declaration   *)
   (form          * :evaluation t)))

(defrule local-function-binding! ()
  (must (local-function-binding)
        "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)"))

(defrule function-bindings ()
    (list (* (<<- bindings (local-function-binding!))))
  (nreverse bindings))

(defrule function-bindings! ()
  (must (function-bindings) "must be a list of function bindings"))

;;; Macro function bindings

(define-syntax local-macro-function-binding
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((destructuring-lambda-list! destructuring-lambda-list)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1 :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :values    'functions))
   (lambda-list   1 :evaluation :compound)
   (documentation ?)
   (declaration   *)
   (form          * :evaluation t)))

(defrule local-macro-function-binding! ()
    (must (local-macro-function-binding)
          "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)"))

(defrule macro-function-bindings ()
    (list (* (<<- bindings (local-macro-function-binding!))))
  (nreverse bindings))

(defrule macro-function-bindings! ()
  (must (macro-function-bindings) "must be a list of macro bindings"))

;;; Symbol-macro bindings

(define-syntax symbol-macro-binding
    (list (<- name ((variable-name! names))) (<- expansion ((form! forms))))
  ((name      1 :evaluation (make-instance 'binding-semantics
                                           :namespace 'value
                                           :scope     :lexical
                                           :values    'expansion))
   (expansion 1 :evaluation t)))

(defrule symbol-macro-binding! ()
  (must (symbol-macro-binding) "must be a binding of the form (NAME FORM)"))

(defrule symbol-macro-bindings ()
    (list (* (<<- bindings (symbol-macro-binding!))))
  (nreverse bindings))

(defrule symbol-macro-bindings! ()
  (must (symbol-macro-bindings) "must be a list of symbol macro bindings"))
