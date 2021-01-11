;;;; bindings.lisp --- Rules for binding constructs.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

;;; Value bindings

(defrule value-binding ()
    (or (:transform
         (or (<- name ((variable-name names))) ; TODO repeated variable names
             (list (<- name ((variable-name! names)))))
         (setf value nil))
        (list (<- name ((variable-name! names)))
              (<- value ((form! forms)))))
  (list name value))

(defrule value-binding! ()
  (must (value-binding) "must be a binding of the form NAME, (NAME) or (NAME FORM)"))

(defrule value-bindings ()
    (list (* (<<- (names values) (value-binding!))))
  (list (nreverse names) (nreverse values)))

(defrule value-bindings! ()
  (must (value-bindings) "must be a list of bindings"))

;;; Function bindings

(defrule local-function ()
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil)) ; TODO macro lambda-list for macrolet
           (<- (docstring declarations forms) ((docstring-body forms))))
  (list name (list 'parsed-lambda lambda-list docstring declarations forms)))

(defrule function-bindings ()
    (list (* (<<- (names functions) (local-function))))
  (list (nreverse names) (nreverse functions)))

;;; Symbol-macro bindings

(defrule symbol-macro-binding ()
    (list (<- name ((variable-name! names)))
          (<- value ((form! forms))))
  (list name value))

(defrule symbol-macro-binding! ()
  (must (symbol-macro-binding) "must be a binding of the form (NAME FORM)"))

(defrule symbol-macro-bindings ()
    (list (* (<<- (names values) (symbol-macro-binding!))))
  (list (nreverse names) (nreverse values)))
