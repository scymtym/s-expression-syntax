;;;; bindings.lisp --- Rules for binding constructs.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Value bindings

(defrule value-binding ()
    (value (source)
      (or (:transform
             (or (<- name ((variable-name names))) ; TODO repeated variable names
                 (list (<- name ((variable-name! names)))))
           (setf value nil))
          (list (<- name ((variable-name! names)))
                (<- value ((form! forms))))))
  (list name value))

(defrule value-binding! ()
  (must (value-binding) "must be a binding of the form NAME, (NAME) or (NAME FORM)"))

(defrule value-bindings ()
    (list (* (<<- (names values) (value-binding!))))
  (list (nreverse names) (nreverse values)))

(defrule value-bindings! ()
  (must (value-bindings) "must be a list of bindings"))

;;; Function bindings

(define-syntax local-function
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1 :evaluation :compound)
   (documentation ?)
   (declarations  *)
   (forms         * :evaluation t)))

(defrule local-function-binding ()
    (<- function (local-function))
  (list (bp:node-relation* :name function) function))

(defrule local-function-binding! ()
    (must (local-function-binding)
          "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)"))

(defrule function-bindings ()
    (list (* (<<- (names functions) (local-function-binding!))))
  (list (nreverse names) (nreverse functions)))

;;; Macro function bindings

(define-syntax local-macro-function
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((destructuring-lambda-list! destructuring-lambda-list)))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1 :evaluation :compound)
   (documentation ?)
   (declarations  *)
   (forms         *)))

(defrule local-macro-function-binding ()
    (<- function (local-macro-function))
  (list (bp:node-relation* :name function) function))

(defrule local-macro-function-binding! ()
    (must (local-macro-function-binding)
          "must be of the form (NAME LAMBDA-LIST [DECLARATIONS] FORMS*)"))

(defrule macro-function-bindings ()
    (list (* (<<- (names functions) (local-macro-function-binding!))))
  (list (nreverse names) (nreverse functions)))

;;; Symbol-macro bindings

(defrule symbol-macro-binding ()
    (list (<- name ((variable-name! names)))
          (<- form ((form! forms))))
  (list name form))

(defrule symbol-macro-binding! ()
  (must (symbol-macro-binding) "must be a binding of the form (NAME FORM)"))

(defrule symbol-macro-bindings ()
    (list (* (<<- (names forms) (symbol-macro-binding!))))
  (list (nreverse names) (nreverse forms)))
