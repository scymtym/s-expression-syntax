;;;; bindings.lisp --- Rules for binding constructs.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

(parser:defrule value-bindings ()
    (list (* (or (:transform
                  (or (<<- names (variable-name)) ; TODO repeated variable names
                      (list (<<- names (variable-name))))
                  (push nil values))
                 (list (:must (<<- names (variable-name)) "must be a variable name")
                       (<<- values (form!))))))
  (list (nreverse names) (nreverse values)))

(parser:defrule value-bindings! ()
  (:must (value-bindings) "must be a list of bindings"))

(parser:defrule symbol-macro-bindings ()
    (list (* (list (<<- names (variable-name))
                   (<<- values))))
  (list (nreverse names) (nreverse values)))

(parser:defrule local-function ()
    (list* (<- name (function-name))
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil)) ; TODO macro lambda-list for macrolet
           (<- (docstring declarations forms) (docstring-body)))
  (list name (list 'parsed-lambda lambda-list docstring declarations forms)))

(parser:defrule function-bindings ()
    (list (* (<<- (names functions) (local-function))))
  (list (nreverse names) (nreverse functions)))
