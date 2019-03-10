;;;; bindings.lisp --- Rules for binding constructs.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

(parser:defrule value-bindings ()
    (list (* (or (:transform
                  (or (<<- names (variable-name))
                      (list (<<- names (variable-name))))
                  (push nil values))
                 (list (<<- names (variable-name))
                       (<<- values)))))
  (list (nreverse names) (nreverse values)))

(parser:defrule symbol-macro-bindings ()
    (list (* (list (<<- names (variable-name))
                   (<<- values))))
  (list (nreverse names) (nreverse values)))

(parser:defrule local-function ()
    (list* (<- name (function-name))
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil)) ; TODO macro lambda-list for macrolet
           (:compose (docstring-body) (list docstring declarations forms)))
  (list name (list 'parsed-lambda lambda-list docstring declarations forms)))

(parser:defrule function-bindings ()
    (list (* (:compose (local-function) (list (<<- names)
                                              (<<- functions)))))
  (list (nreverse names) (nreverse functions)))
