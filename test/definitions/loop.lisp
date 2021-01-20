;;;; loop.lisp --- Tests for loop rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.standard-macros.loop
  :in :s-expression-syntax.standard-macros)

;;; Helper rules

(test loop-form-or-it

  (parser.packrat:parse '(syn::loop-form-or-it :unconditional) ':it))

(parser.packrat:parse '(syn::extended-loop)
                      '(:named foo :with 1)
                      :grammar 'syn::special-operators)


;;; Clauses

(test extended-loop
  "Test for the `extended-loop' rule."

  (rule-test-cases ((syn::extended-loop syn::special-operators))
    '((:named foo :with 1)
      :fatal 1 "variable name must be a symbol")))

#+TODO (defrule loop-with-clauses-helper ()
   (list :skip (* (loop-with-clause)))
   1)

#+TODO (test loop-with-clause
  "Test for the `loop-with-clause' rule."

  (rule-test-cases ((loop-with-clauses-helper syn::special-operators))
    '((:skip with bar with foo) t t t)))

;;; `loop' macro

(test loop
  "Test for the `loop' standard macro syntax."

  (syntax-test-cases (loop)
    '((loop)           (syn::clauses (:loop)))
    '((loop named foo) (syn::clauses (:loop :name foo)))))
