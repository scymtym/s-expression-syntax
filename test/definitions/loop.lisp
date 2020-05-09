;;;; loop.lisp --- Tests for loop rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.standard-macros.loop
  :in :syntax.standard-macros)

;;; Helper rules

(test loop-form-or-it

  (parser.packrat:parse '(syntax::loop-form-or-it :unconditional) ':it))

;;; Clauses

(test extended-loop
  "Test for the `extended-loop' rule."

  (rule-test-cases ((syntax::extended-loop syntax::special-operators))
    '((:named foo :with 1)
      :fatal 1 "variable name must be a symbol")))

#+TODO (defrule loop-with-clauses-helper ()
   (list :skip (* (loop-with-clause)))
   1)

#+TODO (test loop-with-clause
  "Test for the `loop-with-clause' rule."

  (rule-test-cases ((loop-with-clauses-helper syntax::special-operators))
    '((:skip with bar with foo) t t t)))

;;; `loop' macro

(test loop
  "Test for the `loop' standard macro syntax."

  (syntax-test-cases (loop)
    '((loop)           (syntax::clauses (:loop)))
    '((loop named foo) (syntax::clauses (:loop :name foo)))))
