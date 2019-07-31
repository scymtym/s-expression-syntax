;;;; names.lisp --- Tests for name-related rules.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.names
  :in :syntax)

;;; Names

(test variable-name
  "Smoke test for the `variable-name' rule."

  (is-false (parser.packrat:parse '(syntax::variable-name) :foo)))

(test function-name
  "Smoke test for the `function-name' rule."

  (is-false (parser.packrat:parse '(syntax::function-name) 1))
  (is-true (parser.packrat:parse '(syntax::function-name) 'foo))

  (is-true (parser.packrat:parse '(syntax::function-name) '(setf foo)))
  (is-false (parser.packrat:parse '(syntax::function-name) '(setf 1))))

;;; References

(test function-reference
  "Smoke test for `function-reference' rule."

  (is-false (parser.packrat.parse '(syntax::function-reference) '(function 1)))
  (is-true (parser.packrat.parse '(syntax::function-reference) '(function foo)))

  (is-false (parser.packrat.parse '(syntax::function-reference) '(function (setf 1))))
  (is-true (parser.packrat.parse '(syntax::function-reference) '(function (setf foo)))))
