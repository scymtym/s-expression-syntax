;;;; classify.lisp --- Tests for the form classifier.
;;;;
;;;; Copyright (C) 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(in-suite* :s-expression-syntax)

(test classify.smoke
  "Smoke test for the `classify' function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected) input-and-expected
            (let ((expected-syntax (syn:find-syntax expected))
                  (actual-syntax   (syn::classify t input)))
              (is (eq expected-syntax actual-syntax)
                  "When classifying ~S, expected syntax ~A but got ~A"
                  input expected-syntax actual-syntax))))
        '((foo     syn::variable-reference)

          (:foo    syn::self-evaluating)
          (1       syn::self-evaluating)
          ("foo"   syn::self-evaluating)
          (#()     syn::self-evaluating)

          ((foo)   syn::application)

          ((progn) progn))))
