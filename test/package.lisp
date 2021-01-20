;;;; package.lisp --- Package definition for tests of the s-expression-syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:s-expression-syntax.test
  (:use
   #:cl
   #:alexandria

   #:fiveam)

  (:local-nicknames
   (#:syn #:s-expression-syntax))

  (:export
   #:run-tests))

(cl:in-package #:s-expression-syntax.test)

(def-suite :s-expression-syntax)

(defun run-tests ()
  (run! :s-expression-syntax))

;;; Utilities

(defun %rule-test-case (grammar rule arguments case)
  (destructuring-bind
      (input expected-success? expected-position expected-value) case
    (declare (ignore expected-position))
    (let ((arguments (map 'list #'funcall arguments)))
      (multiple-value-bind (success? position value)
          (parser.packrat:parse `(,rule ,@arguments) input :grammar grammar)
        (declare (ignore position))
        (is (eq expected-success? success?))
        (is (equal expected-value value))))))

(defmacro rule-test-cases (((rule grammar) &rest arguments) &body cases)
  (let ((arguments (map 'list (lambda (argument) `(lambda () ,argument))
                        arguments)))
    `(mapc (curry #'%rule-test-case ',grammar ',rule (list ,@arguments))
           (list ,@cases))))

(defun %syntax-test-case (syntax case)
  (destructuring-bind (input expected) case
    (flet ((do-it ()
             (syn:parse nil syntax input)))
      (case expected
        (syn:invalid-syntax-error
         (signals syn:invalid-syntax-error (do-it)))
        (t
         (is (equal expected (do-it))))))))

(defmacro syntax-test-cases ((syntax-name) &body cases)
  `(let ((syntax (syn:find-syntax ',syntax-name)))
     (mapc (curry #'%syntax-test-case syntax)
           (list ,@cases))))
