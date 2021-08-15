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
          (architecture.builder-protocol:with-builder ('list)
            (parser.packrat:parse `(,rule ,@arguments) input :grammar grammar))
        (declare (ignore position))
        (is (eq expected-success? success?))
        (is (ast-equal expected-value value))))))

(defmacro rule-test-cases (((rule grammar) &rest arguments) &body cases)
  (let ((arguments (map 'list (lambda (argument) `(lambda () ,argument))
                        arguments)))
    `(mapc (curry #'%rule-test-case ',grammar ',rule (list ,@arguments))
           (list ,@cases))))

(defun ast-equal (left right)
  (labels ((rec (left right)
             (typecase left
               (cons                         (and (consp right)
                                                  (rec (car left) (car right))
                                                  (rec (cdr left) (cdr right))))
               (vector                       (and (= (length left) (length right))
                                                  (every #'rec left right)))
               ((member :binding :reference) t)
               (t                            (eql left right)))))
    (rec left right)))

(defun %syntax-test-case (syntax case)
  (destructuring-bind (input expected
                       &optional expected-value expected-message)
      case
    (flet ((do-it ()
             (syn:parse 'list syntax input)))
      (case expected
        (syn:invalid-syntax-error
         (signals (syn:invalid-syntax-error
                   "~@<For input form ~S, the ~S parser did not signal ~
                   an error.~@:>"
                   input (syn:name syntax) expected)
           (do-it))
         (handler-case (do-it)
           (syn:invalid-syntax-error (condition)
             (is (eq syntax (syn:syntax condition)))
             (when expected-value
               (let ((value (syn::value condition)))
                 (is (eql expected-value value)
                     "~@<For input form ~S, expected value ~S, but got ~
                      ~S.~@:>"
                     input expected-value value)))
             (when expected-message
               (let ((message (syn::message condition)))
                 (is (string= expected-message message)
                     "~@<For input form ~S, expected message ~S, but ~
                      got ~S.~@:>"
                     input expected-message message))))))
        (t
         (is (ast-equal expected (do-it))))))))

(defmacro syntax-test-cases ((syntax-name) &body cases)
  `(let ((syntax (syn:find-syntax ',syntax-name)))
     (mapc (curry #'%syntax-test-case syntax)
           (list ,@cases))))
