;;;; package.lisp --- Package definition for tests of the s-expression-syntax system.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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

(defun check-parse-result-value-and-position
    (input expected-value value
     &optional (expected-position nil expected-position-p)
               position)
  (is (ast-equal expected-value value)
      "~@<For input expression ~S, expected value ~S but got ~S.~@:>"
      input expected-value value)
  (when expected-position-p
    (let ((expected-position (if (eq expected-position :input)
                                 input
                                 expected-position)))
      (is (eq expected-position position)
          "~@<For input expression ~S, expected position ~S but got ~S.~@:>"
          input expected-position position))))

(defun %rule-test-case (grammar rule arguments case)
  (destructuring-bind
      (input expected-success? expected-position expected-value) case
    (let ((arguments (map 'list #'funcall arguments)))
      (multiple-value-bind (success? position value)
          (architecture.builder-protocol:with-builder ('list)
            (parser.packrat:parse `(,rule ,@arguments) input :grammar grammar))
        (is (eq expected-success? success?)
            "~@<For input expression ~S, expected success ~S at position ~S ~
             but got success ~S at position ~S~@:>"
            input expected-success? expected-position success? position)
        (apply #'check-parse-result-value-and-position
               input expected-value value
               (unless (eq expected-success? t)
                 (list expected-position position)))))))

(defmacro rule-test-cases (((rule grammar) &rest arguments) &body cases)
  (let ((arguments (map 'list (lambda (argument) `(lambda () ,argument))
                        arguments)))
    `(mapc (curry #'%rule-test-case ',grammar ',rule (list ,@arguments))
           (list ,@cases))))

(defun ast-equal (left right)
  (labels ((rec (left right)
             (typecase left
               ((cons (eql :binding) cons)
                (destructuring-bind (&key namespace scope) (rest left)
                  (and (typep right 'syn::binding-semantics)
                       (eq (syn::namespace right) namespace)
                       (eq (syn::scope     right) scope))))
               ((cons (eql :reference) cons)
                (destructuring-bind (&key namespace) (rest left)
                  (and (typep right 'syn::reference-semantics)
                       (eq (syn::namespace right) namespace))))
               ((cons (eql :assignment))
                (destructuring-bind (&key namespace) (rest left)
                  (and (typep right 'syn::assignment-semantics)
                       (eq (syn::namespace right) namespace))))
               (cons
                (and (consp right)
                     (rec (car left) (car right))
                     (rec (cdr left) (cdr right))))
               (vector
                (and (= (length left) (length right))
                     (every #'rec left right)))
               (t
                (eql left right)))))
    (rec left right)))

(defun %syntax-test-case (syntax case)
  (destructuring-bind (input expected &optional (expected-value :input)
                                                expected-message)
      case
    (flet ((do-it ()
             (syn:parse 'list syntax input)))
      (case expected
        (syn:invalid-syntax-error
         (signals (syn:invalid-syntax-error
                   "~@<For input expression ~S, the ~S parser did not signal ~
                    an error.~@:>"
                   input (syn:name syntax) expected)
           (do-it))
         (handler-case (do-it)
           (syn:invalid-syntax-error (condition)
             (is (eq syntax (syn:syntax condition)))
             (let ((value (syn:expression condition)))
               (check-parse-result-value-and-position
                input nil nil expected-value value))
             (when expected-message
               (let ((message (syn:message condition)))
                 (is (string= expected-message message)
                     "~@<For input expression ~S, expected message ~S, but ~
                      got ~S.~@:>"
                     input expected-message message))))))
        (t
         (check-parse-result-value-and-position
          input expected (do-it)))))))

(defmacro syntax-test-cases ((syntax-name) &body cases)
  `(let ((syntax (syn:find-syntax ',syntax-name)))
     (mapc (curry #'%syntax-test-case syntax)
           (list ,@cases))))

(defmacro define-syntax-test ((syntax-name) &body cases)
  `(test ,syntax-name
     ,(format nil "Test for the `~(~A~)' special operator syntax."
              syntax-name)
     (syntax-test-cases (,syntax-name) ,@cases)))
