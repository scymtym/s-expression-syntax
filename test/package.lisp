(cl:defpackage #:syntax.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:syntax)

  (:export
   #:run-tests))

(cl:in-package #:syntax.test)

(def-suite :syntax)

(defun run-tests ()
  (run! :syntax))

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
             (parse nil syntax input)))
      (case expected
        (invalid-syntax-error
         (signals invalid-syntax-error (do-it)))
        (t
         (is (equal expected (do-it))))))))

(defmacro syntax-test-cases ((syntax-name) &body cases)
  `(let ((syntax (find-syntax ',syntax-name)))
     (mapc (curry #'%syntax-test-case syntax)
           (list ,@cases))))
