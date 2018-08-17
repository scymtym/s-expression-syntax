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

(defun %rule-test-case (rule case)
  (destructuring-bind
      (input expected-success? expected-position expected-value) case
    (declare (ignore expected-position))
    (multiple-value-bind (success? position value)
        (parser.packrat:parse `(,rule) input)
      (declare (ignore position))
      (is (eq expected-success? success?))
      (is (equal expected-value value)))))

(defmacro rule-test-cases ((rule) &body cases)
  `(mapc (curry #'%rule-test-case ',rule)
         (list ,@cases)))
