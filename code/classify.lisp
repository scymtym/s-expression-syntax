;;;; classify.lisp --- Find the correct syntax to parse a given form.
;;;;
;;;; Copyright (C) 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defun classify (expression)
  (multiple-value-bind (success syntax)
      (parser.packrat:parse
       '(or (:transform (list* (guard operator symbolp) :any)
              (let ((operator (eg::%naturalize operator)))
                (a:if-let ((syntax (find-syntax operator :if-does-not-exist nil)))
                  syntax
                  (:fail))))
            (:transform (list* :any)
              (load-time-value (find-syntax 'application)))
            (:transform (and (guard symbolp) (not (guard keywordp)))
              (load-time-value (find-syntax 'variable-reference)))
            (:transform :any
              (load-time-value (find-syntax 'self-evaluating))))
       expression :grammar 'special-operators)
    (assert (eq success t))
    syntax))

(defmethod parse ((client t) (syntax (eql t)) (expression t))
  (parse client (classify expression) expression))
