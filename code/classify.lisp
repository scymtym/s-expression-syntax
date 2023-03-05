;;;; classify.lisp --- Find the correct syntax to parse a given form.
;;;;
;;;; Copyright (C) 2021,2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(defmethod classify ((client t) (expression t))
  (multiple-value-bind (success syntax)
      (parser.packrat:parse
       '(or (:transform (list* (structure 'symbol
                                          (symbol-name symbol-name)
                                          (symbol-package (structure 'package
                                                                     (package-name package-name))))
                               :any)
              (or (a:when-let* ((package  (find-package package-name))
                                (operator (find-symbol symbol-name package))
                                (syntax   (find-syntax operator :if-does-not-exist nil)))
                    syntax)
                  (:fail)))
            (:transform (and (not 'nil) (list* :any))
              (load-time-value (find-syntax 'application)))
            (:transform (and (guard (typep 'symbol)) (not (guard (typep 'keyword))))
              (load-time-value (find-syntax 'variable-reference)))
            (:transform :any
              (load-time-value (find-syntax 'self-evaluating))))
       expression :grammar 'special-operators)
    (assert (eq success t))
    syntax))

(defmethod parse ((client t) (syntax (eql t)) (expression t))
  (parse client (classify client expression) expression))
