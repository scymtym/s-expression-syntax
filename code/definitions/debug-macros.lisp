;;;; debug-macros.lisp --- Standard macros for debugging.
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macros `trace' and `untrace'

(macrolet ((define (name)
             `(define-macro ,name
                  (list (* (<<- function-name ((function-name! names)))))
                ((function-name *)))))
  (define trace)
  (define untrace))
