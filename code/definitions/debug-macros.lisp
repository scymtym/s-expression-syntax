;;;; debug-macros.lisp --- Standard macros for debugging.
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macro `assert'

(define-macro assert
    (list (<- test ((form! forms)))
          (? (seq (and :any
                       (must (list (* (<<- place ((place forms)))))
                             "must be a list of places"))
                  (? (seq (<- datum ((form forms)))
                          (* (<<- argument ((form forms)))))))))
  ((test     1 :evaluation t)
   (place    *)
   (datum    ? :evaluation t)
   (argument * :evaluation t)))

;;; Standard macro `check-type'

(define-macro check-type
    (list (<- place ((place! forms)))
          (<- type ((type-specifier! type-specifiers)))
          (? (<- description ((form! forms)))))
  ((place       1)
   (type        1)
   (description ? :evaluation t)))

;;; Standard macros `trace' and `untrace'

(macrolet ((define (name)
             `(define-macro ,name
                  (list (* (<<- function-name ((function-name! names)))))
                ((function-name *)))))
  (define trace)
  (define untrace))

;;; Standard macros `time' and `step'

(macrolet ((define (name)
             `(define-macro ,name
                  (list* (must (list (<- form ((form! forms))))
                               "must be a single form"))
                ((form 1 :evaluation t)))))
  (define time)
  (define step))
