;;;; modify-macros.lisp --- Standard macros for input and output.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macros `setf' and `psetf'

(define-macro setf
    (list (* (seq (<<- place     ((place! forms)))
                  (<<- new-value ((form! forms)))))) ; TODO explicit pair?
  ((place     *)
   (new-value * :evaluation t)))

;;; TODO See "Should ~psetq~ allow multiple assignments to the same variable name?" in DESIGN.org
(define-macro psetf
    (list (* (seq (<<- place     ((place! forms)))
                  (<<- new-value ((form! forms))))))
  ((place     *)
   (new-value * :evaluation t)))

;;; Standard macros `shiftf' and `rotatef'

(define-macro shiftf
    (list (+ (and (seq :any :any) (<<- place ((place! forms)))))
          (<- new-value ((form! forms))))
  ((place     *)
   (new-value 1 :evaluation t)))

(define-macro rotatef
    (list (* (<<- place ((place! forms)))))
  ((place *)))

(macrolet ((define (name)
             `(define-macro ,name
                  (list (<- place ((place! forms)))
                        (? (<- delta ((form! forms)))))
                ((place 1)
                 (delta ? :evaluation t)))))
  (define decf)
  (define incf))

;;; Standard macros `push', `pushnew' and `pop'

(define-macro push
    (list (<- item ((form! forms))) (<- place ((place! forms))))
  ((item  1 :evaluation t)
   (place 1)))

(define-macro pushnew
    (list (<- item ((form! forms))) (<- place ((place! forms)))
          (* (or (seq :key      (or (<- key ((form! forms)))
                                    (:transform (seq) (:fatal "argument must follow :key"))))
                 (seq :test     (<- test     ((form! forms)))) ; TODO only store first occurrence
                 (seq :test-not (<- test-not ((form! forms))))
                 (:transform :any
                   (:fatal "valid keywords are :key, :test and :test-not")))))
  ((item     1 :evaluation t)
   (place    1)
   (key      ? :evaluation t)
   (test     ? :evaluation t)
   (test-not ? :evaluation t)))

(define-macro pop
    (list (<- place ((place! forms))))
  ((place 1)))

;;; Standard macro `remf'

(define-macro remf
  (list (<- place ((place! forms))) (<- indicator ((form! forms))))
  ((place     1)
   (indicator 1 :evaluation t)))
