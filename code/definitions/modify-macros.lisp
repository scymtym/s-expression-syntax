;;;; modify-macros.lisp --- Standard macros for input and output.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macro `multiple-value-setq'

(define-macro multiple-value-setq
    (list (<- seen (:transform (seq) (make-hash-table :test #'eq)))
          (list (* (<<- name ((unique-variable-name! lambda-lists) seen))))
          (<- values ((form! forms))))
    `((,@name) ,values)
  ((name   * :evaluation (make-instance 'assignment-semantics
                                        :namespace 'variable))
   (values 1)))

;;; Standard macros `setf' and `psetf'

(define-macro setf
    (list (* (seq (<<- place ((place! forms)))
                  (<<- value ((form! forms)))))) ; TODO explicit pair?
    (a:mappend #'list place value)
  ((place *)
   (value * :evaluation t)))

;;; TODO See "Should ~psetq~ allow multiple assignments to the same variable name?" in DESIGN.org
(define-macro psetf
    (list (* (seq (<<- place ((place! forms)))
                  (<<- value ((form! forms))))))
    (a:mappend #'list place value)
  ((place *)
   (value * :evaluation t)))

;;; Standard macros `shiftf' and `rotatef'

(define-macro shiftf
    (list (+ (and (seq :any :any) (<<- place ((place! forms)))))
          (<- value ((form! forms))))
    `(,@place ,value)
  ((place *)
   (value 1 :evaluation t)))

(define-macro rotatef
    (list (* (<<- place ((place! forms)))))
    place
  ((place *)))

(macrolet ((define (name)
             `(define-macro ,name
                  (list (<- place ((place! forms)))
                        (? (<- delta ((form! forms)))))
                  `(,place ,@(? delta-supplied? delta))
                ((place 1)
                 (delta ? :evaluation t)))))
  (define decf)
  (define incf))

;;; Standard macros `push', `pushnew' and `pop'

(define-macro push
    (list (<- item ((form! forms))) (<- place ((place! forms))))
    `(,item ,place)
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
    `(,item ,place
      ,@(? key-supplied?      :key      key)
      ,@(? test-supplied?     :test     test)
      ,@(? test-not-supplied? :test-not test-not))
  ((item     1 :evaluation t)
   (place    1)
   (key      ? :evaluation t)
   (test     ? :evaluation t)
   (test-not ? :evaluation t)))

(define-macro pop
    (list (<- place ((place! forms))))
    `(,place)
  ((place 1)))

;;; Standard macro `remf'

(define-macro remf
    (list (<- place ((place! forms))) (<- indicator ((form! forms))))
    `(,place ,indicator)
  ((place     1)
   (indicator 1 :evaluation t)))
