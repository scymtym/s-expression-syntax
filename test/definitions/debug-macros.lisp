;;;; debug-macros.lisp --- Tests for debug macro rules.
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.debug-macros
  :in :s-expression-syntax)

;;; `trace' and `untrace'

(macrolet ((define (name kind)
             `(define-macro-test (,name)
                ;; Valid syntax
                '((,name #1=1)
                  syn:invalid-syntax-error #1# "must be a function name")
                ;; Invalid syntax
                (let ((form '(,name)))
                  `(,form
                    (,',kind () :source ,form)))
                (let* ((name1 'a)
                       (form  `(,',name ,name1)))
                  `(,form
                    (,',kind
                     ((:function-name . *) (((:function-name () :name a :source ,name1))))
                     :source ,form)))
                (let* ((name1 'b)
                       (name2 '(setf c))
                       (form  `(,',name ,name1 ,name2)))
                  `(,form
                    (,',kind
                     ((:function-name . *) (((:function-name () :name b :source ,name1))
                                            ((:function-name () :name (setf c) :source ,name2))))
                     :source ,form))))))
  (define trace   :trace)
  (define untrace :untrace))

;;; `time' and `step'

(macrolet ((define (name kind)
             `(define-macro-test (,name)
                ;; Invalid syntax
                '((,name . #1=())
                  syn:invalid-syntax-error #1# "must be a single form")
                (let* ((forms '(1 2))
                       (form  `(,',name ,@forms)))
                  `(,form
                    syn:invalid-syntax-error ,forms "must be a single form"))
                ;; Valid syntax
                (let* ((form1 '1)
                       (form  `(,',name ,form1)))
                  `(,form
                    (,',kind
                     ((:form . 1) (((:unparsed () :expression 1 :context :form :source ,form1)
                                    :evaluation t)))
                     :source ,form))))))
  (define time :time)
  (define step :step))
