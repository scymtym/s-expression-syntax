;;;; client.lisp --- Tests for the client class of the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.concrete-syntax-tree.test)

(in-suite :s-expression-syntax.concrete-syntax-tree)

(defmacro rule-test-cases (((rule grammar) &rest arguments) &body cases)
  `(let ((eg:*client* s-expression-syntax.concrete-syntax-tree::*client*))
     (s-expression-syntax.test::rule-test-cases ((,rule ,grammar) ,@arguments)
       ,@cases)))

(test names.smoke
  "Smoke test for using the `cst-client' with names."
  (rule-test-cases ((syn::function-name syn::names))
    (list (cst:cst-from-expression 1) nil :input nil)

    (let ((cst (cst:cst-from-expression 'a)))
      (list cst t :input `(:function-name () :name a :source ,cst)))

    (let* ((cst    (cst:cst-from-expression '(setf 1)))
           (second (cst:second cst)))
      (list cst :fatal second "second element of SETF function name must be a symbol"))))

(test lambda-lists.smoke
  "Smoke test for using the `cst-client' with lambda list."
  (rule-test-cases ((syn::specialized-lambda-list syn::lambda-lists))
    (let* ((a           (cst:cst-from-expression 'a))
           (specializer (cst:cst-from-expression 'integer))
           (parameter   (cst:list a specializer))
           (b           (cst:cst-from-expression 'b))
           (c           (cst:cst-from-expression 'c))
           (d           (cst:cst-from-expression 'd))
           (r           (cst:cst-from-expression 'r))
           (e           (cst:cst-from-expression 'e))
           (cst         (cst:list parameter b c
                                  (cst:cst-from-expression '&optional) d
                                  (cst:cst-from-expression '&rest) r
                                  (cst:cst-from-expression '&key) e)))
      (list
       cst t :input
       `(:specialized-lambda-list
         ((:required . *)  (((:specialized-parameter
                              ((:name        . 1) (((:variable-name
                                                     ()
                                                     :name a :source ,a)))
                               (:specializer . 1) (((:type-name
                                                     ()
                                                     :name integer :source ,specializer)
                                                    :evaluation :compound)))
                              :source ,parameter)
                             :evaluation :compound)
                            ((:specialized-parameter
                              ((:name . 1) (((:variable-name
                                              ()
                                              :name b :source ,b))))
                              :source ,b)
                             :evaluation :compound)
                            ((:specialized-parameter
                              ((:name . 1) (((:variable-name
                                              ()
                                              :name c :source ,c))))
                              :source ,c)
                             :evaluation :compound))
          (:optional . *)  (((:optional-parameter
                              ((:name . 1) (((:variable-name
                                              ()
                                              :name d :source ,d))))
                              :source ,d)
                             :evaluation :compound))
          (:rest     . 1)  (((:variable-name
                              ()
                              :name r :source ,r)))
          (:keyword  . *)  (((:keyword-parameter
                              ((:name . 1) (((:variable-name
                                              ()
                                              :name e :source ,e))))
                              :source ,e)
                             :evaluation :compound)))
         :source ,cst)))

    (let* ((cst         (cst:cst-from-expression
                         `((a 1 2) b c &optional d &rest r &key e)))
           (first       (cst:first cst))
           (specializer (cst:second first)))
     (list cst :fatal specializer "must be a class name"))))
