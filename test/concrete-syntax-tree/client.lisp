;;;; client.lisp --- Tests for the client class of the concrete-syntax-tree module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
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
    (list (cst:cst-from-expression 1) nil t nil)

    (let ((cst (cst:cst-from-expression 'a)))
      (list cst t t cst))

    (list (cst:cst-from-expression '(setf 1))
          :fatal t "second element of SETF function name must be a symbol")))

(test lambda-lists.smoke
  "Smoke test for using the `cst-client' with lambda list."

  (rule-test-cases ((syn::specialized-lambda-list syn::lambda-lists)
                    (make-hash-table :test #'eq))
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
      (list cst t t `(((,a ,specializer) (,b nil) (,c nil))
                      ((,d nil nil))
                      ,r
                      (((keyword ,e) ,e nil nil)) nil
                      ())))

    (list (cst:cst-from-expression `((a 1 2) b c &optional d &rest r &key e))
          :fatal t "must be a class name")))
