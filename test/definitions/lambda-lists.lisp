;;;; lambda-lists.lisp --- Tests for lambda list related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.lambda-lists
  :in :s-expression-syntax)

;;; Ordinary lambda list

(test keyword-parameter
  "Smoke test for the `keyword-parameter' rule."

  (rule-test-cases ((syn::keyword-parameter syn::lambda-lists)
                    (make-hash-table :test #'eq))
    '((x (declare)) :fatal (declare) "declare is not allowed here")
    '(5             :fatal 5         "must be a lambda list variable name")
    '(((#3=6))     :fatal #3#        "must be a symbol")
    '(x             t      t         ((keyword x) x nil nil))))

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' rule."

  (rule-test-cases ((syn::ordinary-lambda-list syn::lambda-lists)
                    (make-hash-table :test #'eq))
    '((&optional (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&key (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&aux (foo (declare)))
      :fatal (declare) "declare is not allowed here")

    '((&aux a)
      t nil (() () nil () nil ((a nil))))

    '((foo bar &optional (hash-table-rehash-size default)
       &rest x
       &key ((:x-kw y) 1 supplied?) b &allow-other-keys &aux (a 1))
      t nil ((foo bar) ((hash-table-rehash-size default nil)) x
             ((:x-kw y 1 supplied?) ((keyword b) b nil nil)) &allow-other-keys
             ((a 1))))

    '((foo foo2 &rest pie &key ((:foo bar) :default bar-p)
       &aux (a 1) b)
      t nil ((foo foo2) () pie ((:foo bar :default bar-p)) nil ((a 1) (b nil))))))

;;; Specialized lambda list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' rule."

  (rule-test-cases ((syn::specialized-lambda-list syn::lambda-lists)
                    (make-hash-table :test #'eq))
    '(((foo 1))
      :fatal 1 "must be a class name")
    '(((foo t 1))
      :fatal 1 "must be of the form (NAME SPECIALIZER)")
    '(((foo (eql 1 2)))
      :fatal (1 2) "must be a single object")

    '(((baz fez) (foo bar) &rest foo)
      :fatal foo "must be a lambda list variable name")

    '(((baz fez) (foo bar) &rest whoop)
      t nil (((baz fez) (foo bar)) () whoop () nil ()))

    '((&aux a)
      t nil (() () nil () nil ((a nil))))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."

  (rule-test-cases ((syn::destructuring-lambda-list syn::destructuring-lambda-list)
                    (make-hash-table :test #'eq))
    ;; Repeated section
    '((&environment e1 foo bar #1=&environment e2)
      :fatal #1# "&ENVIRONMENT must not be repeated")
    ;; Valid syntax
    '(((foo bar))
      t nil (:destructuring-lambda-list
             nil nil
             ((:pattern nil (foo bar) () nil () nil () nil))
             ()
             nil
             ()
             nil
             ()
             nil))

    '((&whole whole (foo &key a) . (&rest fez))
      t nil (:destructuring-lambda-list
             whole nil
             ((:pattern nil (foo) () nil (((keyword a) a nil nil)) nil () nil))
             ()
             fez
             ()
             nil
             ()
             nil))

    '((&optional ((bar baz) (5 6) bar-baz-p))
      t nil (:destructuring-lambda-list
             nil nil ()
             (((:pattern nil (bar baz) () nil () nil () nil)
               (5 6) bar-baz-p))
             nil () nil () nil))

    '((&aux a (b 1))
      t nil (:destructuring-lambda-list
             nil nil () () nil () nil ((a nil) (b 1)) nil))

    '((a . rest)
      t nil (:destructuring-lambda-list
             nil nil (a) () nil () nil () rest))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."

  (rule-test-cases ((syn::deftype-lambda-list syn::deftype-lambda-list)
                    (make-hash-table :test #'eq))
    '((foo bar)
      t nil (:destructuring-lambda-list
             nil nil (foo bar) () nil () nil () nil))))
