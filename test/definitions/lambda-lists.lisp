;;;; lambda-lists.lisp --- Tests for lambda list related rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.lambda-lists
  :in :syntax)

;;; Ordinary lambda list

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' rule."

  (rule-test-cases ((syntax::ordinary-lambda-list syntax::lambda-lists)
                    (make-hash-table :test #'eq))
    '((foo bar &optional (hash-table-rehash-size default)
       &rest x
       &key ((:x-kw y) 1 supplied?) b &allow-other-keys
                                        ; &aux (a 1)
       )
      t nil ((foo bar) ((hash-table-rehash-size default nil)) x
             ((:b b nil nil) (:x-kw y 1 supplied?)) &allow-other-keys))

    '((foo foo2 &rest pie &key ((:foo bar) :default bar-p)
                                        ; &aux (a 1) b
       )
      t nil ((foo foo2) () pie ((:foo bar :default bar-p)) nil))))

;;; Specialized lambda list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' rule."

  (rule-test-cases ((syntax::specialized-lambda-list syntax::lambda-lists)
                    (make-hash-table :test #'eq))
    '(((baz fez) (foo bar) &rest whoop)
      t nil (((foo bar) (baz fez)) () whoop () nil))
    '(((baz fez) (foo bar) &rest foo)
      nil nil nil)))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."

  (rule-test-cases ((syntax::destructuring-lambda-list syntax::destructuring-lambda-list)
                    (make-hash-table :test #'eq))
    '(((foo bar))
      t nil (nil ((nil (foo bar) () nil () nil ())) () nil () nil ()))

    '((&whole whole (foo &key a) . (&rest fez))
      t nil (whole ((nil (foo) () nil ((:a a nil nil)) nil ())) () fez () nil ()))

    '((foo &optional ((bar baz) (5 6) bar-baz-p))
      t nil (nil (foo) ((bar baz) (5 6) bar-baz-p)))))

;;; Deftype lambda list

(test deftyep-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."

  (rule-test-cases ((syntax::deftype-lambda-list syntax::deftype-lambda-list)
                    (make-hash-table :test #'eq))
    '((foo bar)
      t nil (nil (foo bar) () nil () nil ()))))
