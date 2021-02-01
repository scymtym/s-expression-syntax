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

  (rule-test-cases ((syn::ordinary-lambda-list syn::lambda-lists))
    '(((a))
      :fatal (a) "must be a lambda list variable name")
    '((&optional (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&key (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&aux (foo (declare)))
      :fatal (declare) "declare is not allowed here")

    '(#4=(&aux a)
      t nil (:ordinary-lambda-list (:aux (((a nil)))) :source #4#))

    '(#5=(foo bar &optional (hash-table-rehash-size default)
          &rest x
          &key ((:x-kw y) 1 supplied?) b &allow-other-keys &aux (a 1))
      t nil (:ordinary-lambda-list
             (:required         ((foo) (bar))
              :optional         (((hash-table-rehash-size default nil)))
              :rest             ((x))
              :keyword          (((:x-kw y 1 supplied?))
                                 (((keyword b) b nil nil)))
              :allow-other-keys ((&allow-other-keys))
              :aux              (((a 1))))
             :source #5#))

    '(#6=(foo foo2 &rest pie &key ((:foo bar) :default bar-p)
          &aux (a 1) b)
      t nil (:ordinary-lambda-list
             (:required ((foo) (foo2))
              :rest     ((pie))
              :keyword  (((:foo bar :default bar-p)))
              :aux      (((a 1)) ((b nil))))
             :source #6#))))

;;; Specialized lambda list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' rule."

  (rule-test-cases ((syn::specialized-lambda-list syn::lambda-lists))
    '(((foo 1))
      :fatal 1 "must be a class name")
    '(((foo t 1))
      :fatal 1 "must be of the form (NAME SPECIALIZER)")
    '(((foo (eql 1 2)))
      :fatal (1 2) "must be a single object")

    '(((baz fez) (foo bar) &rest foo)
      :fatal foo "must be a lambda list variable name")

    '(#4=((baz fez) (foo bar) &rest whoop)
      t nil (:specialized-lambda-list
             (:required (((baz fez)) ((foo bar)))
              :rest     ((whoop)))
             :source #4#))

    '(#5=(&aux a)
      t nil (:specialized-lambda-list
             (:aux (((a nil))))
             :source #5#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."

  (rule-test-cases ((syn::destructuring-lambda-list syn::destructuring-lambda-list))
    ;; Repeated section
    '((&environment e1 foo bar #1=&environment e2)
      :fatal #1# "&ENVIRONMENT must not be repeated")
    ;; Valid syntax
    '(#2=(#3=(foo bar))
      t nil (:destructuring-lambda-list
             (:required (((:pattern
                           (:required ((foo) (bar)))
                           :source #3#))))
             :source #2#))

    '(#4=(&whole whole #5=(foo &key a) . (&rest fez))
      t nil (:destructuring-lambda-list
             (:whole    ((whole))
              :required (((:pattern
                           (:required ((foo))
                            :keyword  ((((keyword a) a nil nil))))
                           :source #5#)))
              :rest     ((fez)))
             :source #4#))

    '(#6=(&optional (#7=(bar baz) (5 6) bar-baz-p))
      t nil (:destructuring-lambda-list
             (:optional ((((:pattern
                            (:required ((bar) (baz)))
                            :source #7#)
                           (5 6)
                           bar-baz-p))))
             :source #6#))

    '(#8=(&aux a (b 1))
      t nil (:destructuring-lambda-list
             (:aux (((a nil)) ((b 1))))
             :source #8#))

    '(#9=(a . rest)
      t nil (:destructuring-lambda-list
             (:required ((a))
              :cdr      ((rest)))
             :source #9#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."

  (rule-test-cases ((syn::deftype-lambda-list syn::deftype-lambda-list))
    '(#1=(foo bar)
      t nil (:destructuring-lambda-list
             (:required ((foo) ( bar)))
             :source #1#))))
