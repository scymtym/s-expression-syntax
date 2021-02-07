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
    '((x (declare))    :fatal (declare) "declare is not allowed here")
    '(5                :fatal 5         "must be a lambda list variable name")
    '(((#3=6))         :fatal #3#       "must be a symbol")

    '(#4=x             t      t         (:keyword-parameter (:name ((x))) :source #4#))
    '(#5=(x 1 xp)      t      t         (:keyword-parameter
                                         (:name     ((x))
                                          :default  ((1))
                                          :supplied ((xp)))
                                         :source #5#))
    '(#6=((:x x) 1 xp) t      t         (:keyword-parameter
                                         (:name     ((x))
                                          :keyword  ((:x))
                                          :default  ((1))
                                          :supplied ((xp)))
                                         :source #6#))))

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

    '(#4=(&aux #5=a)
      t nil (:ordinary-lambda-list
             (:aux (((:aux-parameter (:name ((a))) :source #5#))))
             :source #4#))

    '(#6=(#7=foo #8=bar &optional #9=(hash-table-rehash-size default)
          &rest x
          &key #11=((:x-kw y) 1 supplied?) #12=b &allow-other-keys
          &aux #13=(a 1))
      t nil (:ordinary-lambda-list
             (:required         (((:required-parameter
                                   ((:name . 1) ((foo)))
                                   :source #7#))
                                 ((:required-parameter
                                   ((:name . 1) ((bar)))
                                   :source #8#)))
              :optional         (((:optional-parameter
                                   (:name    ((hash-table-rehash-size))
                                    :default ((default)))
                                   :source #9#)))
              :rest             ((x))
              :keyword          (((:keyword-parameter
                                   (:name     ((y))
                                    :keyword  ((:x-kw))
                                    :default  ((1))
                                    :supplied ((supplied?)))
                                   :source #11#))
                                 ((:keyword-parameter
                                   (:name ((b)))
                                   :source #12#)))
              :allow-other-keys ((&allow-other-keys))
              :aux              (((:aux-parameter
                                   (:name  ((a))
                                    :value ((1)))
                                   :source #13#))))
             :source #6#))

    '(#14=(#15=foo #16=foo2 &rest pie &key #18=((:foo bar) :default bar-p)
           &aux #19=(a 1) #20=b)
      t nil (:ordinary-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) ((foo)))
                           :source #15#))
                         ((:required-parameter
                           ((:name . 1) ((foo2)))
                           :source #16#)))
              :rest     ((pie))
              :keyword  (((:keyword-parameter
                           (:name     ((bar))
                            :keyword  ((:foo))
                            :default  ((:default))
                            :supplied ((bar-p)))
                           :source #18#)))
              :aux      (((:aux-parameter
                           (:name  ((a))
                            :value ((1)))
                           :source #19#))
                         ((:aux-parameter
                           (:name ((b)))
                           :source #20#))))
             :source #14#))))

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

    '(#4=(#5=(baz fez) #6=(foo bar) &rest whoop)
      t nil (:specialized-lambda-list
             (:required (((:specialized-parameter
                           (:name        ((baz))
                            :specializer ((fez)))
                           :source #5#))
                         ((:specialized-parameter
                           (:name        ((foo))
                            :specializer ((bar)))
                           :source #6#)))
              :rest     ((whoop)))
             :source #4#))

    '(#8=(&aux #9=a)
      t nil (:specialized-lambda-list
             (:aux (((:aux-parameter (:name ((a))):source #9#))))
             :source #8#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."

  (rule-test-cases ((syn::destructuring-lambda-list syn::destructuring-lambda-list))
    ;; Repeated section
    '((&environment e1 foo bar #1=&environment e2)
      :fatal #1# "&ENVIRONMENT must not be repeated")
    ;; Valid syntax
    '(#2=(#3=(#4=foo #5=bar))
      t nil (:destructuring-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) (((:pattern
                                           (:required (((:required-parameter
                                                         ((:name . 1) ((foo)))
                                                         :source #4#))
                                                       ((:required-parameter
                                                         ((:name . 1) ((bar)))
                                                         :source #5#))))
                                           :source #3#))))
                           :source #3#))))
             :source #2#))

    '(#6=(&whole whole #7=(#8=foo &key #9=a) . #10=(&rest fez))
      t nil (:destructuring-lambda-list
             (:whole    ((whole))
              :required (((:required-parameter
                           ((:name . 1) (((:pattern
                                           (:required (((:required-parameter
                                                         ((:name . 1) ((foo)))
                                                         :source #8#)))
                                            :keyword  (((:keyword-parameter
                                                         (:name ((a)))
                                                         :source #9#))))
                                           :source #7#))))
                           :source #7#)))
              :rest     ((fez)))
             :source #6#))

    '(#12=(&optional #13=(#14=(#15=bar #16=baz) (5 6) bar-baz-p))
      t nil (:destructuring-lambda-list
             (:optional (((:optional-parameter
                           (:name     (((:pattern
                                         (:required (((:required-parameter
                                                       ((:name . 1) ((bar)))
                                                       :source #15#))
                                                     ((:required-parameter
                                                       ((:name . 1) ((baz)))
                                                       :source #16#))))
                                         :source #14#)))
                            :default  (((5 6)))
                            :supplied ((bar-baz-p)))
                           :source #13#))))
             :source #12#))

    '(#17=(&aux #18=a #19=(b 1))
      t nil (:destructuring-lambda-list
             (:aux (((:aux-parameter (:name ((a))) :source #18#))
                    ((:aux-parameter
                      (:name ((b)) :value ((1)))
                      :source #19#))))
             :source #17#))

    '(#20=(#21=a . rest)
      t nil (:destructuring-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) ((a)))
                           :source #21#)))
              :cdr      ((rest)))
             :source #20#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."

  (rule-test-cases ((syn::deftype-lambda-list syn::deftype-lambda-list))
    '(#1=(#2=foo #3=bar)
      t nil (:destructuring-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) ((foo)))
                           :source #2#))
                         ((:required-parameter
                           ((:name . 1) ((bar)))
                           :source #3#))))
             :source #1#))))
