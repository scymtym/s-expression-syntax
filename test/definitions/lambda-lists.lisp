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
    '(((#3=6))      :fatal #3#       "must be a symbol")

    '(#4=x
      t #4# (:keyword-parameter
             (:name (((:variable-name () :name x :source #4#))))
             :source #4#))
    '(#5=(#6=x 1 #7=xp)
      t #5# (:keyword-parameter
             (:name     (((:variable-name () :name x :source #6#)))
              :default  ((1))
              :supplied (((:variable-name () :name xp :source #7#))))
             :source #5#))
    '(#8=((:x #9=x) 1 #10=xp)
      t #8# (:keyword-parameter
             (:name     (((:variable-name () :name x :source #9#)))
              :keyword  ((:x))
              :default  ((1))
              :supplied (((:variable-name () :name xp :source #10#))))
             :source #8#))))

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' rule."

  (rule-test-cases ((syn::ordinary-lambda-list syn::lambda-lists))
    '(((a))
      :fatal (a) "must be a lambda list variable name")
    '(((a a))
      :fatal a "the variable name A occurs more than once")
    '((&optional (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&key (foo (declare)))
      :fatal (declare) "declare is not allowed here")
    '((&aux (foo (declare)))
      :fatal (declare) "declare is not allowed here")

    '(#1=(&optional #2=a #3=b)
      t nil (:ordinary-lambda-list
             (:optional (((:optional-parameter
                           ((:name . 1) (((:variable-name () :name b :source #2#))))
                           :source #2#)
                          :evalution :compound)
                         ((:optional-parameter
                           ((:name . 1) (((:variable-name () :name b :source #3#))))
                           :source #3#)
                          :evaluation :compound)))
             :source #1#))

    '(#4=(&aux #5=a)
      t nil (:ordinary-lambda-list
             (:aux (((:aux-parameter
                      (:name (((:variable-name () :name a :source #5#))))
                      :source #5#))))
             :source #4#))

    '(#6=(#7=foo #8=bar &optional #9=(#10=hash-table-rehash-size #11=default)
          &rest #12=x
          &key #13=((#99=:x-kw #14=y) 1 #15=supplied?) #16=b &allow-other-keys
          &aux #17=(#18=a 1))
      t nil (:ordinary-lambda-list
             (:required         (((:required-parameter
                                   ((:name . 1) (((:variable-name
                                                   ()
                                                   :name foo :source #7#))))
                                   :source #7#))
                                 ((:required-parameter
                                   ((:name . 1) (((:variable-name
                                                   ()
                                                   :name bar :source #8#))))
                                   :source #8#)))
              :optional         (((:optional-parameter
                                   (:name    (((:variable-name
                                                ()
                                                :name hash-table-rehash-size :source #10#)))
                                    :default ((default)))
                                   :source #9#)))
              :rest             (((:variable-name
                                   ()
                                   :name x :source #12#)))
              :keyword          (((:keyword-parameter
                                   (:name     (((:variable-name
                                                 ()
                                                 :name y :source #14#)))
                                    :keyword  (((:keyword
                                                 ()
                                                 :name :x-kw :source #99#)))
                                    :default  ((1))
                                    :supplied (((:variable-name
                                                 ()
                                                 :name supplied? :source #15#))))
                                   :source #13#))
                                 ((:keyword-parameter
                                   (:name (((:variable-name
                                             ()
                                             :name b :source #16#))))
                                   :source #16#)))
              :allow-other-keys ((&allow-other-keys))
              :aux              (((:aux-parameter
                                   (:name  (((:variable-name
                                              ()
                                              :name a :source #18#)))
                                    :value ((1)))
                                   :source #17#))))
             :source #6#))

    '(#19=(#20=foo #21=foo2 &rest #22=pie
           &key #23=((#24=:foo #25=bar) :default #26=bar-p)
           &aux #27=(#28=a 1) #29=b)
      t nil (:ordinary-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name foo :source #20#))))
                           :source #20#))
                         ((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name foo2 :source #21#))))
                           :source #21#)))
              :rest     (((:variable-name () :name pie :source #22#)))
              :keyword  (((:keyword-parameter
                           (:name     (((:variable-name
                                         ()
                                         :name bar :source #25#)))
                            :keyword  (((:keyword
                                         ()
                                         :name :foo :source #24#)))
                            :default  ((:default))
                            :supplied (((:variable-name
                                         ()
                                         :name bar-p :source #26#))))
                           :source #23#)))
              :aux      (((:aux-parameter
                           (:name  (((:variable-name () :name a :source #28#)))
                            :value ((1)))
                           :source #27#))
                         ((:aux-parameter
                           (:name (((:variable-name () :name b :source #29#))))
                           :source #29#))))
             :source #19#))))

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
      :fatal foo "the variable name FOO occurs more than once")

    '(#4=(#5=(#6=baz #7=fez) #8=(#9=foo #10=bar) &rest #11=whoop)
      t #4# (:specialized-lambda-list
             (:required (((:specialized-parameter
                           (:name        (((:variable-name
                                            ()
                                            :name baz :source #6#)))
                            :specializer (((:type-name
                                            ()
                                            :name fez :source #7#))))
                           :source #5#))
                         ((:specialized-parameter
                           (:name        (((:variable-name
                                            ()
                                            :name foo :source #9#)))
                            :specializer (((:type-name
                                            ()
                                            :name bar :source #10#))))
                           :source #8#)))
              :rest     (((:variable-name () :name whoop :source #11#))))
             :source #4#))

    '(#12=(&aux #13=a)
      t #12# (:specialized-lambda-list
              (:aux (((:aux-parameter
                       (:name (((:variable-name () :name a :source #13#))))
                       :source #13#))))
              :source #12#))))

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
                                                         ((:name . 1) (((:variable-name
                                                                         ()
                                                                         :name foo :source #4#))))
                                                         :source #4#))
                                                       ((:required-parameter
                                                         ((:name . 1) (((:variable-name
                                                                         ()
                                                                         :name bar :source #5#))))
                                                         :source #5#))))
                                           :source #3#))))
                           :source #3#))))
             :source #2#))

    '(#6=(&whole #100=whole #7=(#8=foo &key #9=a) . #10=(&rest #11=fez))
      t nil (:destructuring-lambda-list
             (:whole    (((:variable-name () :name whole :source #100#)))
              :required (((:required-parameter
                           ((:name . 1) (((:pattern
                                           (:required (((:required-parameter
                                                         ((:name . 1) (((:variable-name
                                                                         ()
                                                                         :name foo :source #8#))))
                                                         :source #8#)))
                                            :keyword  (((:keyword-parameter
                                                         (:name (((:variable-name
                                                                   ()
                                                                   :name a :source #9#))))
                                                         :source #9#))))
                                           :source #7#))))
                           :source #7#)))
              :rest     (((:variable-name () :name fez :source #11#))))
             :source #6#))

    '(#12=(&optional #13=(#14=(#15=bar #16=baz) (5 6) #17=bar-baz-p))
      t nil (:destructuring-lambda-list
             (:optional (((:optional-parameter
                           (:name     (((:pattern
                                         (:required (((:required-parameter
                                                       ((:name . 1) (((:variable-name
                                                                       ()
                                                                       :name bar :source #15#))))
                                                       :source #15#))
                                                     ((:required-parameter
                                                       ((:name . 1) (((:variable-name
                                                                       ()
                                                                       :name baz :source #16#))))
                                                       :source #16#))))
                                         :source #14#)))
                            :default  (((5 6)))
                            :supplied (((:variable-name
                                         ()
                                         :name bar-baz-p :source #17#))))
                           :source #13#))))
             :source #12#))

    '(#18=(&aux #19=a #20=(#21=b 1))
      t nil (:destructuring-lambda-list
             (:aux (((:aux-parameter
                      (:name (((:variable-name () :name a :source #19#))))
                      :source #19#))
                    ((:aux-parameter
                      (:name  (((:variable-name () :name b :source #21#)))
                       :value ((1)))
                      :source #20#))))
             :source #18#))

    '(#22=(#23=a . #24=rest)
      t nil (:destructuring-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) (((:variable-name () :name a :source #23#))))
                           :source #23#)))
              :cdr      (((:variable-name () :name rest :source #24#))))
             :source #22#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."

  (rule-test-cases ((syn::deftype-lambda-list syn::deftype-lambda-list))
    '(#1=(#2=foo #3=bar)
      t nil (:destructuring-lambda-list
             (:required (((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name foo :source #2#))))
                           :source #2#))
                         ((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name bar :source #3#))))
                           :source #3#))))
             :source #1#))))
