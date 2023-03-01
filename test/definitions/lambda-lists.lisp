;;;; lambda-lists.lisp --- Tests for lambda list related rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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
    '((x #1=(declare)) :fatal #1# "declare is not allowed here")
    '(#2=5             :fatal #2# "variable name must be a symbol")
    '(((#3=6))         :fatal #3# "must be a symbol")

    '(#4=x
      t #4# (:keyword-parameter
             ((:name . 1) (((:variable-name () :name x :source #4#))))
             :source #4#))
    '(#5=(#6=x 1 #7=xp)
      t #5# (:keyword-parameter
             ((:name     . 1) (((:variable-name () :name x :source #6#)))
              (:default  . 1) ((1 :evaluation t))
              (:supplied . 1) (((:variable-name () :name xp :source #7#))))
             :source #5#))
    '(#8=((#9=:x #10=x) 1 #11=xp)
      t #8# (:keyword-parameter
             ((:name     . 1) (((:variable-name () :name x :source #10#)))
              (:keyword  . 1) (((:keyword () :name :x :source #9#)))
              (:default  . 1) ((1 :evaluation t))
              (:supplied . 1) (((:variable-name () :name xp :source #11#))))
             :source #8#))))

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' rule."
  (rule-test-cases ((syn::ordinary-lambda-list syn::lambda-lists))
    '((#1=(a))
      :fatal #1# "variable name must be a symbol")
    '((a #2=a)
      :fatal #2# "the variable name A occurs more than once")
    '((&optional (foo #3=(declare)))
      :fatal #3# "declare is not allowed here")
    '((&key (foo #4=(declare)))
      :fatal #4# "declare is not allowed here")
    '((&aux (foo #5=(declare)))
      :fatal #5# "declare is not allowed here")

    '(#6=(&optional #7=a #8=b)
      t nil (:ordinary-lambda-list
             ((:optional . *) (((:optional-parameter
                                 ((:name . 1) (((:variable-name () :name a :source #7#))))
                                 :source #7#)
                                :evaluation :compound)
                               ((:optional-parameter
                                 ((:name . 1) (((:variable-name () :name b :source #8#))))
                                 :source #8#)
                                :evaluation :compound)))
             :source #6#))

    '(#9=(&aux #10=a)
      t nil (:ordinary-lambda-list
             ((:aux . *) (((:aux-parameter
                            ((:name . 1) (((:variable-name () :name a :source #10#))))
                            :source #10#)
                           :evaluation :compound)))
             :source #9#))

    '(#11=(#12=foo #13=bar &optional #14=(#15=hash-table-rehash-size default)
          &rest #16=x
          &key #17=((#18=:x-kw #19=y) 1 #20=supplied?) #21=b &allow-other-keys
          &aux #22=(#23=a 1))
      t nil (:ordinary-lambda-list
             ((:required         . *) (((:required-parameter
                                         ((:name . 1) (((:variable-name
                                                         ()
                                                         :name foo :source #12#))))
                                         :source #12#))
                                       ((:required-parameter
                                         ((:name . 1) (((:variable-name
                                                         ()
                                                         :name bar :source #13#))))
                                         :source #13#)))
              (:optional         . *) (((:optional-parameter
                                         ((:name    . 1) (((:variable-name
                                                            ()
                                                            :name hash-table-rehash-size :source #15#)))
                                          (:default . 1) ((default :evaluation t)))
                                         :source #14#)
                                        :evaluation :compound))
              (:rest             . 1) (((:variable-name () :name x :source #16#)))
              (:keyword          . *) (((:keyword-parameter
                                         ((:name     . 1) (((:variable-name
                                                             ()
                                                             :name y :source #19#)))
                                          (:keyword . 1)  (((:keyword
                                                             ()
                                                             :name :x-kw :source #18#)))
                                          (:default  . 1) ((1 :evaluation t))
                                          (:supplied . 1) (((:variable-name
                                                             ()
                                                             :name supplied? :source #20#))))
                                         :source #17#)
                                        :evaluation :compound)
                                       ((:keyword-parameter
                                         ((:name . 1) (((:variable-name
                                                         ()
                                                         :name b :source #21#))))
                                         :source #21#)
                                        :evaluation :compound))
              (:allow-other-keys? . 1) ((&allow-other-keys))
              (:aux              . *) (((:aux-parameter
                                         ((:name  . 1) (((:variable-name
                                                          ()
                                                          :name a :source #23#)))
                                          (:value . 1) ((1 :evaluation t)))
                                         :source #22#)
                                        :evaluation :compound)))
             :source #11#))

    '(#24=(#25=foo #26=foo2 &rest #27=pie
           &key #28=((#29=:foo #30=bar) :default #31=bar-p)
           &aux #32=(#33=a 1) #34=b)
      t nil (:ordinary-lambda-list
             ((:required . *) (((:required-parameter
                                 ((:name . 1) (((:variable-name
                                                 ()
                                                 :name foo :source #25#))))
                                 :source #25#))
                               ((:required-parameter
                                 ((:name . 1) (((:variable-name
                                                 ()
                                                 :name foo2 :source #26#))))
                                 :source #26#)))
              (:rest     . 1)     (((:variable-name () :name pie :source #27#)))
              (:keyword  . *) (((:keyword-parameter
                                 ((:name     . 1) (((:variable-name
                                                     ()
                                                     :name bar :source #30#)))
                                  (:keyword  . 1) (((:keyword
                                                     ()
                                                     :name :foo :source #29#)))
                                  (:default  . 1) ((:default :evaluation t))
                                  (:supplied . 1) (((:variable-name
                                                     ()
                                                     :name bar-p :source #31#))))
                                 :source #28#)
                                :evaluation :compound))
              (:aux      . *) (((:aux-parameter
                                 ((:name  . 1) (((:variable-name () :name a :source #33#)))
                                  (:value . 1) ((1 :evaluation t)))
                                 :source #32#)
                                :evaluation :compound)
                               ((:aux-parameter
                                 ((:name . 1) (((:variable-name () :name b :source #34#))))
                                 :source #34#)
                                :evaluation :compound)))
             :source #24#))))

;;; Specialized lambda list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' rule."
  (rule-test-cases ((syn::specialized-lambda-list syn::lambda-lists))
    '(((foo #1=1))
      :fatal #1# "must be a class name")
    '((#2=(foo t 1))
      :fatal #2# "must be of the form (NAME SPECIALIZER)")
    '(((foo (eql . #3=(1 2))))
      :fatal #3# "must be a single object")

    '(((baz fez) (#4=foo bar) &rest foo)
      :fatal #4# "the variable name FOO occurs more than once")

    '(#5=(#6=(#7=baz #8=fez) #9=(#10=foo #11=bar) &rest #12=whoop)
      t #5# (:specialized-lambda-list
             ((:required . *) (((:specialized-parameter
                                 ((:name        . 1) (((:variable-name
                                                        ()
                                                        :name baz :source #7#)))
                                  (:specializer . 1) (((:type-name
                                                        ()
                                                        :name fez :source #8#))))
                                 :source #6#))
                               ((:specialized-parameter
                                 ((:name        . 1) (((:variable-name
                                                        ()
                                                        :name foo :source #10#)))
                                  (:specializer . 1) (((:type-name
                                                        ()
                                                        :name bar :source #11#))))
                                 :source #9#)))
              (:rest     . 1) (((:variable-name () :name whoop :source #12#))))
             :source #5#))

    '(#13=(&aux #14=a)
      t #13# (:specialized-lambda-list
              ((:aux . *) (((:aux-parameter
                             ((:name . 1) (((:variable-name () :name a :source #14#))))
                             :source #14#)
                            :evaluation :compound)))
              :source #13#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."
  (rule-test-cases ((syn::destructuring-lambda-list syn::destructuring-lambda-list))
    ;; Repeated section
    '((&environment e1 foo bar . #1= (&environment e2))
      :fatal #1# "&ENVIRONMENT must not be repeated")
    ;; Valid syntax
    '(#2=(#3=(#4=foo #5=bar))
      t nil (:destructuring-lambda-list
             ((:required . *) (((:required-parameter
                                 ((:name . 1) (((:pattern
                                                 ((:required . *) (((:required-parameter
                                                                     ((:name . 1) (((:variable-name
                                                                                     ()
                                                                                     :name foo :source #4#)
                                                                                    :evaluation nil)))
                                                                     :source #4#)
                                                                    :evaluation :compound)
                                                                   ((:required-parameter
                                                                     ((:name . 1) (((:variable-name
                                                                                     ()
                                                                                     :name bar :source #5#)
                                                                                    :evaluation nil)))
                                                                     :source #5#)
                                                                    :evaluation :compound)))
                                                 :source #3#)
                                                :evaluation :compound)))
                                 :source #3#)
                                :evaluation :compound)))
             :source #2#))

    '(#6=(&whole #7=whole #8=(#9=foo &key #10=a) . (&rest #11=fez))
      t nil (:destructuring-lambda-list
             ((:whole    . 1) (((:variable-name () :name whole :source #7#)))
              (:required . *) (((:required-parameter
                                 ((:name . 1) (((:pattern
                                                 ((:required . *) (((:required-parameter
                                                                     ((:name . 1) (((:variable-name
                                                                                     ()
                                                                                     :name foo :source #9#)
                                                                                    :evaluation nil)))
                                                                     :source #9#)
                                                                    :evaluation :compound))
                                                  (:key      . *) (((:keyword-parameter
                                                                     ((:name . 1) (((:variable-name
                                                                                     ()
                                                                                     :name a :source #10#))))
                                                                     :source #10#)
                                                                    :evaluation :compound)))
                                                 :source #8#)
                                                :evaluation :compound)))
                                 :source #8#)
                                :evaluation :compound))
              (:rest     . 1) (((:variable-name () :name fez :source #11#)
                                :evaluation :compound)))
             :source #6#))

    '(#12=(&optional #13=(#14=(#15=bar #16=baz) (5 6) #17=bar-baz-p))
      t nil (:destructuring-lambda-list
             ((:optional . *) (((:optional-parameter
                                 ((:name . 1)     (((:pattern
                                                     ((:required . *) (((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name bar :source #15#)
                                                                                        :evaluation nil)))
                                                                         :source #15#)
                                                                        :evaluation :compound)
                                                                       ((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name baz :source #16#)
                                                                                        :evaluation nil)))
                                                                         :source #16#)
                                                                        :evaluation :compound)))
                                                 :source #14#)))
                                  (:default  . 1) (((5 6) :evaluation t))
                                  (:supplied . 1) (((:variable-name
                                                     ()
                                                     :name bar-baz-p :source #17#))))
                                 :source #13#)
                                :evaluation :compound)))
             :source #12#))

    '(#18=(&aux #19=a #20=(#21=b 1))
      t nil (:destructuring-lambda-list
             ((:aux . *) (((:aux-parameter
                            ((:name . 1) (((:variable-name () :name a :source #19#))))
                            :source #19#)
                           :evaluation :compound)
                          ((:aux-parameter
                            ((:name  . 1) (((:variable-name () :name b :source #21#)))
                             (:value . 1) ((1 :evaluation t)))
                            :source #20#)
                           :evaluation :compound)))
             :source #18#))

    '(#22=(#23=a . #24=rest)
      t nil (:destructuring-lambda-list
             ((:required . *) (((:required-parameter
                                 ((:name . 1) (((:variable-name () :name a :source #23#)
                                                :evaluation nil)))
                                 :source #23#)
                                :evaluation :compound))
              (:cdr      . 1) (((:variable-name () :name rest :source #24#)
                                :evaluation :compound)))
             :source #22#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' rule."
  (rule-test-cases ((syn::deftype-lambda-list syn::deftype-lambda-list))
    '(#1=(#2=foo #3=bar)
      t nil (:deftype-lambda-list
             ((:required . *) (((:required-parameter
                                 ((:name . 1) (((:variable-name
                                                 ()
                                                 :name foo :source #2#)
                                                :evaluation nil)))
                                 :source #2#)
                                :evaluation :compound)
                               ((:required-parameter
                                 ((:name . 1) (((:variable-name
                                                 ()
                                                 :name bar :source #3#)
                                                :evaluation nil)))
                                 :source #3#)
                                :evaluation :compound)))
             :source #1#))))
