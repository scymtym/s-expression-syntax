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
  "Smoke test for the `keyword-parameter' syntax-description."
  (syntax-test-cases (syn:keyword-parameter)
    ;; Invalid syntax
    '((x #1=(declare))
      syn:invalid-syntax-error #1# "declare is not allowed here")
    '(#2=5
      syn:invalid-syntax-error #2# "variable name must be a symbol")
    '(((#3=6))
      syn:invalid-syntax-error #3# "must be a symbol")
    ;; Valid syntax
    '(#4=x
      (:keyword-parameter
       ((:name . 1) (((:variable-name () :name x :source #4#))))
       :source #4#))
    '(#5=(#6=x #7=1 #8=xp)
      (:keyword-parameter
       ((:name     . 1) (((:variable-name () :name x :source #6#)))
        (:default  . 1) (((:unparsed
                           ()
                           :expression 1
                           :context    :form
                           :source     #7#)
                          :evaluation t))
        (:supplied . 1) (((:variable-name () :name xp :source #8#))))
       :source #5#))
    '(#9=((#10=:x #11=x) #12=1 #13=xp)
      (:keyword-parameter
       ((:name     . 1) (((:variable-name () :name x :source #11#)))
        (:keyword  . 1) (((:keyword () :name :x :source #10#)))
        (:default  . 1) (((:unparsed
                           ()
                           :expression 1
                           :context    :form
                           :source     #12#)
                          :evaluation t))
        (:supplied . 1) (((:variable-name () :name xp :source #13#))))
       :source #9#))))

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' description."
  (syntax-test-cases (syn:ordinary-lambda-list)
    ;; Invalid syntax
    '((#1=(a))
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    '((a #2=a)
      syn:invalid-syntax-error #2# "the variable name A occurs more than once")
    '((&optional (foo #3=(declare)))
      syn:invalid-syntax-error #3# "declare is not allowed here")
    '((&key (foo #4=(declare)))
      syn:invalid-syntax-error #4# "declare is not allowed here")
    '((&aux (foo #5=(declare)))
      syn:invalid-syntax-error #5# "declare is not allowed here")
    ;; Valid syntax
    '(#6=(#7=&optional #8=a #9=b)
      (:ordinary-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #7#)))
            (:parameter . *) (((:optional-parameter
                                ((:name . 1) (((:variable-name () :name a :source #8#))))
                                :source #8#)
                               :evaluation :compound)
                              ((:optional-parameter
                                ((:name . 1) (((:variable-name () :name b :source #9#))))
                                :source #9#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #6#))
    '(#10=(#11=&aux #12=a)
      (:ordinary-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #11#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #12#))))
                                :source #12#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #10#))
    '(#13=(#14=foo #15=bar #16=&optional #17=(#18=hash-table-rehash-size #19=default)
          #20=&rest #21=x
          #22=&key #23=((#24=:x-kw #25=y) #26=1 #27=supplied?) #28=b #29=&allow-other-keys
          #30=&aux #31=(#32=a #33=1))
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #14#))))
                                :source #14#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name bar :source #15#))))
                                :source #15#)))))))
        (:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #16#)))
            (:parameter . *) (((:optional-parameter
                                ((:name    . 1) (((:variable-name
                                                   ()
                                                   :name hash-table-rehash-size :source #18#)))
                                 (:default . 1) (((:unparsed
                                                   ()
                                                   :expression #19#
                                                   :context    :form
                                                   :source     #19#)
                                                  :evaluation t)))
                                :source #17#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #20#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name x :source #21#))))
                                :source #21#)))))))
        (:keyword-section  . 1)
        (((:keyword-section
           ((:keyword          . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &key
                                       :source  #22#)))
            (:parameter        . *) (((:keyword-parameter
                                       ((:name     . 1) (((:variable-name
                                                           ()
                                                           :name y :source #25#)))
                                        (:keyword  . 1) (((:keyword
                                                           ()
                                                           :name :x-kw :source #24#)))
                                        (:default  . 1) (((:unparsed
                                                           ()
                                                           :expression 1
                                                           :context    :form
                                                           :source     #26#)
                                                          :evaluation t))
                                        (:supplied . 1) (((:variable-name
                                                           ()
                                                           :name supplied? :source #27#))))
                                       :source #23#)
                                      :evaluation :compound)
                                     ((:keyword-parameter
                                       ((:name . 1) (((:variable-name
                                                       ()
                                                       :name b :source #28#))))
                                       :source #28#)
                                      :evaluation :compound))
            (:allow-other-keys . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &allow-other-keys
                                       :source  #29#)))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #30#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name
                                                 ()
                                                 :name a :source #32#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression #33#
                                                 :context    :form
                                                 :source     #33#)
                                                :evaluation t)))
                                :source #31#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #13#))
    '(#34=(#35=foo #36=foo2 #37=&rest #38=pie
           #39=&key #40=((#41=:foo #42=bar) #43=:default #44=bar-p)
           #45=&aux #46=(#47=a #48=1) #49=b)
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #35#))))
                                :source #35#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo2 :source #36#))))
                                :source #36#)))))))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #37#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name pie :source #38#))))
                                :source #38#)))))))
        (:keyword-section . 1)
        (((:keyword-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &key :source #39#)))
            (:parameter . *) (((:keyword-parameter
                                ((:name     . 1) (((:variable-name
                                                    ()
                                                    :name bar :source #42#)))
                                 (:keyword  . 1) (((:keyword
                                                    ()
                                                    :name :foo :source #41#)))
                                 (:default  . 1) (((:unparsed
                                                    ()
                                                    :expression :default
                                                    :context    :form
                                                    :source     #43#)
                                                   :evaluation t))
                                 (:supplied . 1) (((:variable-name
                                                    ()
                                                    :name bar-p :source #44#))))
                                :source #40#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #45#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name a :source #47#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1
                                                 :context    :form
                                                 :source     #48#)
                                                :evaluation t)))
                                :source #46#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name . 1) (((:variable-name () :name b :source #49#))))
                                :source #49#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #34#))))

;;; Specialized lambda list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' syntax description."
  (syntax-test-cases (syn:specialized-lambda-list)
    ;; Invalid syntax
    '(((foo #1=1))
      syn:invalid-syntax-error #1# "must be a class name")
    '((#2=(foo t 1))
      syn:invalid-syntax-error #2# "must be of the form (NAME SPECIALIZER)")
    '(((foo (eql . #3=(1 2))))
      syn:invalid-syntax-error #3# "must be a single object")
    '(((foo (eql #4=(declare))))
      syn:invalid-syntax-error #4# "declare is not allowed here")
    ;; Repeated names
    '(((baz fez) (#5=foo bar) &rest foo)
      syn:invalid-syntax-error #5# "the variable name FOO occurs more than once")
    ;; Valid syntax
    '(#6=(#7=(#8=baz #9=fez) #10=(#11=foo #12=bar) #13=&rest #14=whoop)
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name baz :source #8#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name fez :source #9#)
                                                      :evaluation :compound)))
                                :source #7#)
                               :evaluation :compound)
                              ((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name foo :source #11#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name bar :source #12#)
                                                      :evaluation :compound)))
                                :source #10#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #13#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whoop :source #14#))))
                                :source #14#))))))))
       :source #6#))
    '(#15=(#16=(#17=x #18=(eql #19=5)))
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name x :source #17#)))
                                 (:specializer . 1) (((:eql-specializer
                                                       ((:object . 1) (((:unparsed
                                                                         ()
                                                                         :expression 5
                                                                         :context    :form
                                                                         :source     #19#)
                                                                        :evaluation t)))
                                                       :source #18#)
                                                      :evaluation :compound)))
                                :source #16#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #15#))
    '(#20=(#21=&aux #22=a)
      (:specialized-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #21#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #22#))))
                                :source #22#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #20#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' syntax description."
  (syntax-test-cases (syn:destructuring-lambda-list)
    ;; Invalid syntax
    '((a . #1=1)
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    ;; Valid syntax
    '(#2=(#3=&rest #4=r)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #3#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name r :source #4#))))
                                :source #4#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #2#))
    '(#5=(#6=&body #7=b)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #6#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name b :source #7#))))
                                :source #7#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #5#))
    '(#8=(#9=(#10=foo #11=bar))
      (:destructuring-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1)
                                 (((:pattern
                                    ((:required-section . 1)
                                     (((:required-section
                                        ((:parameter . *) (((:required-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name foo :source #10#)
                                                                            :evaluation nil)))
                                                             :source #10#)
                                                            :evaluation :compound)
                                                           ((:required-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name bar :source #11#)
                                                                            :evaluation nil)))
                                                             :source #11#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #9#)
                                   :evaluation :compound)))
                                :source #9#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #8#))
    '(#12=(#13=&whole #14=whole #15=(#16=foo #17=&key #18=a) #19=&rest #20=fez)
      (:destructuring-lambda-list
       ((:whole-section . 1)
        (((:whole-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &whole :source #13#)))
            (:parameter . 1) (((:whole-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whole :source #14#))))
                                :source #14#)))))))
        (:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1)
                                 (((:pattern
                                    ((:required-section . 1)
                                     (((:required-section
                                        ((:parameter . *) (((:required-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name foo :source #16#)
                                                                            :evaluation nil)))
                                                             :source #16#)
                                                            :evaluation :compound))))
                                       :evaluation :compound))
                                     (:keyword-section . 1)
                                     (((:keyword-section
                                        ((:keyword   . 1) (((:lambda-list-keyword
                                                             ()
                                                             :keyword &key :source #17#)))
                                         (:parameter . *) (((:keyword-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name a :source #18#))))
                                                             :source #18#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #15#)
                                   :evaluation :compound)))
                                :source #15#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #19#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name fez :source #20#))))
                                :source #20#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #12#))
    '(#21=(#22=&optional #23=(#24=(#25=bar #26=baz) #27=(5 6) #28=bar-baz-p))
      (:destructuring-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1)
            (((:lambda-list-keyword () :keyword &optional :source #22#)))
            (:parameter . *)
            (((:optional-parameter
               ((:name     . 1) (((:pattern
                                   ((:required-section . 1)
                                    (((:required-section
                                       ((:parameter . *) (((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name bar :source #25#)
                                                                           :evaluation nil)))
                                                            :source #25#)
                                                           :evaluation :compound)
                                                          ((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name baz :source #26#)
                                                                           :evaluation nil)))
                                                            :source #26#)
                                                           :evaluation :compound))))
                                      :evaluation :compound)))
                                   :source #24#)))
                (:default  . 1) (((:unparsed
                                   ()
                                   :expression (5 6)
                                   :context    :form
                                   :source     #27#)
                                  :evaluation t))
                (:supplied . 1) (((:variable-name
                                   ()
                                   :name bar-baz-p :source #28#))))
               :source #23#)
              :evaluation :compound))))
          :evaluation :compound)))
       :source #21#))
    '(#29=(#30=&aux #31=a #32=(#33=b #34=1))
      (:destructuring-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword . 1) (((:lambda-list-keyword () :keyword &aux :source #30#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #31#))))
                                :source #31#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name b :source #33#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1 :context :form :source #34#)
                                                :evaluation t)))
                                :source #32#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #29#))
    '(#35=(#36=a . #37=rest)
      (:destructuring-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name () :name a :source #36#)
                                               :evaluation nil)))
                                :source #36#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:cdr . 1)
        (((:variable-name () :name rest :source #37#))))
       :source #35#))))

;;; Macro lambda list

(test macro-lambda-list
  "Smoke test for the `macro-lambda-list' syntax description."
  (syntax-test-cases (syn:macro-lambda-list)
    ;; Invalid syntax
    '((&whole #1=1)
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    '((a . #2=1)
      syn:invalid-syntax-error #2# "variable name must be a symbol")
    ;; Repeated section
    '((&environment e1 foo bar . #3=(&environment e2))
      syn:invalid-syntax-error #3# "&ENVIRONMENT must not be repeated")
    ;; Repeated names
    '((x #4=x)
      syn:invalid-syntax-error #4# "the variable name X occurs more than once")
    ;; Valid syntax
    '(#5=()
      (:macro-lambda-list () :source #5#))
    '(#6=(#7=&rest #8=rest)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #7#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name rest :source #8#))))
                                :source #8#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #6#))
    '(#9=(#10=&body #11=body)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #10#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name body :source #11#))))
                                :source #11#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #9#))
    '(#12=(#13=&aux #14=a #15=&environment #16=e)
      (:macro-lambda-list
       ((:environment-section . 1)
        (((:environment-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &environment :source #15#)))
            (:parameter . 1) (((:environment-parameter
                                ((:name . 1) (((:variable-name () :name e :source #16#))))
                                :source #16#)))))))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &aux :source #13#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name a :source #14#))))
                                :source #14#)
                               :evaluation :compound))))
          :evaluation :compound)))
        :source #12#))
    '(#17=(#18=(#19=x #20=y))
      (:macro-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:pattern
                                                ((:required-section . 1)
                                                 (((:required-section
                                                    ((:parameter . *) (((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name x :source #19#)
                                                                                        :evaluation nil)))
                                                                         :source #19#)
                                                                        :evaluation :compound)
                                                                       ((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name y :source #20#)
                                                                                        :evaluation nil)))
                                                                         :source #20#)
                                                                        :evaluation :compound))))
                                                   :evaluation :compound)))
                                                :source #18#)
                                               :evaluation :compound)))
                                :source #18#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #17#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' syntax description."
  (syntax-test-cases (syn:deftype-lambda-list)
    '(#1=(#2=foo #3=bar)
      (:deftype-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
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
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #1#))))
