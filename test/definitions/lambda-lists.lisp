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
    '((&rest r . #6=(5))
      syn:invalid-syntax-error #6# "not allowed at this position in an ordinary lambda list")
    ;; Repeated sections
    '((&rest r . #7=(&rest s1))
      syn:invalid-syntax-error #7# "&REST is not allowed at this position in an ordinary lambda list")
    ;; Valid syntax
    '(#8=(#9=&optional #10=a #11=b)
      (:ordinary-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #9#)))
            (:parameter . *) (((:optional-parameter
                                ((:name . 1) (((:variable-name () :name a :source #10#))))
                                :source #10#)
                               :evaluation :compound)
                              ((:optional-parameter
                                ((:name . 1) (((:variable-name () :name b :source #11#))))
                                :source #11#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #8#))
    '(#12=(#13=&aux #14=a)
      (:ordinary-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #13#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #14#))))
                                :source #14#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #12#))
    '(#15=(#16=foo #17=bar #18=&optional #19=(#20=hash-table-rehash-size #21=default)
          #22=&rest #23=x
          #24=&key #25=((#26=:x-kw #27=y) #28=1 #29=supplied?) #30=b #31=&allow-other-keys
          #32=&aux #33=(#34=a #35=1))
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #16#))))
                                :source #16#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name bar :source #17#))))
                                :source #17#)))))))
        (:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #18#)))
            (:parameter . *) (((:optional-parameter
                                ((:name    . 1) (((:variable-name
                                                   ()
                                                   :name hash-table-rehash-size :source #20#)))
                                 (:default . 1) (((:unparsed
                                                   ()
                                                   :expression #21#
                                                   :context    :form
                                                   :source     #21#)
                                                  :evaluation t)))
                                :source #19#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #22#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name x :source #23#))))
                                :source #23#)))))))
        (:keyword-section  . 1)
        (((:keyword-section
           ((:keyword          . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &key
                                       :source  #24#)))
            (:parameter        . *) (((:keyword-parameter
                                       ((:name     . 1) (((:variable-name
                                                           ()
                                                           :name y :source #27#)))
                                        (:keyword  . 1) (((:keyword
                                                           ()
                                                           :name :x-kw :source #26#)))
                                        (:default  . 1) (((:unparsed
                                                           ()
                                                           :expression 1
                                                           :context    :form
                                                           :source     #28#)
                                                          :evaluation t))
                                        (:supplied . 1) (((:variable-name
                                                           ()
                                                           :name supplied? :source #29#))))
                                       :source #25#)
                                      :evaluation :compound)
                                     ((:keyword-parameter
                                       ((:name . 1) (((:variable-name
                                                       ()
                                                       :name b :source #30#))))
                                       :source #30#)
                                      :evaluation :compound))
            (:allow-other-keys . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &allow-other-keys
                                       :source  #31#)))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #32#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name
                                                 ()
                                                 :name a :source #34#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression #35#
                                                 :context    :form
                                                 :source     #35#)
                                                :evaluation t)))
                                :source #33#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #15#))
    '(#36=(#37=foo #38=foo2 #39=&rest #40=pie
           #41=&key #42=((#43=:foo #44=bar) #45=:default #46=bar-p)
           #47=&aux #48=(#49=a #50=1) #51=b)
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #37#))))
                                :source #37#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo2 :source #38#))))
                                :source #38#)))))))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #39#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name pie :source #40#))))
                                :source #40#)))))))
        (:keyword-section . 1)
        (((:keyword-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &key :source #41#)))
            (:parameter . *) (((:keyword-parameter
                                ((:name     . 1) (((:variable-name
                                                    ()
                                                    :name bar :source #44#)))
                                 (:keyword  . 1) (((:keyword
                                                    ()
                                                    :name :foo :source #43#)))
                                 (:default  . 1) (((:unparsed
                                                    ()
                                                    :expression :default
                                                    :context    :form
                                                    :source     #45#)
                                                   :evaluation t))
                                 (:supplied . 1) (((:variable-name
                                                    ()
                                                    :name bar-p :source #46#))))
                                :source #42#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #47#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name a :source #49#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1
                                                 :context    :form
                                                 :source     #50#)
                                                :evaluation t)))
                                :source #48#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name . 1) (((:variable-name () :name b :source #51#))))
                                :source #51#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #36#))))

;;; Generic function lambda list

(test generic-function-lambda-list
  (syntax-test-cases (syn:generic-function-lambda-list)
    ;; Invalid syntax
    '((&optional (k #1=2))
      syn:invalid-syntax-error #1# "optional parameter initializer form is not allowed in a generic function lambda list")
    '((&key (k #2=3))
      syn:invalid-syntax-error #2# "keyword parameter initializer form is not allowed in a generic function lambda list")
    '(#3=(&aux)
      syn:invalid-syntax-error #3# "&AUX is not allowed at this position in a generic function lambda list")
    ;; Valid syntax
    '(#4=(#5=&optional #6=a)
      (:generic-function-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &optional :source #5#)))
            (:parameter . *) (((:optional-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name a :source #6#))))
                                :source #6#)
                               :evaluation nil)))))))
       :source #4#))))

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
    '(((foo t) . #5=5)
      syn:invalid-syntax-error #5# "not allowed at this position in a specialized lambda list")
    ;; Repeated sections
    '((&rest r . #6=(&rest s2))
      syn:invalid-syntax-error #6# "&REST is not allowed at this position in a specialized lambda list")
    ;; Repeated names
    '(((baz fez) (#7=foo bar) &rest foo)
      syn:invalid-syntax-error #7# "the variable name FOO occurs more than once")
    ;; Valid syntax
    '(#8=(#9=(#10=baz #11=fez) #12=(#13=foo #14=bar) #15=&rest #16=whoop)
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name baz :source #10#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name fez :source #11#)
                                                      :evaluation :compound)))
                                :source #9#)
                               :evaluation :compound)
                              ((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name foo :source #13#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name bar :source #14#)
                                                      :evaluation :compound)))
                                :source #12#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #15#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whoop :source #16#))))
                                :source #16#))))))))
       :source #8#))
    '(#17=(#18=(#19=x #20=(eql #21=5)))
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name x :source #19#)))
                                 (:specializer . 1) (((:eql-specializer
                                                       ((:object . 1) (((:unparsed
                                                                         ()
                                                                         :expression 5
                                                                         :context    :form
                                                                         :source     #21#)
                                                                        :evaluation t)))
                                                       :source #20#)
                                                      :evaluation :compound)))
                                :source #18#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #17#))
    '(#22=(#23=&aux #24=a)
      (:specialized-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #23#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #24#))))
                                :source #24#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #22#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' syntax description."
  (syntax-test-cases (syn:destructuring-lambda-list)
    ;; Invalid syntax
    '((a . #1=1)
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    '((&rest r . #2=5)
      syn:invalid-syntax-error #2# "not allowed at this position in a destructuring lambda list")
    ;; Repeated sections
    '((&rest r . #3=(&rest s3))
      syn:invalid-syntax-error #3# "&REST is not allowed at this position in a destructuring lambda list")
    ;; Valid syntax
    '(#4=(#5=&rest #6=r)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #5#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name r :source #6#))))
                                :source #6#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #4#))
    '(#7=(#8=&body #9=b)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #8#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name b :source #9#))))
                                :source #9#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #7#))
    '(#10=(#11=(#12=foo #13=bar))
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
                                                                             :name foo :source #12#)
                                                                            :evaluation nil)))
                                                             :source #12#)
                                                            :evaluation :compound)
                                                           ((:required-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name bar :source #13#)
                                                                            :evaluation nil)))
                                                             :source #13#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #11#)
                                   :evaluation :compound)))
                                :source #11#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #10#))
    '(#14=(#15=&whole #16=whole #17=(#18=foo #19=&key #20=a) #21=&rest #22=fez)
      (:destructuring-lambda-list
       ((:whole-section . 1)
        (((:whole-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &whole :source #15#)))
            (:parameter . 1) (((:whole-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whole :source #16#))))
                                :source #16#)))))))
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
                                                                             :name foo :source #18#)
                                                                            :evaluation nil)))
                                                             :source #18#)
                                                            :evaluation :compound))))
                                       :evaluation :compound))
                                     (:keyword-section . 1)
                                     (((:keyword-section
                                        ((:keyword   . 1) (((:lambda-list-keyword
                                                             ()
                                                             :keyword &key :source #19#)))
                                         (:parameter . *) (((:keyword-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name a :source #20#))))
                                                             :source #20#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #17#)
                                   :evaluation :compound)))
                                :source #17#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #21#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name fez :source #22#))))
                                :source #22#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #14#))
    '(#23=(#24=&optional #25=(#26=(#27=bar #28=baz) #29=(5 6) #30=bar-baz-p))
      (:destructuring-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1)
            (((:lambda-list-keyword () :keyword &optional :source #24#)))
            (:parameter . *)
            (((:optional-parameter
               ((:name     . 1) (((:pattern
                                   ((:required-section . 1)
                                    (((:required-section
                                       ((:parameter . *) (((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name bar :source #27#)
                                                                           :evaluation nil)))
                                                            :source #27#)
                                                           :evaluation :compound)
                                                          ((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name baz :source #28#)
                                                                           :evaluation nil)))
                                                            :source #28#)
                                                           :evaluation :compound))))
                                      :evaluation :compound)))
                                   :source #26#)))
                (:default  . 1) (((:unparsed
                                   ()
                                   :expression (5 6)
                                   :context    :form
                                   :source     #29#)
                                  :evaluation t))
                (:supplied . 1) (((:variable-name
                                   ()
                                   :name bar-baz-p :source #30#))))
               :source #25#)
              :evaluation :compound))))
          :evaluation :compound)))
       :source #23#))
    '(#31=(#32=&aux #33=a #34=(#35=b #36=1))
      (:destructuring-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword . 1) (((:lambda-list-keyword () :keyword &aux :source #32#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #33#))))
                                :source #33#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name b :source #35#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1 :context :form :source #36#)
                                                :evaluation t)))
                                :source #34#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #31#))
    '(#37=(#38=a . #39=rest)
      (:destructuring-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name () :name a :source #38#)
                                               :evaluation nil)))
                                :source #38#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:cdr . 1)
        (((:variable-name () :name rest :source #39#))))
       :source #37#))))

;;; Macro lambda list

(test macro-lambda-list
  "Smoke test for the `macro-lambda-list' syntax description."
  (syntax-test-cases (syn:macro-lambda-list)
    ;; Invalid syntax
    '((&whole #1=1)
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    '((a . #2=1)
      syn:invalid-syntax-error #2# "variable name must be a symbol")
    '((&rest r . #3=5)
      syn:invalid-syntax-error #3# "not allowed at this position in a macro lambda list")
    ;; Repeated section
    '((&environment e1 foo bar . #4=(&environment e2))
      syn:invalid-syntax-error #4# "&ENVIRONMENT must not be repeated")
    ;; Repeated names
    '((x #5=x)
      syn:invalid-syntax-error #5# "the variable name X occurs more than once")
    ;; Valid syntax
    '(#6=()
      (:macro-lambda-list () :source #6#))
    '(#7=(#8=&rest #9=rest)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #8#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name rest :source #9#))))
                                :source #9#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #7#))
    '(#10=(#11=&body #12=body)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #11#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name body :source #12#))))
                                :source #12#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #10#))
    '(#13=(#14=&aux #15=a #16=&environment #17=e)
      (:macro-lambda-list
       ((:environment-section . 1)
        (((:environment-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &environment :source #16#)))
            (:parameter . 1) (((:environment-parameter
                                ((:name . 1) (((:variable-name () :name e :source #17#))))
                                :source #17#)))))))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &aux :source #14#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name a :source #15#))))
                                :source #15#)
                               :evaluation :compound))))
          :evaluation :compound)))
        :source #13#))
    '(#18=(#19=(#20=x #21=y))
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
                                                                                         :name x :source #20#)
                                                                                        :evaluation nil)))
                                                                         :source #20#)
                                                                        :evaluation :compound)
                                                                       ((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name y :source #21#)
                                                                                        :evaluation nil)))
                                                                         :source #21#)
                                                                        :evaluation :compound))))
                                                   :evaluation :compound)))
                                                :source #19#)
                                               :evaluation :compound)))
                                :source #19#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #18#))))

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
