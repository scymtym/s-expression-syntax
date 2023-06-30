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
    '((&optional (foo #2=(declare)))
      syn:invalid-syntax-error #2# "declare is not allowed here")
    '((&rest . #3=())
      syn:invalid-syntax-error #3# "a variable name must follow &REST")
    '((&key (foo #4=(declare)))
      syn:invalid-syntax-error #4# "declare is not allowed here")
    '((&aux (foo #5=(declare)))
      syn:invalid-syntax-error #5# "declare is not allowed here")
    '((&rest r . #6=(5))
      syn:invalid-syntax-error #6# "not allowed at this position in an ordinary lambda list")
    ;; Repeated sections
    '((&rest r . #7=(&rest s1))
      syn:invalid-syntax-error #7# "&REST is not allowed at this position in an ordinary lambda list")
    ;; Repeated names
    '((a #8=a)
      syn:invalid-syntax-error #8# "the variable name A occurs more than once")
    ;; Valid syntax
    '(#9=(#10=&optional #11=a #12=b)
      (:ordinary-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #10#)))
            (:parameter . *) (((:optional-parameter
                                ((:name . 1) (((:variable-name () :name a :source #11#))))
                                :source #11#)
                               :evaluation :compound)
                              ((:optional-parameter
                                ((:name . 1) (((:variable-name () :name b :source #12#))))
                                :source #12#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #9#))
    '(#13=(#14=&aux #15=a)
      (:ordinary-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #14#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #15#))))
                                :source #15#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #13#))
    '(#16=(#17=foo #18=bar #19=&optional #20=(#21=hash-table-rehash-size #22=default)
          #23=&rest #24=x
          #25=&key #26=((#27=:x-kw #28=y) #29=1 #30=supplied?) #31=b #32=&allow-other-keys
          #33=&aux #34=(#35=a #36=1))
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #17#))))
                                :source #17#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name bar :source #18#))))
                                :source #18#)))))))
        (:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &optional :source #19#)))
            (:parameter . *) (((:optional-parameter
                                ((:name    . 1) (((:variable-name
                                                   ()
                                                   :name hash-table-rehash-size :source #21#)))
                                 (:default . 1) (((:unparsed
                                                   ()
                                                   :expression #22#
                                                   :context    :form
                                                   :source     #22#)
                                                  :evaluation t)))
                                :source #20#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #23#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name x :source #24#))))
                                :source #24#)))))))
        (:keyword-section  . 1)
        (((:keyword-section
           ((:keyword          . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &key
                                       :source  #25#)))
            (:parameter        . *) (((:keyword-parameter
                                       ((:name     . 1) (((:variable-name
                                                           ()
                                                           :name y :source #28#)))
                                        (:keyword  . 1) (((:keyword
                                                           ()
                                                           :name :x-kw :source #27#)))
                                        (:default  . 1) (((:unparsed
                                                           ()
                                                           :expression 1
                                                           :context    :form
                                                           :source     #29#)
                                                          :evaluation t))
                                        (:supplied . 1) (((:variable-name
                                                           ()
                                                           :name supplied? :source #30#))))
                                       :source #26#)
                                      :evaluation :compound)
                                     ((:keyword-parameter
                                       ((:name . 1) (((:variable-name
                                                       ()
                                                       :name b :source #31#))))
                                       :source #31#)
                                      :evaluation :compound))
            (:allow-other-keys . 1) (((:lambda-list-keyword
                                       ()
                                       :keyword &allow-other-keys
                                       :source  #32#)))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #33#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name
                                                 ()
                                                 :name a :source #35#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression #36#
                                                 :context    :form
                                                 :source     #36#)
                                                :evaluation t)))
                                :source #34#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #16#))
    '(#37=(#38=foo #39=foo2 #40=&rest #41=pie
           #42=&key #43=((#44=:foo #45=bar) #46=:default #47=bar-p)
           #48=&aux #49=(#50=a #51=1) #52=b)
      (:ordinary-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo :source #38#))))
                                :source #38#))
                              ((:required-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name foo2 :source #39#))))
                                :source #39#)))))))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #40#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name pie :source #41#))))
                                :source #41#)))))))
        (:keyword-section . 1)
        (((:keyword-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &key :source #42#)))
            (:parameter . *) (((:keyword-parameter
                                ((:name     . 1) (((:variable-name
                                                    ()
                                                    :name bar :source #45#)))
                                 (:keyword  . 1) (((:keyword
                                                    ()
                                                    :name :foo :source #44#)))
                                 (:default  . 1) (((:unparsed
                                                    ()
                                                    :expression :default
                                                    :context    :form
                                                    :source     #46#)
                                                   :evaluation t))
                                 (:supplied . 1) (((:variable-name
                                                    ()
                                                    :name bar-p :source #47#))))
                                :source #43#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #48#)))
            (:parameter . *) (((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name a :source #50#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1
                                                 :context    :form
                                                 :source     #51#)
                                                :evaluation t)))
                                :source #49#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name . 1) (((:variable-name () :name b :source #52#))))
                                :source #52#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #37#))))

;;; Generic function lambda list

(test generic-function-lambda-list
  (syntax-test-cases (syn:generic-function-lambda-list)
    ;; Invalid syntax
    '((&optional (k #1=2))
      syn:invalid-syntax-error #1# "optional parameter initializer form is not allowed in a generic function lambda list")
    '((&rest . #2=())
      syn:invalid-syntax-error #2# "a variable name must follow &REST")
    '((&key (k #3=3))
      syn:invalid-syntax-error #3# "keyword parameter initializer form is not allowed in a generic function lambda list")
    '(#4=(&aux)
      syn:invalid-syntax-error #4# "&AUX is not allowed at this position in a generic function lambda list")
    ;; Valid syntax
    '(#5=(#6=&optional #7=a)
      (:generic-function-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &optional :source #6#)))
            (:parameter . *) (((:optional-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name a :source #7#))))
                                :source #7#)
                               :evaluation nil)))))))
       :source #5#))))

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
    '((&rest . #6=())
      syn:invalid-syntax-error #6# "a variable name must follow &REST")
    ;; Repeated sections
    '((&rest r . #7=(&rest s2))
      syn:invalid-syntax-error #7# "&REST is not allowed at this position in a specialized lambda list")
    ;; Repeated names
    '(((baz fez) (#8=foo bar) &rest foo)
      syn:invalid-syntax-error #8# "the variable name FOO occurs more than once")
    ;; Valid syntax
    '(#9=(#10=(#11=baz #12=fez) #13=(#14=foo #15=bar) #16=&rest #17=whoop)
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name baz :source #11#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name fez :source #12#)
                                                      :evaluation :compound)))
                                :source #10#)
                               :evaluation :compound)
                              ((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name foo :source #14#)))
                                 (:specializer . 1) (((:type-name
                                                       ()
                                                       :name bar :source #15#)
                                                      :evaluation :compound)))
                                :source #13#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #16#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whoop :source #17#))))
                                :source #17#))))))))
       :source #9#))
    '(#18=(#19=(#20=x #21=(eql #22=5)))
      (:specialized-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:specialized-parameter
                                ((:name        . 1) (((:variable-name
                                                       ()
                                                       :name x :source #20#)))
                                 (:specializer . 1) (((:eql-specializer
                                                       ((:object . 1) (((:unparsed
                                                                         ()
                                                                         :expression 5
                                                                         :context    :form
                                                                         :source     #22#)
                                                                        :evaluation t)))
                                                       :source #21#)
                                                      :evaluation :compound)))
                                :source #19#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #18#))
    '(#23=(#24=&aux #25=a)
      (:specialized-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &aux :source #24#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #25#))))
                                :source #25#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #23#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' syntax description."
  (syntax-test-cases (syn:destructuring-lambda-list)
    ;; Invalid syntax
    '((&whole . #1=())
      syn:invalid-syntax-error #1# "a variable name must follow &WHOLE")
    '((a . #2=1)
      syn:invalid-syntax-error #2# "variable name must be a symbol")
    '((&rest . #3=())
      syn:invalid-syntax-error #3# "a variable name must follow &REST")
    '((&rest r . #4=5)
      syn:invalid-syntax-error #4# "not allowed at this position in a destructuring lambda list")
    ;; Repeated sections
    '((&rest r . #5=(&rest s3))
      syn:invalid-syntax-error #5# "&REST is not allowed at this position in a destructuring lambda list")
    ;; Valid syntax
    '(#6=(#7=&rest #8=r)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #7#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name r :source #8#))))
                                :source #8#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #6#))
    '(#9=(#10=&body #11=b)
      (:destructuring-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #10#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name b :source #11#))))
                                :source #11#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #9#))
    '(#12=(#13=(#14=foo #15=bar))
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
                                                                             :name foo :source #14#)
                                                                            :evaluation nil)))
                                                             :source #14#)
                                                            :evaluation :compound)
                                                           ((:required-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name bar :source #15#)
                                                                            :evaluation nil)))
                                                             :source #15#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #13#)
                                   :evaluation :compound)))
                                :source #13#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #12#))
    '(#16=(#17=&whole #18=whole #19=(#20=foo #21=&key #22=a) #23=&rest #24=fez)
      (:destructuring-lambda-list
       ((:whole-section . 1)
        (((:whole-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &whole :source #17#)))
            (:parameter . 1) (((:whole-parameter
                                ((:name . 1) (((:variable-name
                                                () :name whole :source #18#))))
                                :source #18#)))))))
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
                                                                             :name foo :source #20#)
                                                                            :evaluation nil)))
                                                             :source #20#)
                                                            :evaluation :compound))))
                                       :evaluation :compound))
                                     (:keyword-section . 1)
                                     (((:keyword-section
                                        ((:keyword   . 1) (((:lambda-list-keyword
                                                             ()
                                                             :keyword &key :source #21#)))
                                         (:parameter . *) (((:keyword-parameter
                                                             ((:name . 1) (((:variable-name
                                                                             ()
                                                                             :name a :source #22#))))
                                                             :source #22#)
                                                            :evaluation :compound))))
                                       :evaluation :compound)))
                                    :source #19#)
                                   :evaluation :compound)))
                                :source #19#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword () :keyword &rest :source #23#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name fez :source #24#))))
                                :source #24#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #16#))
    '(#25=(#26=&optional #27=(#28=(#29=bar #30=baz) #31=(5 6) #32=bar-baz-p))
      (:destructuring-lambda-list
       ((:optional-section . 1)
        (((:optional-section
           ((:keyword   . 1)
            (((:lambda-list-keyword () :keyword &optional :source #26#)))
            (:parameter . *)
            (((:optional-parameter
               ((:name     . 1) (((:pattern
                                   ((:required-section . 1)
                                    (((:required-section
                                       ((:parameter . *) (((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name bar :source #29#)
                                                                           :evaluation nil)))
                                                            :source #29#)
                                                           :evaluation :compound)
                                                          ((:required-parameter
                                                            ((:name . 1) (((:variable-name
                                                                            ()
                                                                            :name baz :source #30#)
                                                                           :evaluation nil)))
                                                            :source #30#)
                                                           :evaluation :compound))))
                                      :evaluation :compound)))
                                   :source #28#)))
                (:default  . 1) (((:unparsed
                                   ()
                                   :expression (5 6)
                                   :context    :form
                                   :source     #31#)
                                  :evaluation t))
                (:supplied . 1) (((:variable-name
                                   ()
                                   :name bar-baz-p :source #32#))))
               :source #27#)
              :evaluation :compound))))
          :evaluation :compound)))
       :source #25#))
    '(#33=(#34=&aux #35=a #36=(#37=b #38=1))
      (:destructuring-lambda-list
       ((:aux-section . 1)
        (((:aux-section
           ((:keyword . 1) (((:lambda-list-keyword () :keyword &aux :source #34#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name () :name a :source #35#))))
                                :source #35#)
                               :evaluation :compound)
                              ((:aux-parameter
                                ((:name  . 1) (((:variable-name () :name b :source #37#)))
                                 (:value . 1) (((:unparsed
                                                 ()
                                                 :expression 1 :context :form :source #38#)
                                                :evaluation t)))
                                :source #36#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #33#))
    '(#39=(#40=a . #41=rest)
      (:destructuring-lambda-list
       ((:required-section . 1)
        (((:required-section
           ((:parameter . *) (((:required-parameter
                                ((:name . 1) (((:variable-name () :name a :source #40#)
                                               :evaluation nil)))
                                :source #40#)
                               :evaluation :compound))))
          :evaluation :compound))
        (:cdr . 1)
        (((:variable-name () :name rest :source #41#))))
       :source #39#))))

;;; Macro lambda list

(test macro-lambda-list
  "Smoke test for the `macro-lambda-list' syntax description."
  (syntax-test-cases (syn:macro-lambda-list)
    ;; Invalid syntax
    '((&environment . #1=())
      syn:invalid-syntax-error #1# "a variable name must follow &ENVIRONMENT")
    '((&whole . #2=())
      syn:invalid-syntax-error #2# "a variable name must follow &WHOLE")
    '((&whole #3=1)
      syn:invalid-syntax-error #3# "variable name must be a symbol")
    '((a . #4=1)
      syn:invalid-syntax-error #4# "variable name must be a symbol")
    '((&rest . #5=())
      syn:invalid-syntax-error #5# "a variable name must follow &REST")
    '((&rest r . #6=5)
      syn:invalid-syntax-error #6# "not allowed at this position in a macro lambda list")
    ;; Repeated section
    '((&environment e1 foo bar . #7=(&environment e2))
      syn:invalid-syntax-error #7# "&ENVIRONMENT must not be repeated")
    ;; Repeated names
    '((x #8=x)
      syn:invalid-syntax-error #8# "the variable name X occurs more than once")
    ;; Valid syntax
    '(#9=()
      (:macro-lambda-list () :source #9#))
    '(#10=(#11=&rest #12=rest)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &rest :source #11#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name rest :source #12#))))
                                :source #12#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #10#))
    '(#13=(#14=&body #15=body)
      (:macro-lambda-list
       ((:rest-section . 1)
        (((:rest-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &body :source #14#)))
            (:parameter . 1) (((:rest-parameter
                                ((:name . 1) (((:variable-name
                                                () :name body :source #15#))))
                                :source #15#)
                               :evaluation nil))))
          :evaluation :compound)))
       :source #13#))
    '(#16=(#17=&aux #18=a #19=&environment #20=e)
      (:macro-lambda-list
       ((:environment-section . 1)
        (((:environment-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &environment :source #19#)))
            (:parameter . 1) (((:environment-parameter
                                ((:name . 1) (((:variable-name () :name e :source #20#))))
                                :source #20#)))))))
        (:aux-section . 1)
        (((:aux-section
           ((:keyword   . 1) (((:lambda-list-keyword
                                ()
                                :keyword &aux :source #17#)))
            (:parameter . *) (((:aux-parameter
                                ((:name . 1) (((:variable-name
                                                ()
                                                :name a :source #18#))))
                                :source #18#)
                               :evaluation :compound))))
          :evaluation :compound)))
        :source #16#))
    '(#21=(#22=(#23=x #24=y))
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
                                                                                         :name x :source #23#)
                                                                                        :evaluation nil)))
                                                                         :source #23#)
                                                                        :evaluation :compound)
                                                                       ((:required-parameter
                                                                         ((:name . 1) (((:variable-name
                                                                                         ()
                                                                                         :name y :source #24#)
                                                                                        :evaluation nil)))
                                                                         :source #24#)
                                                                        :evaluation :compound))))
                                                   :evaluation :compound)))
                                                :source #22#)
                                               :evaluation :compound)))
                                :source #22#)
                               :evaluation :compound))))
          :evaluation :compound)))
       :source #21#))))

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
