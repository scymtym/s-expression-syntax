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
    '(#6=(&optional #7=a #8=b)
      (:ordinary-lambda-list
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
      (:ordinary-lambda-list
       ((:aux . *) (((:aux-parameter
                      ((:name . 1) (((:variable-name () :name a :source #10#))))
                      :source #10#)
                     :evaluation :compound)))
       :source #9#))
    '(#11=(#12=foo #13=bar &optional #14=(#15=hash-table-rehash-size #16=default)
           &rest #17=x
           &key #18=((#19=:x-kw #20=y) #21=1 #22=supplied?) #23=b #24=&allow-other-keys
           &aux #25=(#26=a #27=1))
      (:ordinary-lambda-list
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
                                    (:default . 1) (((:unparsed
                                                      ()
                                                      :expression #16#
                                                      :context    :form
                                                      :source     #16#)
                                                     :evaluation t)))
                                   :source #14#)
                                  :evaluation :compound))
        (:rest             . 1) (((:rest-parameter
                                   ((:name . 1) (((:variable-name
                                                   () :name x :source #17#))))
                                   :source #17#)))
        (:keyword          . *) (((:keyword-parameter
                                   ((:name     . 1) (((:variable-name
                                                       ()
                                                       :name y :source #20#)))
                                    (:keyword . 1)  (((:keyword
                                                       ()
                                                       :name :x-kw :source #19#)))
                                    (:default  . 1) (((:unparsed
                                                       ()
                                                       :expression 1
                                                       :context    :form
                                                       :source     #21#)
                                                      :evaluation t))
                                    (:supplied . 1) (((:variable-name
                                                       ()
                                                       :name supplied? :source #22#))))
                                   :source #18#)
                                  :evaluation :compound)
                                 ((:keyword-parameter
                                   ((:name . 1) (((:variable-name
                                                   ()
                                                   :name b :source #23#))))
                                   :source #23#)
                                  :evaluation :compound))
        (:allow-other-keys? . 1) (((:lambda-list-keyword
                                    ()
                                    :keyword &allow-other-keys
                                    :source  #24#)))
        (:aux              . *) (((:aux-parameter
                                   ((:name  . 1) (((:variable-name
                                                    ()
                                                    :name a :source #26#)))
                                    (:value . 1) (((:unparsed
                                                    ()
                                                    :expression #27#
                                                    :context    :form
                                                    :source     #27#)
                                                   :evaluation t)))
                                   :source #25#)
                                  :evaluation :compound)))
       :source #11#))
    '(#28=(#29=foo #30=foo2 &rest #31=pie
           &key #32=((#33=:foo #34=bar) #35=:default #36=bar-p)
           &aux #37=(#38=a #39=1) #40=b)
      (:ordinary-lambda-list
       ((:required . *) (((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name foo :source #29#))))
                           :source #29#))
                         ((:required-parameter
                           ((:name . 1) (((:variable-name
                                           ()
                                           :name foo2 :source #30#))))
                           :source #30#)))
        (:rest     . 1) (((:rest-parameter
                           ((:name . 1) (((:variable-name
                                           () :name pie :source #31#))))
                           :source #31#)))
        (:keyword  . *) (((:keyword-parameter
                           ((:name     . 1) (((:variable-name
                                               ()
                                               :name bar :source #34#)))
                            (:keyword  . 1) (((:keyword
                                               ()
                                               :name :foo :source #33#)))
                            (:default  . 1) (((:unparsed
                                               ()
                                               :expression :default
                                               :context    :form
                                               :source     #35#)
                                              :evaluation t))
                            (:supplied . 1) (((:variable-name
                                               ()
                                               :name bar-p :source #36#))))
                           :source #32#)
                          :evaluation :compound))
        (:aux      . *) (((:aux-parameter
                           ((:name  . 1) (((:variable-name () :name a :source #38#)))
                            (:value . 1) (((:unparsed
                                            ()
                                            :expression 1
                                            :context    :form
                                            :source     #39#)
                                           :evaluation t)))
                           :source #37#)
                          :evaluation :compound)
                         ((:aux-parameter
                           ((:name . 1) (((:variable-name () :name b :source #40#))))
                           :source #40#)
                          :evaluation :compound)))
       :source #28#))))

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
    '(#6=(#7=(#8=baz #9=fez) #10=(#11=foo #12=bar) &rest #13=whoop)
      (:specialized-lambda-list
       ((:required . *) (((:specialized-parameter
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
                          :evaluation :compound))
        (:rest     . 1) (((:rest-parameter
                           ((:name . 1) (((:variable-name
                                           () :name whoop :source #13#))))
                           :source #13#))))
       :source #6#))
    '(#14=(#15=(#16=x #17=(eql #18=5)))
      (:specialized-lambda-list
       ((:required . *) (((:specialized-parameter
                           ((:name        . 1) (((:variable-name
                                                  ()
                                                  :name x :source #16#)))
                            (:specializer . 1) (((:eql-specializer
                                                  ((:object . 1) (((:unparsed
                                                                    ()
                                                                    :expression 5
                                                                    :context    :form
                                                                    :source     #18#)
                                                                   :evaluation t)))
                                                  :source #17#)
                                                 :evaluation :compound)))
                           :source #15#)
                          :evaluation :compound)))
       :source #14#))
    '(#19=(&aux #20=a)
      (:specialized-lambda-list
       ((:aux . *) (((:aux-parameter
                      ((:name . 1) (((:variable-name () :name a :source #20#))))
                      :source #20#)
                     :evaluation :compound)))
       :source #19#))))

;;; Destructuring lambda list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' syntax description."
  (syntax-test-cases (syn:destructuring-lambda-list)
    ;; Invalid syntax
    '((a . #1=1)
      syn:invalid-syntax-error #1# "variable name must be a symbol")
    ;; Repeated section
    '((&environment e1 foo bar . #2=(&environment e2))
      syn:invalid-syntax-error #2# "&ENVIRONMENT must not be repeated")
    ;; Valid syntax
    '(#3=(&rest #4=r)
      (:destructuring-lambda-list
       ((:rest . 1) (((:rest-parameter
                       ((:name . 1) (((:variable-name
                                       () :name r :source #4#))))
                       :source #4#)
                      :evaluation :compound)))
       :source #3#))
    '(#5=(&body #6=b)
      (:destructuring-lambda-list
       ((:rest . 1) (((:rest-parameter
                       ((:name . 1) (((:variable-name
                                       () :name b :source #6#))))
                       :source #6#)
                      :evaluation :compound)))
       :source #5#))
    '(#7=(#8=(#9=foo #10=bar))
      (:destructuring-lambda-list
       ((:required . *) (((:required-parameter
                           ((:name . 1) (((:pattern
                                           ((:required . *) (((:required-parameter
                                                               ((:name . 1) (((:variable-name
                                                                               ()
                                                                               :name foo :source #9#)
                                                                              :evaluation nil)))
                                                               :source #9#)
                                                              :evaluation :compound)
                                                             ((:required-parameter
                                                               ((:name . 1) (((:variable-name
                                                                               ()
                                                                               :name bar :source #10#)
                                                                              :evaluation nil)))
                                                               :source #10#)
                                                              :evaluation :compound)))
                                           :source #8#)
                                          :evaluation :compound)))
                           :source #8#)
                          :evaluation :compound)))
       :source #7#))
    '(#11=(&whole #12=whole #13=(#14=foo &key #15=a) . (&rest #16=fez))
      (:destructuring-lambda-list
       ((:whole    . 1) (((:whole-parameter
                           ((:name . 1) (((:variable-name
                                           () :name whole :source #12#))))
                           :source #12#)))
        (:required . *) (((:required-parameter
                           ((:name . 1) (((:pattern
                                           ((:required . *) (((:required-parameter
                                                               ((:name . 1) (((:variable-name
                                                                               ()
                                                                               :name foo :source #14#)
                                                                              :evaluation nil)))
                                                               :source #14#)
                                                              :evaluation :compound))
                                            (:key      . *) (((:keyword-parameter
                                                               ((:name . 1) (((:variable-name
                                                                               ()
                                                                               :name a :source #15#))))
                                                               :source #15#)
                                                              :evaluation :compound)))
                                           :source #13#)
                                          :evaluation :compound)))
                           :source #13#)
                          :evaluation :compound))
        (:rest     . 1) (((:rest-parameter
                           ((:name . 1) (((:variable-name
                                           () :name fez :source #16#))))
                           :source #16#)
                          :evaluation :compound)))
       :source #11#))
    '(#17=(&optional #18=(#19=(#20=bar #21=baz) #22=(5 6) #23=bar-baz-p))
      (:destructuring-lambda-list
       ((:optional . *) (((:optional-parameter
                           ((:name . 1)     (((:pattern
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name bar :source #20#)
                                                                                  :evaluation nil)))
                                                                   :source #20#)
                                                                  :evaluation :compound)
                                                                 ((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name baz :source #21#)
                                                                                  :evaluation nil)))
                                                                   :source #21#)
                                                                  :evaluation :compound)))
                                               :source #19#)))
                            (:default  . 1) (((:unparsed
                                               ()
                                               :expression (5 6)
                                               :context    :form
                                               :source     #22#)
                                              :evaluation t))
                            (:supplied . 1) (((:variable-name
                                               ()
                                               :name bar-baz-p :source #23#))))
                           :source #18#)
                          :evaluation :compound)))
       :source #17#))
    '(#24=(&aux #25=a #26=(#27=b #28=1))
      (:destructuring-lambda-list
       ((:aux . *) (((:aux-parameter
                      ((:name . 1) (((:variable-name () :name a :source #25#))))
                      :source #25#)
                     :evaluation :compound)
                    ((:aux-parameter
                      ((:name  . 1) (((:variable-name () :name b :source #27#)))
                       (:value . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source #28#)
                                      :evaluation t)))
                      :source #26#)
                     :evaluation :compound)))
       :source #24#))
    '(#29=(#30=a . #31=rest)
      (:destructuring-lambda-list
       ((:required . *) (((:required-parameter
                           ((:name . 1) (((:variable-name () :name a :source #30#)
                                          :evaluation nil)))
                           :source #30#)
                          :evaluation :compound))
        (:cdr      . 1) (((:variable-name () :name rest :source #31#))))
       :source #29#))))

;;; Deftype lambda list

(test deftype-lambda-list
  "Smoke test for the `deftype-lambda-list' syntax description."
  (syntax-test-cases (syn:deftype-lambda-list)
    '(#1=(#2=foo #3=bar)
      (:deftype-lambda-list
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
