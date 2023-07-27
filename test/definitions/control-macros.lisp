;;;; control-macros.lisp --- Tests for control macro rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.control-macros
  :in :s-expression-syntax)

;;; `lambda'

(define-macro-test (lambda)
  ;; Invalid syntax
  '((lambda #1=1)
    syn:invalid-syntax-error #1# "must be an ordinary lambda list")
  '((lambda (x #2=x))
    syn:invalid-syntax-error #2# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#3=(lambda #4=())
    (:lambda
     ((:lambda-list . 1) (((:ordinary-lambda-list () :source #4#)
                           :evaluation :compound)))
     :source #3#))
  '(#5=(lambda #6=(#7=a #8=&rest #9=b) #10=(foo))
    (:lambda
     ((:lambda-list . 1) (((:ordinary-lambda-list
                            ((:required-section . 1)
                             (((:required-section
                                ((:parameter . *) (((:required-parameter
                                                     ((:name . 1) (((:variable-name
                                                                     ()
                                                                     :name a :source #7#)
                                                                    :evaluation nil)))
                                                     :source #7#)))))))
                             (:rest-section . 1)
                             (((:rest-section
                                ((:keyword   . 1) (((:lambda-list-keyword
                                                     ()
                                                     :keyword &rest :source #8#)))
                                 (:parameter . 1) (((:rest-parameter
                                                     ((:name . 1) (((:variable-name
                                                                     ()
                                                                     :name b :source #9#))))
                                                     :source #9#))))))))
                            :source #6#)
                           :evaluation :compound))
      (:form        . *) (((:unparsed
                            ()
                            :expression (foo) :context :form :source #10#)
                           :evaluation t)))
     :source #5#)))

;;; `and' and `or'

(macrolet ((define (name)
             (let ((kind (make-keyword name)))
               `(define-macro-test (,name)
                  ;; Invalid syntax
                  (let* ((declare '(declare))
                         (form    `(,',name ,declare)))
                    `(,form
                      syn:invalid-syntax-error ,declare))
                  ;; Valid syntax
                  (let ((form `(,',name)))
                    `(,form
                      (,',kind () :source ,form)))
                  (let* ((test '1)
                         (form `(,',name ,test)))
                    `(,form
                      (,',kind
                       ((:form . *) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source ,test)
                                      :evaluation t)))
                       :source ,form)))
                  (let* ((form1 '1)
                         (form2 '2)
                         (form  `(,',name ,form1 ,form2)))
                    `(,form
                      (,',kind
                       ((:form . *) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source ,form1)
                                      :evaluation t)
                                     ((:unparsed
                                       ()
                                       :expression 2 :context :form :source ,form2)
                                      :evaluation t)))
                       :source ,form)))))))
  (define and)
  (define or))

;;; `cond'

(define-macro-test (cond)
  ;; Invalid syntax
  '((cond #1=1)
    syn:invalid-syntax-error #1# "must be a clause of the form (TEST FORM*)")
  '((cond #2=())
    syn:invalid-syntax-error #2# "must be a clause of the form (TEST FORM*)")
  '((cond (1 #3=(declare)))
    syn:invalid-syntax-error #3# "declare is not allowed here")
  ;; Valid syntax
  '(#4=(cond)
    (:cond () :source #4#))
  '(#5=(cond #6=(#7=1))
    (:cond
     ((:clause . *) (((:cond-clause
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source #7#)
                                      :evaluation t)))
                       :source #6#)
                      :evaluation :compound)))
     :source #5#))
  '(#8=(cond #9=(#10=1 #11=2))
    (:cond
     ((:clause . *) (((:cond-clause
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source #10#)
                                      :evaluation t))
                        (:form . *) (((:unparsed
                                       ()
                                       :expression 2 :context :form :source #11#)
                                      :evaluation t)))
                       :source #9#)
                      :evaluation :compound)))
     :source #8#))
  '(#12=(cond #13=(#14=1) #15=(#16=2))
    (:cond
     ((:clause . *) (((:cond-clause
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source #14#)
                                      :evaluation t)))
                       :source #13#)
                      :evaluation :compound)
                     ((:cond-clause
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 2 :context :form :source #16#)
                                      :evaluation t)))
                       :source #15#)
                      :evaluation :compound)))
     :source #12#)))

;;; `when' and `unless'

(macrolet ((define (name)
             (let ((kind (make-keyword name)))
               `(define-macro-test (,name)
                  ;; Invalid syntax
                  '((,name)
                    syn:invalid-syntax-error)
                  (let* ((declare '(declare))
                         (form    `(,',name ,declare)))
                    `(,form
                      syn:invalid-syntax-error ,declare))
                  ;; Valid syntax
                  (let* ((test '1)
                         (form `(,',name ,test)))
                    `(,form
                      (,',kind
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source ,test)
                                      :evaluation t)))
                       :source ,form)))
                  (let* ((test  '1)
                         (form1 '2)
                         (form  `(,',name ,test ,form1)))
                    `(,form
                      (,',kind
                       ((:test . 1) (((:unparsed
                                       ()
                                       :expression 1 :context :form :source ,test)
                                      :evaluation t))
                        (:form . *) (((:unparsed
                                       ()
                                       :expression 2 :context :form :source ,form1)
                                      :evaluation t)))
                       :source ,form)))))))
  (define when)
  (define unless))

;;; `[ec]case'

(define-macro-test (case)
  ;; Invalid syntax
  '(#1=(case)
    syn:invalid-syntax-error #1#)
  '((case x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")
  '((case x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")
  '((case x (otherwise 1) #4=(otherwise 2))
    syn:invalid-syntax-error #4# "otherwise clause must not be repeated")
  '((case x (otherwise 1) #5=(:normal 2))
    syn:invalid-syntax-error #5#
    "normal clause must not follow otherwise clause")
  ;; Valid syntax
  '(#6=(case #7=x)
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #7#)
                       :evaluation t)))
      :source #6#))
  '(#8=(case #9=x #10=(#11=y #12=1))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #9#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #11#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #12#)
                                       :evaluation t)))
                        :source #10#)
                       :evaluation :compound)))
     :source #8#))
  '(#13=(case #14=x #15=(#16=y #17=1) #18=(#19=z #20=2))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #14#)
                          :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #16#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #17#)
                                       :evaluation t)))
                        :source #15#)
                       :evaluation :compound)
                      ((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression z :context :key :source #19#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #20#)
                                       :evaluation t)))
                        :source #18#)
                       :evaluation :compound)))
     :source #13#))
  '(#21=(case #22=x #23=((#24=y #25=z) #26=1 #27=2))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #22#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #24#))
                                      ((:unparsed
                                        ()
                                        :expression z :context :key :source #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #26#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2 :context :form :source #27#)
                                       :evaluation t)))
                        :source #23#)
                       :evaluation :compound)))
     :source #21#))
  '(#28=(case #29=x #30=(otherwise #31=1))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #29#)
                       :evaluation t))
      (:clause  . *) (((:case-otherwise-clause
                        ((:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #31#)
                                       :evaluation t)))
                        :source #30#)
                       :evaluation :compound)))
     :source #28#))
  '(#32=(case #33=x #34=((#35=otherwise) #36=1))
    (:case
        ((:keyform . 1) (((:unparsed
                           ()
                           :expression x :context :form :source #33#)
                          :evaluation t))
         (:clause  . *) (((:case-normal-clause
                           ((:key  . *) (((:unparsed
                                           ()
                                           :expression otherwise :context :key :source #35#)))
                            (:form . *) (((:unparsed
                                           ()
                                           :expression 1 :context :form :source #36#)
                                          :evaluation t)))
                           :source #34#)
                          :evaluation :compound)))
      :source #32#)))

(define-macro-test (ccase)
  ;; Invalid syntax
  '(#1=(ccase)
    syn:invalid-syntax-error #1#)
  '((ccase #2=1)
    syn:invalid-syntax-error #2# "place must be a cons or a variable name")
  '((ccase x #3=1)
    syn:invalid-syntax-error #3#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  '((ccase x #4=())
    syn:invalid-syntax-error #4#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  ;; Valid syntax
  '(#5=(ccase #6=x)
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #6#)
                        :evaluation t)))
     :source #5#))
  '(#7=(ccase #8=x #9=(#10=otherwise #11=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #8#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression otherwise
                                         :context    :key
                                         :source     #10#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #11#)
                                        :evaluation t)))
                         :source #9#)
                        :evaluation :compound)))
     :source #7#))
  '(#12=(ccase #13=x #14=(#15=t #16=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #13#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression t
                                         :context    :key
                                         :source     #15#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #16#)
                                        :evaluation t)))
                         :source #14#)
                        :evaluation :compound)))
     :source #12#))
  '(#17=(ccase #18=x #19=(#20=y #21=1 #22=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #18#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #20#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #21#)
                                        :evaluation t)
                                       ((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #22#)
                                        :evaluation t)))
                         :source #19#)
                        :evaluation :compound)))
     :source #17#))
  '(#23=(ccase #24=x #25=(#26=y #27=1) #28=(#29=z #30=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #24#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #26#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #27#)
                                        :evaluation t)))
                         :source #25#)
                        :evaluation :compound)
                       ((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression z
                                         :context    :key
                                         :source     #29#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #30#)
                                        :evaluation t)))
                         :source #28#)
                        :evaluation :compound)))
     :source #23#))
  '(#31=(ccase #32=x #33=((#34=y #35=z) #36=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #32#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #34#))
                                       ((:unparsed
                                         ()
                                         :expression z
                                         :context    :key
                                         :source     #35#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #36#)
                                        :evaluation t)))
                         :source #33#)
                        :evaluation :compound)))
     :source #31#))
  '(#37=(ccase #38=x #39=((#40=otherwise) #41=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #38#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression otherwise
                                         :context    :key
                                         :source     #40#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #41#)
                                        :evaluation t)))
                         :source #39#)
                        :evaluation :compound)))
     :source #37#))
  '(#42=(ccase #43=x #44=((#45=1 #46=t) #47=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #43#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :key
                                         :source     #45#))
                                       ((:unparsed
                                         ()
                                         :expression t
                                         :context    :key
                                         :source     #46#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #47#)
                                        :evaluation t)))
                         :source #44#)
                        :evaluation :compound)))
     :source #42#)))

(define-macro-test (ecase)
  ;; Invalid syntax
  '(#1=(ecase)
    syn:invalid-syntax-error #1#)
  '((ecase x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  '((ecase x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  ;; Valid syntax
  '(#4=(ecase #5=x)
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #5#)
                       :evaluation t)))
     :source #4#))
  '(#6=(ecase #7=x #8=(#9=otherwise #10=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #7#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression otherwise
                                        :context    :key
                                        :source     #9#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #10#)
                                       :evaluation t)))
                        :source #8#)
                       :evaluation :compound)))
     :source #6#))
  '(#11=(ecase #12=x #13=(#14=t #15=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #12#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression t
                                        :context    :key
                                        :source     #14#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #15#)
                                       :evaluation t)))
                        :source #13#)
                       :evaluation :compound)))
     :source #11#))
  '(#16=(ecase #17=x #18=(#19=y #20=1 #21=2))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #17#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #19#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #20#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #21#)
                                       :evaluation t)))
                        :source #18#)
                       :evaluation :compound)))
     :source #16#))
  '(#22=(ecase #23=x #24=(#25=y #26=1) #27=(#28=z #29=2))
    (:ecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #23#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #26#)
                                       :evaluation t)))
                        :source #24#)
                       :evaluation :compound)
                      ((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression z
                                        :context    :key
                                        :source     #28#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #29#)
                                       :evaluation t)))
                        :source #27#)
                       :evaluation :compound)))
     :source #22#))
  '(#30=(ecase #31=x #32=((#33=y #34=z) #35=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #31#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #33#))
                                      ((:unparsed
                                        ()
                                        :expression z
                                        :context    :key
                                        :source     #34#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #35#)
                                       :evaluation t)))
                        :source #32#)
                       :evaluation :compound)))
     :source #30#))
  '(#36=(ecase #37=x #38=((#39=otherwise) #40=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #37#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression otherwise
                                        :context    :key
                                        :source     #39#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #40#)
                                       :evaluation t)))
                        :source #38#)
                       :evaluation :compound)))
     :source #36#))
  '(#41=(ecase #42=x #43=((#44=1 #45=t) #46=2))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #42#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :key
                                        :source     #44#))
                                      ((:unparsed
                                        ()
                                        :expression t
                                        :context    :key
                                        :source     #45#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #46#)
                                       :evaluation t)))
                        :source #43#)
                       :evaluation :compound)))
     :source #41#)))

;;; `[ec]typecase'

(define-macro-test (typecase)
  ;; Invalid syntax
  '(#1=(typecase)
    syn:invalid-syntax-error #1#)
  '((typecase x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")
  '((typecase x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")
  '((typecase x (#4=(otherwise) 1))
    syn:invalid-syntax-error #4# "CL:OTHERWISE does not name a compound type")
  '((typecase x (otherwise 1) #5=(otherwise 2))
    syn:invalid-syntax-error #5#
    "otherwise clause must not be repeated")
  '((typecase x (otherwise 1) #6=(:normal 2))
    syn:invalid-syntax-error #6#
    "normal clause must not follow otherwise clause")
  ;; Valid syntax
  '(#7=(typecase #8=x)
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #8#)
                       :evaluation t)))
     :source #7#))
  '(#9=(typecase #10=x #11=(#12=y #13=1))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #10#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #12#))))
                                        :source #12#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #13#)
                                       :evaluation t)))
                        :source #11#)
                       :evaluation :compound)))
     :source #9#))
  '(#14=(typecase #15=x #16=(#17=y #18=1) #19=(#20=z #21=2))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #15#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #17#))))
                                        :source #17#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #18#)
                                       :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name z :source #20#))))
                                        :source #20#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #21#)
                                       :evaluation t)))
                        :source #19#)
                       :evaluation :compound)))
     :source #14#))
  '(#22=(typecase #23=x #24=(#25=(#26=y) #27=1 #28=2))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #23#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:compound-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #26#))))
                                        :source #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #27#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2 :context :form :source #28#)
                                       :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
     :source #22#))
  '(#29=(typecase #30=x #31=(otherwise #32=1))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #30#)
                       :evaluation t))
      (:clause  . *) (((:typecase-otherwise-clause
                        ((:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #32#)
                                       :evaluation t)))
                        :source #31#)
                       :evaluation :compound)))
     :source #29#)))

(define-macro-test (ctypecase)
  ;; Invalid syntax
  '(#1=(ctypecase)
    syn:invalid-syntax-error #1#)
  '((ctypecase #2=1)
    syn:invalid-syntax-error #2# "place must be a cons or a variable name")
  '((ctypecase x #3=1)
    syn:invalid-syntax-error #3# "must be a clause of the form (TYPE FORM*)")
  '((ctypecase x #4=())
    syn:invalid-syntax-error #4# "must be a clause of the form (TYPE FORM*)")
  '((ctypecase x (#5=otherwise 1))
    syn:invalid-syntax-error #5# "CL:OTHERWISE does not name a type")
  '((ctypecase x (#6=(otherwise) 1))
    syn:invalid-syntax-error #6# "CL:OTHERWISE does not name a compound type")
  ;; Valid syntax
  '(#7=(ctypecase #8=x)
    (:ctypecase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #8#)
                        :evaluation t)))
     :source #7#))
  '(#9=(ctypecase #10=x #11=(#12=y #13=1 #14=2))
    (:ctypecase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #10#)
                        :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #12#))))
                                         :source #12#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1 :context :form :source #13#)
                                        :evaluation t)
                                       ((:unparsed
                                         ()
                                         :expression 2 :context :form :source #14#)
                                        :evaluation t)))
                         :source #11#)
                        :evaluation :compound)))
     :source #9#))
  '(#15=(ctypecase #16=x #17=(#18=y #19=1) #20=(#21=z #22=2))
    (:ctypecase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #16#)
                        :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #18#))))
                                         :source #18#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1 :context :form :source #19#)
                                        :evaluation t)))
                         :source #17#)
                        :evaluation :compound)
                       ((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name z :source #21#))))
                                         :source #21#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 2 :context :form :source #22#)
                                        :evaluation t)))
                         :source #20#)
                        :evaluation :compound)))
     :source #15#))
  '(#23=(ctypecase #24=x #25=(#26=(#27=y) #28=1))
    (:ctypecase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #24#)
                        :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:compound-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #27#))))
                                         :source #26#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1 :context :form :source #28#)
                                        :evaluation t)))
                         :source #25#)
                        :evaluation :compound)))
     :source #23#)))

(define-macro-test (etypecase)
  ;; Invalid syntax
  '(#1=(etypecase)
    syn:invalid-syntax-error #1#)
  '((etypecase x #2=1)
    syn:invalid-syntax-error #2# "must be a clause of the form (TYPE FORM*)")
  '((etypecase x #3=())
    syn:invalid-syntax-error #3# "must be a clause of the form (TYPE FORM*)")
  '((etypecase x (#4=otherwise 1))
    syn:invalid-syntax-error #4# "CL:OTHERWISE does not name a type")
  '((etypecase x (#5=(otherwise) 1))
    syn:invalid-syntax-error #5# "CL:OTHERWISE does not name a compound type")
  ;; Valid syntax
  '(#6=(etypecase #7=x)
    (:etypecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #7#)
                       :evaluation t)))
     :source #6#))
  '(#8=(etypecase #9=x #10=(#11=y #12=1 #13=2))
    (:etypecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #9#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #11#))))
                                        :source #11#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #12#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2 :context :form :source #13#)
                                       :evaluation t)))
                        :source #10#)
                       :evaluation :compound)))
     :source #8#))
  '(#14=(etypecase #15=x #16=(#17=y #18=1) #19=(#20=z #21=2))
    (:etypecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #15#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #17#))))
                                        :source #17#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #18#)
                                       :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name z :source #20#))))
                                        :source #20#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #21#)
                                       :evaluation t)))
                        :source #19#)
                       :evaluation :compound)))
     :source #14#))
  '(#22=(etypecase #23=x #24=(#25=(#26=y) #27=1))
    (:etypecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #23#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:compound-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #26#))))
                                        :source #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #27#)
                                       :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
     :source #22#)))

;;; `prog' and `prog*'

(macrolet
    ((define (name)
       (let ((kind (make-keyword name)))
         `(define-macro-test (,name)
            ;; Invalid syntax
            '((,name)
              syn:invalid-syntax-error)
            '((,name #1=1)
              syn:invalid-syntax-error #1# "must be a list of bindings")
            '((,name () nil #2=nil)
              syn:invalid-syntax-error #2# "the tag NIL occurs more than once")
            ;; Valid syntax
            '(#3=(,name ())
              (,kind () :source #3#))
            '(#4=(,name (#5=a #6=(#7=b #8=1)))
              (,kind
               ((:binding . *) (((:value-binding
                                  ((:name . 1) (((:variable-name () :name a :source #5#)
                                                 :evaluation (:binding :namespace variable
                                                                       :scope     :lexical))))
                                  :source #5#)
                                 :evaluation :compound)
                                ((:value-binding
                                  ((:name  . 1) (((:variable-name () :name b :source #7#)
                                                  :evaluation (:binding :namespace variable
                                                                        :scope     :lexical)))
                                                (:value . 1) (((:unparsed () :expression 1
                                                                             :context    :form
                                                                             :source     #8#)
                                                               :evaluation t)))
                                  :source #6#)
                                 :evaluation :compound)))
               :source #4#))
            '(#9=(,name () (declare #10=(ignore #11=a) #12=(inline #13=b)))
              (,kind
               ((:declaration . *) (((:declaration-specifier
                                      ((:argument . *) (((:variable-name () :name a :source #11#))))
                                      :kind ignore :source #10#))
                                    ((:declaration-specifier
                                      ((:argument . *) (((:function-name () :name b :source #13#))))
                                      :kind inline :source #12#))))
               :source #9#))
            '(#14=(,name () #15=:foo #16=(list) #17=:bar #18=(progn))
              (,kind
               ((:segment . *) (((:tagbody-segment
                                  ((:label     . 1) (((:tag () :name :foo :source #15#)
                                                      :evaluation (:binding :namespace syn::tag
                                                                            :scope     :lexical)))
                                   (:statement . *) (((:unparsed () :expression (list)
                                                                    :context    :form
                                                                    :source     #16#)
                                                      :evaluation t)))
                                  :source #15#)
                                 :evaluation :compound)
                                ((:tagbody-segment
                                  ((:label     . 1) (((:tag () :name :bar :source #17#)
                                                      :evaluation (:binding :namespace syn::tag
                                                                            :scope     :lexical)))
                                   (:statement . *) (((:unparsed () :expression (progn)
                                                                    :context    :form
                                                                    :source     #18#)
                                                      :evaluation t)))
                                  :source #17#)
                                 :evaluation :compound)))
               :source #14#))))))
  (define prog)
  (define prog*))

;;; `prog1' and `prog2'

(define-macro-test (prog1)
  ;; Invalid syntax
  '((prog1 . #1=())
    syn:invalid-syntax-error #1# "must be of the form (prog1 FIRST-FORM FORM*)")
  '((prog1 #2=(declare))
    syn:invalid-syntax-error #2# "declare is not allowed here")
  ;; Valid syntax
  '(#3=(prog1 #4=1)
    (:prog1
     ((:first . 1) (((:unparsed () :expression 1 :context :form :source #4#)
                     :evaluation t)))
     :source #3#))
  '(#5=(prog1 #6=1 #7=2)
    (:prog1
     ((:first . 1) (((:unparsed () :expression 1 :context :form :source #6#)
                     :evaluation t))
      (:rest  . *) (((:unparsed () :expression 2 :context :form :source #7#)
                     :evaluation t)))
     :source #5#)))

(define-macro-test (prog2)
  ;; Invalid syntax
  '((prog2 . #1=())
    syn:invalid-syntax-error #1# "must be of the form (prog2 FIRST-FORM SECOND-FORM FORM*)")
  '((prog2 1 . #2=())
    syn:invalid-syntax-error #2# "must be of the form (prog2 FIRST-FORM SECOND-FORM FORM*)")
  '((prog2 #3=(declare))
    syn:invalid-syntax-error #3# "declare is not allowed here")
  ;; Valid syntax
  '(#4=(prog2 #5=1 #6=2)
    (:prog2
     ((:first  . 1) (((:unparsed () :expression 1 :context :form :source #5#)
                      :evaluation t))
      (:second . 1) (((:unparsed () :expression 2 :context :form :source #6#)
                      :evaluation t)))
     :source #4#))
  '(#7=(prog2 #8=1 #9=2 #10=3)
    (:prog2
     ((:first  . 1) (((:unparsed () :expression 1 :context :form :source #8#)
                      :evaluation t))
      (:second . 1) (((:unparsed () :expression 2 :context :form :source #9#)
                      :evaluation t))
      (:rest   . *) (((:unparsed () :expression 3 :context :form :source #10#)
                      :evaluation t)))
     :source #7#)))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-macro-test (handler-bind)
  '((handler-bind . #1=())
    syn:invalid-syntax-error #1# "must be a list of handler bindings")
  '((handler-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of handler bindings")
  '((handler-bind (#3=1))
    syn:invalid-syntax-error #3# "must be of the form (TYPE HANDLER-FORM)")
  '((handler-bind ((#4=1 (lambda (x)))))
    syn:invalid-syntax-error #4# "must be a type specifier")
  ;; Valid syntax
  '(#5=(handler-bind (#6=(#7=foo #8=(lambda (x) (bar))))
         #9=(baz))
    (:handler-bind
     ((:binding . *) (((:handler-binding
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name
                                                        ()
                                                        :name foo :source #7#))))
                                        :source #7#)))
                         (:form . 1) (((:unparsed
                                        ()
                                        :expression (lambda (x) (bar))
                                        :context    :form
                                        :source     #8#)
                                       :evaluation t)))
                        :source #6#)
                       :evaluation :compound))
      (:form    . *) (((:unparsed
                        ()
                        :expression (baz) :context :form :source #9#)
                       :evaluation t)))
     :source #5#)))

(define-macro-test (handler-case)
  '((handler-case)
    syn:invalid-syntax-error)
  '((handler-case nil . #1=(1))
    syn:invalid-syntax-error #1# "must be a list of handler clauses")
  '((handler-case nil (#2=1))
    syn:invalid-syntax-error #2# "must be a type specifier")
  '((handler-case nil (foo #3=1))
    syn:invalid-syntax-error #3# "must be a lambda list with zero or one required parameter")
  '((handler-case nil (foo (#4=1)))
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  ;; Repeated option
  '((handler-case nil (:no-error ()) #5=(:no-error ()))
    syn:invalid-syntax-error #5# "NO-ERROR must not be repeated")
  ;; Valid syntax
  '(#6=(handler-case #7=(foo)
         #8=(#9=bar (#10=x) #11=(baz))
         #12=(:no-error #13=(#14=y #15=z) #16=(fez)))
    (:handler-case
     ((:form   . 1)          (((:unparsed
                                ()
                                :expression (foo) :context :form :source #7#)
                               :evaluation t))
      (:clause . *)          (((:handler-clause
                                ((:type     . 1) (((:atomic-type-specifier
                                                    ((:name . 1) (((:type-name
                                                                    ()
                                                                    :name bar :source #9#))))
                                                    :source #9#)))
                                 (:variable . 1) (((:required-parameter
                                                    ((:name . 1) (((:variable-name
                                                                    ()
                                                                    :name x :source #10#))))
                                                    :source #10#)
                                                   :evaluation (:binding :namespace variable
                                                                         :scope     :lexical)))
                                 (:form     . *) (((:unparsed
                                                    ()
                                                    :expression (baz) :context :form :source #11#)
                                                   :evaluation t)))
                                :source #8#)
                               :evaluation :compound))
      (:no-error-clause . 1) (((:no-error-clause
                                ((:lambda-list . 1) (((:ordinary-lambda-list
                                                       ((:required-section . 1) (((:required-section
                                                                                   ((:parameter . *) (((:required-parameter
                                                                                                        ((:name . 1) (((:variable-name
                                                                                                                        ()
                                                                                                                        :name y :source #14#)
                                                                                                                       :evaluation nil)))
                                                                                                        :source #14#))
                                                                                                      ((:required-parameter
                                                                                                        ((:name . 1) (((:variable-name
                                                                                                                        ()
                                                                                                                        :name z :source #15#)
                                                                                                                       :evaluation nil)))
                                                                                                        :source #15#))))))))
                                                       :source #13#)
                                                      :evaluation :compound))
                                 (:form        . *) (((:unparsed
                                                       ()
                                                       :expression (fez) :context :form :source #16#)
                                                      :evaluation t)))
                                :source #12#)
                               :evaluation :compound)))
     :source #6#)))

(define-macro-test (restart-bind)
  '((restart-bind . #1=())
    syn:invalid-syntax-error #1# "must be a list of restart bindings")
  '((restart-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of restart bindings")
  '((restart-bind (#3=1))
    syn:invalid-syntax-error #3# "must be of the form (NAME FUNCTION [OPTIONS])")
  '((restart-bind ((#4=1 foo)))
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  '((restart-bind ((foo bar :test-function baz . #5=(:test-function fez))))
    syn:invalid-syntax-error #5# ":TEST-FUNCTION option must not be repeated")
  ;; Valid syntax
  '(#6=(restart-bind () #7=1)
    (:restart-bind
     ((:form . *) (((:unparsed
                     ()
                     :expression #7# :context :form :source #7#)
                    :evaluation t)))
     :source #6#))
  '(#8=(restart-bind (#9=(#10=foo #11=bar :report-function #12=baz)) #13=1)
    (:restart-bind
     ((:binding . *) (((:restart-binding
                        ((:name            . 1) (((:variable-name () :name foo :source #10#)))
                         (:function        . 1) (((:unparsed
                                                   ()
                                                   :expression #11# :context :form :source #11#)
                                                  :evaluation t))
                         (:report-function . 1) (((:unparsed
                                                   ()
                                                   :expression #12# :context :form :source #12#)
                                                  :evaluation t)))
                        :source #9#)
                       :evaluation :compound))
      (:form    . *) (((:unparsed
                        ()
                        :expression #13# :context :form :source #13#)
                       :evaluation t)))
     :source #8#))
  '(#14=(restart-bind () #15=a #16=b)
    (:restart-bind
     ((:form . *) (((:unparsed
                     ()
                     :expression a :context :form :source #15#)
                    :evaluation t)
                   ((:unparsed
                     ()
                     :expression b :context :form :source #16#)
                    :evaluation t)))
     :source #14#)))

(define-macro-test (restart-case)
  ;; Invalid syntax
  '((restart-case #1=(declare))
    syn:invalid-syntax-error #1# "declare is not allowed here")
  '((restart-case 1 . #2=(1))
    syn:invalid-syntax-error #2# "must be a list of restart clauses")
  '((restart-case 1 (#3=1))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((restart-case 1 (foo #4=1))
    syn:invalid-syntax-error #4# "must be an ordinary lambda list")
  '((restart-case 1 (nil ()))
    syn:invalid-syntax-error nil "for an unnamed restart, the :REPORT option must be supplied")
  '((restart-case 1 (bar () :interactive . #5=()))
    syn:invalid-syntax-error #5# "must be a function name or a lambda expression")
  '((restart-case 1 (bar () :report . #6=()))
    syn:invalid-syntax-error #6# "must be a string, a function name or a lambda expression")
  '((restart-case 1 (bar () :test . #7=()))
    syn:invalid-syntax-error #7# "must be a function name or a lambda expression")
  ;; Valid syntax
  '(#8=(restart-case #9=1)
    (:restart-case
     ((:form . 1) (((:unparsed
                     ()
                     :expression #9# :context :form :source #9#)
                    :evaluation t)))
     :source #8#))
  '(#10=(restart-case #11=1 #12=(#13=foo #14=(#15=x)
                               :report #16="bar"
                               :test #17=baz
                               #18=1))
    (:restart-case
     ((:form   . 1) (((:unparsed
                       ()
                       :expression #11# :context :form :source #11#)
                      :evaluation t))
      (:clause . *) (((:restart-clause
                       ((:name        . 1) (((:variable-name () :name foo :source #13#)))
                        (:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required-section . 1) (((:required-section
                                                                          ((:parameter . *) (((:required-parameter
                                                                                               ((:name . 1) (((:variable-name
                                                                                                               ()
                                                                                                               :name x :source #15#)
                                                                                                              :evaluation nil)))
                                                                                               :source #15#))))))))
                                              :source #14#)))
                        (:report-string . 1) (((:unparsed
                                                ()
                                                :expression "bar"
                                                :context    :restart-report-string
                                                :source     #16#)))
                        (:test-name     . 1) (((:function-name
                                                ()
                                                :name baz :source #17#)))
                        (:form          . *) (((:unparsed
                                                ()
                                                :expression 1
                                                :context    :form
                                                :source     #18#)
                                               :evaluation t)))
                       :source #12#)
                      :evaluation :compound)))
     :source #10#))
  '(#19=(restart-case #20=1 #21=(#22=foo #23=() (declare #24=(ignore #25=x))))
    (:restart-case
     ((:form    . 1) (((:unparsed
                        ()
                        :expression 1 :context :form :source #20#)
                       :evaluation t))
      (:clause  . *) (((:restart-clause
                        ((:name        . 1) (((:variable-name () :name foo :source #22#)))
                         (:lambda-list . 1) (((:ordinary-lambda-list () :source #23#)))
                         (:declaration . *) (((:declaration-specifier
                                               ((:argument . *)
                                                (((:variable-name
                                                   () :name x :source #25#))))
                                               :kind ignore :source #24#))))
                        :source #21#)
                       :evaluation :compound)))
      :source #19#))
  '(#26=(restart-case #27=1 #28=(#29=bar #30=()
                                   :test #31=(lambda #32=())
                                   :interactive #33=(lambda #34=())
                                   :report #35=(lambda #36=())))
    (:restart-case
        ((:form . 1)   (((:unparsed
                          ()
                          :expression 1 :context :form :source #27#)
                         :evaluation t))
         (:clause . *) (((:restart-clause
                          ((:name               . 1) (((:variable-name () :name bar :source #29#)))
                           (:lambda-list        . 1) (((:ordinary-lambda-list () :source #30#)))
                           (:interactive-lambda . 1) (((:lambda-expression
                                                        ((:lambda-list . 1) (((:ordinary-lambda-list
                                                                               ()
                                                                               :source #32#)
                                                                              :evaluation :compound)))
                                                        :source #31#)))
                           (:report-lambda      . 1) (((:lambda-expression
                                                        ((:lambda-list . 1) (((:ordinary-lambda-list
                                                                               ()
                                                                               :source #34#)
                                                                              :evaluation :compound)))
                                                        :source #33#)))
                           (:test-lambda        . 1) (((:lambda-expression
                                                        ((:lambda-list . 1) (((:ordinary-lambda-list
                                                                               ()
                                                                               :source #36#)
                                                                              :evaluation :compound)))
                                                        :source #35#))))
                          :source #28#)
                         :evaluation :compound)))
     :source #26#)))
