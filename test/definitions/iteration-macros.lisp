;;;; iteration-macros.lisp --- Tests for iteration macro rules.
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.iteration-macros
  :in :s-expression-syntax)

(define-macro-test (do)
  ;; Invalid syntax
  '((do . #1=())
    syn:invalid-syntax-error #1# "must be a list of iteration variable bindings")
  '((do #2=1)
    syn:invalid-syntax-error #2# "must be a list of iteration variable bindings")
  '((do (#3=1) ())
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((do (#4=()) ())
    syn:invalid-syntax-error #4# "must be of the form (VARIABLE [INIT-FORM [STEP-FORM]])")
  '((do (#5=(a b c d)) ())
    syn:invalid-syntax-error #5# "must be of the form (VARIABLE [INIT-FORM [STEP-FORM]])")
  '((do () . #6=())
    syn:invalid-syntax-error #6# "must be of the form (END-TEST RESULT*)")
  '((do () #7=1)
    syn:invalid-syntax-error #7# "must be of the form (END-TEST RESULT*)")
  '((do () #8=())
    syn:invalid-syntax-error #8# "must be of the form (END-TEST RESULT*)")
  ;; Repeated variable names
  '((do (a #9=a))
    syn:invalid-syntax-error #9# "the variable name A occurs more than once")
  ;; Valid syntax
  '(#10=(do () (#11=1))
    (:do
     ((:end-test . 1) (((:unparsed () :expression 1 :context :form :source #11#)
                        :evaluation t)))
     :source #10#))
  '(#12=(do (#13=a #14=(#15=b #16=1 #17=2)) (#18=3 #19=4 #20=5)
          #21=6 #22=(go 7) #23=7)
    (:do
     ((:variable . *) (((:do-iteration-variable
                         ((:variable . 1) (((:variable-name () :name a :source #13#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope     :lexical))))
                         :source #13#)
                        :evaluation :compound)
                       ((:do-iteration-variable
                         ((:variable . 1) (((:variable-name () :name b :source #15#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope     :lexical)))
                          (:init     . 1) (((:unparsed () :expression 1 :context :form :source #16#)
                                            :evaluation t))
                          (:step     . 1) (((:unparsed () :expression 2 :context :form :source #17#)
                                            :evaluation t)))
                         :source #14#)
                        :evaluation :compound))
      (:end-test . 1) (((:unparsed () :expression 3 :context :form :source #18#)
                        :evaluation t))
      (:result   . *) (((:unparsed () :expression 4 :context :form :source #19#)
                        :evaluation t)
                       ((:unparsed () :expression 5 :context :form :source #20#)
                        :evaluation t))
      (:segment  . *) (((:tagbody-segment
                         ((:label     . 1) (((:tag () :name 6 :source #21#)
                                             :evaluation (:binding :namespace syn::tag
                                                                   :scope     :lexical)))
                          (:statement . *) (((:unparsed () :expression (go 7)
                                                           :context    :form
                                                           :source     #22#)
                                             :evaluation t)))
                         :source #21#)
                        :evaluation :compound)
                       ((:tagbody-segment
                         ((:label . 1) (((:tag () :name 7 :source #23#)
                                         :evaluation (:binding :namespace syn::tag
                                                               :scope     :lexical)))
                          )
                         :source #23#)
                        :evaluation :compound)))
     :source #12#)))
