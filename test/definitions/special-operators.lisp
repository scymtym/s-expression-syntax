;;;; special-operators.lisp --- Tests for special operator rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.special-operators
  :in :s-expression-syntax)

;;; Special operators for control

(define-syntax-test (progn)
  '((progn . 1)    syn:invalid-syntax-error)

  '(#1=(progn)     (:progn () :source #1#))
  '(#2=(progn 1)   (:progn ((:form . *) ((1 :evaluation t))) :source #2#))
  '(#3=(progn 1 2) (:progn ((:form . *) ((1 :evaluation t)
                                         (2 :evaluation t)))
                           :source #3#)))

(define-syntax-test (if)
  '((if)                 syn:invalid-syntax-error)
  '((if foo)             syn:invalid-syntax-error)
  '((if foo bar baz fez) syn:invalid-syntax-error)

  '((if foo bar)         (:if
                          ((:test . 1) ((foo :evaluation t))
                           (:then . 1) ((bar :evaluation t)))
                          :source  (if foo bar)))
  '((if foo bar baz)     (:if
                          ((:test . 1) ((foo :evaluation t))
                           (:then . 1) ((bar :evaluation t))
                           (:else . 1) ((baz :evaluation t)))
                          :source  (if foo bar baz))))

;;; Special operators `block', `return-from', `return', `tagbody' and `go'

(define-syntax-test (block)
  '((block)         syn:invalid-syntax-error)
  '((block (foo))   syn:invalid-syntax-error)

  '((block foo 1)   (:block
                     ((:name . 1) ((foo :evaluation :binding))
                      (:form . *) ((1 :evaluation t)))
                     :source (block foo 1)))
  '((block foo a b) (:block
                     ((:name . 1) ((foo :evaluation :binding))
                      (:form . *) ((a :evaluation t)
                                   (b :evaluation t)))
                     :source (block foo a b))))

(define-syntax-test (return-from)
  '((return-from)         syn:invalid-syntax-error)
  '((return-from foo 1 2) syn:invalid-syntax-error)

  '((return-from foo)     (:return-from
                           ((:name . 1) ((foo :evaluation :reference)))
                           :source (return-from foo)))
  '((return-from foo 1)   (:return-from
                           ((:name   . 1) ((foo :evaluation :reference))
                            (:result . 1) ((1 :evaluation t)))
                           :source (return-from foo 1)))

  #+TODO (apply #'unparse-return-from-special-operator
                (parse-return-from-special-operator
                 (lambda (&rest args) (print args))
                 '(return-from foo bla))))

(define-syntax-test (return)
  '((return (declare)) syn:invalid-syntax-error)
  '((return 1 2)       syn:invalid-syntax-error)

  '((return)           (:return
                        ()
                        :source (return)))
  '((return 1)         (:return
                        ((:result . 1) ((1 :evaluation t)))
                        :source (return 1))))

(define-syntax-test (tagbody)
  '((tagbody nil nil)
    syn:invalid-syntax-error nil "the tag NIL occurs more than once")
  '((tagbody 1 nil 1)
    syn:invalid-syntax-error 1 "the tag 1 occurs more than once")

  '(#1=(tagbody)
    (:tagbody () :source #1#))
  '(#2=(tagbody #3=nil)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name nil :source #3#))))
                        :source #3#)
                       :evaluation :compound)))
     :source #2#))
  '(#4=(tagbody #5=(progn))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) (((progn)
                                            :evaluation t)))
                        :source #5#)
                       :evaluation :compound)))
     :source #4#))
  '(#6=(tagbody #7=0 #8=1 #9="foo" #10="bar" #11=a #12=(list 1) #13=(list 3))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name 0 :source #7#))))
                        :source #7#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name 1 :source #8#)))
                         (:statement . *) ((#9# :evaluation t)
                                           (#10# :evaluation t)))
                        :source #8#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name a :source #11#)))
                         (:statement . *) ((#12# :evaluation t)
                                           (#13# :evaluation t)))
                        :source #11#)
                       :evaluation :compound)))
     :source #6#))
  '(#14=(tagbody #15=(1+ a) #16=(1+ b) #17=:foo)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) ((#15# :evaluation t)
                                           (#16# :evaluation t)))
                        :source #15#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label . 1) (((:tag () :name :foo :source #17#))))
                        :source #17#)
                       :evaluation :compound)))
     :source #14#)))

(define-syntax-test (go)
  '((go)
    syn:invalid-syntax-error nil "must be a single tag")
  '((go . #1=(1 2))
    syn:invalid-syntax-error #1# "must be a single tag")
  '((go #2=(foo))
    syn:invalid-syntax-error #2# "tag must be a symbol or an integer")

  '(#3=(go #4=1)  (:go
                   ((:tag . 1) (((:tag () :name 1 :source #4#))))
                   :source #3#)))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(define-syntax-test (eval-when)
  '((eval-when)
    syn:invalid-syntax-error)
  '((eval-when #1=1)
    syn:invalid-syntax-error #1# "must be a list of situations")
  '((eval-when (#2=:foo))
    syn:invalid-syntax-error #2# "must be one of :COMPILE-TOPLEVEL, COMPILE, :LOAD-TOPLEVEL, LOAD, :EXECUTE, EVAL")

  '(#3=(eval-when ())
    (:eval-when () :source #3#))
  '(#4=(eval-when (:execute))
    (:eval-when ((:situation . *) ((:execute))) :source #4#))
  '(#5=(eval-when () a)
    (:eval-when ((:form . *) ((a :evaluation t))) :source #5#)))

(define-syntax-test (load-time-value)
  '((load-time-value)       syn:invalid-syntax-error)
  '((load-time-value 1 2)   syn:invalid-syntax-error)

  '(#1=(load-time-value foo)
    (:load-time-value ((:form . 1) ((foo :evaluation t))) :source #1#))
  '(#2=(load-time-value foo t)
    (:load-time-value ((:form        . 1) ((foo :evaluation t))
                       (:read-only-p . 1) ((t)))
     :source #2#)))

(define-syntax-test (quote)
  '((quote)          syn:invalid-syntax-error)
  '((quote x y)      syn:invalid-syntax-error)

  '(#1=(quote 1)     (:quote ((:material . 1) ((1)))     :source #1#))
  '(#2=(quote x)     (:quote ((:material . 1) ((x)))     :source #2#))
  '(#3=(quote quote) (:quote ((:material . 1) ((quote))) :source #3#)))

(define-syntax-test (function)
  '((function)
    syn:invalid-syntax-error nil "must be a function name or lambda expression")
  '((function 1)
    syn:invalid-syntax-error 1 "must be a function name or lambda expression")
  '((function . #1=(x y))
    syn:invalid-syntax-error #1# "nothing may follow function name or lambda expression")
  '((function (lambda 1))
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '((function (lambda (x #2=x)))
    syn:invalid-syntax-error #2# "the variable name X occurs more than once")

  '(#3=(function #4=foo)
    (:function
     ((:name . 1) (((:function-name () :name foo :source #4#))))
     :source #3#))
  '(#5=(function #6=(setf foo))
    (:function
     ((:name . 1) (((:function-name () :name (setf foo) :source #6#))))
     :source #5#))
  '(#7=(function #8=(lambda #9=()))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list () :source #9#)
                                             :evaluation :compound)))
                       :source #8#)
                      :evaluation :compound)))
     :source #7#))
  '(#10=(function #11=(lambda #12=(#13=a &rest #14=b) (foo)))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required . *) (((:required-parameter
                                                                  ((:name . 1) (((:variable-name
                                                                                  ()
                                                                                  :name a :source #13#)
                                                                                 :evaluation nil)))
                                                                  :source #13#)))
                                               (:rest     . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #14#))))
                                              :source #12#)
                                             :evaluation :compound))
                        (:form        . *) (((foo) :evaluation t)))
                       :source #11#)
                      :evaluation :compound)))
     :source #10#)))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'

(define-syntax-test (symbol-macrolet)
  '((symbol-macrolet)            syn:invalid-syntax-error)
  '((symbol-macrolet 1)          syn:invalid-syntax-error)
  '((symbol-macrolet (foo))      syn:invalid-syntax-error)
  '((symbol-macrolet ((foo)))    syn:invalid-syntax-error)
  '((symbol-macrolet ((:bla 1))) syn:invalid-syntax-error)

  '(#1=(symbol-macrolet ())
    (:symbol-macrolet () :source #1#))
  '(#2=(symbol-macrolet (#3=(#4=a 1) #5=(#6=b 2))
         (declare #7=(type #8=bit #9=d))
         c)
    (:symbol-macrolet
     ((:binding     . *) (((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name a :source #4#)
                                                :evaluation :binding))
                             (:expansion . 1) ((1 :evaluation t)))
                            :source #3#)
                           :evaluation :compound)
                          ((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name b :source #6#)
                                                :evaluation :binding))
                             (:expansion . 1) ((2 :evaluation t)))
                            :source #5#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #8#))))
                                                :source #8#))
                                              ((:variable-name () :name d :source #9#))))
                            :kind type :source #7#)))
      (:form        . *) ((c :evaluation t)))
     :source #2#)))

(define-syntax-test (let)
  '((let)       syn:invalid-syntax-error)
  '((let 1)     syn:invalid-syntax-error)
  '((let (1))   syn:invalid-syntax-error)
  '((let ((1))) syn:invalid-syntax-error)

  '(#1=(let ())     (:let () :source #1#))
  '(#2=(let (#3=a))
    (:let
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #3#)
                                       :evaluation :binding)))
                        :source #3#)
                       :evaluation :binding)))
      :source #2#))
  '(#4=(let (#5=(#6=a 1) #7=b #8=(#9=c 2))
         (declare #10=(type #11=boolean #12=a)) a)
    (:let
     ((:binding . *)     (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #6#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #5#)
                           :evaluation :compound)
                           ((:value-binding
                             ((:name . 1) (((:variable-name () :name b :source #7#)
                                            :evaluation :binding)))
                             :source #7#)
                            :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #9#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #8#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #11#))))
                                                :source #11#))
                                           ((:variable-name () :name a :source #12#))))
                            :kind type :source #10#)))
      (:form        . *) ((a :evaluation t)))
     :source #4#))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  '((let*)       syn:invalid-syntax-error)
  '((let* 1)     syn:invalid-syntax-error)
  '((let* (1))   syn:invalid-syntax-error)
  '((let* ((1))) syn:invalid-syntax-error)

  '(#1=(let* ())     (:let* () :source #1#))
  '(#2=(let* (#3=a))
    (:let*
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #3#)
                                       :evaluation :binding)))
                        :source #3#)
                       :evaluation :compound)))
      :source #2#))
  '(#4=(let* (#5=(#6=a 1) #7=b #8=(#9=c 2))
         (declare #10=(type #11=boolean #12=a))
         a)
    (:let*
     ((:binding     . *) (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #6#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #5#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name . 1) (((:variable-name () :name b :source #7#)
                                           :evaluation :binding)))
                            :source #7#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #9#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #8#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #11#))))
                                                :source #11#))
                                              ((:variable-name () :name a :source #12#))))
                            :kind type :source #10#)))
      (:form        . *) ((a :evaluation t)))
     :source #4#)))

(define-syntax-test (locally)
  '(#1=(locally)
    (:locally () :source #1#))
  '(#2=(locally (declare #3=(type #4=bit #5=a)))
    (:locally
     ((:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name () :name bit :source #4#))))
                                                :source #4#))
                                              ((:variable-name () :name a :source #5#))))
                            :kind type :source #3#))))
      :source #2#))
  '(#6=(locally a)
    (:locally ((:form . *) ((a :evaluation t))) :source #6#))
  '(#7=(locally (declare #8=(type #9=bit #10=a)) a)
    (:locally
     ((:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name () :name bit :source #9#))))
                                                :source #9#))
                                              ((:variable-name () :name a :source #10#))))
                            :kind type :source #8#)))
      (:form        . *) ((a :evaluation t)))
     :source #7#))

  #+TODO (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x)))))

(define-syntax-test (progv)
  '((progv)    syn:invalid-syntax-error)
  '((progv 1)  syn:invalid-syntax-error)
  '((progv ()) syn:invalid-syntax-error)

  '(#1=(progv () ())
    (:progv () :source #1#))
  '(#2=(progv (a) (b))
    (:progv
     ((:symbols . 1) (((a) :evaluation t))
      (:values  . 1) (((b) :evaluation t)))
     :source #2#))
  '(#3=(progv '(a) '(b))
    (:progv
     ((:symbols . 1) (('(a) :evaluation t))
      (:values  . 1) (('(b) :evaluation t)))
     :source #3#))
  '(#4=(progv () () 1)
    (:progv ((:form . *) ((1 :evaluation t))) :source #4#))
  '(#5=(progv () () 1 2)
    (:progv
     ((:form . *) ((1 :evaluation t) (2 :evaluation t)))
     :source #5#)))

;;; Special operators `macrolet', `flet' and `labels'

(define-syntax-test (macrolet)
  '((macrolet)         syn:invalid-syntax-error)
  '((macrolet 1)       syn:invalid-syntax-error)
  '((macrolet ((f)))   syn:invalid-syntax-error)
  '((macrolet ((f 1))) syn:invalid-syntax-error)
  '((macrolet ((f (x #1=x))))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#2=(macrolet ())
    (:macrolet () :source #2#))
  '(#3=(macrolet (#4=(#5=f #6=())))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #5#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ()
                                               :source #6#)
                                              :evaluation :compound)))
                        :source #4#)
                       :evaluation :compound)))
     :source #3#))
  '(#7=(macrolet (#8=(#9=f #10=(&whole #11=w #12=(#13=a #14=b) &rest #15=c)
                        (declare #16=(type #17=string #18=a))
                        a)))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ((:whole    . 1) (((:variable-name
                                                                   ()
                                                                   :name w :source #11#)))
                                                (:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:pattern
                                                                                   ((:required . *) (((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name a :source #13#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #13#)
                                                                                                      :evaluation :compound)
                                                                                                     ((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name b :source #14#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #14#)
                                                                                                      :evaluation :compound)))
                                                                                   :source #12#)
                                                                                  :evaluation :compound)))
                                                                   :source #12#)
                                                                  :evaluation :compound))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name c :source #15#)
                                                                  :evaluation :compound)))
                                               :source #10#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name string :source #17#))))
                                                                   :source #17#))
                                                                 ((:variable-name () :name a :source #18#))))
                                               :kind type :source #16#)))
                         (:form        . *) ((a  :evaluation t)))
                        :source #8#)
                       :evaluation :compound)))
      :source #7#))
  #+TODO (is (equal '(syn::names (foo bar baz)
               syn::functions
               ((syn::parsed-lambda ((a b) () bla () nil ()) nil nil ((list a b)))
                (syn::parsed-lambda (() () nil () nil ()) nil nil ("not-doc-string"))
                (syn::parsed-lambda (() () nil () nil ()) "doc-string" nil (1)))
               syn::declarations nil
               syn::forms ((foo 1 2)))
             (syn:parse nil (syn:find-syntax 'macrolet) '(macrolet ((foo (a b &rest bla) (list a b))
                                                                    (bar ()
                                                                     "not-doc-string")
                                                                    (baz ()
                                                                     "doc-string"
                                                                     1))
                                                          (foo 1 2))))))

(define-syntax-test (flet)
  '((flet)          syn:invalid-syntax-error)
  '((flet 1)        syn:invalid-syntax-error)
  '((flet ((1 ()))) syn:invalid-syntax-error)
  '((flet ((f 1)))  syn:invalid-syntax-error)
  '((flet ((f (x #1=x))))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
   ;; Valid syntax
  '(#2=(flet ())
    (:flet () :source #2#))
  '(#3=(flet (#4=(#5=f #6=())))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #5#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list () :source #6#)
                                              :evaluation :compound)))
                        :source #4#)
                       :evaluation :compound)))
     :source #3#))
  '(#7=(flet (#8=(#9=f #10=(#11=a &rest #12=b))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #11#)
                                                                                  :evaluation nil)))
                                                                   :source #11#)))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name b :source #12#))))
                                               :source #10#)
                                              :evaluation :compound)))
                        :source #8#)
                       :evaluation :compound)))
     :source #7#))
  '(#13=(flet (#14=(#15=f #16=() (declare #17=(type #18=bit #19=a)))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #15#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #16#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #18#))))
                                                                   :source #18#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #19#))))
                                               :kind type :source #17#))))
                        :source #14#)
                       :evaluation :compound)))
     :source #13#))
  '(#20=(flet (#21=(#22=f #23=() a)))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #22#)
                                               :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #23#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #21#)
                       :evaluation :compound)))
      :source #20#)))

(define-syntax-test (labels)
  '((labels)          syn:invalid-syntax-error)
  '((labels 1)        syn:invalid-syntax-error)
  '((labels ((1 ()))) syn:invalid-syntax-error)
  '((labels ((f 1)))  syn:invalid-syntax-error)
  '((labels ((f (x #1=x))))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#2=(labels ())
    (:labels () :source #2#))
  '(#3=(labels (#4=(#5=f #6=())))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #5#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #6#)
                                              :evaluation :compound)))
                        :source #4#)
                       :evaluation :compound)))
     :source #3#))
  '(#7=(labels (#8=(#9=f #10=(#11=a &rest #12=b))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #11#)
                                                                                  :evaluation nil)))
                                                                   :source #11#)))
                                                                (:rest     . 1) (((:variable-name
                                                                                   ()
                                                                                   :name b :source #12#))))
                                               :source #10#)
                                              :evaluation :compound)))
                        :source #8#)
                       :evaluation :compound)))
     :source #7#))
  '(#13=(labels (#14=(#15=f #16=() (declare #17=(type #18=bit #19=a)))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #15#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #16#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #18#))))
                                                                   :source #18#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #19#))))
                                               :kind type :source #17#))))
                        :source #14#)
                       :evaluation :compound)))
     :source #13#))
  '(#20=(labels (#21=(#22=f #23=() a)))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #22#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #23#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #21#)
                       :evaluation :compound)))
     :source #20#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim 1) syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(declaim)
    (:declaim () :source #1#))
  '(#2=(declaim #3=(type #4=bit #5=a))
    (:declaim
     ((:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #4#))))
                                                :source #4#))
                                              ((:variable-name () :name a :source #5#))))
                            :kind type :source #3#))))
     :source #2#)))

(define-syntax-test (the)
  '((the)             syn:invalid-syntax-error)
  '((the bit)         syn:invalid-syntax-error)
  '((the bit 1 extra) syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(the #2=bit 1) (:the
                       ((:type . 1) (((:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #2#))))
                                       :source #2#)))
                        (:form . 1) ((1 :evaluation t)))
                       :source #1#)))

;;; Special operator `setq'

(define-syntax-test (setq)
  '((setq a)                 syn:invalid-syntax-error)
  '((setq a 1 b)             syn:invalid-syntax-error)
  '((setq 1 1)               syn:invalid-syntax-error)
  '((setq (1+ a) 1)          syn:invalid-syntax-error)

  '(#1=(setq)                (:setq () :source #1#))
  '(#2=(setq #3=a 1)         (:setq
                              ((:name       . *) (((:variable-name () :name a :source #3#)))
                               (:value-form . *) ((1 :evaluation t)))
                              :source #2#))
  '(#4=(setq #5=a 1 #6=b 2) (:setq
                              ((:name       . *) (((:variable-name () :name a :source #5#))
                                                  ((:variable-name () :name b :source #6#)))
                               (:value-form . *) ((1 :evaluation t)
                                                  (2 :evaluation t)))
                              :source #4#)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  '((throw)                   syn:invalid-syntax-error)
  '((throw 'foo)              syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)     syn:invalid-syntax-error)

  '(#1=(throw 'foo 1)         (:throw
                               ((:tag-form    . 1) (('foo :evaluation t))
                                (:result-form . 1) ((1    :evaluation t)))
                               :source #1#))
  '(#2=(throw (+ 1 2) :value) (:throw
                               ((:tag-form    . 1) (((+ 1 2) :evaluation t))
                                (:result-form . 1) ((:value  :evaluation t)))
                               :source #2#)))

(define-syntax-test (catch)
  '((catch)                   syn:invalid-syntax-error)

  '(#1=(catch 'foo 1 2)       (:catch
                               ((:tag-form . 1) (('foo :evaluation t))
                                (:form     . *) ((1    :evaluation t)
                                                 (2    :evaluation t)))
                               :source #1#))
  '(#2=(catch (+ 1 2) :value) (:catch
                               ((:tag-form . 1) (((+ 1 2) :evaluation t))
                                (:form     . *) ((:value :evaluation t)))
                               :source #2#)))

(define-syntax-test (unwind-protect)
  '((unwind-protect)                    syn:invalid-syntax-error)

  '(#1=(unwind-protect foo)             (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t)))
                                         :source #1#))
  '(#2=(unwind-protect foo bar)         (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t))
                                          (:cleanup   . *) ((bar :evaluation t)))
                                         :source #2#))
  '(#3=(unwind-protect foo bar baz)     (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t))
                                          (:cleanup   . *) ((bar :evaluation t)
                                                            (baz :evaluation t)))
                                         :source #3#))
  '(#4=(unwind-protect (progn 1 2) 3 4) (:unwind-protect
                                         ((:protected . 1) (((progn 1 2) :evaluation t))
                                          (:cleanup   . *) ((3           :evaluation t)
                                                            (4           :evaluation t)))
                                         :source #4#)))

;;; `destructuring-bind'

(define-syntax-test (destructuring-bind)
  '((destructuring-bind) syn:invalid-syntax-error)
  '((destructuring-bind #1=1)
    syn:invalid-syntax-error #1# "must be a destructuring lambda list")
  '((destructuring-bind ())
    syn:invalid-syntax-error)

  '(#2=(destructuring-bind #3=(#4=a) #5=b)
    (:destructuring-bind
     ((:lambda-list . 1) (((:destructuring-lambda-list
                            ((:required . *) (((:required-parameter
                                                ((:name . 1) (((:variable-name
                                                                ()
                                                                :name #4# :source #4#)
                                                               :evaluation nil)))
                                                :source #4#)
                                               :evaluation :compound)))
                            :source #3#)
                           :evaluation :compound))
      (:expression . 1)  ((b :evaluation t)))
     :source #2#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-bind)
  '((multiple-value-bind) syn:invalid-syntax-error)
  '((multiple-value-bind #1=1)
    syn:invalid-syntax-error #1# "must be a list of variable names")
  '((multiple-value-bind ())
    syn:invalid-syntax-error nil "a value form must follow the list of variable names")

  '(#2=(multiple-value-bind () 1)
    (:multiple-value-bind
          ((:values-form . 1) ((1 :evaluation t)))
        :source #2#))
  '(#3=(multiple-value-bind (#4=a #5=b) 1 2 3)
    (:multiple-value-bind
     ((:name        . *) (((:variable-name () :name a :source #4#)
                           :evaluation :binding)
                          ((:variable-name () :name b :source #5#)
                           :evaluation :binding))
      (:values-form . 1) ((1 :evaluation t))
      (:form        . *) ((2 :evaluation t)
                          (3 :evaluation t)))
     :source #3#)))

(define-syntax-test (multiple-value-call)
  '((multiple-value-call) syn:invalid-syntax-error)

  '(#1=(multiple-value-call foo)
    (:multiple-value-call
     ((:function-form . 1) ((foo  :evaluation t)))
     :source #1#))
  '(#2=(multiple-value-call foo 1)
    (:multiple-value-call
     ((:function-form . 1) ((foo :evaluation t))
      (:argument      . *) ((1   :evaluation t)))
     :source #2#))
  '(#3=(multiple-value-call foo 1 2)
    (:multiple-value-call
     ((:function-form . 1) ((foo :evaluation t))
      (:argument      . *) ((1   :evaluation t)
                            (2   :evaluation t)))
     :source #3#)))

(define-syntax-test (multiple-value-prog1)
  '((multiple-value-prog1) syn:invalid-syntax-error)

  '(#1=(multiple-value-prog1 1)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t)))
     :source #1#))
  '(#2=(multiple-value-prog1 1 2)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t))
      (:form        . *) ((2 :evaluation t)))
     :source #2#))
  '(#3=(multiple-value-prog1 1 2 3)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t))
      (:form        . *) ((2 :evaluation t)
                          (3 :evaluation t)))
     :source #3#)))
