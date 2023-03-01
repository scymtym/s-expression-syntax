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
  '((block)                     syn:invalid-syntax-error)
  '((block #1=(foo))            syn:invalid-syntax-error #1# "block name must be a symbol")

  '(#2=(block #3=foo #4=1)      (:block
                                 ((:name . 1) ((#3# :evaluation :binding))
                                  (:form . *) ((#4# :evaluation t)))
                                 :source #2#))
  '(#5=(block #6=foo #7=a #8=b) (:block
                                 ((:name . 1) ((#6# :evaluation :binding))
                                  (:form . *) ((#7# :evaluation t)
                                               (#8# :evaluation t)))
                                 :source #5#)))

(define-syntax-test (return-from)
  '((return-from)                syn:invalid-syntax-error)
  '((return-from foo 1 2)        syn:invalid-syntax-error)
  '((return-from #1=(foo))       syn:invalid-syntax-error #1# "block name must be a symbol")

  '(#2=(return-from #3=foo)      (:return-from
                                  ((:name . 1) ((#3# :evaluation :reference)))
                                  :source #2#))
  '(#4=(return-from #5=foo #6=1) (:return-from
                                  ((:name   . 1) ((#5# :evaluation :reference))
                                   (:result . 1) ((#6# :evaluation t)))
                                  :source #4#))

  #+TODO (apply #'unparse-return-from-special-operator
                (parse-return-from-special-operator
                 (lambda (&rest args) (print args))
                 '(return-from foo bla))))

(define-syntax-test (return)
  '((return #1=(declare)) syn:invalid-syntax-error #1# "declare is not allowed here")
  '((return 1 2)          syn:invalid-syntax-error)

  '(#2=(return)           (:return
                           ()
                           :source #2#))
  '(#3=(return #4=1)      (:return
                           ((:result . 1) ((#4# :evaluation t)))
                           :source #3#)))

(define-syntax-test (tagbody)
  '((tagbody nil #1=nil)
    syn:invalid-syntax-error #1# "the tag NIL occurs more than once")
  '((tagbody 1 nil 1)
    syn:invalid-syntax-error 1 "the tag 1 occurs more than once")

  '(#2=(tagbody)
    (:tagbody () :source #2#))
  '(#3=(tagbody #4=nil)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name nil :source #4#))))
                        :source #4#)
                       :evaluation :compound)))
     :source #3#))
  '(#5=(tagbody #6=(progn))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) (((progn)
                                            :evaluation t)))
                        :source #6#)
                       :evaluation :compound)))
     :source #5#))
  '(#7=(tagbody #8=0 #9=1 #10="foo" #11="bar" #12=a #13=(list 1) #14=(list 3))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name 0 :source #8#))))
                        :source #8#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name 1 :source #9#)))
                         (:statement . *) ((#10# :evaluation t)
                                           (#11# :evaluation t)))
                        :source #9#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name a :source #12#)))
                         (:statement . *) ((#13# :evaluation t)
                                           (#14# :evaluation t)))
                        :source #12#)
                       :evaluation :compound)))
     :source #7#))
  '(#15=(tagbody #16=(1+ a) #17=(1+ b) #18=:foo)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) ((#16# :evaluation t)
                                           (#17# :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label . 1) (((:tag () :name :foo :source #18#))))
                        :source #18#)
                       :evaluation :compound)))
     :source #15#)))

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
  '((eval-when . #1=())
    syn:invalid-syntax-error #1#)
  '((eval-when #2=1)
    syn:invalid-syntax-error #2# "must be a list of situations")
  '((eval-when (#3=:foo))
    syn:invalid-syntax-error #3# "must be one of :COMPILE-TOPLEVEL, COMPILE, :LOAD-TOPLEVEL, LOAD, :EXECUTE, EVAL")

  '(#4=(eval-when ())
    (:eval-when () :source #4#))
  '(#5=(eval-when (:execute))
    (:eval-when ((:situation . *) ((:execute))) :source #5#))
  '(#6=(eval-when () a)
    (:eval-when ((:form . *) ((a :evaluation t))) :source #6#)))

(define-syntax-test (load-time-value)
  '((load-time-value)     syn:invalid-syntax-error)
  '((load-time-value 1 2) syn:invalid-syntax-error)

  '(#1=(load-time-value #2=foo)
    (:load-time-value ((:form . 1) ((#2# :evaluation t))) :source #1#))
  '(#3=(load-time-value #4=foo #5=t)
    (:load-time-value ((:form        . 1) ((#4# :evaluation t))
                       (:read-only-p . 1) ((#5#)))
     :source #3#)))

(define-syntax-test (quote)
  '((quote)     syn:invalid-syntax-error)
  '((quote x y) syn:invalid-syntax-error)

  '(#1=(quote #2=1)     (:quote ((:material . 1) ((#2#)))     :source #1#))
  '(#3=(quote #4=x)     (:quote ((:material . 1) ((#4#)))     :source #3#))
  '(#5=(quote #6=quote) (:quote ((:material . 1) ((#6#))) :source #5#)))

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
  '((symbol-macrolet)
    syn:invalid-syntax-error)
  '((symbol-macrolet #1=1)
    syn:invalid-syntax-error #1# "must be a list of symbol macro bindings")
  '((symbol-macrolet (#2=foo))
    syn:invalid-syntax-error #2# "must be a binding of the form (NAME FORM)")
  '((symbol-macrolet (#3=(foo)))
    syn:invalid-syntax-error #3# "must be a binding of the form (NAME FORM)")
  '((symbol-macrolet ((#4=:bla 1)))
    syn:invalid-syntax-error #4# "variable name must not be a keyword")

  '(#5=(symbol-macrolet ())
    (:symbol-macrolet () :source #5#))
  '(#6=(symbol-macrolet (#7=(#8=a 1) #9=(#10=b 2))
         (declare #11=(type #12=bit #13=d))
         c)
    (:symbol-macrolet
     ((:binding     . *) (((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name a :source #8#)
                                                :evaluation :binding))
                             (:expansion . 1) ((1 :evaluation t)))
                            :source #7#)
                           :evaluation :compound)
                          ((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name b :source #10#)
                                                :evaluation :binding))
                             (:expansion . 1) ((2 :evaluation t)))
                            :source #9#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #12#))))
                                                :source #12#))
                                              ((:variable-name () :name d :source #13#))))
                            :kind type :source #11#)))
      (:form        . *) ((c :evaluation t)))
     :source #6#)))

(define-syntax-test (let)
  '((let)
    syn:invalid-syntax-error)
  '((let #1=1)
    syn:invalid-syntax-error #1# "must be a list of bindings")
  '((let (#2=1))
    syn:invalid-syntax-error #2# "must be a binding of the form NAME, (NAME) or (NAME FORM)")
  '((let ((#3=1)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")

  '(#4=(let ())     (:let () :source #4#))
  '(#5=(let (#6=a))
    (:let
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #6#)
                                       :evaluation :binding)))
                        :source #6#)
                       :evaluation :binding)))
      :source #5#))
  '(#7=(let (#8=(#9=a 1) #10=b #11=(#12=c 2))
         (declare #13=(type #14=boolean #15=a)) a)
    (:let
     ((:binding . *)     (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #9#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #8#)
                           :evaluation :compound)
                           ((:value-binding
                             ((:name . 1) (((:variable-name () :name b :source #10#)
                                            :evaluation :binding)))
                             :source #10#)
                            :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #12#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #11#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #14#))))
                                                :source #14#))
                                           ((:variable-name () :name a :source #15#))))
                            :kind type :source #13#)))
      (:form        . *) ((a :evaluation t)))
     :source #7#))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  '((let*)
    syn:invalid-syntax-error)
  '((let* #1=1)
    syn:invalid-syntax-error #1# "must be a list of bindings")
  '((let* (#2=1))
    syn:invalid-syntax-error #2# "must be a binding of the form NAME, (NAME) or (NAME FORM)")
  '((let* ((#3=1)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")

  '(#4=(let* ())     (:let* () :source #4#))
  '(#5=(let* (#6=a))
    (:let*
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #6#)
                                       :evaluation :binding)))
                        :source #6#)
                       :evaluation :compound)))
      :source #5#))
  '(#7=(let* (#8=(#9=a 1) #10=b #11=(#12=c 2))
         (declare #13=(type #14=boolean #15=a))
         a)
    (:let*
     ((:binding     . *) (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #9#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #8#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name . 1) (((:variable-name () :name b :source #10#)
                                           :evaluation :binding)))
                            :source #10#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #12#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #11#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #14#))))
                                                :source #14#))
                                              ((:variable-name () :name a :source #15#))))
                            :kind type :source #13#)))
      (:form        . *) ((a :evaluation t)))
     :source #7#)))

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
  '((macrolet)
    syn:invalid-syntax-error)
  '((macrolet #1=1)
    syn:invalid-syntax-error #1# "must be a list of macro bindings")
  '((macrolet (#2=(f)))
    syn:invalid-syntax-error #2# "must be of the form (NAME LAMBDA-LIST DECLARATION* FORM*)")
  '((macrolet ((f #3=1)))
    syn:invalid-syntax-error #3# "must be a destructuring lambda list")
  '((macrolet ((f (x #4=x))))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#5=(macrolet ())
    (:macrolet () :source #5#))
  '(#6=(macrolet (#7=(#8=f #9=())))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #8#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ()
                                               :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(macrolet (#11=(#12=f #13=(&whole #14=w #15=(#16=a #17=b) &rest #18=c)
                        (declare #19=(type #20=string #21=a))
                        a)))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #12#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ((:whole    . 1) (((:variable-name
                                                                   ()
                                                                   :name w :source #14#)))
                                                (:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:pattern
                                                                                   ((:required . *) (((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name a :source #16#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #16#)
                                                                                                      :evaluation :compound)
                                                                                                     ((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name b :source #17#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #17#)
                                                                                                      :evaluation :compound)))
                                                                                   :source #15#)
                                                                                  :evaluation :compound)))
                                                                   :source #15#)
                                                                  :evaluation :compound))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name c :source #18#)
                                                                  :evaluation :compound)))
                                               :source #13#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name string :source #20#))))
                                                                   :source #20#))
                                                                 ((:variable-name () :name a :source #21#))))
                                               :kind type :source #19#)))
                         (:form        . *) ((a  :evaluation t)))
                        :source #11#)
                       :evaluation :compound)))
      :source #10#))
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
  '((flet)
    syn:invalid-syntax-error)
  '((flet #1=1)
    syn:invalid-syntax-error #1# "must be a list of function bindings")
  '((flet ((#2=1 ())))
    syn:invalid-syntax-error #2# "must be a function name")
  '((flet ((f #3=1)))
    syn:invalid-syntax-error #3# "must be an ordinary lambda list")
  '((flet ((f (x #4=x))))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
   ;; Valid syntax
  '(#5=(flet ())
    (:flet () :source #5#))
  '(#6=(flet (#7=(#8=f #9=())))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #8#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list () :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(flet (#11=(#12=f #13=(#14=a &rest #15=b))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #12#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #14#)
                                                                                  :evaluation nil)))
                                                                   :source #14#)))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name b :source #15#))))
                                               :source #13#)
                                              :evaluation :compound)))
                        :source #11#)
                       :evaluation :compound)))
     :source #10#))
  '(#16=(flet (#17=(#18=f #19=() (declare #20=(type #21=bit #22=a)))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #18#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #19#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #21#))))
                                                                   :source #21#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #22#))))
                                               :kind type :source #20#))))
                        :source #17#)
                       :evaluation :compound)))
     :source #16#))
  '(#23=(flet (#24=(#25=f #26=() a)))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #25#)
                                               :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #26#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
      :source #23#)))

(define-syntax-test (labels)
  '((labels)
    syn:invalid-syntax-error)
  '((labels #1=1)
    syn:invalid-syntax-error #1# "must be a list of function bindings")
  '((labels ((#2=1 ())))
    syn:invalid-syntax-error #2# "must be a function name")
  '((labels ((f #3=1)))
    syn:invalid-syntax-error #3# "must be an ordinary lambda list")
  '((labels ((f (x #4=x))))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#5=(labels ())
    (:labels () :source #5#))
  '(#6=(labels (#7=(#8=f #9=())))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #8#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(labels (#11=(#12=f #13=(#14=a &rest #15=b))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #12#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #14#)
                                                                                  :evaluation nil)))
                                                                   :source #14#)))
                                                                (:rest     . 1) (((:variable-name
                                                                                   ()
                                                                                   :name b :source #15#))))
                                               :source #13#)
                                              :evaluation :compound)))
                        :source #11#)
                       :evaluation :compound)))
     :source #10#))
  '(#16=(labels (#17=(#18=f #19=() (declare #20=(type #21=bit #22=a)))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #18#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #19#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #21#))))
                                                                   :source #21#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #22#))))
                                               :kind type :source #20#))))
                        :source #17#)
                       :evaluation :compound)))
     :source #16#))
  '(#23=(labels (#24=(#25=f #26=() a)))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #25#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #26#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
     :source #23#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim #1=1)
    syn:invalid-syntax-error #1# "must be a declaration")
  ;; Valid syntax
  '(#2=(declaim)
    (:declaim () :source #2#))
  '(#3=(declaim #4=(type #5=bit #6=a))
    (:declaim
     ((:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #5#))))
                                                :source #5#))
                                              ((:variable-name () :name a :source #6#))))
                            :kind type :source #4#))))
     :source #3#)))

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
  '((setq a . #1=())
    syn:invalid-syntax-error #1#)
  '((setq a 1 b . #2= ())
    syn:invalid-syntax-error #2# "must be a variable name followed by a form")
  '((setq #3=1 1)
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((setq #4=(1+ a) 1)
    syn:invalid-syntax-error #4# "variable name must be a symbol")

  '(#5=(setq)                (:setq () :source #5#))
  '(#6=(setq #7=a 1)         (:setq
                              ((:name       . *) (((:variable-name () :name a :source #7#)))
                               (:value-form . *) ((1 :evaluation t)))
                              :source #6#))
  '(#8=(setq #9=a 1 #10=b 2) (:setq
                              ((:name       . *) (((:variable-name () :name a :source #9#))
                                                  ((:variable-name () :name b :source #10#)))
                               (:value-form . *) ((1 :evaluation t)
                                                  (2 :evaluation t)))
                              :source #8#)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  '((throw)
    syn:invalid-syntax-error)
  '((throw 'foo)
    syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)
    syn:invalid-syntax-error)

  '(#1=(throw #2='foo #3=1)
    (:throw
     ((:tag-form    . 1) ((#2# :evaluation t))
      (:result-form . 1) ((#3# :evaluation t)))
     :source #1#))
  '(#4=(throw #5=(+ 1 2) #6=:value)
    (:throw
     ((:tag-form    . 1) ((#5# :evaluation t))
      (:result-form . 1) ((#6# :evaluation t)))
     :source #4#)))

(define-syntax-test (catch)
  '((catch)
    syn:invalid-syntax-error)

  '(#1=(catch #2='foo #3=1 #4=2)
    (:catch
     ((:tag-form . 1) ((#2# :evaluation t))
      (:form     . *) ((#3# :evaluation t)
                       (#4# :evaluation t)))
      :source #1#))
  '(#5=(catch #6=(+ 1 2) #7=:value)
    (:catch
     ((:tag-form . 1) ((#6# :evaluation t))
      (:form     . *) ((#7# :evaluation t)))
     :source #5#)))

(define-syntax-test (unwind-protect)
  '((unwind-protect)
    syn:invalid-syntax-error)

  '(#1=(unwind-protect #2=foo)
    (:unwind-protect
     ((:protected . 1) ((#2# :evaluation t)))
     :source #1#))
  '(#3=(unwind-protect #4=foo #5=bar)
    (:unwind-protect
         ((:protected . 1) ((#4# :evaluation t))
          (:cleanup   . *) ((#5# :evaluation t)))
      :source #3#))
  '(#6=(unwind-protect #7=foo #8=bar #9=baz)
    (:unwind-protect
     ((:protected . 1) ((#7# :evaluation t))
      (:cleanup   . *) ((#8# :evaluation t)
                        (#9# :evaluation t)))
     :source #6#))
  '(#10=(unwind-protect #11=(progn 1 2) #12=3 #13=4)
    (:unwind-protect
     ((:protected . 1) ((#11# :evaluation t))
      (:cleanup   . *) ((#12# :evaluation t)
                        (#13#:evaluation t)))
     :source #10#)))

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
      (:expression . 1)  ((#5# :evaluation t)))
     :source #2#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-bind)
  '((multiple-value-bind . #1= ())
    syn:invalid-syntax-error #1# "must be a list of variable names")
  '((multiple-value-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of variable names")
  '((multiple-value-bind ())
    syn:invalid-syntax-error nil "a value form must follow the list of variable names")

  '(#3=(multiple-value-bind () #4=1)
    (:multiple-value-bind
     ((:values-form . 1) ((#4# :evaluation t)))
     :source #3#))
  '(#5=(multiple-value-bind (#6=a #7=b) #8=1 #9=2 #10=3)
    (:multiple-value-bind
     ((:name        . *) (((:variable-name () :name a :source #6#)
                           :evaluation :binding)
                          ((:variable-name () :name b :source #7#)
                           :evaluation :binding))
      (:values-form . 1) ((#8#  :evaluation t))
      (:form        . *) ((#9#  :evaluation t)
                          (#10# :evaluation t)))
     :source #5#)))

(define-syntax-test (multiple-value-call)
  '((multiple-value-call) syn:invalid-syntax-error)

  '(#1=(multiple-value-call #2=foo)
    (:multiple-value-call
     ((:function-form . 1) ((#2#  :evaluation t)))
     :source #1#))
  '(#3=(multiple-value-call #4=foo #5=1)
    (:multiple-value-call
     ((:function-form . 1) ((#4# :evaluation t))
      (:argument      . *) ((#5# :evaluation t)))
     :source #3#))
  '(#6=(multiple-value-call #7=foo #8=1 #9=2)
    (:multiple-value-call
     ((:function-form . 1) ((#7# :evaluation t))
      (:argument      . *) ((#8# :evaluation t)
                            (#9# :evaluation t)))
     :source #6#)))

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
