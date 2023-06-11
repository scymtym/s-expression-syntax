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
  ;; Invalid syntax
  '((progn . 1)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(progn)
    (:progn () :source #1#))
  '(#2=(progn #3=1)
    (:progn ((:form . *) (((:unparsed
                            ()
                            :expression #3# :context :form :source #3#)
                           :evaluation t)))
            :source #2#))
  '(#4=(progn #5=1 #6=2)
    (:progn ((:form . *) (((:unparsed
                            ()
                            :expression #5# :context :form :source #5#)
                           :evaluation t)
                          ((:unparsed
                            ()
                            :expression #6# :context :form :source #6#)
                           :evaluation t)))
            :source #4#)))

(define-syntax-test (if)
  ;; Invalid syntax
  '((if)
    syn:invalid-syntax-error)
  '((if foo)
    syn:invalid-syntax-error)
  '((if foo bar baz fez)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '((if #1=foo #2=bar)
    (:if ((:test . 1) (((:unparsed
                         ()
                         :expression #1# :context :form :source #1#)
                        :evaluation t))
          (:then . 1) (((:unparsed
                         ()
                         :expression #2# :context :form :source #2#)
                        :evaluation t)))
         :source (if foo bar)))
  '((if #3=foo #4=bar #5=baz)
    (:if ((:test . 1) (((:unparsed
                         ()
                         :expression #3# :context :form :source #3#)
                        :evaluation t))
          (:then . 1) (((:unparsed
                         ()
                         :expression #4# :context :form :source #4#)
                        :evaluation t))
          (:else . 1) (((:unparsed
                         ()
                         :expression #5# :context :form :source #5#)
                        :evaluation t)))
         :source (if foo bar baz))))

;;; Special operators `block', `return-from', `return'

(define-syntax-test (block)
  ;; Invalid syntax
  '((block)
    syn:invalid-syntax-error)
  '((block #1=(foo))
    syn:invalid-syntax-error #1# "block name must be a symbol")
  ;; Valid syntax
  '(#2=(block #3=foo #4=1)
    (:block
     ((:name . 1) (((:block-name () :name foo :source #3#)
                    :evaluation (:binding :namespace block
                                          :scope     :lexical)))
      (:form . *) (((:unparsed () :expression 1 :context :form :source #4#)
                    :evaluation t)))
      :source #2#))
  '(#5=(block #6=foo #7=a #8=b)
    (:block
     ((:name . 1) (((:block-name () :name foo :source #6#)
                    :evaluation (:binding :namespace block
                                          :scope     :lexical)))
      (:form . *) (((:unparsed
                     ()
                     :expression a :context :form :source #7#)
                    :evaluation t)
                   ((:unparsed
                     ()
                     :expression b :context :form :source #8#)
                    :evaluation t)))
      :source #5#)))

(define-syntax-test (return-from)
  ;; Invalid syntax
  '((return-from)
    syn:invalid-syntax-error)
  '((return-from foo 1 2)
    syn:invalid-syntax-error)
  '((return-from #1=(foo))
    syn:invalid-syntax-error #1# "block name must be a symbol")
  ;; Valid syntax
  '(#2=(return-from #3=foo)
    (:return-from
     ((:name . 1) (((:block-name () :name foo :source #3#)
                    :evaluation (:reference :namespace block))))
      :source #2#))
  '(#4=(return-from #5=foo #6=1)
    (:return-from
     ((:name   . 1) (((:block-name () :name foo :source #5#)
                      :evaluation (:reference :namespace block)))
      (:result . 1) (((:unparsed () :expression 1 :context :form :source #6#)
                      :evaluation t)))
      :source #4#)))

(define-syntax-test (return)
  ;; Invalid syntax
  '((return #1=(declare))
    syn:invalid-syntax-error #1# "declare is not allowed here")
  '((return 1 2)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#2=(return)
    (:return () :source #2#))
  '(#3=(return #4=1)
    (:return
      ((:result . 1) (((:unparsed () :expression 1 :context :form :source #4#)
                       :evaluation t)))
      :source #3#)))

;;; Special operators `tagbody' and `go'

(define-syntax-test (tagbody)
  ;; Invalid syntax
  '((tagbody nil #1=nil)
    syn:invalid-syntax-error #1# "the tag NIL occurs more than once")
  '((tagbody 1 nil 1)
    syn:invalid-syntax-error 1 "the tag 1 occurs more than once")
  ;; Valid syntax
  '(#2=(tagbody)
    (:tagbody () :source #2#))
  '(#3=(tagbody #4=nil)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name nil :source #4#)
                                        :evaluation (:binding :namespace syn::tag
                                                              :scope     :lexical))))
                        :source #4#)
                       :evaluation :compound)))
     :source #3#))
  '(#5=(tagbody #6=(progn))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) (((:unparsed
                                             ()
                                             :expression (progn)
                                             :context    :form
                                             :source     #6#)
                                            :evaluation t)))
                        :source #6#)
                       :evaluation :compound)))
     :source #5#))
  '(#7=(tagbody #8=0 #9=1 #10="foo" #11="bar" #12=a #13=(list 1) #14=(list 3))
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:label . 1) (((:tag () :name 0 :source #8#)
                                        :evaluation (:binding :namespace syn::tag
                                                              :scope     :lexical))))
                        :source #8#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name 1 :source #9#)
                                            :evaluation (:binding :namespace syn::tag
                                                                  :scope     :lexical)))
                         (:statement . *) (((:unparsed
                                             ()
                                             :expression "foo"
                                             :context    :form
                                             :source     #10#)
                                            :evaluation t)
                                           ((:unparsed
                                             ()
                                             :expression "bar"
                                             :context    :form
                                             :source     #11#)
                                            :evaluation t)))
                        :source #9#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label     . 1) (((:tag () :name a :source #12#)
                                            :evaluation (:binding :namespace syn::tag
                                                                  :scope     :lexical)))
                         (:statement . *) (((:unparsed
                                             ()
                                             :expression (list 1)
                                             :context    :form
                                             :source     #13#)
                                            :evaluation t)
                                           ((:unparsed
                                             ()
                                             :expression (list 3)
                                             :context    :form
                                             :source     #14#)
                                            :evaluation t)))
                        :source #12#)
                       :evaluation :compound)))
     :source #7#))
  '(#15=(tagbody #16=(1+ a) #17=(1+ b) #18=:foo)
    (:tagbody
     ((:segment . *) (((:tagbody-segment
                        ((:statement . *) (((:unparsed
                                             ()
                                             :expression (1+ a)
                                             :context    :form
                                             :source     #16#)
                                            :evaluation t)
                                           ((:unparsed
                                             ()
                                             :expression (1+ b)
                                             :context    :form
                                             :source     #17#)
                                            :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:tagbody-segment
                        ((:label . 1) (((:tag () :name :foo :source #18#)
                                        :evaluation (:binding :namespace syn::tag
                                                              :scope     :lexical))))
                        :source #18#)
                       :evaluation :compound)))
     :source #15#)))

(define-syntax-test (go)
  ;; Invalid syntax
  '((go)
    syn:invalid-syntax-error nil "must be a single tag")
  '((go . #1=(1 2))
    syn:invalid-syntax-error #1# "must be a single tag")
  '((go #2=(foo))
    syn:invalid-syntax-error #2# "tag must be a symbol or an integer")
;; Valid syntax
  '(#3=(go #4=1)
    (:go
     ((:tag . 1) (((:tag () :name 1 :source #4#)
                   :evaluation (:reference :namespace syn::tag))))
     :source #3#)))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(define-syntax-test (eval-when)
  ;; Invalid syntax
  '((eval-when . #1=())
    syn:invalid-syntax-error #1#)
  '((eval-when #2=1)
    syn:invalid-syntax-error #2# "must be a list of situations")
  '((eval-when (#3=:foo))
    syn:invalid-syntax-error #3# "must be one of :COMPILE-TOPLEVEL, COMPILE, :LOAD-TOPLEVEL, LOAD, :EXECUTE, EVAL")
;; Valid syntax
  '(#4=(eval-when ())
    (:eval-when () :source #4#))
  '(#5=(eval-when (#6=:execute))
    (:eval-when
     ((:situation . *) (((:eval-when-situation () :situation :execute :source #6#))))
     :source #5#))
  '(#7=(eval-when () #8=a)
    (:eval-when
     ((:form . *) (((:unparsed () :expression a :context :form :source #8#)
                    :evaluation t)))
     :source #7#)))

(define-syntax-test (load-time-value)
  ;; Invalid syntax
  '((load-time-value)
    syn:invalid-syntax-error)
  '((load-time-value 1 #1=2)
    syn:invalid-syntax-error #1# "READ-ONLY-P must be either T or NIL, not a generalized boolean")
  ;; Valid syntax
  '(#2=(load-time-value #3=foo)
    (:load-time-value
     ((:form . 1) (((:unparsed () :expression foo :context :form :source #3#)
                    :evaluation t)))
     :source #2#))
  '(#4=(load-time-value #5=foo #6=t)
    (:load-time-value
     ((:form        . 1) (((:unparsed
                            ()
                            :expression foo :context :form :source #5#)
                           :evaluation t))
      (:read-only-p . 1) (((:unparsed
                            ()
                            :expression t :source #6#))))
     :source #4#)))

(define-syntax-test (quote)
  ;; Invalid syntax
  '((quote)
    syn:invalid-syntax-error)
  '((quote x y)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(quote #2=1)
    (:quote
     ((:material . 1) (((:unparsed
                         ()
                         :expression 1
                         :context    :quote
                         :source     #2#))))
     :source #1#))
  '(#3=(quote #4=x)
    (:quote
     ((:material . 1) (((:unparsed
                         ()
                         :expression x
                         :context    :quote
                         :source     #4#))))
     :source #3#))
  '(#5=(quote #6=quote)
    (:quote
     ((:material . 1) (((:unparsed
                         ()
                         :expression quote
                         :context    :quote
                         :source     #6#))))
     :source #5#)))

(define-syntax-test (function)
  ;; Invalid syntax
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
  ;; valid syntax
  '(#3=(function #4=foo)
    (:function
     ((:name . 1) (((:function-name () :name foo :source #4#)
                    :evaluation (:reference :namespace function))))
     :source #3#))
  '(#5=(function #6=(setf foo))
    (:function
     ((:name . 1) (((:function-name () :name (setf foo) :source #6#)
                    :evaluation (:reference :namespace function))))
     :source #5#))
  '(#7=(function #8=(lambda #9=()))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list () :source #9#)
                                             :evaluation :compound)))
                       :source #8#)
                      :evaluation :compound)))
     :source #7#))
  '(#10=(function #11=(lambda #12=(#13=a #101=&rest #14=b) #15=(foo)))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required-section . 1) (((:required-section
                                                                          ((:parameter . *) (((:required-parameter
                                                                                               ((:name . 1) (((:variable-name
                                                                                                               ()
                                                                                                               :name a :source #13#)
                                                                                                              :evaluation nil)))
                                                                                               :source #13#)))))))
                                               (:rest-section     . 1) (((:rest-section
                                                                          ((:keyword   . 1) (((:lambda-list-keyword
                                                                                               ()
                                                                                               :keyword &rest :source #101#)))
                                                                           (:parameter . 1) (((:variable-name
                                                                                               ()
                                                                                               :name b :source #14#))))))))
                                              :source #12#)
                                             :evaluation :compound))
                        (:form        . *) (((:unparsed
                                              ()
                                              :expression (foo) :context :form :source #15#)
                                             :evaluation t)))
                       :source #11#)
                      :evaluation :compound)))
     :source #10#)))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'

(define-syntax-test (symbol-macrolet)
  ;; Invalid syntax
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
  ;; Valid syntax
  '(#5=(symbol-macrolet ())
    (:symbol-macrolet () :source #5#))
  '(#6=(symbol-macrolet (#7=(#8=a #9=1) #10=(#11=b #12=2))
         (declare #13=(type #14=bit #15=d))
         #16=c)
    (:symbol-macrolet
     ((:binding     . *) (((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name a :source #8#)
                                                :evaluation (:binding :namespace variable
                                                                      :scope    :lexical)))
                             (:expansion . 1) (((:unparsed
                                                 ()
                                                 :expression 1 :context :form :source #9#)
                                                :evaluation t)))
                            :source #7#)
                           :evaluation :compound)
                          ((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name b :source #11#)
                                                :evaluation (:binding :namespace variable
                                                                      :scope    :lexical)))
                             (:expansion . 1) (((:unparsed
                                                 ()
                                                 :expression 2 :context :form :source #12#)
                                                :evaluation t)))
                            :source #10#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #14#))))
                                                :source #14#))
                                              ((:variable-name () :name d :source #15#))))
                            :kind type :source #13#)))
      (:form        . *) (((:unparsed
                            ()
                            :expression c :context :form :source #16#)
                           :evaluation t)))
     :source #6#)))

(define-syntax-test (let)
  ;; Invalid syntax
  '((let)
    syn:invalid-syntax-error)
  '((let #1=1)
    syn:invalid-syntax-error #1# "must be a list of bindings")
  '((let (#2=1))
    syn:invalid-syntax-error #2# "must be a binding of the form NAME, (NAME) or (NAME FORM)")
  '((let ((#3=1)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  ;; Valid syntax
  '(#4=(let ())
    (:let () :source #4#))
  '(#5=(let (#6=a))
    (:let
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #6#)
                                       :evaluation (:binding :namespace variable
                                                             :scope    :lexical))))
                        :source #6#)
                       :evaluation :compound)))
      :source #5#))
  '(#7=(let (#8=(#9=a #10=1) #11=b #12=(#13=c #14=2))
         (declare #15=(type #16=boolean #17=a)) #18=a)
    (:let
     ((:binding . *)     (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #9#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope    :lexical)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression 1 :context :form :source #10#)
                                            :evaluation t)))
                            :source #8#)
                           :evaluation :compound)
                           ((:value-binding
                             ((:name . 1) (((:variable-name () :name b :source #11#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope    :lexical))))
                             :source #11#)
                            :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #13#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope    :lexical)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression 2 :context :form :source #14#)
                                            :evaluation t)))
                            :source #12#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #16#))))
                                                :source #16#))
                                           ((:variable-name () :name a :source #17#))))
                            :kind type :source #15#)))
      (:form        . *) (((:unparsed
                            ()
                            :expression a :context :form :source #18#)
                           :evaluation t)))
     :source #7#))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  ;; Invalid syntax
  '((let*)
    syn:invalid-syntax-error)
  '((let* #1=1)
    syn:invalid-syntax-error #1# "must be a list of bindings")
  '((let* (#2=1))
    syn:invalid-syntax-error #2# "must be a binding of the form NAME, (NAME) or (NAME FORM)")
  '((let* ((#3=1)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  ;; Valid syntax
  '(#4=(let* ())
    (:let* () :source #4#))
  '(#5=(let* (#6=a))
    (:let*
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #6#)
                                       :evaluation (:binding :namespace variable
                                                             :scope    :lexical))))
                        :source #6#)
                       :evaluation :compound)))
      :source #5#))
  '(#7=(let* (#8=(#9=a #10=1) #11=b #12=(#13=c #14=2))
         (declare #15=(type #16=boolean #17=a))
         #18=a)
    (:let*
     ((:binding     . *) (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #9#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope    :lexical)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression 1 :context :form :source #10#)
                                            :evaluation t)))
                            :source #8#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name . 1) (((:variable-name () :name b :source #11#)
                                           :evaluation (:binding :namespace variable
                                                                 :scope    :lexical))))
                            :source #11#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #13#)
                                            :evaluation (:binding :namespace variable
                                                                  :scope    :lexical)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression 2 :context :form :source #14#)
                                            :evaluation t)))
                            :source #12#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #16#))))
                                                :source #16#))
                                              ((:variable-name () :name a :source #17#))))
                            :kind type :source #15#)))
      (:form        . *) (((:unparsed
                            ()
                            :expression a :context :form :source #18#)
                           :evaluation t)))
     :source #7#)))

(define-syntax-test (locally)
  ;; Valid syntax
  '(#1=(locally)
    (:locally () :source #1#))
  '(#2=(locally (declare #3=(type #4=bit #5=a)))
    (:locally
     ((:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name () :name bit :source #4#))))
                                                :source #4#))
                                              ((:variable-name () :name a :source #5#))))
                            :kind type :source #3#))))
      :source #2#))
  '(#6=(locally #7=a)
    (:locally
     ((:form . *) (((:unparsed () :expression a :context :form :source #7#)
                    :evaluation t)))
     :source #6#))
  '(#8=(locally (declare #9=(type #10=bit #11=a)) #12=b)
    (:locally
     ((:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name () :name bit :source #10#))))
                                                :source #10#))
                                              ((:variable-name () :name a :source #11#))))
                            :kind type :source #9#)))
      (:form        . *) (((:unparsed () :expression b :context :form :source #12#)
                           :evaluation t)))
     :source #8#))

  #+TODO (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x)))))

(define-syntax-test (progv)
  ;; Invalid syntax
  '((progv)
    syn:invalid-syntax-error)
  '((progv 1)
    syn:invalid-syntax-error)
  '((progv ())
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(progv #2=() #3=())
    (:progv
     ((:symbols . 1) (((:unparsed
                        ()
                        :expression () :context :form :source #2#)
                       :evaluation t))
      (:values . 1)  (((:unparsed
                        ()
                        :expression () :context :form :source #3#)
                       :evaluation t)))
     :source #1#))
  '(#4=(progv #5=(a) #6=(b))
    (:progv
     ((:symbols . 1) (((:unparsed
                        ()
                        :expression (a) :context :form :source #5#)
                       :evaluation t))
      (:values  . 1) (((:unparsed
                        ()
                        :expression (b) :context :form :source #6#)
                       :evaluation t)))
     :source #4#))
  '(#7=(progv #8='(a) #9='(b))
    (:progv
     ((:symbols . 1) (((:unparsed
                        ()
                        :expression '(a) :context :form :source #8#)
                       :evaluation t))
      (:values  . 1) (((:unparsed
                        ()
                        :expression '(b) :context :form :source #9#)
                       :evaluation t)))
     :source #7#))
  '(#10=(progv #11=() #12=() #13=1)
    (:progv
     ((:symbols . 1) (((:unparsed
                        ()
                        :expression () :context :form :source #11#)
                       :evaluation t))
      (:values . 1)  (((:unparsed
                        ()
                        :expression () :context :form :source #12#)
                       :evaluation t))
      (:form . *)    (((:unparsed
                        ()
                        :expression 1 :context :form :source #13#)
                       :evaluation t)))
     :source #10#))
  '(#14=(progv #15=() #16=() #17=1 #18=2)
    (:progv
     ((:symbols . 1) (((:unparsed
                        ()
                        :expression () :context :form :source #15#)
                       :evaluation t))
      (:values . 1)  (((:unparsed
                        ()
                        :expression () :context :form :source #16#)
                       :evaluation t))
      (:form . *)    (((:unparsed
                        ()
                        :expression 1 :context :form :source #17#)
                       :evaluation t)
                      ((:unparsed
                        ()
                        :expression 2 :context :form :source #18#)
                       :evaluation t)))
     :source #14#)))

;;; Special operators `macrolet', `flet' and `labels'

(define-syntax-test (macrolet)
  ;; Invalid syntax
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
                                              :evaluation (:binding :namespace function
                                                                    :scope    :lexical)))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ()
                                               :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(macrolet (#11=(#12=f #13=(#101=&whole #14=w #15=(#16=a #17=b) #102=&rest #18=c)
                        (declare #19=(type #20=string #21=a))
                        #22=a)))
    (:macrolet
     ((:binding . *)
      (((:local-macro-function-binding
         ((:name        . 1) (((:function-name () :name f :source #12#)
                               :evaluation (:binding :namespace function
                                                     :scope    :lexical)))
          (:lambda-list . 1) (((:destructuring-lambda-list
                                ((:whole-section . 1)
                                 (((:whole-section
                                    ((:keyword   . 1) (((:lambda-list-keyword
                                                         ()
                                                         :keyword &whole :source #101#)))
                                                      (:parameter . 1) (((:variable-name
                                                                          ()
                                                                          :name w :source #14#)))))))
                                 (:required-section . 1)
                                 (((:required-section
                                    ((:parameter . *) (((:required-parameter
                                                         ((:name . 1) (((:pattern
                                                                         ((:required-section . 1)
                                                                          (((:required-section
                                                                             ((:parameter . *) (((:required-parameter
                                                                                                  ((:name . 1) (((:variable-name
                                                                                                                  ()
                                                                                                                  :name a :source #16#)
                                                                                                                 :evaluation nil)))
                                                                                                  :source #16#))
                                                                                                ((:required-parameter
                                                                                                  ((:name . 1) (((:variable-name
                                                                                                                  ()
                                                                                                                  :name b :source #17#)
                                                                                                                 :evaluation nil)))
                                                                                                  :source #17#)))))
                                                                            :evaluation :compound)))
                                                                         :source #15#)
                                                                        :evaluation :compound)))
                                                         :source #15#)
                                                        ; TODO :evaluation :compound
                                                        ))))
                                   :evaluation :compound))
                                 (:rest-section . 1)
                                 (((:rest-section
                                    ((:keyword   . 1) (((:lambda-list-keyword
                                                         ()
                                                         :keyword &rest :source #102#)))
                                     (:parameter . 1) (((:variable-name
                                                         ()
                                                         :name c :source #18#)))))
                                   :evaluation :compound)))
                                :source #13#)
                               :evaluation :compound))
          (:declaration . *) (((:declaration-specifier
                                ((:argument . *) (((:atomic-type-specifier
                                                    ((:name . 1) (((:type-name
                                                                    ()
                                                                    :name string :source #20#))))
                                                    :source #20#))
                                                  ((:variable-name () :name a :source #21#))))
                                :kind type :source #19#)))
          (:form        . *) (((:unparsed
                                ()
                                :expression a :context :form :source #22#)
                               :evaluation t)))
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
  ;; Invalid syntax
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
                                              :evaluation (:binding :namespace function
                                                                    :scope    :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list () :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(flet (#11=(#12=f #13=(#14=a #101=&rest #15=b))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #12#)
                                              :evaluation (:binding :namespace function
                                                                    :scope    :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required-section . 1) (((:required-section
                                                                           ((:parameter . *) (((:required-parameter
                                                                                                ((:name . 1) (((:variable-name
                                                                                                                ()
                                                                                                                :name a :source #14#)
                                                                                                               :evaluation nil)))
                                                                                                :source #14#)))))))
                                                (:rest-section     . 1) (((:rest-section
                                                                           ((:keyword   . 1) (((:lambda-list-keyword
                                                                                                ()
                                                                                                :keyword &rest :source #101#)))
                                                                            (:parameter . 1) (((:variable-name
                                                                                                ()
                                                                                                :name b :source #15#))))))))
                                               :source #13#)
                                              :evaluation :compound)))
                        :source #11#)
                       :evaluation :compound)))
     :source #10#))
  '(#16=(flet (#17=(#18=f #19=() (declare #20=(type #21=bit #22=a)))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #18#)
                                              :evaluation (:binding :namespace function
                                                                    :scope    :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #19#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration-specifier
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
  '(#23=(flet (#24=(#25=f #26=() #27=a)))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #25#)
                                              :evaluation (:binding :namespace function
                                                                    :scope    :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #26#)
                                              :evaluation :compound))
                         (:form        . *) (((:unparsed
                                               ()
                                               :expression a :context :form :source #27#)
                                              :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
      :source #23#)))

(define-syntax-test (labels)
  ;; Invalid syntax
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
                                              :evaluation (:binding :namespace function
                                                                    :scope     :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #9#)
                                              :evaluation :compound)))
                        :source #7#)
                       :evaluation :compound)))
     :source #6#))
  '(#10=(labels (#11=(#12=f #13=(#14=a #101=&rest #15=b))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #12#)
                                              :evaluation (:binding :namespace function
                                                                    :scope     :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required-section . 1) (((:required-section
                                                                           ((:parameter . *) (((:required-parameter
                                                                                                ((:name . 1) (((:variable-name
                                                                                                                ()
                                                                                                                :name a :source #14#)
                                                                                                               :evaluation nil)))
                                                                                                :source #14#)))))))
                                                (:rest-section     . 1) (((:rest-section
                                                                           ((:keyword   . 1) (((:lambda-list-keyword
                                                                                                ()
                                                                                                :keyword &rest :source #101#)))
                                                                            (:parameter . 1) (((:variable-name
                                                                                                ()
                                                                                                :name b :source #15#))))))))
                                               :source #13#)
                                              :evaluation :compound)))
                        :source #11#)
                       :evaluation :compound)))
     :source #10#))
  '(#16=(labels (#17=(#18=f #19=() (declare #20=(type #21=bit #22=a)))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #18#)
                                              :evaluation (:binding :namespace function
                                                                    :scope     :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #19#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration-specifier
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
  '(#23=(labels (#24=(#25=f #26=() #27=a)))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #25#)
                                              :evaluation (:binding :namespace function
                                                                    :scope     :lexical)))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #26#)
                                              :evaluation :compound))
                         (:form        . *) (((:unparsed
                                               ()
                                               :expression a :context :form :source #27#)
                                              :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
     :source #23#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  ;; Invalid syntax
  '((declaim #1=1)
    syn:invalid-syntax-error #1# "must be a declaration specifier")
  ;; Valid syntax
  '(#2=(declaim)
    (:declaim () :source #2#))
  '(#3=(declaim #4=(type #5=bit #6=a))
    (:declaim
     ((:declaration . *) (((:declaration-specifier
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #5#))))
                                                :source #5#))
                                              ((:variable-name () :name a :source #6#))))
                            :kind type :source #4#))))
     :source #3#)))

(define-syntax-test (the)
  ;; Invalid syntax
  '((the)
    syn:invalid-syntax-error)
  '((the bit)
    syn:invalid-syntax-error)
  '((the bit 1 extra)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(the #2=bit #3=1)
    (:the
     ((:type . 1) (((:atomic-type-specifier
                     ((:name . 1) (((:type-name () :name bit :source #2#))))
                     :source #2#)))
      (:form . 1) (((:unparsed
                     ()
                     :expression 1 :context :form :source #3#)
                    :evaluation t)))
     :source #1#)))

;;; Special operators `[p]setq'

(define-syntax-test (setq)
  ;; Invalid syntax
  '((setq a . #1=())
    syn:invalid-syntax-error #1# "must be a variable name followed by a form")
  '((setq a 1 b . #2=())
    syn:invalid-syntax-error #2# "must be a variable name followed by a form")
  '((setq #3=1 1)
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((setq #4=(1+ a) 1)
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  ;; Valid syntax
  '(#5=(setq)
    (:setq () :source #5#))
  '(#6=(setq #7=a #8=1)
    (:setq
     ((:name       . *) (((:variable-name () :name a :source #7#)
                          :evaluation (:assignment :namespace variable)))
      (:value-form . *) (((:unparsed
                           ()
                           :expression 1 :context :form :source #8#)
                          :evaluation t)))
     :source #6#))
  '(#9=(setq #10=a #11=1 #12=b #13=2)
    (:setq
     ((:name       . *) (((:variable-name () :name a :source #10#)
                          :evaluation (:assignment :namespace variable))
                         ((:variable-name () :name b :source #12#)
                          :evaluation (:assignment :namespace variable)))
      (:value-form . *) (((:unparsed
                           ()
                           :expression 1 :context :form :source #11#)
                          :evaluation t)
                         ((:unparsed
                           ()
                           :expression 2 :context :form :source #13#)
                          :evaluation t)))
     :source #9#)))

(define-syntax-test (psetq)
  ;; Invalid syntax
  '((psetq a . #1=()) ; TODO not yet reported accurately
    syn:invalid-syntax-error #1# "must be a variable name followed by a form")
  '((psetq a 1 b . #2=()) ; TODO not yet reported accurately
    syn:invalid-syntax-error #2# "must be a variable name followed by a form")
  '((psetq #3=1 1)
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((psetq #4=(1+ a) 1)
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  '((psetq a 1 #5=a 2)
    syn:invalid-syntax-error #5# "the variable name A occurs more than once")
  ;; Valid syntax
  '(#6=(psetq)
    (:psetq () :source #6#))
  '(#7=(psetq #8=a #9=1)
    (:psetq
     ((:name       . *) (((:variable-name () :name a :source #8#)
                          :evaluation (:assignment :namespace variable)))
      (:value-form . *) (((:unparsed
                           ()
                           :expression 1 :context :form :source #9#)
                          :evaluation t)))
     :source #7#))
  '(#10=(psetq #11=a #12=1 #13=b #14=2)
    (:psetq
     ((:name       . *) (((:variable-name () :name a :source #11#)
                          :evaluation (:assignment :namespace variable))
                         ((:variable-name () :name b :source #13#)
                          :evaluation (:assignment :namespace variable)))
      (:value-form . *) (((:unparsed
                           () :expression 1 :context :form :source #12#)
                          :evaluation t)
                         ((:unparsed
                           () :expression 2 :context :form :source #14#)
                          :evaluation t)))
     :source #10#)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  ;; Invalid syntax
  '((throw)
    syn:invalid-syntax-error)
  '((throw 'foo)
    syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(throw #2='foo #3=1)
    (:throw
     ((:tag-form    . 1) (((:unparsed
                            ()
                            :expression 'foo :context :form :source #2#)
                           :evaluation t))
      (:result-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #3#)
                           :evaluation t)))
     :source #1#))
  '(#4=(throw #5=(+ 1 2) #6=:value)
    (:throw
     ((:tag-form    . 1) (((:unparsed
                            ()
                            :expression (+ 1 2) :context :form :source #5#)
                           :evaluation t))
      (:result-form . 1) (((:unparsed
                            ()
                            :expression :value :context :form :source #6#)
                           :evaluation t)))
     :source #4#)))

(define-syntax-test (catch)
  ;; Invalid syntax
  '((catch)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(catch #2='foo #3=1 #4=2)
    (:catch
     ((:tag-form . 1) (((:unparsed
                         ()
                         :expression 'foo :context :form :source #2#)
                        :evaluation t))
      (:form     . *) (((:unparsed
                         ()
                         :expression 1 :context :form :source #3#)
                        :evaluation t)
                       ((:unparsed
                         ()
                         :expression 2 :context :form :source #4#)
                        :evaluation t)))
     :source #1#))
  '(#5=(catch #6=(+ 1 2) #7=:value)
    (:catch
     ((:tag-form . 1) (((:unparsed
                         ()
                         :expression (+ 1 2) :context :form :source #6#)
                        :evaluation t))
      (:form     . *) (((:unparsed
                         ()
                         :expression :value :context :form :source #7#)
                        :evaluation t)))
     :source #5#)))

(define-syntax-test (unwind-protect)
  ;; Invalid syntax
  '((unwind-protect)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(unwind-protect #2=foo)
    (:unwind-protect
     ((:protected . 1) (((:unparsed
                          ()
                          :expression foo :context :form :source #2#)
                         :evaluation t)))
     :source #1#))
  '(#3=(unwind-protect #4=foo #5=bar)
    (:unwind-protect
     ((:protected . 1) (((:unparsed
                          ()
                          :expression foo :context :form :source #4#)
                         :evaluation t))
      (:cleanup   . *) (((:unparsed
                          ()
                          :expression bar :context :form :source #5#)
                         :evaluation t)))
     :source #3#))
  '(#6=(unwind-protect #7=foo #8=bar #9=baz)
    (:unwind-protect
     ((:protected . 1) (((:unparsed
                          ()
                          :expression foo :context :form :source #7#)
                         :evaluation t))
      (:cleanup   . *) (((:unparsed
                          ()
                          :expression bar :context :form :source #8#)
                         :evaluation t)
                        ((:unparsed
                          ()
                          :expression baz :context :form :source #9#)
                         :evaluation t)))
     :source #6#))
  '(#10=(unwind-protect #11=(progn 1 2) #12=3 #13=4)
    (:unwind-protect
     ((:protected . 1) (((:unparsed
                          ()
                          :expression (progn 1 2) :context :form :source #11#)
                         :evaluation t))
      (:cleanup   . *) (((:unparsed
                          ()
                          :expression 3 :context :form :source #12#)
                         :evaluation t)
                        ((:unparsed
                          ()
                          :expression 4 :context :form :source #13#)
                         :evaluation t)))
     :source #10#)))

;;; `destructuring-bind'

(define-syntax-test (destructuring-bind)
  ;; Invalid syntax
  '((destructuring-bind) syn:invalid-syntax-error)
  '((destructuring-bind #1=1)
    syn:invalid-syntax-error #1# "must be a destructuring lambda list")
  '((destructuring-bind ())
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#2=(destructuring-bind #3=(#4=a) #5=b)
    (:destructuring-bind
     ((:lambda-list . 1) (((:destructuring-lambda-list
                            ((:required-section . 1) (((:required-section
                                                        ((:parameter . *) (((:required-parameter
                                                                             ((:name . 1) (((:variable-name
                                                                                             ()
                                                                                             :name #4# :source #4#)
                                                                                            :evaluation nil)))
                                                                             :source #4#)))))
                                                       :evaluation :compound)))
                            :source #3#)
                           :evaluation :compound))
      (:expression . 1)  (((:unparsed
                            ()
                            :expression b :context :form :source #5#)
                           :evaluation t)))
     :source #2#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-bind)
  ;; Invalid syntax
  '((multiple-value-bind . #1= ())
    syn:invalid-syntax-error #1# "must be a list of variable names")
  '((multiple-value-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of variable names")
  '((multiple-value-bind ())
    syn:invalid-syntax-error nil "a value form must follow the list of variable names")
  ;; Valid syntax
  '(#3=(multiple-value-bind () #4=1)
    (:multiple-value-bind
     ((:values-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #4#)
                           :evaluation t)))
     :source #3#))
  '(#5=(multiple-value-bind (#6=a #7=b) #8=1 #9=2 #10=3)
    (:multiple-value-bind
     ((:name        . *) (((:variable-name () :name a :source #6#)
                           :evaluation (:binding :namespace variable
                                                 :scope     :lexical))
                          ((:variable-name () :name b :source #7#)
                           :evaluation (:binding :namespace variable
                                                 :scope     :lexical)))
      (:values-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #8#)
                           :evaluation t))
      (:form        . *) (((:unparsed
                            ()
                            :expression 2 :context :form :source #9#)
                           :evaluation t)
                          ((:unparsed
                            ()
                            :expression 3 :context :form :source #10#)
                           :evaluation t)))
     :source #5#)))

(define-syntax-test (multiple-value-call)
  ;; Invalid syntax
  '((multiple-value-call)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(multiple-value-call #2=foo)
    (:multiple-value-call
     ((:function-form . 1) (((:unparsed
                              ()
                              :expression foo :context :form :source #2#)
                             :evaluation t)))
     :source #1#))
  '(#3=(multiple-value-call #4=foo #5=1)
    (:multiple-value-call
     ((:function-form . 1) (((:unparsed
                              ()
                              :expression foo :context :form :source #4#)
                             :evaluation t))
      (:argument      . *) (((:unparsed
                              ()
                              :expression 1 :context :form :source #5#)
                             :evaluation t)))
     :source #3#))
  '(#6=(multiple-value-call #7=foo #8=1 #9=2)
    (:multiple-value-call
     ((:function-form . 1) (((:unparsed
                              ()
                              :expression foo :context :form :source #7#)
                             :evaluation t))
      (:argument      . *) (((:unparsed
                              ()
                              :expression 1 :context :form :source #8#)
                             :evaluation t)
                            ((:unparsed
                              ()
                              :expression 2 :context :form :source #9#)
                             :evaluation t)))
     :source #6#)))

(define-syntax-test (multiple-value-prog1)
  ;; Invalid syntax
  '((multiple-value-prog1)
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#1=(multiple-value-prog1 #2=1)
    (:multiple-value-prog1
     ((:values-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #2#)
                           :evaluation t)))
     :source #1#))
  '(#3=(multiple-value-prog1 #4=1 #5=2)
    (:multiple-value-prog1
     ((:values-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #4#)
                           :evaluation t))
      (:form        . *) (((:unparsed
                            ()
                            :expression 2 :context :form :source #5#)
                           :evaluation t)))
     :source #3#))
  '(#6=(multiple-value-prog1 #7=1 #8=2 #9=3)
    (:multiple-value-prog1
     ((:values-form . 1) (((:unparsed
                            ()
                            :expression 1 :context :form :source #7#)
                           :evaluation t))
      (:form        . *) (((:unparsed
                            ()
                            :expression 2 :context :form :source #8#)
                           :evaluation t)
                          ((:unparsed
                            ()
                            :expression 3 :context :form :source #9#)
                           :evaluation t)))
     :source #6#)))
