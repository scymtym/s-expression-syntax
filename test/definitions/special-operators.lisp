;;;; special-operators.lisp --- Tests for special operator rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.special-operators
  :in :s-expression-syntax)

(defmacro define-syntax-test ((syntax-name) &body cases)
  `(test ,syntax-name
     ,(format nil "Test for the `~(~A~)' special operator syntax."
              syntax-name)
     (syntax-test-cases (,syntax-name) ,@cases)))

;;; Special operators for control

(define-syntax-test (progn)
  '((progn . 1)    syn:invalid-syntax-error)

  '(#2=(progn)     (:progn () :source #2#))
  '(#3=(progn 1)   (:progn ((:form . *) ((1 :evaluation t))) :source #3#))
  '(#4=(progn 1 2) (:progn ((:form . *) ((1 :evaluation t)
                                         (2 :evaluation t)))
                           :source #4#)))

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
                           ((:name  . 1) ((foo :evaluation :reference))
                            (:value . 1) ((1 :evaluation t)))
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
                        ((:value . 1) ((1 :evaluation t)))
                        :source (return 1))))

(define-syntax-test (tagbody)
  '((tagbody nil nil) syn:invalid-syntax-error)

  '(#2=(tagbody 0 1 "bla" "bli" 2 (list 1) (list 3))
    (:tagbody
     ((:tag     . *) ((0) (1) (2))
      (:segment . *) ((("bla" "bli") :evaluation t)
                      (((list 1) (list 3)) :evaluation t)))
     :source #2#))
  '(#3=(tagbody (1+ a) (1+ b) :foo)
    (:tagbody
     ((:tag     . *) ((:foo))
      (:segment . *) ((((1+ a) (1+ b)) :evaluation t)))
     :source #3#))

  #+TODO (check-roundtrip-cases 'tagbody
                         '(tagbody)
                         '(tagbody nil)
                         '(tagbody nil :foo)
                         '(tagbody nil :foo :bar)
                         '(tagbody nil (1+ a))
                         '(tagbody nil :foo (1+ a))
                         '(tagbody nil (1+ a) :foo)
                         '(tagbody (1+ a))
                         '(tagbody (1+ a) (1+ b))
                         '(tagbody (1+ a) (1+ b) (1+ c))
                         '(tagbody :foo (1+ a) (1+ b))
                         '(tagbody (1+ a) :foo (1+ b))
                         '(tagbody (1+ a) (1+ b) :foo)))

(define-syntax-test (go)
  '((go)       syn:invalid-syntax-error)
  '((go 1 2)   syn:invalid-syntax-error)
  '((go (foo)) syn:invalid-syntax-error)

  '(#4=(go 1)  (:go ((:tag . 1) ((1))) :source #4#)))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(define-syntax-test (eval-when)
  '((eval-when)
    syn:invalid-syntax-error)
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
  '((load-time-value)       syn:invalid-syntax-error)
  '((load-time-value 1 2)   syn:invalid-syntax-error)

  '(#3=(load-time-value foo)
    (:load-time-value ((:form . 1) ((foo :evaluation t))) :source #3#))
  '(#4=(load-time-value foo t)
    (:load-time-value ((:form        . 1) ((foo :evaluation t))
                       (:read-only-p . 1) ((t)))
     :source #4#)))

(define-syntax-test (quote)
  '((quote)          syn:invalid-syntax-error)
  '((quote x y)      syn:invalid-syntax-error)

  '(#3=(quote 1)     (:quote ((:material . 1) ((1)))     :source #3#))
  '(#4=(quote x)     (:quote ((:material . 1) ((x)))     :source #4#))
  '(#5=(quote quote) (:quote ((:material . 1) ((quote))) :source #5#)))

(define-syntax-test (function)
  '((function)
    syn:invalid-syntax-error nil "must be a function name or lambda expression")
  '((function 1)
    syn:invalid-syntax-error 1 "must be a function name or lambda expression")
  '((function . #3=(x y))
    syn:invalid-syntax-error #3# "nothing may follow function name or lambda expression")
  '((function (lambda 1))
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '((function (lambda (x #5=x)))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")

  '(#6=(function #7=foo)
    (:function
     ((:name . 1) (((:function-name () :name foo :source #7#))))
     :source #6#))
  '(#8=(function #9=(setf foo))
    (:function
     ((:name . 1) (((:function-name () :name (setf foo) :source #9#))))
     :source #8#))
  '(#10=(function #11=(lambda #12=()))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list () :source #12#)
                                             :evaluation :compound)))
                       :source #11#)
                      :evaluation :compound)))
     :source #10#))
  '(#13=(function #14=(lambda #15=(#16=a &rest #17=b) (foo)))
    (:function
     ((:lambda . 1) (((:lambda-expression
                       ((:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required . *) (((:required-parameter
                                                                  ((:name . 1) (((:variable-name
                                                                                  ()
                                                                                  :name a :source #16#)
                                                                                 :evaluation nil)))
                                                                  :source #16#)))
                                               (:rest     . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #17#))))
                                              :source #15#)
                                             :evaluation :compound))
                        (:form        . *) (((foo) :evaluation t)))
                       :source #14#)
                      :evaluation :compound)))
     :source #13#)))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'

(define-syntax-test (symbol-macrolet)
  '((symbol-macrolet)            syn:invalid-syntax-error)
  '((symbol-macrolet 1)          syn:invalid-syntax-error)
  '((symbol-macrolet (foo))      syn:invalid-syntax-error)
  '((symbol-macrolet ((foo)))    syn:invalid-syntax-error)
  '((symbol-macrolet ((:bla 1))) syn:invalid-syntax-error)

  '(#6=(symbol-macrolet ())
    (:symbol-macrolet () :source #6#))
  '(#7=(symbol-macrolet (#8=(#9=a 1) #10=(#11=b 2))
         (declare #12=(type #13=bit #14=d))
         c)
    (:symbol-macrolet
     ((:binding     . *) (((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name a :source #9#)
                                                :evaluation :binding))
                             (:expansion . 1) ((1 :evaluation t)))
                            :source #8#)
                           :evaluation :compound)
                          ((:symbol-macro-binding
                            ((:name      . 1) (((:variable-name () :name b :source #11#)
                                                :evaluation :binding))
                             (:expansion . 1) ((2 :evaluation t)))
                            :source #10#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name bit :source #13#))))
                                                :source #13#))
                                              ((:variable-name () :name d :source #14#))))
                            :kind type :source #12#)))
      (:form        . *) ((c :evaluation t)))
     :source #7#)))

(define-syntax-test (let)
  '((let)       syn:invalid-syntax-error)
  '((let 1)     syn:invalid-syntax-error)
  '((let (1))   syn:invalid-syntax-error)
  '((let ((1))) syn:invalid-syntax-error)

  '(#5=(let ())     (:let () :source #5#))
  '(#6=(let (#7=a))
    (:let
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #7#)
                                       :evaluation :binding)))
                        :source #7#)
                       :evaluation :binding)))
      :source #6#))
  '(#8=(let (#9=(#10=a 1) #11=b #12=(#13=c 2))
         (declare #14=(type #15=boolean #16=a)) a)
    (:let
     ((:binding . *)     (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #10#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #9#)
                           :evaluation :compound)
                           ((:value-binding
                             ((:name . 1) (((:variable-name () :name b :source #11#)
                                            :evaluation :binding)))
                             :source #11#)
                            :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #13#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #12#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #15#))))
                                                :source #15#))
                                           ((:variable-name () :name a :source #16#))))
                            :kind type :source #14#)))
      (:form        . *) ((a :evaluation t)))
     :source #8#))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  '((let*)       syn:invalid-syntax-error)
  '((let* 1)     syn:invalid-syntax-error)
  '((let* (1))   syn:invalid-syntax-error)
  '((let* ((1))) syn:invalid-syntax-error)

  '(#5=(let* ())     (:let* () :source #5#))
  '(#6=(let* (#7=a))
    (:let*
     ((:binding . *) (((:value-binding
                        ((:name . 1) (((:variable-name () :name a :source #7#)
                                       :evaluation :binding)))
                        :source #7#)
                       :evaluation :compound)))
      :source #6#))
  '(#8=(let* (#10=(#11=a 1) #12=b #13=(#14=c 2))
         (declare #15=(type #16=boolean #17=a))
         a)
    (:let*
     ((:binding     . *) (((:value-binding
                            ((:name  . 1) (((:variable-name () :name a :source #11#)
                                            :evaluation :binding))
                             (:value . 1) ((1 :evaluation t)))
                            :source #10#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name . 1) (((:variable-name () :name b :source #12#)
                                           :evaluation :binding)))
                            :source #12#)
                           :evaluation :compound)
                          ((:value-binding
                            ((:name  . 1) (((:variable-name () :name c :source #14#)
                                            :evaluation :binding))
                             (:value . 1) ((2 :evaluation t)))
                            :source #13#)
                           :evaluation :compound))
      (:declaration . *) (((:declaration
                            ((:argument . *) (((:atomic-type-specifier
                                                ((:name . 1) (((:type-name
                                                                ()
                                                                :name boolean :source #16#))))
                                                :source #16#))
                                              ((:variable-name () :name a :source #17#))))
                            :kind type :source #15#)))
      (:form        . *) ((a :evaluation t)))
     :source #8#)))

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

  '(#4=(progv () ())
    (:progv () :source #4#))
  '(#5=(progv (a) (b))
    (:progv
     ((:symbols . 1) (((a) :evaluation t))
      (:values  . 1) (((b) :evaluation t)))
     :source #5#))
  '(#6=(progv '(a) '(b))
    (:progv
     ((:symbols . 1) (('(a) :evaluation t))
      (:values  . 1) (('(b) :evaluation t)))
     :source #6#))
  '(#7=(progv () () 1)
    (:progv ((:form . *) ((1 :evaluation t))) :source #7#))
  '(#8=(progv () () 1 2)
    (:progv
     ((:form . *) ((1 :evaluation t) (2 :evaluation t)))
     :source #8#)))

;;; Special operators `macrolet', `flet' and `labels'

(define-syntax-test (macrolet)
  '((macrolet)         syn:invalid-syntax-error)
  '((macrolet 1)       syn:invalid-syntax-error)
  '((macrolet ((f)))   syn:invalid-syntax-error)
  '((macrolet ((f 1))) syn:invalid-syntax-error)
  '((macrolet ((f (x #5=x))))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#6=(macrolet ())
    (:macrolet () :source #6#))
  '(#7=(macrolet (#8=(#9=f #10=())))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ()
                                               :source #10#)
                                              :evaluation :compound)))
                        :source #8#)
                       :evaluation :compound)))
     :source #7#))
  '(#11=(macrolet (#12=(#13=f #14=(&whole #15=w #16=(#17=a #18=b) &rest #19=c)
                        (declare #20=(type #21=string #22=a))
                        a)))
    (:macrolet
     ((:binding . *) (((:local-macro-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #13#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:destructuring-lambda-list
                                               ((:whole    . 1) (((:variable-name
                                                                   ()
                                                                   :name w :source #15#)))
                                                (:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:pattern
                                                                                   ((:required . *) (((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name a :source #17#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #17#)
                                                                                                      :evaluation :compound)
                                                                                                     ((:required-parameter
                                                                                                       ((:name . 1) (((:variable-name
                                                                                                                       ()
                                                                                                                       :name b :source #18#)
                                                                                                                      :evaluation nil)))
                                                                                                       :source #18#)
                                                                                                      :evaluation :compound)))
                                                                                   :source #16#)
                                                                                  :evaluation :compound)))
                                                                   :source #16#)
                                                                  :evaluation :compound))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name c :source #19#)
                                                                  :evaluation :compound)))
                                               :source #14#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name string :source #21#))))
                                                                   :source #21#))
                                                                 ((:variable-name () :name a :source #22#))))
                                               :kind type :source #20#)))
                         (:form        . *) ((a  :evaluation t)))
                        :source #12#)
                       :evaluation :compound)))
      :source #11#))
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
  '((flet ((f (x #5=x))))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
   ;; Valid syntax
  '(#6=(flet ())
    (:flet () :source #6#))
  '(#7=(flet (#8=(#9=f #10=())))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list () :source #10#)
                                              :evaluation :compound)))
                        :source #8#)
                       :evaluation :compound)))
     :source #7#))
  '(#12=(flet (#13=(#14=f #15=(#16=a &rest #17=b))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #14#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #16#)
                                                                                  :evaluation nil)))
                                                                   :source #16#)))
                                                (:rest     . 1) (((:variable-name
                                                                   ()
                                                                   :name b :source #17#))))
                                               :source #15#)
                                              :evaluation :compound)))
                        :source #13#)
                       :evaluation :compound)))
     :source #12#))
  '(#19=(flet (#20=(#21=f #22=() (declare #23=(type #24=bit #25=a)))))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #21#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #22#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #24#))))
                                                                   :source #24#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #25#))))
                                               :kind type :source #23#))))
                        :source #20#)
                       :evaluation :compound)))
     :source #19#))
  '(#27=(flet (#28=(#29=f #30=() a)))
    (:flet
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #29#)
                                               :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #30#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #28#)
                       :evaluation :compound)))
      :source #27#)))

(define-syntax-test (labels)
  '((labels)          syn:invalid-syntax-error)
  '((labels 1)        syn:invalid-syntax-error)
  '((labels ((1 ()))) syn:invalid-syntax-error)
  '((labels ((f 1)))  syn:invalid-syntax-error)
  '((labels ((f (x #5=x))))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#6=(labels ())
    (:labels () :source #6#))
  '(#7=(labels (#8=(#9=f #10=())))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #9#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #10#)
                                              :evaluation :compound)))
                        :source #8#)
                       :evaluation :compound)))
     :source #7#))
  '(#12=(labels (#13=(#14=f #15=(#16=a &rest #17=b))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #14#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ((:required . *) (((:required-parameter
                                                                   ((:name . 1) (((:variable-name
                                                                                   ()
                                                                                   :name a :source #16#)
                                                                                  :evaluation nil)))
                                                                   :source #16#)))
                                                                (:rest     . 1) (((:variable-name
                                                                                   ()
                                                                                   :name b :source #17#))))
                                               :source #15#)
                                              :evaluation :compound)))
                        :source #13#)
                       :evaluation :compound)))
     :source #12#))
  '(#19=(labels (#20=(#21=f #22=() (declare #23=(type #24=bit #25=a)))))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #21#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #22#)
                                              :evaluation :compound))
                         (:declaration . *) (((:declaration
                                               ((:argument . *) (((:atomic-type-specifier
                                                                   ((:name . 1) (((:type-name
                                                                                   ()
                                                                                   :name bit :source #24#))))
                                                                   :source #24#))
                                                                 ((:variable-name
                                                                   ()
                                                                   :name a :source #25#))))
                                               :kind type :source #23#))))
                        :source #20#)
                       :evaluation :compound)))
     :source #19#))
  '(#27=(labels (#28=(#29=f #30=() a)))
    (:labels
     ((:binding . *) (((:local-function-binding
                        ((:name        . 1) (((:function-name () :name f :source #29#)
                                              :evaluation :binding))
                         (:lambda-list . 1) (((:ordinary-lambda-list
                                               ()
                                               :source #30#)
                                              :evaluation :compound))
                         (:form        . *) ((a :evaluation t)))
                        :source #28#)
                       :evaluation :compound)))
     :source #27#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim 1) syn:invalid-syntax-error)
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
  '(#5=(the #6=bit 1) (:the
                       ((:type . 1) (((:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #6#))))
                                       :source #6#)))
                        (:form . 1) ((1 :evaluation t)))
                       :source #5#)))

;;; Special operator `setq'

(define-syntax-test (setq)
  '((setq a)                 syn:invalid-syntax-error)
  '((setq a 1 b)             syn:invalid-syntax-error)
  '((setq 1 1)               syn:invalid-syntax-error)
  '((setq (1+ a) 1)          syn:invalid-syntax-error)

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
  '((throw)                   syn:invalid-syntax-error)
  '((throw 'foo)              syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)     syn:invalid-syntax-error)

  '(#4=(throw 'foo 1)         (:throw
                               ((:tag-form    . 1) (('foo :evaluation t))
                                (:result-form . 1) ((1    :evaluation t)))
                               :source #4#))
  '(#5=(throw (+ 1 2) :value) (:throw
                               ((:tag-form    . 1) (((+ 1 2) :evaluation t))
                                (:result-form . 1) ((:value  :evaluation t)))
                               :source #5#)))

(define-syntax-test (catch)
  '((catch)                   syn:invalid-syntax-error)

  '(#2=(catch 'foo 1 2)       (:catch
                               ((:tag-form . 1) (('foo :evaluation t))
                                (:form     . *) ((1    :evaluation t)
                                                 (2    :evaluation t)))
                               :source #2#))
  '(#3=(catch (+ 1 2) :value) (:catch
                               ((:tag-form . 1) (((+ 1 2) :evaluation t))
                                (:form     . *) ((:value :evaluation t)))
                               :source #3#)))

(define-syntax-test (unwind-protect)
  '((unwind-protect)                    syn:invalid-syntax-error)

  '(#2=(unwind-protect foo)             (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t)))
                                         :source #2#))
  '(#3=(unwind-protect foo bar)         (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t))
                                          (:cleanup   . *) ((bar :evaluation t)))
                                         :source #3#))
  '(#4=(unwind-protect foo bar baz)     (:unwind-protect
                                         ((:protected . 1) ((foo :evaluation t))
                                          (:cleanup   . *) ((bar :evaluation t)
                                                            (baz :evaluation t)))
                                         :source #4#))
  '(#5=(unwind-protect (progn 1 2) 3 4) (:unwind-protect
                                         ((:protected . 1) (((progn 1 2) :evaluation t))
                                          (:cleanup   . *) ((3           :evaluation t)
                                                            (4           :evaluation t)))
                                         :source #5#)))

;;; `destructuring-bind'

(define-syntax-test (destructuring-bind)
  '((destructuring-bind) syn:invalid-syntax-error)
  '((destructuring-bind #2=1)
    syn:invalid-syntax-error #2# "must be a destructuring lambda list")
  '((destructuring-bind ())
    syn:invalid-syntax-error)

  '(#4=(destructuring-bind #5=(#6=a) #7=b)
    (:destructuring-bind
     ((:lambda-list . 1) (((:destructuring-lambda-list
                            ((:required . *) (((:required-parameter
                                                ((:name . 1) (((:variable-name
                                                                ()
                                                                :name #6# :source #6#)
                                                               :evaluation nil)))
                                                :source #6#)
                                               :evaluation :compound)))
                            :source #5#)
                           :evaluation :compound))
      (:expression . 1)  ((b :evaluation t)))
     :source #4#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-call)
  '((multiple-value-call) syn:invalid-syntax-error)

  '(#2=(multiple-value-call foo)
    (:multiple-value-call
     ((:function-form . 1) ((foo  :evaluation t)))
     :source #2#))
  '(#3=(multiple-value-call foo 1)
    (:multiple-value-call
     ((:function-form . 1) ((foo :evaluation t))
      (:argument      . *) ((1   :evaluation t)))
     :source #3#))
  '(#4=(multiple-value-call foo 1 2)
    (:multiple-value-call
     ((:function-form . 1) ((foo :evaluation t))
      (:argument      . *) ((1   :evaluation t)
                            (2   :evaluation t)))
     :source #4#)))

(define-syntax-test (multiple-value-prog1)
  '((multiple-value-prog1) syn:invalid-syntax-error)

  '(#2=(multiple-value-prog1 1)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t)))
     :source #2#))
  '(#3=(multiple-value-prog1 1 2)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t))
      (:form        . *) ((2 :evaluation t)))
     :source #3#))
  '(#4=(multiple-value-prog1 1 2 3)
    (:multiple-value-prog1
     ((:values-form . 1) ((1 :evaluation t))
      (:form        . *) ((2 :evaluation t)
                          (3 :evaluation t)))
     :source #4#)))
