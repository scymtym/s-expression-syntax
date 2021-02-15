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
  '(#3=(progn 1)   (:progn (:forms ((1))) :source #3#))
  '(#4=(progn 1 2) (:progn (:forms ((1) (2))) :source #4#)))

(define-syntax-test (if)
  '((if)                 syn:invalid-syntax-error)
  '((if foo)             syn:invalid-syntax-error)
  '((if foo bar baz fez) syn:invalid-syntax-error)

  '((if foo bar)         (:if
                          ((:test . 1) ((foo))
                           (:then . 1) ((bar)))
                          :source  (if foo bar)))
  '((if foo bar baz)     (:if
                          ((:test . 1) ((foo))
                           (:then . 1) ((bar))
                           :else       ((baz)))
                          :source  (if foo bar baz))))

;;; Special operators `block', `return-from', `return', `tagbody' and `go'

(define-syntax-test (block)
  '((block)         syn:invalid-syntax-error)
  '((block (foo))   syn:invalid-syntax-error)

  '((block foo 1)   (:block
                     ((:name . 1) ((foo))
                      :forms      ((1)))
                     :source (block foo 1)))
  '((block foo a b) (:block
                     ((:name . 1) ((foo))
                      :forms      ((a) (b)))
                     :source (block foo a b))))

(define-syntax-test (return-from)
  '((return-from)         syn:invalid-syntax-error)
  '((return-from foo 1 2) syn:invalid-syntax-error)

  '((return-from foo)     (:return-from
                           ((:name . 1) ((foo)))
                           :source (return-from foo)))
  '((return-from foo 1)   (:return-from
                           ((:name . 1)  ((foo))
                            (:value . 1) ((1)))
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
                        ((:value . 1) ((1)))
                        :source (return 1))))

(define-syntax-test (tagbody)
  '((tagbody nil nil) syn:invalid-syntax-error)

  '(#2=(tagbody 0 1 "bla" "bli" 2 (list 1) (list 3))
    (:tagbody
     (:tags     ((0) (1) (2))
      :segments ((("bla" "bli")) (((list 1) (list 3)))))
     :source #2#))
  '(#3=(tagbody (1+ a) (1+ b) :foo)
    (:tagbody
     (:tags     ((:foo))
      :segments ((((1+ a) (1+ b)))))
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
    (:eval-when (:situations ((:execute))) :source #5#))
  '(#6=(eval-when () a)
    (:eval-when (:forms ((a))) :source #6#)))

(define-syntax-test (load-time-value)
  '((load-time-value)       syn:invalid-syntax-error)
  '((load-time-value 1 2)   syn:invalid-syntax-error)

  '(#3=(load-time-value foo)
    (:load-time-value ((:form . 1) ((foo))) :source #3#))
  '(#4=(load-time-value foo t)
    (:load-time-value ((:form . 1) ((foo)) :read-only-p ((t))) :source #4#)))

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
    syn:invalid-syntax-error #5# "must be a lambda list variable name")

  '(#6=(function #7=foo)
    (:function
     (:name (((:function-name () :name foo :source #7#))))
     :source #6#))
  '(#8=(function #9=(setf foo))
    (:function
     (:name (((:function-name () :name (setf foo) :source #9#))))
     :source #8#))
  '(#10=(function #11=(lambda #12=()))
    (:function
     (:lambda (((:lambda-expression
                 ((:lambda-list . 1) (((:ordinary-lambda-list () :source #12#))))
                 :source #11#))))
     :source #10#))
  '(#13=(function #14=(lambda #15=(#16=a &rest #17=b) (foo)))
    (:function
     (:lambda (((:lambda-expression
                 ((:lambda-list . 1) (((:ordinary-lambda-list
                                        (:required (((:required-parameter
                                                      ((:name . 1) (((:variable-name
                                                                      ()
                                                                      :name a :source #16#))))
                                                      :source #16#)))
                                         :rest     (((:variable-name
                                                      ()
                                                      :name b :source #17#))))
                                        :source #15#)))
                  :form              (((foo))))
                 :source #14#))))
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
  '(#7=(symbol-macrolet ((#8=a 1) (#9=b 2)) (declare #10=(type bit #11=d)) c)
    (:symbol-macrolet
     (:names        (((:variable-name () :name a :source #8#))
                     ((:variable-name () :name b :source #9#)))
      :expansions   ((1) (2))
      :declarations (((:declaration
                       (:argument ((bit)
                                   ((:variable-name () :name d :source #11#))))
                       :kind type :source #10#)))
      :forms ((c)))
     :source #7#)))

(define-syntax-test (let)
  '((let)       syn:invalid-syntax-error)
  '((let 1)     syn:invalid-syntax-error)
  '((let (1))   syn:invalid-syntax-error)
  '((let ((1))) syn:invalid-syntax-error)

  '(#5=(let ())     (:let () :source #5#))
  '(#6=(let (#7=a)) (:let
                     (:names (((:variable-name () :name a :source #7#)))
                      #+TODO :values #+TODO ((nil)))
                     :source #6#))
  '(#8=(let ((#9=a 1) #10=b (#11=c 2))
         (declare #12=(type boolean #13=a)) a)
    (:let
     (:names        (((:variable-name () :name a :source #9#))
                     ((:variable-name () :name b :source #10#))
                     ((:variable-name () :name c :source #11#)))
      :values       ((1)
                     #+TODO (nil)
                     (2))
      :declarations (((:declaration
                       (:argument ((boolean)
                                   ((:variable-name () :name a :source #13#))))
                       :kind type :source #12#)))
      :forms        ((a)))
     :source #8#))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  '((let*)       syn:invalid-syntax-error)
  '((let* 1)     syn:invalid-syntax-error)
  '((let* (1))   syn:invalid-syntax-error)
  '((let* ((1))) syn:invalid-syntax-error)

  '(#5=(let* ())     (:let* () :source #5#))
  '(#6=(let* (#7=a)) (:let*
                      (:names (((:variable-name () :name a :source #7#)))
                       #+TODO :values #+TODO ((nil)))
                      :source #6#))
  '(#8=(let* ((#9=a 1) #10=b (#11=c 2))
         (declare #12=(type boolean #13=a))
         a)
    (:let*
     (:names        (((:variable-name () :name a :source #9#))
                     ((:variable-name () :name b :source #10#))
                     ((:variable-name () :name c :source #11#)))
      :values       ((1) #+TODO (nil) (2))
      :declarations (((:declaration
                       (:argument ((boolean)
                                   ((:variable-name () :name a :source #13#))))
                       :kind type :source #12#)))
      :forms        ((a)))
     :source #8#)))

(define-syntax-test (locally)
  '(#1=(locally)
    (:locally () :source #1#))
  '(#2=(locally (declare #3=(type bit #4=a)))
    (:locally
     (:declarations (((:declaration
                       (:argument ((bit)
                                   ((:variable-name () :name a :source #4#))))
                       :kind type :source #3#))))
     :source #2#))
  '(#5=(locally a)
    (:locally (:forms ((a))) :source #5#))
  '(#6=(locally (declare #7=(type bit #8=a)) a)
    (:locally
     (:declarations (((:declaration
                       (:argument ((bit)
                                   ((:variable-name () :name a :source #8#))))
                       :kind type :source #7#)))
      :forms        ((a)))
     :source #6#))

  #+TODO (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x)))))

(define-syntax-test (progv)
  '((progv)    syn:invalid-syntax-error)
  '((progv 1)  syn:invalid-syntax-error)
  '((progv ()) syn:invalid-syntax-error)

  '(#4=(progv () ())
    (:progv () :source #4#))
  '(#5=(progv (a) (b))
    (:progv ((:symbols . 1) (((a))) (:values . 1) (((b)))) :source #5#))
  '(#6=(progv '(a) '(b))
    (:progv ((:symbols . 1) (('(a))) (:values . 1) (('(b)))) :source #6#))
  '(#7=(progv () () 1)
    (:progv (:forms ((1))) :source #7#))
  '(#8=(progv () () 1 2)
    (:progv (:forms ((1) (2))) :source #8#)))

;;; Special operators `macrolet', `flet' and `labels'

(define-syntax-test (macrolet)
  '((macrolet)         syn:invalid-syntax-error)
  '((macrolet 1)       syn:invalid-syntax-error)
  '((macrolet ((f)))   syn:invalid-syntax-error)
  '((macrolet ((f 1))) syn:invalid-syntax-error)
  '((macrolet ((f (x #5=x))))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '(#6=(macrolet ())
    (:macrolet () :source #6#))
  '(#7=(macrolet (#8=(#9=f #10=())))
    (:macrolet
     (:names     (((:function-name () :name f :source #8#)))
      :functions (((:local-macro-function
                    (:lambda-list (((:destructuring-lambda-list
                                     ()
                                     :source #9#))))
                    :source #8#))))
     :source #7#))
  '(#11=(macrolet (#12=(#13=f #14=(&whole #15=w #16=(#17=a #18=b) &rest #19=c)
                        (declare #20=(type string #21=a))
                        a)))
    (:macrolet
     (:names     (((:function-name () :name f :source #12#)))
      :functions (((:local-macro-function
                    (:lambda-list  (((:destructuring-lambda-list
                                      (:whole    (((:variable-name
                                                    ()
                                                    :name w :source #15#)))
                                       :required (((:required-parameter
                                                    ((:name . 1) (((:pattern
                                                                    (:required (((:required-parameter
                                                                                  ((:name . 1) (((:variable-name
                                                                                                  ()
                                                                                                  :name a :source #17#))))
                                                                                  :source #17#))
                                                                                ((:required-parameter
                                                                                  ((:name . 1) (((:variable-name
                                                                                                  ()
                                                                                                  :name b :source #18#))))
                                                                                  :source #18#))))
                                                                    :source #16#))))
                                                    :source #16#)))
                                       :rest     (((:variable-name
                                                    ()
                                                    :name c :source #19#))))
                                      :source #14#)))
                     :declarations (((:declaration
                                      (:argument ((string)
                                                  ((:variable-name () :name a :source #21#))))
                                      :kind type :source #20#)))
                     :forms        ((a)))
                    :source #12#))))
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
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
   ;; Valid syntax
  '(#6=(flet ())
    (:flet () :source #6#))
  '(#7=(flet (#8=(#9=f #10=())))
    (:flet
     (:names (((:function-name () :name f :source #7#)))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list () :source #8#))))
                    :source #6#))))
     :source #5#))
  '(#11=(flet (#12=(#13=f #14=(#15=a &rest #16=b))))
    (:flet
     (:names     (((:function-name () :name f :source #12#)))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     (:required (((:required-parameter
                                                   ((:name . 1) (((:variable-name
                                                                   ()
                                                                   :name a :source #15#))))
                                                   :source #15#)))
                                      :rest     (((:variable-name
                                                   ()
                                                   :name b :source #16#))))
                                     :source #14#))))
                    :source #12#))))
     :source #11#))
  '(#17=(flet (#18=(#19=f #20=() (declare #21=(type bit #22=a)))))
    (:flet
     (:names     (((:function-name () :name f :source #19#)))
      :functions (((:local-function
                    (:lambda-list  (((:ordinary-lambda-list
                                      ()
                                      :source #20#)))
                     :declarations (((:declaration
                                      (:argument ((bit)
                                                  ((:variable-name
                                                    ()
                                                    :name a :source #22#))))
                                      :kind type :source #21#))))
                    :source #18#))))
     :source #17#))
  '(#23=(flet (#24=(#25=f #26=() a)))
    (:flet
     (:names     (((:function-name () :name f :source #25#)))
      :functions (((:local-function
                    (:lambda-list   (((:ordinary-lambda-list
                                       ()
                                       :source #26#)))
                     :forms         ((a)))
                    :source #24#))))
     :source #23#)))

(define-syntax-test (labels)
  '((labels)          syn:invalid-syntax-error)
  '((labels 1)        syn:invalid-syntax-error)
  '((labels ((1 ()))) syn:invalid-syntax-error)
  '((labels ((f 1)))  syn:invalid-syntax-error)
  '((labels ((f (x #5=x))))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '(#6=(labels ())
    (:labels () :source #6#))
  '(#7=(labels (#8=(#9=f #10=())))
    (:labels
     (:names     (((:function-name () :name f :source #8#)))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     ()
                                     :source #9#))))
                    :source #7#))))
     :source #6#))
  '(#11=(labels (#12=(#13=f #14=(#15=a &rest #16=b))))
    (:labels
     (:names     (((:function-name () :name f :source #13#)))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     (:required (((:required-parameter
                                                   ((:name . 1) (((:variable-name
                                                                   ()
                                                                   :name a :source #15#))))
                                                   :source #15#)))
                                      :rest     (((:variable-name
                                                   ()
                                                   :name b :source #16#))))
                                     :source #14#))))
                    :source #12#))))
     :source #11#))
  '(#17=(labels (#18=(#19=f #20=() (declare #21=(type bit #22=a)))))
    (:labels
     (:names     (((:function-name () :name f :source #18#)))
      :functions (((:local-function
                    (:lambda-list  (((:ordinary-lambda-list
                                      ()
                                      :source #19#)))
                     :declarations (((:declaration
                                      (:argument ((bit)
                                                  ((:variable-name
                                                    ()
                                                    :name a :source #21#))))
                                      :kind type :source #20#))))
                    :source #17#))))
     :source #16#))
  '(#23=(labels (#24=(#25=f #26=() a)))
    (:labels
     (:names     (((:function-name () :name f :source #25#)))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     ()
                                     :source #26#)))
                     :forms       ((a)))
                    :source #24#))))
     :source #23#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim 1)                  syn:invalid-syntax-error)
  ;; Valid syntax
  '(#2=(declaim)
    (:declaim () :source #2#))
  '(#3=(declaim #4=(type bit #5=a))
    (:declaim
     (:declarations (((:declaration
                       (:argument ((bit)
                                   ((:variable-name () :name a :source #5#))))
                       :kind type :source #4#))))
     :source #3#)))

(define-syntax-test (the)
  '((the)             syn:invalid-syntax-error)
  '((the bit)         syn:invalid-syntax-error)
  '((the bit 1 extra) syn:invalid-syntax-error)
  ;; Valid syntax
  '(#5=(the bit 1)    (:the
                       ((:type . 1) ((bit))
                        (:form . 1) ((1)))
                       :source #5#)))

;;; Special operator `setq'

(define-syntax-test (setq)
  '((setq a)                 syn:invalid-syntax-error)
  '((setq a 1 b)             syn:invalid-syntax-error)
  '((setq 1 1)               syn:invalid-syntax-error)
  '((setq (1+ a) 1)          syn:invalid-syntax-error)

  '(#5=(setq)                (:setq () :source #5#))
  '(#6=(setq #7=a 1)         (:setq
                              (:names       (((:variable-name () :name a :source #7#)))
                               :value-forms ((1)))
                              :source #6#))
  '(#8=(setq #9=a 1 #10=b 2) (:setq
                              (:names       (((:variable-name () :name a :source #9#))
                                             ((:variable-name () :name b :source #10#)))
                               :value-forms ((1) (2)))
                              :source #8#)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  '((throw)                   syn:invalid-syntax-error)
  '((throw 'foo)              syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)     syn:invalid-syntax-error)

  '(#4=(throw 'foo 1)         (:throw
                               ((:tag-form . 1)    (('foo))
                                (:result-form . 1) ((1)))
                               :source #4#))
  '(#5=(throw (+ 1 2) :value) (:throw
                               ((:tag-form . 1)    (((+ 1 2)))
                                (:result-form . 1) ((:value)))
                               :source #5#)))

(define-syntax-test (catch)
  '((catch)                   syn:invalid-syntax-error)

  '(#2=(catch 'foo 1 2)       (:catch
                               ((:tag-form . 1) (('foo))
                                :forms          ((1) (2)))
                               :source #2#))
  '(#3=(catch (+ 1 2) :value) (:catch
                               ((:tag-form . 1) (((+ 1 2)))
                                :forms          ((:value)))
                               :source #3#)))

(define-syntax-test (unwind-protect)
  '((unwind-protect)                    syn:invalid-syntax-error)

  '(#2=(unwind-protect foo)             (:unwind-protect
                                         ((:protected . 1) ((foo)))
                                         :source #2#))
  '(#3=(unwind-protect foo bar)         (:unwind-protect
                                         ((:protected . 1) ((foo))
                                          :cleanup   ((bar)))
                                         :source #3#))
  '(#4=(unwind-protect foo bar baz)     (:unwind-protect
                                         ((:protected . 1) ((foo))
                                          :cleanup   ((bar) (baz)))
                                         :source #4#))
  '(#5=(unwind-protect (progn 1 2) 3 4) (:unwind-protect
                                         ((:protected . 1) (((progn 1 2)))
                                          :cleanup         ((3) (4)))
                                         :source #5#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-call)
  '((multiple-value-call) syn:invalid-syntax-error)

  '(#2=(multiple-value-call foo)
    (:multiple-value-call
     ((:function-form . 1) ((foo)))
     :source #2#))
  '(#3=(multiple-value-call foo 1)
    (:multiple-value-call
     ((:function-form . 1) ((foo))
      :arguments           ((1)))
     :source #3#))
  '(#4=(multiple-value-call foo 1 2)
    (:multiple-value-call
     ((:function-form . 1) ((foo))
      :arguments           ((1) (2)))
     :source #4#)))

(define-syntax-test (multiple-value-prog1)
  '((multiple-value-prog1) syn:invalid-syntax-error)

  '(#2=(multiple-value-prog1 1)
    (:multiple-value-prog1
     ((:values-form . 1) ((1)))
     :source #2#))
  '(#3=(multiple-value-prog1 1 2)
    (:multiple-value-prog1
     ((:values-form . 1) ((1))
      :forms             ((2)))
     :source #3#))
  '(#4=(multiple-value-prog1 1 2 3)
    (:multiple-value-prog1
     ((:values-form . 1) ((1))
      :forms       ((2) (3)))
     :source #4#)))
