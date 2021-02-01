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
                          (:test ((foo))
                            :then ((bar)))
                          :source  (if foo bar)))
  '((if foo bar baz)     (:if
                          (:test ((foo))
                            :then ((bar))
                            :else ((baz)))
                          :source  (if foo bar baz))))

;;; Special operators `block', `return-from', `return', `tagbody' and `go'

(define-syntax-test (block)
  '((block)         syn:invalid-syntax-error)
  '((block (foo))   syn:invalid-syntax-error)

  '((block foo 1)   (:block
                        (:name  ((foo))
                         :forms ((1)))
                      :source (block foo 1)))
  '((block foo a b) (:block
                        (:name  ((foo))
                         :forms ((a) (b)))
                      :source (block foo a b))))

(define-syntax-test (return-from)
  '((return-from)         syn:invalid-syntax-error)
  '((return-from foo 1 2) syn:invalid-syntax-error)

  '((return-from foo)     (:return-from
                           (:name ((foo)))
                            :source (return-from foo)))
  '((return-from foo 1)   (:return-from
                           (:name  ((foo))
                            :value ((1)))
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
                         (:value ((1)))
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

  '(#4=(go 1)  (:go (:tag ((1))) :source #4#)))

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
    (:load-time-value (:form ((foo))) :source #3#))
  '(#4=(load-time-value foo t)
    (:load-time-value (:form ((foo)) :read-only-p ((t))) :source #4#)))

(define-syntax-test (quote)
  '((quote)          syn:invalid-syntax-error)
  '((quote x y)      syn:invalid-syntax-error)

  '(#3=(quote 1)     (:quote (:material ((1))) :source #3#))
  '(#4=(quote x)     (:quote (:material ((x))) :source #4#))
  '(#5=(quote quote) (:quote (:material ((quote))) :source #5#)))

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

  '(#6=(function foo)        (:function (:name ((foo))) :source #6#))
  '(#7=(function (setf foo)) (:function (:name (((setf foo)))) :source #7#))
  '(#8=(function #9=(lambda #10=()))
    (:function
     (:lambda (((:lambda-expression
                 (:lambda-list (((:ordinary-lambda-list () :source #10#))))
                 :source #9#))))
     :source #8#))
  '(#11=(function #12=(lambda #13=(#14=a &rest b) (foo)))
    (:function
     (:lambda (((:lambda-expression
                 (:lambda-list (((:ordinary-lambda-list
                                  (:required (((:required-parameter
                                                (:name ((a)))
                                                :source #14#)))
                                   :rest     ((b)))
                                  :source #13#)))
                  :form        (((foo))))
                 :source #12#))))
     :source #11#)))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'

(define-syntax-test (symbol-macrolet)
  '((symbol-macrolet)            syn:invalid-syntax-error)
  '((symbol-macrolet 1)          syn:invalid-syntax-error)
  '((symbol-macrolet (foo))      syn:invalid-syntax-error)
  '((symbol-macrolet ((foo)))    syn:invalid-syntax-error)
  '((symbol-macrolet ((:bla 1))) syn:invalid-syntax-error)

  '(#6=(symbol-macrolet ())
    (:symbol-macrolet () :source #6#))
  '(#7=(symbol-macrolet ((a 1) (b 2)) (declare #8=(type bit d)) c)
    (:symbol-macrolet
        (:names        ((a) (b))
         :expansions   ((1) (2))
         :declarations (((:declaration
                          (:argument ((bit) (d)))
                          :kind type :source #8#)))
         :forms ((c)))
      :source #7#)))

(define-syntax-test (let)
  '((let)       syn:invalid-syntax-error)
  '((let 1)     syn:invalid-syntax-error)
  '((let (1))   syn:invalid-syntax-error)
  '((let ((1))) syn:invalid-syntax-error)

  '(#5=(let ())     (:let () :source #5#))
  '(#6=(let (#7=a)) (:let (:names ((#7#)) #+TODO :values #+TODO ((nil))) :source #6#))
  '(#8=(let ((a 1) b (c 2))
         (declare #9=(type boolean a)) a)
    (:let
     (:names        ((a) (b)   (c))
      :values       ((1) #+TODO (nil) (2))
      :declarations (((:declaration
                       (:argument ((boolean) (a)))
                       :kind type :source #9#)))
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
  '(#6=(let* (#7=a)) (:let* (:names ((#7#)) #+TODO :values #+TODO ((nil))) :source #6#))
  '(#8=(let* ((a 1) b (c 2))
         (declare #9=(type boolean a))
         a)
    (:let*
     (:names        ((a) (b)   (c))
      :values       ((1) #+TODO (nil) (2))
      :declarations (((:declaration
                       (:argument ((boolean) (a)))
                       :kind type :source #9#)))
      :forms        ((a)))
     :source #8#)))

(define-syntax-test (locally)
  '(#1=(locally)
    (:locally () :source #1#))
  '(#2=(locally (declare #3=(type bit a)))
    (:locally
     (:declarations (((:declaration
                       (:argument ((bit) (a)))
                       :kind type :source #3#))))
     :source #2#))
  '(#4=(locally a)
    (:locally (:forms ((a))) :source #4#))
  '(#5=(locally (declare #6=(type bit a)) a)
    (:locally
     (:declarations (((:declaration
                       (:argument ((bit) (a)))
                       :kind type :source #6#)))
      :forms        ((a)))
     :source #5#))

  #+TODO (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x)))))

(define-syntax-test (progv)
  '((progv)    syn:invalid-syntax-error)
  '((progv 1)  syn:invalid-syntax-error)
  '((progv ()) syn:invalid-syntax-error)

  '(#4=(progv () ())
    (:progv () :source #4#))
  '(#5=(progv (a) (b))
    (:progv (:symbols (((a))) :values (((b)))) :source #5#))
  '(#6=(progv '(a) '(b))
    (:progv (:symbols (('(a))) :values (('(b)))) :source #6#))
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
  '(#7=(macrolet (#8=(f #9=())))
    (:macrolet
     (:names     ((f))
      :functions (((:local-macro-function
                    (:lambda-list (((:destructuring-lambda-list
                                     ()
                                     :source #9#))))
                    :source #8#))))
     :source #7#))
  '(#10=(macrolet (#11=(f #12=(&whole w #13=(#14=a #15=b) &rest c)
                        (declare #16=(type string a))
                        a)))
    (:macrolet
     (:names     ((f))
      :functions (((:local-macro-function
                    (:lambda-list  (((:destructuring-lambda-list
                                      (:whole    ((w))
                                       :required (((:required-parameter
                                                    (:name (((:pattern
                                                              (:required (((:required-parameter
                                                                            (:name ((a)))
                                                                            :source #14#))
                                                                          ((:required-parameter
                                                                            (:name ((b)))
                                                                            :source #15#))))
                                                              :source #13#))))
                                                    :source #13#)))
                                       :rest     ((c)))
                                      :source #12#)))
                     :declarations (((:declaration
                                      (:argument ((string) (a)))
                                      :kind type :source #16#)))
                     :forms        ((a)))
                    :source #11#))))
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
  '((flet)          syn:invalid-syntax-error)
  '((flet 1)        syn:invalid-syntax-error)
  '((flet ((1 ()))) syn:invalid-syntax-error)
  '((flet ((f 1)))  syn:invalid-syntax-error)
  '((flet ((f (x #5=x))))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '(#6=(flet ())
    (:flet () :source #6#))
  '(#7=(flet (#8=(f #9=())))
    (:flet
     (:names ((f))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list () :source #9#))))
                    :source #8#))))
     :source #7#))
  '(#10=(flet (#11=(f #12=(#13=a &rest b))))
    (:flet
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     (:required (((:required-parameter
                                                   (:name ((a)))
                                                   :source #13#)))
                                      :rest     ((b)))
                                     :source #12#))))
                    :source #11#))))
     :source #10#))
  '(#14=(flet (#15=(f #16=() (declare #17=(type bit a)))))
    (:flet
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list  (((:ordinary-lambda-list
                                      ()
                                      :source #16#)))
                     :declarations (((:declaration
                                      (:argument ((bit) (a)))
                                      :kind type :source #17#))))
                    :source #15#))))
     :source #14#))
  '(#18=(flet (#19=(f #20=() a)))
    (:flet
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list   (((:ordinary-lambda-list
                                       ()
                                       :source #20#)))
                     :forms         ((a)))
                    :source #19#))))
     :source #18#)))

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
  '(#7=(labels (#8=(f #9=())))
    (:labels
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     ()
                                     :source #9#))))
                    :source #8#))))
     :source #7#))
  '(#10=(labels (#11=(f #12=(#13=a &rest b))))
    (:labels
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     (:required (((:required-parameter
                                                   (:name ((a)))
                                                   :source #13#)))
                                      :rest     ((b)))
                                     :source #12#))))
                    :source #11#))))
     :source #10#))
  '(#14=(labels (#15=(f #16=() (declare #17=(type bit a)))))
    (:labels
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list  (((:ordinary-lambda-list
                                      ()
                                      :source #16#)))
                     :declarations (((:declaration
                                      (:argument ((bit) (a)))
                                      :kind type :source #17#))))
                    :source #15#))))
     :source #14#))
  '(#18=(labels (#19=(f #20=() #21=a)))
    (:labels
     (:names     ((f))
      :functions (((:local-function
                    (:lambda-list (((:ordinary-lambda-list
                                     ()
                                     :source #20#)))
                     :forms       ((#21#)))
                    :source #19#))))
     :source #18#)))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim 1)                  syn:invalid-syntax-error)

  '(#2=(declaim)                 (:declaim () :source #2#))
  '(#3=(declaim #4=(type bit a)) (:declaim
                                  (:declarations (((:declaration
                                                    (:argument ((bit) (a)))
                                                    :kind type :source #4#))))
                                  :source #3#)))

(define-syntax-test (the)
  '((the)             syn:invalid-syntax-error)
  '((the bit)         syn:invalid-syntax-error)
  '((the bit 1 extra) syn:invalid-syntax-error)

  '(#5=(the bit 1)    (:the (:type ((bit)) :form ((1))) :source #5#)))

;;; Special operator `setq'

(define-syntax-test (setq)
  '((setq a)          syn:invalid-syntax-error)
  '((setq a 1 b)      syn:invalid-syntax-error)
  '((setq 1 1)        syn:invalid-syntax-error)
  '((setq (1+ a) 1)   syn:invalid-syntax-error)

  '(#5=(setq)         (:setq () :source #5#))
  '(#6=(setq a 1)     (:setq
                       (:names       ((a))
                        :value-forms ((1)))
                       :source #6#))
  '(#7=(setq a 1 b 2) (:setq
                       (:names       ((a) (b))
                        :value-forms ((1) (2)))
                       :source #7#)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  '((throw)                   syn:invalid-syntax-error)
  '((throw 'foo)              syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)     syn:invalid-syntax-error)

  '(#4=(throw 'foo 1)         (:throw
                                  (:tag-form    (('foo))
                                   :result-form ((1)))
                                :source #4#))
  '(#5=(throw (+ 1 2) :value) (:throw
                                  (:tag-form    (((+ 1 2)))
                                   :result-form ((:value)))
                                :source #5#)))

(define-syntax-test (catch)
  '((catch)                   syn:invalid-syntax-error)

  '(#2=(catch 'foo 1 2)       (:catch
                                  (:tag-form (('foo))
                                   :forms    ((1) (2)))
                                :source #2#))
  '(#3=(catch (+ 1 2) :value) (:catch
                                  (:tag-form (((+ 1 2)))
                                   :forms    ((:value)))
                                :source #3#)))

(define-syntax-test (unwind-protect)
  '((unwind-protect)                    syn:invalid-syntax-error)

  '(#2=(unwind-protect foo)             (:unwind-protect
                                             (:protected ((foo)))
                                          :source #2#))
  '(#3=(unwind-protect foo bar)         (:unwind-protect
                                             (:protected ((foo))
                                              :cleanup   ((bar)))
                                          :source #3#))
  '(#4=(unwind-protect foo bar baz)     (:unwind-protect
                                             (:protected ((foo))
                                              :cleanup   ((bar) (baz)))
                                          :source #4#))
  '(#5=(unwind-protect (progn 1 2) 3 4) (:unwind-protect
                                             (:protected (((progn 1 2)))
                                              :cleanup   ((3) (4)))
                                          :source #5#)))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-call)
  '((multiple-value-call) syn:invalid-syntax-error)

  '(#2=(multiple-value-call foo)
    (:multiple-value-call (:function-form ((foo))) :source #2#))
  '(#3=(multiple-value-call foo 1)
    (:multiple-value-call
        (:function-form ((foo))
         :arguments ((1)))
      :source #3#))
  '(#4=(multiple-value-call foo 1 2)
    (:multiple-value-call
        (:function-form ((foo))
         :arguments     ((1) (2)))
      :source #4#)))

(define-syntax-test (multiple-value-prog1)
  '((multiple-value-prog1) syn:invalid-syntax-error)

  '(#2=(multiple-value-prog1 1)
    (:multiple-value-prog1
        (:values-form ((1)))
      :source #2#))
  '(#3=(multiple-value-prog1 1 2)
    (:multiple-value-prog1
        (:values-form ((1))
         :forms       ((2)))
      :source #3#))
  '(#4=(multiple-value-prog1 1 2 3)
    (:multiple-value-prog1
        (:values-form ((1))
         :forms       ((2) (3)))
      :source #4#)))

;;; Pseudo-operator "application"

(define-syntax-test (syn::application)
  '((1)
    syn:invalid-syntax-error 1 "must be a symbol naming a function or a lambda expression")
  '(((lambda 1) 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '(((lambda (x #3=x)) 1)
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  ;; Valid syntax
  '(#4=(foo)
    (:application
     (:abstraction ((foo)))
     :source #4#))
  '(#5=(foo 1)
    (:application
     (:abstraction ((foo))
      :arguments   ((1)))
     :source #5#))
  '(#6=(#7=(lambda #8=(#9=x) x) 1)
    (:application
     (:abstraction (((:lambda-expression
                      (:lambda-list (((:ordinary-lambda-list
                                       (:required (((:required-parameter
                                                     (:name ((x)))
                                                     :source #9#))))
                                       :source #8#)))
                       :form        ((x)))
                      :source #7#)))
      :arguments   ((1)))
     :source #6#)))
