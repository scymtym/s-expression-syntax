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
  '((progn . 1) syn:invalid-syntax-error)

  '((progn)     (syn::forms ()))
  '((progn 1)   (syn::forms (1)))
  '((progn 1 2) (syn::forms (1 2))))

(define-syntax-test (if)
  '((if)                 syn:invalid-syntax-error)
  '((if foo)             syn:invalid-syntax-error)
  '((if foo bar baz fez) syn:invalid-syntax-error)

  '((if foo bar)         (syn::test foo syn::then bar syn::else nil))
  '((if foo bar baz)     (syn::test foo syn::then bar syn::else baz)))

;;; Special operators `block', `return-from', `return', `tagbody' and `go'

(define-syntax-test (block)
  '((block)         syn:invalid-syntax-error)
  '((block (foo))   syn:invalid-syntax-error)

  '((block foo 1)   (syn::name foo syn::forms (1)))
  '((block foo a b) (syn::name foo syn::forms (a b))))

(define-syntax-test (return-from)
  '((return-from)         syn:invalid-syntax-error)
  '((return-from foo 1 2) syn:invalid-syntax-error)

  '((return-from foo)     (syn::name foo syn::value nil))
  '((return-from foo 1)   (syn::name foo syn::value 1))

  #+TODO (apply #'unparse-return-from-special-operator
                (parse-return-from-special-operator
                 (lambda (&rest args) (print args))
                 '(return-from foo bla))))

(define-syntax-test (return)
  '((return (declare)) syn:invalid-syntax-error)
  '((return 1 2)       syn:invalid-syntax-error)

  '((return)           (syn::value nil))
  '((return 1)         (syn::value 1)))

(define-syntax-test (tagbody)
  '((tagbody nil nil) syn:invalid-syntax-error)

  '((tagbody 0 1 "bla" "bli" 2 (list 1) (list 3))
    (syn::tags     (0 1 2)
     syn::segments (() () ("bla" "bli") ((list 1) (list 3)))))
  '((tagbody (1+ a) (1+ b) :foo)
    (syn::tags     (:foo)
     syn::segments (((1+ a) (1+ b)))))

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

  '((go 1)     (syn::tag 1)))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(define-syntax-test (eval-when)
  '((eval-when)
    syn:invalid-syntax-error)
  '((eval-when #2=1)
    syn:invalid-syntax-error #2# "must be a list of situations")
  '((eval-when (#3=:foo))
    syn:invalid-syntax-error #3# "must be one of :COMPILE-TOPLEVEL, COMPILE, :LOAD-TOPLEVEL, LOAD, :EXECUTE, EVAL")

  '((eval-when ())         (syn::situations ()         syn::forms ()))
  '((eval-when (:execute)) (syn::situations (:execute) syn::forms ()))
  '((eval-when () a)       (syn::situations ()         syn::forms (a))))

(define-syntax-test (load-time-value)
  '((load-time-value)       syn:invalid-syntax-error)
  '((load-time-value 1 2)   syn:invalid-syntax-error)

  '((load-time-value foo)   (syn::form foo syn::read-only-p nil))
  '((load-time-value foo t) (syn::form foo syn::read-only-p t)))

(define-syntax-test (quote)
  '((quote)       syn:invalid-syntax-error)
  '((quote x y)   syn:invalid-syntax-error)

  '((quote 1)     (syn::material 1))
  '((quote x)     (syn::material x))
  '((quote quote) (syn::material quote)))

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

  '((function foo)
    (syn::name   foo
     syn::lambda nil))
  '((function (setf foo))
    (syn::name   (setf foo)
     syn::lambda nil))
  '((function (lambda ()))
    (syn::name   nil
     syn::lambda (syn::lambda-list   (() () nil () nil ())
                  syn::documentation nil
                  syn::declarations  ()
                  syn::forms         ())))
  '((function (lambda (a &rest b) (foo)))
    (syn::name   nil
     syn::lambda (syn::lambda-list   ((a) () b () nil ())
                  syn::documentation nil
                  syn::declarations  ()
                  syn::forms         ((foo))))))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'

(define-syntax-test (symbol-macrolet)
  '((symbol-macrolet)            syn:invalid-syntax-error)
  '((symbol-macrolet 1)          syn:invalid-syntax-error)
  '((symbol-macrolet (foo))      syn:invalid-syntax-error)
  '((symbol-macrolet ((foo)))    syn:invalid-syntax-error)
  '((symbol-macrolet ((:bla 1))) syn:invalid-syntax-error)

  '((symbol-macrolet ())         (syn::names        ()
                                  syn::expansions   ()
                                  syn::declarations ()
                                  syn::forms        ()))
  '((symbol-macrolet ((a 1) (b 2)) (declare (type bit d)) c)
    (syn::names        (a b)
     syn::expansions   (1 2)
     syn::declarations ((type (bit d)))
     syn::forms        (c))))

(define-syntax-test (let)
  '((let)       syn:invalid-syntax-error)
  '((let 1)     syn:invalid-syntax-error)
  '((let (1))   syn:invalid-syntax-error)
  '((let ((1))) syn:invalid-syntax-error)

  '((let ())    (syn::names        ()
                 syn::values       ()
                 syn::declarations ()
                 syn::forms        ()))
  '((let (a))   (syn::names        (a)
                 syn::values       (nil)
                 syn::declarations ()
                 syn::forms        ()))
  '((let ((a 1) b (c 2))
      (declare (type boolean a))
      a)
    (syn::names        (a b c)
     syn::values       (1 nil 2)
     syn::declarations ((type (boolean a)))
     syn::forms        (a)))

  #+TODO (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar))))))

(define-syntax-test (let*)
  '((let*)       syn:invalid-syntax-error)
  '((let* 1)     syn:invalid-syntax-error)
  '((let* (1))   syn:invalid-syntax-error)
  '((let* ((1))) syn:invalid-syntax-error)

  '((let* ())    (syn::names        ()
                  syn::values       ()
                  syn::declarations ()
                  syn::forms        ()))
  '((let* (a))   (syn::names        (a)
                  syn::values       (nil)
                  syn::declarations ()
                  syn::forms        ()))
  '((let* ((a 1) b (c 2))
      (declare (type boolean a))
      a)
    (syn::names        (a b c)
     syn::values       (1 nil 2)
     syn::declarations ((type (boolean a)))
     syn::forms        (a))))

(define-syntax-test (locally)
  '((locally)
    (syn::declarations () syn::forms ()))
  '((locally (declare (type bit a)))
    (syn::declarations ((type (bit a))) syn::forms ()))
  '((locally a)
    (syn::declarations () syn::forms (a)))
  '((locally (declare (type bit a)) a)
    (syn::declarations ((type (bit a))) syn::forms (a)))

  #+TODO (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x)))))

(define-syntax-test (progv)
  '((progv)           syn:invalid-syntax-error)
  '((progv 1)         syn:invalid-syntax-error)
  '((progv ())        syn:invalid-syntax-error)

  '((progv () ())     (syn::symbols      nil
                       syn::values       nil
                       syn::declarations ()
                       syn::forms        ()))
  '((progv (a) (b))   (syn::symbols      (a)
                       syn::values       (b)
                       syn::declarations ()
                       syn::forms        ()))
  '((progv '(a) '(b)) (syn::symbols      '(a)
                       syn::values       '(b)
                       syn::declarations ()
                       syn::forms        ()))
  '((progv () () 1)   (syn::symbols      nil
                       syn::values       nil
                       syn::declarations ()
                       syn::forms        (1)))
  '((progv () () 1 2) (syn::symbols      nil
                       syn::values       nil
                       syn::declarations ()
                       syn::forms        (1 2))))

;;; Special operators `macrolet', `flet' and `labels'

(define-syntax-test (macrolet)
  '((macrolet)         syn:invalid-syntax-error)
  '((macrolet 1)       syn:invalid-syntax-error)
  '((macrolet ((f)))   syn:invalid-syntax-error)
  '((macrolet ((f 1))) syn:invalid-syntax-error)
  '((macrolet ((f (x #5=x))))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '((macrolet ())
    (syn::names        ()
     syn::functions    ()
     syn::declarations ()
     syn::forms        ()))
  '((macrolet ((f ())))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (:destructuring-lambda-list
                          nil nil () () nil () nil () nil)
                         nil () ()))
     syn::declarations ()
     syn::forms        ())
    )
  '((macrolet ((f (&whole w (a b) &rest c)
                 (declare (type string a))
                 a)))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (:destructuring-lambda-list
                          w nil
                          ((:pattern
                            nil (a b) () nil () nil () nil))
                          () c () nil () nil)
                         nil ((type (string a))) (a)))
     syn::declarations ()
     syn::forms        ()))
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
  '((flet ())
    (syn::names        ()
     syn::functions    ()
     syn::declarations ()
     syn::forms        ()))
  '((flet ((f ())))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil () ()))
     syn::declarations ()
     syn::forms        ()))
  '((flet ((f (a &rest b))))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         ((a) () b () nil ()) nil () ()))
     syn::declarations ()
     syn::forms        ()))
  '((flet ((f () (declare (type bit a)))))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil ((type (bit a))) ()))
     syn::declarations ()
     syn::forms        ()))
  '((flet ((f () a)))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil () (a)))
     syn::declarations ()
     syn::forms        ())))

(define-syntax-test (labels)
  '((labels)          syn:invalid-syntax-error)
  '((labels 1)        syn:invalid-syntax-error)
  '((labels ((1 ()))) syn:invalid-syntax-error)
  '((labels ((f 1)))  syn:invalid-syntax-error)
  '((labels ((f (x #5=x))))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '((labels ())
    (syn::names        ()
     syn::functions    ()
     syn::declarations ()
     syn::forms        ()))
  '((labels ((f ())))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil () ()))
     syn::declarations ()
     syn::forms        ()))
  '((labels ((f (a &rest b))))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         ((a) () b () nil ()) nil () ()))
     syn::declarations ()
     syn::forms        ()))
  '((labels ((f () (declare (type bit a)))))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil ((type (bit a))) ()))
     syn::declarations ()
     syn::forms        ()))
  '((labels ((f () a)))
    (syn::names        (f)
     syn::functions    ((syn::parsed-lambda
                         (() () nil () nil ()) nil () (a)))
     syn::declarations ()
     syn::forms        ())))

;;; Special operators `declaim' and `the'

(define-syntax-test (declaim)
  '((declaim 1)            syn:invalid-syntax-error)

  '((declaim)              (syn::declarations ()))
  '((declaim (type bit a)) (syn::declarations ((type (bit a))))))

(define-syntax-test (the)
  '((the)             syn:invalid-syntax-error)
  '((the bit)         syn:invalid-syntax-error)
  '((the bit 1 extra) syn:invalid-syntax-error)

  '((the bit 1)       (syn::type bit syn::form 1)))

;;; Special operator `setq'

(define-syntax-test (setq)
  '((setq a)        syn:invalid-syntax-error)
  '((setq a 1 b)    syn:invalid-syntax-error)
  '((setq 1 1)      syn:invalid-syntax-error)
  '((setq (1+ a) 1) syn:invalid-syntax-error)

  '((setq)          (syn::names ()    syn::value-forms ()))
  '((setq a 1)      (syn::names (a)   syn::value-forms (1)))
  '((setq a 1 b 2)  (syn::names (a b) syn::value-forms (1 2))))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-syntax-test (throw)
  '((throw)                syn:invalid-syntax-error)
  '((throw 'foo)           syn:invalid-syntax-error)
  '((throw 'foo 1 :extra)  syn:invalid-syntax-error)

  '((throw 'foo 1)         (syn::tag-form    'foo
                            syn::result-form 1))
  '((throw (+ 1 2) :value) (syn::tag-form    (+ 1 2)
                            syn::result-form :value)))

(define-syntax-test (catch)
  '((catch)                syn:invalid-syntax-error)

  '((catch 'foo 1 2)       (syn::tag-form 'foo
                            syn::forms    (1 2)))
  '((catch (+ 1 2) :value) (syn::tag-form (+ 1 2)
                            syn::forms    (:value))))

(define-syntax-test (unwind-protect)
  '((unwind-protect)                 syn:invalid-syntax-error)

  '((unwind-protect foo)             (syn::protected foo syn::cleanup ()))
  '((unwind-protect foo bar)         (syn::protected foo syn::cleanup (bar)))
  '((unwind-protect foo bar baz)     (syn::protected foo
                                      syn::cleanup   (bar baz)))
  '((unwind-protect (progn 1 2) 3 4) (syn::protected (progn 1 2)
                                      syn::cleanup   (3 4))))

;;; Special operators for multiple values

(define-syntax-test (multiple-value-call)
  '((multiple-value-call)         syn:invalid-syntax-error)

  '((multiple-value-call foo)     (syn::function-form foo
                                   syn::arguments     ()))
  '((multiple-value-call foo 1)   (syn::function-form foo
                                   syn::arguments     (1)))
  '((multiple-value-call foo 1 2) (syn::function-form foo
                                   syn::arguments     (1 2))))

(define-syntax-test (multiple-value-prog1)
  '((multiple-value-prog1)       syn:invalid-syntax-error)

  '((multiple-value-prog1 1)     (syn::values-form 1 syn::forms ()))
  '((multiple-value-prog1 1 2)   (syn::values-form 1 syn::forms (2)))
  '((multiple-value-prog1 1 2 3) (syn::values-form 1 syn::forms (2 3))))

;;; Pseudo-operator "application"

(define-syntax-test (syn::application)
  '((1)
    syn:invalid-syntax-error 1 "must be a symbol naming a function or a lambda expression")
  '(((lambda 1) 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '(((lambda (x #3=x)) 1)
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  ;; Valid syntax
  '((foo)              (syn::abstraction foo
                        syn::arguments   ()))
  '((foo 1)            (syn::abstraction foo
                        syn::arguments   (1)))
  '(((lambda (x) x) 1) (syn::abstraction (syn::lambda-list   ((x) nil nil nil nil nil)
                                          syn::documentation nil
                                          syn::declarations  nil
                                          syn::forms         (x))
                        syn::arguments   (1))))
