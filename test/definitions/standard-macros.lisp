;;;; standard-macros.lisp --- Tests for standard macro rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.standard-macros
  :in :s-expression-syntax)

(defmacro define-macro-test ((macro-name) &body cases)
  `(test ,macro-name
     ,(format nil "Test for the `~(~A~)' macro syntax." macro-name)
     (syntax-test-cases (,macro-name) ,@cases)))

;;; `defconstant', `defvar' and `defparameter'

(define-macro-test (defconstant)
  '((defconstant)            syn:invalid-syntax-error)
  '((defconstant 1)          syn:invalid-syntax-error)
  '((defconstant foo 1 2)    syn:invalid-syntax-error)
  '((defconstant foo 1 "" 2) syn:invalid-syntax-error)

  '((defconstant foo
      (bar 1)
      "bla")
    (syn::name foo syn::initial-value (bar 1) documentation "bla")))

(define-macro-test (defvar)
  '((defvar)            syn:invalid-syntax-error)
  '((defvar 1)          syn:invalid-syntax-error)
  '((defvar foo 1 2)    syn:invalid-syntax-error)
  '((defvar foo 1 "" 2) syn:invalid-syntax-error)

  '((defvar foo)
    (syn::name foo syn::initial-value nil documentation nil))
  '((defvar foo
      (bar 1)
      "bla")
    (syn::name foo syn::initial-value (bar 1) documentation "bla")))

(define-macro-test (defparameter)
  '((defparameter)            syn:invalid-syntax-error)
  '((defparameter 1)          syn:invalid-syntax-error)
  '((defparameter foo)        syn:invalid-syntax-error)
  '((defparameter foo 1 2)    syn:invalid-syntax-error)
  '((defparameter foo 1 "" 2) syn:invalid-syntax-error)

  '((defparameter foo
      (bar 1)
      "bla")
    (syn::name foo syn::initial-value (bar 1) documentation "bla")))

(define-macro-test (defun)
  '((defun (setf 1) ())
    syn:invalid-syntax-error)
  '((defun foo 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '((defun foo (x #3=x))
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  '((defun foo () (declare 1))
    syn:invalid-syntax-error)
  ;; Valid syntax
  '((defun foo (bar baz)
      "bla"
      (declare (type integer bar baz))
      (+ 1 2))
    (syn::name foo syn::lambda-list ((bar baz) () nil () nil ())
     documentation "bla"
     syn::declarations ((type (integer bar baz)))
     syn::forms ((+ 1 2)))))

(define-macro-test (defmacro)
  '((defmacro)       syn:invalid-syntax-error)
  '((defmacro 1)     syn:invalid-syntax-error)
  '((defmacro foo 1)
    syn:invalid-syntax-error 1 "must be a destructuring lambda list")
  '((defmacro foo (x (y #3=x)))
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  ;; Valid syntax
  `((defmacro foo (a b)
      "bla"
      (declare (ignore a))
      (list 'cons b b))
    (syn::name         foo
     syn::lambda-list  (:destructuring-lambda-list
                        nil nil (a b) () nil () nil () nil)
     documentation     "bla"
     syn::declarations ((ignore (a)))
     syn::forms        ((list 'cons b b)))))

;;; `defstruct' including slots

(define-syntax-test (syn::slot-description)
  '((#1=1)
    syn:invalid-syntax-error #1# "variable name must be a symbol")
  '((foo #2=(declare))
    syn:invalid-syntax-error #2# "declare is not allowed here")
  '(#3=(foo 1 :bar 2)
    syn:invalid-syntax-error #3# "must be of the form (NAME [INITFORM] ...)")
  ;; Repeated options
  '((foo nil :read-only t . #4=(:read-only t))
    syn:invalid-syntax-error #4# ":READ-ONLY option must not be repeated")
  '((foo nil :type bit . #5=(:type bit))
    syn:invalid-syntax-error #5# ":TYPE option must not be repeated")
  ;; Valid
  '(foo
    (syn::name      foo
     syn::initform  nil
     syn::read-only nil
     syn::type      nil))
  '((foo 1 :type bit :read-only t)
    (syn::name      foo
     syn::initform  1
     syn::read-only t
     syn::type      bit)))

(define-macro-test (defstruct)
  '((defstruct)
    syn:invalid-syntax-error)
  '((defstruct #2=1)
    syn:invalid-syntax-error #2# "must be a class name")
  '((defstruct (foo (:constructor foo #3=1)))
    syn:invalid-syntax-error #3# "must be an ordinary lambda list")
  '((defstruct (foo (:constructor foo (x #4=x))))
    syn:invalid-syntax-error #4# "must be a lambda list variable name")
  '((defstruct (foo (:constructor nil) #5=(:constructor bar)))
    syn:invalid-syntax-error #5# "(:constructor nil) and named constructors are mutually exclusive")
  '((defstruct (foo (:constructor . #6=(foo () 1))))
    syn:invalid-syntax-error #6# ":CONSTRUCTOR option accepts one value")
  ;; Repeated options
  '((defstruct (foo (:include bar) #7=(:include bar)))
    syn:invalid-syntax-error #7# ":INCLUDE option must not be repeated")
  '((defstruct (foo (:initial-offset 1) #8=(:initial-offset 1)))
    syn:invalid-syntax-error #8# ":INITIAL-OFFSET option must not be repeated")
  '((defstruct (foo (:type list) #9=(:type list)))
    syn:invalid-syntax-error #9# ":TYPE option must not be repeated")
  ;; Valid
  '((defstruct foo)
    (syn::name          foo
     syn::constructors  ()
     syn::include       nil
     syn::include-slots ()
     syn::documentation nil
     syn::slots         ()))
  '((defstruct (foo))
    (syn::name          foo
     syn::constructors  ()
     syn::include       nil
     syn::include-slots ()
     syn::documentation nil
     syn::slots         ()))
  '((defstruct (foo (:constructor nil)))
    (syn::name          foo
     syn::constructors  ((nil nil))
     syn::include       nil
     syn::include-slots ()
     syn::documentation nil
     syn::slots         ()))
  '((defstruct (foo (:constructor foo (a b))))
    (syn::name          foo
     syn::constructors  ((foo ((a b) () nil () nil ())))
     syn::include       nil
     syn::include-slots ()
     syn::documentation nil
     syn::slots         ()))
  '((defstruct foo "doc")
    (syn::name          foo
     syn::constructors  ()
     syn::include       nil
     syn::include-slots ()
     syn::documentation "doc"
     syn::slots         ())))

;;; `defclass' including slots

(test slot-specifier
  "Test for `slot-specifier' rule."

  (rule-test-cases ((syn::slot-specifier syn::special-operators))
    ;; Invalid syntax
    '((foo :reader 1)                             :fatal 1          "reader must be a symbol function name")
    '((foo :reader (setf foo))                    :fatal (setf foo) "reader must be a symbol function name")
    '((foo :writer 1)                             :fatal 1          "writer must be an extended function name")
    '((foo :writer (setf 1))                      :fatal 1          "second element of SETF function name must be a symbol")
    '((foo :accessor 1)                           :fatal 1          "accessor must be a symbol function name")
    '((foo :accessor (setf foo))                  :fatal (setf foo) "accessor must be a symbol function name")
    '((foo :initarg 1)                            :fatal 1          "initarg must be a symbol")
    '((foo :initform (declare))                   :fatal 1          "declare is not allowed here")
    '((foo :type 1)                               :fatal 1          "must be a type specifier")
    '((foo :documentation 1)                      :fatal 1          "must be a documentation string")
    ;; Repeated options
    '((foo :allocation :class :allocation :class) :fatal :class     ":ALLOCATION option must not be repeated")
    '((foo :initform 1 :initform 1)               :fatal 1          ":INITFORM option must not be repeated")
    '((foo :type bit :type bit)                   :fatal bit        ":TYPE option must not be repeated")
    '((foo :documentation "" :documentation "")   :fatal ""         ":DOCUMENTATION option must not be repeated")
    ;; Valid syntax
    '((foo :initform (+ 1) :custom-option :foo :reader bar)
      t t (syn::name foo
           syn::initargs ()
           syn::readers (bar)
           syn::writers ()
           syn::accessors ()
           syn::allocation nil
           syn::initform (+ 1)
           type nil
           documentation nil
           syn::option-names (:custom-option)
           syn::option-values (:foo)))))

(define-macro-test (defclass)
  '((defclass)
    syn:invalid-syntax-error)
  '((defclass 1)
    syn:invalid-syntax-error)
  #+TODO '((defclass foo #3=1)
    syn:invalid-syntax-error #3# "must be a list")
  '((defclass foo () () (:default-initargs #4=1))
    syn:invalid-syntax-error #4# "default initarg must be a symbol followed by an expression")
  '((defclass foo () () (:default-initargs :foo))
    syn:invalid-syntax-error nil "default initarg must be a symbol followed by an expression")
  '((defclass foo () () (:metaclass #6=1))
    syn:invalid-syntax-error #6# "metaclass must be a class name")
  '((defclass foo () () (:metaclass . #7=(foo 1)))
    syn:invalid-syntax-error #7# ":METACLASS option accepts one value")
  '((defclass foo () () (:documentation #8=1))
    syn:invalid-syntax-error #8# "must be a documentation string")
  '((defclass foo () () (:documentation . #9=("" 1)))
    syn:invalid-syntax-error #9# ":DOCUMENTATION option accepts one value")
  ;; Repeated options
  '((defclass foo () () (:metaclass bar) #10=(:metaclass bar))
    syn:invalid-syntax-error #10# ":METACLASS option must not be repeated")
  '((defclass foo () () (:documentation "") #11=(:documentation ""))
    syn:invalid-syntax-error #11# ":DOCUMENTATION option must not be repeated")
  ;; Slot options
  '((defclass foo () ((a :documentation #12=1)))
    syn:invalid-syntax-error #12# "must be a documentation string")
  ;; Repeated slot options
  '((defclass foo () ((a :type t . #13=(:type t))))
    syn:invalid-syntax-error #13# ":TYPE option must not be repeated")
  ;; Valid syntax
  '((defclass foo (bar baz)
      ((foo :initform (+ 1) :custom-option :foo :reader bar))
      (:metaclass foo)
      (:default-initargs
       :bar 1)
      (:my-class-option 1))
    (syn::name             foo
     syn::superclasses     (bar baz)
     syn::slots            ((syn::name foo
                             syn::initargs ()
                             syn::readers (bar)
                             syn::writers ()
                             syn::accessors ()
                             syn::allocation nil
                             syn::initform (+ 1)
                             type nil
                             documentation nil
                             syn::option-names (:custom-option)
                             syn::option-values (:foo)))
     syn::default-initargs (:bar)
     syn::default-initforms (1)
     syn::metaclass         foo
     documentation          nil
     syn::option-names      (:my-class-option) syn::option-values ((1)))))

(define-macro-test (deftype)
  '((deftype)       syn:invalid-syntax-error)
  '((deftype 1)     syn:invalid-syntax-error)
  '((deftype foo)   syn:invalid-syntax-error)
  '((deftype foo #4=1)
    syn:invalid-syntax-error #4#"must be a DEFTYPE lambda list")
  '((deftype foo (x #5=x))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '((deftype foo (a &key b)
      "bla bli"
      (declare (ignore a))
      (declare (ignore b))
      (list a b))
    (syn::name         foo
     syn::lambda-list  (:destructuring-lambda-list
                        nil nil (a) () nil (((keyword b) b nil nil)) nil () nil)
     documentation     "bla bli"
     syn::declarations ((ignore (a)) (ignore (b)))
     syn::forms        ((list a b)))))

(define-macro-test (defgeneric)
  '((defgeneric foo 1)
    syn:invalid-syntax-error 1 "must be a generic function lambda list")
  #+TODO '((defgeneric foo (&key (a 1)))
           syn:invalid-syntax-error 1 "must be a generic function lambda list")
  '((defgeneric foo (x #3=x))
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  '((defgeneric foo ()
      (:generic-function-class))
    syn:invalid-syntax-error nil ":GENERIC-FUNCTION-CLASS option accepts one value")
  '((defgeneric foo ()
      (:generic-function-class . #5=(bar 1)))
    syn:invalid-syntax-error #5# ":GENERIC-FUNCTION-CLASS option accepts one value")
  '((defgeneric foo ()
      (:generic-function-class #6="foo"))
    syn:invalid-syntax-error #6# "must be a class name")
  '((defgeneric foo ()
      (:argument-precedence-order #7=1))
    syn:invalid-syntax-error #7# "must be a lambda list variable name")
  '((defgeneric foo ()
      (:method-combination #8=1))
    syn:invalid-syntax-error #8# "method combination name must be a symbol")
  '((defgeneric foo ()
      (:method-class #9=1))
    syn:invalid-syntax-error #9# "must be a class name")
  '((defgeneric foo ()
      #10=(:method))
    syn:invalid-syntax-error #10# "must be of the for (:method [QUALIFIERS] LAMBDA-LIST [DECLARATION] FORM*)")
  ;; Repeated options
  '((defgeneric foo ()
      (:generic-function-class bar)
      #11=(:generic-function-class bar))
    syn:invalid-syntax-error #11# ":GENERIC-FUNCTION-CLASS option must not be repeated")
  '((defgeneric foo (a)
      (:argument-precedence-order a)
      #12=(:argument-precedence-order a))
    syn:invalid-syntax-error #12# ":ARGUMENT-PRECEDENCE-ORDER option must not be repeated")
  '((defgeneric foo ()
      (:documentation "foo")
      #13=(:documentation "foo"))
    syn:invalid-syntax-error #13# ":DOCUMENTATION option must not be repeated")
  ;; Valid syntax
  '((defgeneric foo (a b)
      (:documentation "foo")
      (:generic-function-class clazz))
    (syn::name                         foo
     syn::lambda-list                  ((a b) () nil () nil)
     syn::generic-function-class       clazz
     syn::argument-precedence-order    nil
     method-combination                ()
     syn::method-combination-arguments ()
     syn::method-class                 nil
     syn::declarations                 ()
     documentation                     "foo"
     syn::methods                      ()
     syn::option-names                 ()
     syn::option-values                ()))
  '((defgeneric foo (a b)
      (:argument-precedence-order b a))
    (syn::name                         foo
     syn::lambda-list                  ((a b) () nil () nil)
     syn::generic-function-class       nil
     syn::argument-precedence-order    (b a)
     method-combination                ()
     syn::method-combination-arguments ()
     syn::method-class                 nil
     syn::declarations                 ()
     documentation                     nil
     syn::methods                      ()
     syn::option-names                 ()
     syn::option-values                ()))
  '((defgeneric foo (a)
      (:method :custom 1 "foo" (a)))
    (syn::name                         foo
     syn::lambda-list                  ((a) () nil () nil)
     syn::generic-function-class       nil
     syn::argument-precedence-order    ()
     method-combination                ()
     syn::method-combination-arguments ()
     syn::method-class                 nil
     syn::declarations                 ()
     documentation                     nil
     syn::methods                      ((syn::qualifiers   (:custom 1 "foo")
                                         syn::lambda-list  (((a nil)) () nil () nil ())
                                         documentation     nil
                                         syn::declarations ()
                                         syn::forms        ()))
     syn::option-names                 ()
     syn::option-values                ())))

(define-macro-test (defmethod)
  '((defmethod 1)
    syn:invalid-syntax-error 1 "must be a function name")
  #+TODO '((defmethod foo)
           syn:invalid-syntax-error nil "must be a specialized lambda list")
  '((defmethod foo (1))
    syn:invalid-syntax-error 1 "must be a lambda list variable name")
  '((defmethod foo (x #4=x))
    syn:invalid-syntax-error #4# "must be a lambda list variable name")
  '((defmethod foo ((x 1)))
    syn:invalid-syntax-error 1 "must be a class name")
  '((defmethod foo (#6=(x t 1)))
    syn:invalid-syntax-error #6# "must be of the form (NAME SPECIALIZER)")
  ;; Valid syntax
  '((defmethod foo ())
    (syn::name          foo
     syn::qualifiers    ()
     syn::lambda-list   (() () nil () nil ())
     syn::documentation nil
     syn::declarations  ()
     syn::forms         ()))
  '((defmethod foo :around ())
    (syn::name          foo
     syn::qualifiers    (:around)
     syn::lambda-list   (() () nil () nil ())
     syn::documentation nil
     syn::declarations  ()
     syn::forms         ()))
  '((defmethod foo :custom 1 "foo" ())
    (syn::name          foo
     syn::qualifiers    (:custom 1 "foo")
     syn::lambda-list   (() () nil () nil ())
     syn::documentation nil
     syn::declarations  ()
     syn::forms         ()))
  '((defmethod foo ((x t)))
    (syn::name          foo
     syn::qualifiers    ()
     syn::lambda-list   (((x t)) () nil () nil ())
     syn::documentation nil
     syn::declarations  ()
     syn::forms         ()))
  '((defmethod foo ()
      "foo" (declare (ignore)) 1)
    (syn::name          foo
     syn::qualifiers    ()
     syn::lambda-list   (() () nil () nil ())
     syn::documentation "foo"
     syn::declarations  ((ignore nil))
     syn::forms         (1))))

(define-macro-test (defpackage)
  '((defpackage #1=1)
    syn:invalid-syntax-error #1# "must be a string designator")
  '((defpackage foo #2=2)
    syn:invalid-syntax-error #2# "option must be a list")
  '((defpackage foo (:documentation #3=1))
    syn:invalid-syntax-error #3# "must be a documentation string")
  '((defpackage foo (:size #4="a"))
    syn:invalid-syntax-error #4# "must be a non-negative integer" )
  '((defpackage foo (:size . #5=(1 2)))
    syn:invalid-syntax-error #5# ":SIZE option accepts one value" )
  ;; Repeated options
  '((defpackage foo (:documentation "") #6=(:documentation ""))
    syn:invalid-syntax-error #6# ":DOCUMENTATION option must not be repeated")
  '((defpackage foo (:size 1) #7=(:size 2))
    syn:invalid-syntax-error #7# ":SIZE option must not be repeated")
  ;; Valid syntax
  '((defpackage foo
      (:documentation "bla")
      (:use :bar "bar")
      (:size 1)
      (:import-from :foo #\c :bar)
      (:shadowing-import-from :foo2 "BAZ2" :bar2))
    (syn::name                           foo
     syn::nicknames                      ()
     documentation                       "bla"
     syn::use                            (:bar "bar")
     shadow                              ()
     syn::shadowing-import-from-packages (:foo2)
     syn::shadowing-import-from-names    (("BAZ2" :bar2))
     syn::import-from-packages           (:foo)
     syn::import-from-names              ((#\c :bar))
     export                              ()
     intern                              ()
     syn::size                           1))

  '((defpackage foo
      (:documentation "bla")
      (:use :bar "bar")
      (:size 1)
      (:import-from :foo :bar :baz)
      (:shadowing-import-from :foo2 "BAZ2" :bar2))
    (syn::name                           foo
     syn::nicknames                      ()
     documentation                       "bla"
     syn::use                            (:bar "bar")
     shadow                              ()
     syn::shadowing-import-from-packages (:foo2)
     syn::shadowing-import-from-names    (("BAZ2" :bar2))
     syn::import-from-packages           (:foo)
     syn::import-from-names              ((:bar :baz))
     export                              ()
     intern                              ()
     syn::size                           1)))

(define-macro-test (in-package)
  '((in-package)       syn:invalid-syntax-error)
  '((in-package 1)     syn:invalid-syntax-error)
  '((in-package foo 1) syn:invalid-syntax-error)

  '((in-package foo)   (syn::name foo))
  '((in-package "FOO") (syn::name "FOO")))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-macro-test (handler-bind)
  '((handler-bind)
    syn:invalid-syntax-error)
  '((handler-bind (#2=1))
    syn:invalid-syntax-error #2# "must be of the form (TYPE HANDLER-FORM)")
  '((handler-bind ((#3=1 (lambda (x)))))
    syn:invalid-syntax-error #3# "must be a type specifier")
  ;; Valid syntax
  '((handler-bind ((foo (lambda (x) (bar))))
      (baz))
    (syn::types         (foo)
     syn::handler-forms ((lambda (x) (bar)))
     syn::forms         ((baz)))))

(define-macro-test (handler-case)
  '((handler-case nil (#1=1))
    syn:invalid-syntax-error #1# "must be a type specifier")
  '((handler-case nil (foo #2=1))
    syn:invalid-syntax-error #2# "must be a lambda list with zero or one required parameter")
  '((handler-case nil (foo (#3=1)))
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  ;; Repeated option
  '((handler-case nil (:no-error ()) #4=(:no-error ()))
    syn:invalid-syntax-error #4# "NO-ERROR must not be repeated")
  ;; Valid syntax
  '((handler-case (foo)
      (bar (x) (baz))
      (:no-error (y z) (fez)))
    (syn::form                 (foo)
     syn::types                (bar)
     syn::variables             (x)
     syn::declarations          (())
     syn::forms                 (((baz)))
     syn::no-error-lambda-list  ((y z) () nil () nil ())
     syn::no-error-declarations ()
     syn::no-error-forms        ((fez)))))

(define-macro-test (restart-bind)
  '((restart-bind 1)
    syn:invalid-syntax-error)
  '((restart-bind (#2=1))
    syn:invalid-syntax-error #2# "must be of the form (NAME FUNCTION [OPTIONS])")
  '((restart-bind ((#3=1 foo)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((restart-bind ((foo bar :test-function baz . #4=(:test-function fez))))
    syn:invalid-syntax-error #4# ":TEST-FUNCTION option must not be repeated")
  ;; Valid syntax
  '((restart-bind () 1)
    (syn::names                 ()
     syn::functions             ()
     syn::interactive-functions ()
     syn::report-functions      ()
     syn::test-functions        ()
     syn::forms                 (1)))
  '((restart-bind ((foo bar :report-function baz)) 1)
    (syn::names                 (foo)
     syn::functions             (bar)
     syn::interactive-functions (nil)
     syn::report-functions      (baz)
     syn::test-functions        (nil)
     syn::forms                 (1))))

(define-macro-test (restart-case)
  '((restart-case #1=(declare))
    syn:invalid-syntax-error #1# "declare is not allowed here")
  '((restart-case 1 #2=1)
    syn:invalid-syntax-error)
  '((restart-case 1 (#3=1))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((restart-case 1 (foo #4=1))
    syn:invalid-syntax-error #4# "must be an ordinary lambda list")
  ;; Valid syntax
  '((restart-case 1)
    (syn::form         1
     syn::names        ()
     syn::lambda-lists ()
     syn::interactives ()
     syn::reports      ()
     syn::tests        ()
     syn::declarations ()
     syn::forms        ()))
  '((restart-case 1 (foo (x) :report "bar" :test baz))
    (syn::form         1
     syn::names        (foo)
     syn::lambda-lists (((x) () nil () nil ()))
     syn::interactives (nil)
     syn::reports      ("bar")
     syn::tests        (baz)
     syn::declarations (())
     syn::forms        (()))))
