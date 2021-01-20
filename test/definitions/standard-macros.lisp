;;;; standard-macros.lisp --- Tests for standard macro rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.standard-macros
  :in :s-expression-syntax)

;;; `defconstant', `defvar' and `defparameter'

(test defconstant
  "Test for the `defconstant' standard macro syntax."

  (syntax-test-cases (defconstant)
    '((defconstant)            syn:invalid-syntax-error)
    '((defconstant 1)          syn:invalid-syntax-error)
    '((defconstant foo 1 2)    syn:invalid-syntax-error)
    '((defconstant foo 1 "" 2) syn:invalid-syntax-error)

    '((defconstant foo
        (bar 1)
        "bla")
      (syn::name foo syn::initial-value (bar 1) documentation "bla"))))

(test defvar
  "Test for the `defvar' standard macro syntax."

  (syntax-test-cases (defvar)
    '((defvar)            syn:invalid-syntax-error)
    '((defvar 1)          syn:invalid-syntax-error)
    '((defvar foo 1 2)    syn:invalid-syntax-error)
    '((defvar foo 1 "" 2) syn:invalid-syntax-error)

    '((defvar foo)
      (syn::name foo syn::initial-value nil documentation nil))
    '((defvar foo
        (bar 1)
        "bla")
      (syn::name foo syn::initial-value (bar 1) documentation "bla"))))

(test defparameter
  "Test for the `defparameter' standard macro syntax."

  (syntax-test-cases (defparameter)
    '((defparameter)            syn:invalid-syntax-error)
    '((defparameter 1)          syn:invalid-syntax-error)
    '((defparameter foo)        syn:invalid-syntax-error)
    '((defparameter foo 1 2)    syn:invalid-syntax-error)
    '((defparameter foo 1 "" 2) syn:invalid-syntax-error)

    '((defparameter foo
        (bar 1)
        "bla")
      (syn::name foo syn::initial-value (bar 1) documentation "bla"))))

(test defun
  "Test for the `defun' standard macro syntax."

  (syntax-test-cases (defun)
    '((defun (setf 1) ())        syn:invalid-syntax-error)
    '((defun foo () (declare 1)) syn:invalid-syntax-error)

    '((defun foo (bar baz)
        "bla"
        (declare (type integer bar baz))
        (+ 1 2))
      (syn::name foo syn::lambda-list ((bar baz) () nil () nil ())
       documentation "bla"
       syn::declarations ((type (integer bar baz)))
       syn::forms ((+ 1 2))))))

(test defmacro
  "Test for the `defmacro' standard macro syntax."

  (syntax-test-cases (defmacro)
    '((defmacro)       syn:invalid-syntax-error)
    '((defmacro 1)     syn:invalid-syntax-error)
    '((defmacro foo 1) syn:invalid-syntax-error)

    `((defmacro foo (a b)
        "bla"
        (declare (ignore a))
        (list 'cons b b))
      (syn::name foo
       syn::lambda-list (:destructuring-lambda-list
                            nil nil (a b) () nil () nil () nil)
       documentation "bla"
       syn::declarations ((ignore (a)))
       syn::forms ((list 'cons b b))))))

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
    '((foo :allocation :class :allocation :class) :fatal :class     "option must not be repeated")
    '((foo :initform 1 :initform 1)               :fatal 1          "option must not be repeated")
    '((foo :type bit :type bit)                   :fatal bit        "option must not be repeated")
    '((foo :documentation "" :documentation "")   :fatal ""         "option must not be repeated")
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

(test defclass
  "Test for `defclass' standard macro syntax."

  (syntax-test-cases (defclass)
    '((defclass)                                                   syn:invalid-syntax-error)
    '((defclass 1)                                                 syn:invalid-syntax-error)
    '((defclass foo 1)                                             syn:invalid-syntax-error)
    '((defclass foo () () (:documentation 1))                      syn:invalid-syntax-error)
    '((defclass foo () ((a :documentation 1)))                     syn:invalid-syntax-error)
    ;; Repeated options
    '((defclass foo () () (:metaclass bar) (:metaclass bar))       syn:invalid-syntax-error)
    '((defclass foo () () (:documentation "") (:documentation "")) syn:invalid-syntax-error)
    ;; Valid syntax
    '((defclass foo (bar baz)
        ((foo :initform (+ 1) :custom-option :foo :reader bar))
        (:metaclass foo)
        (:default-initargs
         :bar 1)
        (:my-class-option 1))
      (syn::name foo syn::superclasses (bar baz)
       syn::slots ((syn::name foo
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
       syn::metaclass foo
       documentation nil
       syn::option-names (:my-class-option) syn::option-values ((1))))))

(test deftype
  "Test for `deftype' standard macro syntax."

  (syntax-test-cases (deftype)
    '((deftype)       syn:invalid-syntax-error)
    '((deftype 1)     syn:invalid-syntax-error)
    '((deftype foo)   syn:invalid-syntax-error)
    '((deftype foo 1) syn:invalid-syntax-error)

    '((deftype foo (a &key b)
        "bla bli"
        (declare (ignore a))
        (declare (ignore b))
        (list a b))
      (syn::name foo
       syn::lambda-list (:destructuring-lambda-list
                         nil nil (a) () nil (((keyword b) b nil nil)) nil () nil)
       documentation "bla bli"
       syn::declarations ((ignore (a)) (ignore (b)))
       syn::forms ((list a b))))))

(test defgeneric
  "Test for `defgeneric' standard macro syntax."

  (syntax-test-cases (defgeneric)
    '((defgeneric foo ()
        (:generic-function-class "foo"))
      syn:invalid-syntax-error)
    ;; Repeated options
    '((defgeneric foo ()
        (:generic-function-class bar)
        (:generic-function-class bar))
      syn:invalid-syntax-error)
    '((defgeneric foo ()
        (:argument-precedence-order :most-specific-first)
        (:argument-precedence-order :most-specific-first))
      syn:invalid-syntax-error)
    '((defgeneric foo ()
        (:documentation "foo")
        (:documentation "foo"))
      syn:invalid-syntax-error)
    ;; Valid syntax
    '((defgeneric foo (a b)
        (:documentation "foo")
        (:generic-function-class clazz))
      (syn::name foo syn::lambda-list ((a b) () nil () nil)
       syn::generic-function-class clazz
       syn::argument-precedence-order nil
       documentation "foo"
       syn::option-names () syn::option-values ()))))

(test defpackage
  "Test for the `defpackage' standard macro syntax."

  (syntax-test-cases (defpackage)
    '((defpackage 1)                                           syn:invalid-syntax-error)
    '((defpackage foo 2)                                       syn:invalid-syntax-error)
    '((defpackage foo (:documentation 1))                      syn:invalid-syntax-error)
    '((defpackage foo (:size "a"))                             syn:invalid-syntax-error)
    ;; Repeated options
    '((defpackage foo (:documentation "") (:documentation "")) syn:invalid-syntax-error)
    '((defpackage foo (:size 1) (:size 2))                     syn:invalid-syntax-error)
    ;; Valid syntax
    '((defpackage foo
        (:documentation "bla")
        (:use :bar "bar")
        (:size 1)
        (:import-from :foo #\c :bar)
        (:shadowing-import-from :foo2 "BAZ2" :bar2))
      (syn::name foo syn::nicknames () documentation "bla" syn::use (:bar "bar")
       shadow () syn::shadowing-import-from-packages (:foo2)
       syn::shadowing-import-from-names (("BAZ2" :bar2))
       syn::import-from-packages (:foo)
       syn::import-from-names ((#\c :bar))
       export () intern () syn::size 1))

    '((defpackage foo
        (:documentation "bla")
        (:use :bar "bar")
        (:size 1)
        (:import-from :foo :bar :baz)
        (:shadowing-import-from :foo2 "BAZ2" :bar2))
      (syn::name foo syn::nicknames () documentation "bla" syn::use (:bar "bar")
       shadow () syn::shadowing-import-from-packages (:foo2)
       syn::shadowing-import-from-names (("BAZ2" :bar2))
       syn::import-from-packages (:foo)
       syn::import-from-names ((:bar :baz))
       export () intern () syn::size 1))))

(test in-package
  "test for the `in-package' standard macro syntax."

  (syntax-test-cases (in-package)
    '((in-package)       syn:invalid-syntax-error)
    '((in-package 1)     syn:invalid-syntax-error)
    '((in-package foo 1) syn:invalid-syntax-error)

    '((in-package foo)   (syn::name foo))
    '((in-package "FOO") (syn::name "FOO"))))
