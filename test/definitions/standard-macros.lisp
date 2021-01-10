;;;; standard-macros.lisp --- Tests for standard macro rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax.test)

(def-suite* :syntax.standard-macros
  :in :syntax)

;;; `defconstant', `defvar' and `defparameter'

(test defconstant
  "Test for the `defconstant' standard macro syntax."

  (syntax-test-cases (defconstant)
    '((defconstant)            invalid-syntax-error)
    '((defconstant 1)          invalid-syntax-error)
    '((defconstant foo 1 2)    invalid-syntax-error)
    '((defconstant foo 1 "" 2) invalid-syntax-error)

    '((defconstant foo
        (bar 1)
        "bla")
      (name foo syntax::initial-value (bar 1) documentation "bla"))))

(test defvar
  "Test for the `defvar' standard macro syntax."

  (syntax-test-cases (defvar)
    '((defvar)            invalid-syntax-error)
    '((defvar 1)          invalid-syntax-error)
    '((defvar foo 1 2)    invalid-syntax-error)
    '((defvar foo 1 "" 2) invalid-syntax-error)

    '((defvar foo)
      (name foo syntax::initial-value nil documentation nil))
    '((defvar foo
        (bar 1)
        "bla")
      (name foo syntax::initial-value (bar 1) documentation "bla"))))

(test defparameter
  "Test for the `defparameter' standard macro syntax."

  (syntax-test-cases (defparameter)
    '((defparameter)            invalid-syntax-error)
    '((defparameter 1)          invalid-syntax-error)
    '((defparameter foo)        invalid-syntax-error)
    '((defparameter foo 1 2)    invalid-syntax-error)
    '((defparameter foo 1 "" 2) invalid-syntax-error)

    '((defparameter foo
        (bar 1)
        "bla")
      (name foo syntax::initial-value (bar 1) documentation "bla"))))

(test defun
  "Test for the `defun' standard macro syntax."

  (syntax-test-cases (defun)
    '((defun (setf 1) ())        invalid-syntax-error)
    '((defun foo () (declare 1)) invalid-syntax-error)

    '((defun foo (bar baz)
        "bla"
        (declare (type integer bar baz))
        (+ 1 2))
      (name foo syntax::lambda-list ((bar baz) () nil () nil ())
       documentation "bla"
       syntax::declarations ((type (integer bar baz)))
       syntax::forms ((+ 1 2))))))

(test defmacro
  "Test for the `defmacro' standard macro syntax."

  (syntax-test-cases (defmacro)
    '((defmacro)       invalid-syntax-error)
    '((defmacro 1)     invalid-syntax-error)
    '((defmacro foo 1) invalid-syntax-error)

    `((defmacro foo (a b)
        "bla"
        (declare (ignore a))
        (list 'cons b b))
      (name foo
       syntax::lambda-list (:destructuring-lambda-list
                            nil nil (a b) () nil () nil () nil)
       documentation "bla"
       syntax::declarations ((ignore (a)))
       syntax::forms ((list 'cons b b))))))

(test slot-specifier
  "Test for `slot-specifier' rule."

  (rule-test-cases ((syntax::slot-specifier syntax::special-operators))
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
      t t (name foo
           syntax::initargs ()
           syntax::readers (bar)
           syntax::writers ()
           syntax::accessors ()
           syntax::allocation nil
           syntax::initform (+ 1)
           type nil
           documentation nil
           syntax::option-names (:custom-option)
           syntax::option-values (:foo)))))

(test defclass
  "Test for `defclass' standard macro syntax."

  (syntax-test-cases (defclass)
    '((defclass)                                                   invalid-syntax-error)
    '((defclass 1)                                                 invalid-syntax-error)
    '((defclass foo 1)                                             invalid-syntax-error)
    '((defclass foo () () (:documentation 1))                      invalid-syntax-error)
    '((defclass foo () ((a :documentation 1)))                     invalid-syntax-error)
    ;; Repeated options
    '((defclass foo () () (:metaclass bar) (:metaclass bar))       invalid-syntax-error)
    '((defclass foo () () (:documentation "") (:documentation "")) invalid-syntax-error)
    ;; Valid syntax
    '((defclass foo (bar baz)
        ((foo :initform (+ 1) :custom-option :foo :reader bar))
        (:metaclass foo)
        (:default-initargs
         :bar 1)
        (:my-class-option 1))
      (name foo syntax::superclasses (bar baz)
       syntax::slots ((name foo
                       syntax::initargs ()
                       syntax::readers (bar)
                       syntax::writers ()
                       syntax::accessors ()
                       syntax::allocation nil
                       syntax::initform (+ 1)
                       type nil
                       documentation nil
                       syntax::option-names (:custom-option)
                       syntax::option-values (:foo)))
       syntax::default-initargs (:bar)
       syntax::default-initforms (1)
       syntax::metaclass foo
       documentation nil
       syntax::option-names (:my-class-option) syntax::option-values ((1))))))

(test deftype
  "Test for `deftype' standard macro syntax."

  (syntax-test-cases (deftype)
    '((deftype)       invalid-syntax-error)
    '((deftype 1)     invalid-syntax-error)
    '((deftype foo)   invalid-syntax-error)
    '((deftype foo 1) invalid-syntax-error)

    '((deftype foo (a &key b)
        "bla bli"
        (declare (ignore a))
        (declare (ignore b))
        (list a b))
      (name foo
       syntax::lambda-list (:destructuring-lambda-list
                            nil nil (a) () nil (((keyword b) b nil nil)) nil () nil)
       documentation "bla bli"
       syntax::declarations ((ignore (a)) (ignore (b)))
       syntax::forms ((list a b))))))

(test defgeneric
  "Test for `defgeneric' standard macro syntax."

  (syntax-test-cases (defgeneric)
    '((defgeneric foo ()
        (:generic-function-class "foo"))
      invalid-syntax-error)
    ;; Repeated options
    '((defgeneric foo ()
        (:generic-function-class bar)
        (:generic-function-class bar))
      invalid-syntax-error)
    '((defgeneric foo ()
        (:argument-precedence-order :most-specific-first)
        (:argument-precedence-order :most-specific-first))
      invalid-syntax-error)
    '((defgeneric foo ()
        (:documentation "foo")
        (:documentation "foo"))
      invalid-syntax-error)
    ;; Valid syntax
    '((defgeneric foo (a b)
        (:documentation "foo")
        (:generic-function-class clazz))
      (name foo syntax::lambda-list ((a b) () nil () nil)
       syntax::generic-function-class clazz
       syntax::argument-precedence-order nil
       documentation "foo"
       syntax::option-names () syntax::option-values ()))))

(test defpackage
  "Test for the `defpackage' standard macro syntax."

  (syntax-test-cases (defpackage)
    '((defpackage 1)                                           invalid-syntax-error)
    '((defpackage foo 2)                                       invalid-syntax-error)
    '((defpackage foo (:documentation 1))                      invalid-syntax-error)
    '((defpackage foo (:size "a"))                             invalid-syntax-error)
    ;; Repeated options
    '((defpackage foo (:documentation "") (:documentation "")) invalid-syntax-error)
    '((defpackage foo (:size 1) (:size 2))                     invalid-syntax-error)
    ;; Valid syntax
    '((defpackage foo
        (:documentation "bla")
        (:use :bar "bar")
        (:size 1)
        (:import-from :foo #\c :bar)
        (:shadowing-import-from :foo2 "BAZ2" :bar2))
      (name foo syntax::nicknames () documentation "bla" syntax::use (:bar "bar")
       shadow () syntax::shadowing-import-from-packages (:foo2)
       syntax::shadowing-import-from-names (("BAZ2" :bar2))
       syntax::import-from-packages (:foo)
       syntax::import-from-names ((#\c :bar))
       export () intern () syntax::size 1))

    '((defpackage foo
        (:documentation "bla")
        (:use :bar "bar")
        (:size 1)
        (:import-from :foo :bar :baz)
        (:shadowing-import-from :foo2 "BAZ2" :bar2))
      (name foo syntax::nicknames () documentation "bla" syntax::use (:bar "bar")
       shadow () syntax::shadowing-import-from-packages (:foo2)
       syntax::shadowing-import-from-names (("BAZ2" :bar2))
       syntax::import-from-packages (:foo)
       syntax::import-from-names ((:bar :baz))
       export () intern () syntax::size 1))))

(test in-package
  "test for the `in-package' standard macro syntax."

  (syntax-test-cases (in-package)
    '((in-package)       invalid-syntax-error)
    '((in-package 1)     invalid-syntax-error)
    '((in-package foo 1) invalid-syntax-error)

    '((in-package foo)   (name foo))
    '((in-package "FOO") (name "FOO"))))
