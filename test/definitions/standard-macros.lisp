(cl:in-package #:syntax.test)

(def-suite* :syntax.standard-macros
  :in :syntax)

;;; `defconstant', `defvar' and `defparameter'

(test defconstant
  "Test for the `defconstant' standard macro syntax."

  (is (equal '(name foo
               syntax::initial-value (bar 1)
               documentation "bla")
             (parse t (find-syntax 'defconstant)
                    '(defconstant foo
                      (bar 1)
                      "bla")))))

(test defvar
  "Test for the `defvar' standard macro syntax."

  (is (equal '(name foo
               syntax::initial-value (bar 1)
               documentation "bla")
             (parse t (find-syntax 'defvar)
                    '(defvar foo
                      (bar 1)
                      "bla")))))

(test defparameter
  "Test for the `defparameter' standard macro syntax."

  (is (equal '(name foo
               syntax::initial-value (bar 1)
               documentation "bla")
             (parse t (find-syntax 'defparameter)
                    '(defparameter foo
                      (bar 1)
                      "bla")))))

(test defun
  "Test for the `defun' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list ((bar baz) nil nil nil nil)
               documentation "bla"
               syntax::declarations ((type integer bar baz))
               syntax::forms ((+ 1 2)))
             (parse t (find-syntax 'defun)
                    '(defun foo (bar baz)
                      "bla"
                      (declare (type integer bar baz))
                      (+ 1 2))))))

(test defmacro
  "Test for the `defmacro' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list (() (a b) () () () () ())
               documentation "bla"
               syntax::declarations ((ignore a))
               syntax::forms ((list 'cons b b)))
             (parse t (find-syntax 'defmacro)
                    '(defmacro foo (a b)
                      "bla"
                      (declare (ignore a))
                      (list 'cons b b))))))

(test defclass
  "Test for `defclass' standard macro syntax."

  (is (equal '(name foo
               syntax::superclasses (bar baz)
               syntax::slots ((name foo
                               syntax::options (syntax::readers (bar)
                                                syntax::writers ()
                                                syntax::accessor ()
                                                syntax::allocation nil
                                                syntax::initform (+ 1)
                                                type nil
                                                documentation nil
                                                syntax::option-names (:custom-option)
                                                syntax::option-values (:foo))))
               syntax::default-initargs (:bar)
               syntax::default-initforms (1)
               syntax::metaclass foo
               documentation nil
               syntax::option-names (:my-class-option)
               syntax::option-values ((1)))
             (parse t (find-syntax 'defclass)
                    '(defclass foo (bar baz)
                      ((foo :initform (+ 1) :custom-option :foo :reader bar))
                      (:metaclass foo)
                      (:default-initargs
                       :bar 1)
                      (:my-class-option 1))))))

(test deftype
  "Test for `deftype' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list (nil (a) () () ((:b b nil nil)) () ())
               documentation "bla bli"
               syntax::declarations ((ignore a) (ignore b))
               syntax::forms ((list a b)))

             (parse nil (find-syntax 'deftype)
                    '(deftype foo (a &key b)
                      "bla bli"
                      (declare (ignore a))
                      (declare (ignore b))
                      (list a b))))))

(test defgeneric
  "Test for `defgeneric' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list ((a b) nil nil nil nil)
               syntax::generic-function-class clazz
               syntax::argument-precedence-order nil
               documentation "foo"
               syntax::option-names ()
               syntax::option-values ())
             (parse t (find-syntax 'defgeneric)
                    '(defgeneric foo (a b)
                      (:documentation "foo")
                      (:generic-function-class clazz)))))

  (signals invalid-syntax-error
    (parse t (find-syntax 'defgeneric)
           '(defgeneric foo ()
             (:generic-function-class "foo")))))

(test defpackage
  "Test for the `defpackage' standard macro syntax."

  (is (equal '(name foo
               syntax::nicknames ()
               documentation "bla"
               syntax::use (:bar "bar")
               shadow ()
               syntax::shadowing-import-from-packages (:foo2)
               syntax::shadowing-import-from-names (("BAZ2" :bar2))
               syntax::import-from-packages (:foo)
               syntax::import-from-names ((:bar :baz))
               export ()
               intern ()
               syntax::size 1)
             (parse t (find-syntax 'defpackage)
                    '(defpackage foo
                      (:documentation "bla")
                      (:use :bar "bar")
                      (:size 1)
                      (:import-from :foo :bar :baz)
                      (:shadowing-import-from :foo2 "BAZ2" :bar2)))))

  (signals invalid-syntax-error
    (parse t (find-syntax 'defpackage)
           '(defpackage foo
             (:documentation "bla")
             (:use :bar "bar")
             (:size 1)
             (:import-from :foo #\c :bar)
             (:shadowing-import-from :foo2 "BAZ2" :bar2)))))
