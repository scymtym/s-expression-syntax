(cl:in-package #:syntax.test)

(def-suite* :syntax.standard-macros
  :in :syntax)

(test defun
  "Test for the `defun' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list ((baz bar) nil nil nil nil)
               documentation "bla"
               syntax::declarations ((type integer bar baz))
               syntax::forms ((+ 1 2)))
             (parse t (find-syntax 'defun)
                    '(defun foo (bar baz)
                      "bla"
                      (declare (type integer bar baz))
                      (+ 1 2))))))

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

(test defgeneric
  "Test for `defgeneric' standard macro syntax."

  (is (equal '(name foo
               syntax::lambda-list ((b a) nil nil nil nil)
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
               syntax::import-from-names ((#\c :bar))
               export ()
               intern ()
               syntax::size 1)
             (parse t (find-syntax 'defpackage) '(defpackage foo
                                                  (:documentation "bla")
                                                  (:use :bar "bar")
                                                  (:size 1)
                                                  (:import-from :foo #\c :bar)
                                                  (:shadowing-import-from :foo2 "BAZ2" :bar2))))))
