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

  '(#5=(defconstant foo
         (bar 1)
         "bla")
    (:defconstant
     (:name          ((foo))
      :initial-value (((bar 1)))
      :documentation (("bla")))
     :source #5#)))

(define-macro-test (defvar)
  '((defvar)            syn:invalid-syntax-error)
  '((defvar 1)          syn:invalid-syntax-error)
  '((defvar foo 1 2)    syn:invalid-syntax-error)
  '((defvar foo 1 "" 2) syn:invalid-syntax-error)

  '(#5=(defvar foo)
    (:defvar (:name ((foo))) :source #5#))
  '(#6=(defvar foo
         (bar 1)
         "bla")
    (:defvar
     (:name          ((foo))
      :initial-value (((bar 1)))
      :documentation (("bla")))
     :source #6#)))

(define-macro-test (defparameter)
  '((defparameter)            syn:invalid-syntax-error)
  '((defparameter 1)          syn:invalid-syntax-error)
  '((defparameter foo)        syn:invalid-syntax-error)
  '((defparameter foo 1 2)    syn:invalid-syntax-error)
  '((defparameter foo 1 "" 2) syn:invalid-syntax-error)

  '(#6=(defparameter foo
         (bar 1)
         "bla")
    (:defparameter
     (:name          ((foo))
      :initial-value (((bar 1)))
      :documentation (("bla")))
     :source #6#)))

(define-macro-test (defun)
  '((defun (setf 1) ())
    syn:invalid-syntax-error)
  '((defun foo 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '((defun foo (x #3=x))
    syn:invalid-syntax-error #3# "must be a lambda list variable name")
  #+TODO '((defun foo ((a b)))
           syn:invalid-syntax-error) ; this should fail but doesn't because of the way grammars use each other
  '((defun foo () (declare 1))
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#6=(defun foo #7=(bar baz)
         "bla"
         (declare #8=(type integer bar baz))
         (+ 1 2))
    (:defun
     (:name ((foo))
      :lambda-list   (((:ordinary-lambda-list
                        (:required ((bar) (baz)))
                        :source #7#)))
      :documentation (("bla"))
      :declarations  (((:declaration
                        (:argument ((integer) (bar) (baz)))
                        :kind type :source #8#)))
      :forms         (((+ 1 2))))
     :source #6#)))

(define-macro-test (defmacro)
  '((defmacro)       syn:invalid-syntax-error)
  '((defmacro 1)     syn:invalid-syntax-error)
  '((defmacro foo 1)
    syn:invalid-syntax-error 1 "must be a destructuring lambda list")
  '((defmacro foo (x (y #4=x)))
    syn:invalid-syntax-error #4# "must be a lambda list variable name")
  ;; Valid syntax
  `(#5=(defmacro foo #6=(a b)
         "bla"
         (declare #7=(ignore a))
         (list 'cons b b))
    (:defmacro
     (:name          ((foo))
      :lambda-list   (((:destructuring-lambda-list
                        (:required ((a) (b)))
                        :source #6#)))
      :documentation (("bla"))
      :declarations  (((:declaration
                        (:argument ((a)))
                        :kind ignore :source #7#)))
      :forms         (((list 'cons b b))))
     :source #5#)))

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
  '(#6=foo
    (:slot-description
     (:name ((foo)))
     :source #6#))
  '(#7=(foo 1 :type bit :read-only t)
    (:slot-description
     (:name      ((foo))
      :initform  ((1))
      :read-only ((t))
      :type      ((bit)))
     :source #7#)))

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
  '(#10=(defstruct foo)
    (:defstruct (:name ((foo))) :source #10#))
  '(#11=(defstruct (foo))
    (:defstruct (:name ((foo))) :source #11#))
  '(#12=(defstruct (foo (:constructor nil)))
    (:defstruct
     (:name         ((foo))
      :constructors (((nil nil))))
    :source #12#))
  '(#13=(defstruct (foo (:constructor foo #14=(a b))))
    (:defstruct
        (:name         ((foo))
         :constructors (((foo
                          (:ordinary-lambda-list
                           (:required ((a) (b)))
                           :source #14#)))))
      :source #13#))
  '(#15=(defstruct foo "doc")
    (:defstruct
     (:name          ((foo))
      :documentation (("doc")))
     :source #15#)))

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
    '(#15=(foo :initform (+ 1) :custom-option :foo :reader bar)
      t t (:slot-specifier
           (:name          ((foo))
            :readers       ((bar))
            :initform      (((+ 1)))
            :option-names  ((:custom-option))
            :option-values ((:foo)))
           :source #15#))))

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
  '(#14=(defclass foo (bar baz)
         (#15=(foo :initform (+ 1) :custom-option :foo :reader bar))
         (:metaclass foo)
         (:documentation "foo")
         (:default-initargs
          :bar 1)
         (:my-class-option 1))
    (:defclass
     (:name              ((foo))
      :superclasses      ((bar) (baz))
      :slots             (((:slot-specifier
                            (:name          ((foo))
                             :readers       ((bar))
                             :initform      (((+ 1)))
                             :option-names  ((:custom-option))
                             :option-values ((:foo)))
                            :source #15#)))
      :default-initargs  ((:bar))
      :default-initforms ((1))
      :metaclass         ((foo))
      :documentation     (("foo"))
      :option-names      ((:my-class-option))
      :option-values     (((1))))
     :source #14#)))

(define-macro-test (deftype)
  '((deftype)       syn:invalid-syntax-error)
  '((deftype 1)     syn:invalid-syntax-error)
  '((deftype foo)   syn:invalid-syntax-error)
  '((deftype foo 1) syn:invalid-syntax-error 1 "must be a DEFTYPE lambda list")
  '((deftype foo (x #5=x))
    syn:invalid-syntax-error #5# "must be a lambda list variable name")
  ;; Valid syntax
  '(#6=(deftype foo #7=(a &key b)
         "bla bli"
         (declare #8=(ignore a))
         (declare #9=(ignore b))
         (list a b))
    (:deftype
     (:name          ((foo))
      :lambda-list   (((:destructuring-lambda-list
                        (:required ((a))
                         :keyword  ((((keyword b) b nil nil))))
                        :source #7#)))
      :documentation (("bla bli"))
      :declarations  (((:declaration
                        (:argument ((a)))
                        :kind ignore :source #8#))
                      ((:declaration
                        (:argument ((b)))
                        :kind ignore :source #9#)))
      :forms         (((list a b))))
     :source #6#)))

(define-macro-test (defgeneric)
  '((defgeneric foo #1=1)
    syn:invalid-syntax-error #1# "must be a generic function lambda list")
  #+TODO '((defgeneric foo #2=(&key (a 1)))
           syn:invalid-syntax-error #2# "must be a generic function lambda list")
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
  '(#14=(defgeneric foo #15=(a b)
         (:documentation "foo")
         (:generic-function-class clazz))
    (:defgeneric
     (:name                      ((foo))
      :lambda-list               (((:generic-function-lambda-list
                                    (:required ((a) (b)))
                                    :source #15#)))
      :generic-function-class    ((clazz))
      :documentation             (("foo")))
     :source #14#))
  '(#16=(defgeneric foo #17=(a b)
          (:argument-precedence-order b a))
    (:defgeneric
     (:name                      ((foo))
      :lambda-list               (((:generic-function-lambda-list
                                    (:required ((a) (b)))
                                    :source #17#)))
      :argument-precedence-order (((b a))))
      :source #16#))
  '(#18=(defgeneric foo #19=(a)
          #20=(:method :custom 1 "foo" #21=(a)))
    (:defgeneric
     (:name        ((foo))
      :lambda-list (((:generic-function-lambda-list
                      (:required ((a)))
                      :source #19#)))
      :methods     (((:method-description
                      (:qualifiers  ((:custom) (1) ("foo"))
                       :lambda-list (((:specialized-lambda-list
                                       (:required (((a nil))))
                                       :source #21#))))
                      :source #20#))))
     :source #18#)))

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
  '(#7=(defmethod foo #8=())
    (:defmethod
     (:name        ((foo))
      :lambda-list (((:specialized-lambda-list () :source #8#))))
     :source #7#))
  '(#9=(defmethod foo :around #10=())
    (:defmethod
     (:name        ((foo))
      :qualifiers  ((:around))
      :lambda-list (((:specialized-lambda-list () :source #10#))))
     :source #9#))
  '(#11=(defmethod foo :custom 1 "foo" #12=())
    (:defmethod
     (:name        ((foo))
      :qualifiers  ((:custom) (1) ("foo"))
      :lambda-list (((:specialized-lambda-list
                      ()
                      :source #12#))))
     :source #11#))
  '(#13=(defmethod foo #14=((x t)))
    (:defmethod
     (:name        ((foo))
      :lambda-list (((:specialized-lambda-list
                      (:required (((x t))))
                      :source #14#))))
     :source #13#))
  '(#15=(defmethod foo #16=()
          "foo" (declare #17=(ignore)) 1)
    (:defmethod
     (:name          ((foo))
      :lambda-list   (((:specialized-lambda-list () :source #16#)))
      :documentation (("foo"))
      :declarations  (((:declaration () :kind ignore :source #17#)))
      :forms         ((1)))
     :source #15#)))

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
  '(#8=(defpackage foo
         (:documentation "bla")
         (:use :bar "bar")
         (:size 1)
         (:import-from :foo #\c :bar)
         (:shadowing-import-from :foo2 "BAZ2" :bar2))
    (:defpackage
     (:name                           ((foo))
      :documentation                  (("bla"))
      :use                            ((:bar) ("bar"))
      :shadowing-import-from-packages ((:foo2))
      :shadowing-import-from-names    ((("BAZ2" :bar2)))
      :import-from-packages           ((:foo))
      :import-from-names              (((#\c :bar)))
      :size                           ((1)))
     :source #8#))

  '(#9=(defpackage foo
         (:documentation "bla")
         (:use :bar "bar")
         (:size 1)
         (:import-from :foo :bar :baz)
         (:shadowing-import-from :foo2 "BAZ2" :bar2))
    (:defpackage
     (:name                           ((foo))
      :documentation                  (("bla"))
      :use                            ((:bar) ("bar"))
      :shadowing-import-from-packages ((:foo2))
      :shadowing-import-from-names    ((("BAZ2" :bar2)))
      :import-from-packages           ((:foo))
      :import-from-names              (((:bar :baz)))
      :size                           ((1)))
     :source #9#)))

(define-macro-test (in-package)
  '((in-package)          syn:invalid-syntax-error)
  '((in-package 1)        syn:invalid-syntax-error)
  '((in-package foo 1)    syn:invalid-syntax-error)

  '(#4=(in-package foo)   (:in-package (:name ((foo)))   :source #4#))
  '(#5=(in-package "FOO") (:in-package (:name (("FOO"))) :source #5#)))


;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-macro-test (handler-bind)
  '((handler-bind)
    syn:invalid-syntax-error)
  '((handler-bind (#2=1))
    syn:invalid-syntax-error #2# "must be of the form (TYPE HANDLER-FORM)")
  '((handler-bind ((#3=1 (lambda (x)))))
    syn:invalid-syntax-error #3# "must be a type specifier")
  ;; Valid syntax
  '(#4=(handler-bind ((foo (lambda (x) (bar))))
         (baz))
    (:handler-bind
     (:types         ((foo))
      :handler-forms (((lambda (x) (bar))))
      :forms         (((baz))))
     :source #4#)))

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
  '(#5=(handler-case (foo)
         (bar (x) (baz))
         (:no-error #6=(y z) (fez)))
    (:handler-case
     (:form                 (((foo)))
      :types                ((bar))
      :variables            ((x))
      :forms                ((((baz))))
      :no-error-lambda-list (((:ordinary-lambda-list
                               (:required ((y) (z)))
                               :source #6#)))
      :no-error-forms       (((fez))))
     :source #5#)))

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
  '(#5=(restart-bind () 1)
    (:restart-bind (:forms ((1))) :source #5#))
  '(#6=(restart-bind ((foo bar :report-function baz)) 1)
    (:restart-bind
     (:names            ((foo))
      :functions        ((bar))
      :report-functions ((baz))
      :forms            ((1)))
     :source #6#)))

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
  '(#5=(restart-case 1)
    (:restart-case (:form ((1))) :source #5#))
  '(#6=(restart-case 1 (foo #7=(x) :report "bar" :test baz))
    (:restart-case
     (:form         ((1))
      :names        ((foo))
      :lambda-lists (((:ordinary-lambda-list
                       (:required ((x)))
                       :source #7#)))
      :reports      (("bar"))
      :tests        ((baz)))
     :source #6#)))
