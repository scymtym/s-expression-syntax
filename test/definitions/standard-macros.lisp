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

  '(#5=(defconstant #6=foo
         (bar 1)
         "bla")
    (:defconstant
     ((:name . 1)          (((:variable-name () :name foo :source #6#)))
      (:initial-value . 1) (((bar 1)))
      :documentation       (("bla")))
     :source #5#)))

(define-macro-test (defvar)
  '((defvar)            syn:invalid-syntax-error)
  '((defvar 1)          syn:invalid-syntax-error)
  '((defvar foo 1 2)    syn:invalid-syntax-error)
  '((defvar foo 1 "" 2) syn:invalid-syntax-error)

  '(#5=(defvar #6=foo)
    (:defvar
     ((:name . 1) (((:variable-name () :name foo :source #6#))))
     :source #5#))
  '(#7=(defvar #8=foo
         (bar 1)
         "bla")
    (:defvar
     ((:name . 1)    (((:variable-name () :name foo :source #8#)))
      :initial-value (((bar 1)))
      :documentation (("bla")))
     :source #7#)))

(define-macro-test (defparameter)
  '((defparameter)            syn:invalid-syntax-error)
  '((defparameter 1)          syn:invalid-syntax-error)
  '((defparameter foo)        syn:invalid-syntax-error)
  '((defparameter foo 1 2)    syn:invalid-syntax-error)
  '((defparameter foo 1 "" 2) syn:invalid-syntax-error)

  '(#6=(defparameter #7=foo
         (bar 1)
         "bla")
    (:defparameter
     ((:name . 1)          (((:variable-name () :name foo :source #7#)))
      (:initial-value . 1) (((bar 1)))
      :documentation       (("bla")))
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
  '(#6=(defun #7=foo #8=(#9=bar #10=baz)
         "bla"
         (declare #11=(type integer #12=bar #13=baz))
         (+ 1 2))
    (:defun
     ((:name . 1)        (((:function-name () :name foo :source #7#)))
      (:lambda-list . 1) (((:ordinary-lambda-list
                            (:required (((:required-parameter
                                          ((:name . 1) (((:variable-name
                                                          ()
                                                          :name bar :source #9#))))
                                          :source #9#))
                                        ((:required-parameter
                                          ((:name . 1) (((:variable-name
                                                          ()
                                                          :name baz :source #10#))))
                                          :source #10#))))
                            :source #8#)))
      :documentation     (("bla"))
      :declarations      (((:declaration
                            (:argument ((integer)
                                        ((:variable-name () :name bar :source #12#))
                                        ((:variable-name () :name baz :source #13#))))
                            :kind type :source #11#)))
      :forms             (((+ 1 2))))
     :source #6#)))

(define-macro-test (defmacro)
  '((defmacro)       syn:invalid-syntax-error)
  '((defmacro 1)     syn:invalid-syntax-error)
  '((defmacro foo 1)
    syn:invalid-syntax-error 1 "must be a destructuring lambda list")
  '((defmacro foo (x (y #4=x)))
    syn:invalid-syntax-error #4# "must be a lambda list variable name")
  ;; Valid syntax
  `(#5=(defmacro #6=foo #7=(#8=a #9=b)
         "bla"
         (declare #10=(ignore #11=a))
         (list 'cons b b))
    (:defmacro
     ((:name . 1)        (((:function-name () :name foo :source #6#)))
      (:lambda-list . 1) (((:destructuring-lambda-list
                            (:required (((:required-parameter
                                          ((:name . 1) (((:variable-name
                                                          ()
                                                          :name a :source #8#))))
                                          :source #8#))
                                        ((:required-parameter
                                          ((:name . 1) (((:variable-name
                                                          ()
                                                          :name b :source #9#))))
                                          :source #9#))))
                            :source #7#)))
      :documentation     (("bla"))
      :declarations      (((:declaration
                            (:argument (((:variable-name () :name a :source #11#))))
                            :kind ignore :source #10#)))
      :forms             (((list 'cons b b))))
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
     ((:name . 1) (((:variable-name () :name foo :source #6#))))
     :source #6#))
  '(#7=(#8=foo 1 :type bit :read-only t)
    (:slot-description
     ((:name . 1) (((:variable-name () :name foo :source #8#)))
      :initform   ((1))
      :read-only  ((t))
      :type       ((bit)))
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
  '(#10=(defstruct #11=foo)
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #11#))))
     :source #10#))
  '(#12=(defstruct (#13=foo))
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #13#))))
     :source #12#))
  '(#14=(defstruct (#15=foo (:constructor nil)))
    (:defstruct
     ((:name . 1)   (((:type-name () :name foo :source #15#)))
      :constructors (((nil nil))))
     :source #14#))
  '(#16=(defstruct (#17=foo (:constructor #18=foo #19=(#20=a #21=b))))
    (:defstruct
     ((:name . 1)   (((:type-name () :name foo :source #17#)))
      :constructors ((((:function-name () :name foo :source #18#)
                       (:ordinary-lambda-list
                        (:required (((:required-parameter
                                      ((:name . 1) (((:variable-name
                                                      ()
                                                      :name a :source #20#))))
                                      :source #20#))
                                    ((:required-parameter
                                      ((:name . 1) (((:variable-name
                                                      ()
                                                      :name b :source #21#))))
                                      :source #21#))))
                        :source #19#)))))
     :source #16#))
  '(#22=(defstruct #23=foo "doc")
    (:defstruct
     ((:name . 1)    (((:type-name () :name foo :source #23#)))
      :documentation (("doc")))
     :source #22#)))

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
    '(#15=(#16=foo :initform (+ 1) :custom-option :foo :reader #17=bar)
      t t (:slot-specifier
           ((:name . 1)    (((:variable-name () :name foo :source #16#)))
            :readers       (((:function-name () :name bar :source #17#)))
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
  '(#14=(defclass #15=foo (#16=bar #17=baz)
         (#18=(#19=foo :initform (+ 1) :custom-option :foo :reader #20=bar))
         (:metaclass #21=foo)
         (:documentation "foo")
         (:default-initargs
          :bar 1)
         (:my-class-option 1))
    (:defclass
     ((:name . 1)        (((:type-name () :name foo :source #15#)))
      :superclasses      (((:type-name () :name bar :source #16#))
                          ((:type-name () :name baz :source #17#)))
      :slots             (((:slot-specifier
                            ((:name . 1)    (((:variable-name () :name foo :source #19#)))
                             :readers       (((:function-name () :name bar :source #20#)))
                             :initform      (((+ 1)))
                             :option-names  ((:custom-option))
                             :option-values ((:foo)))
                            :source #18#)))
      :default-initargs  ((:bar))
      :default-initforms ((1))
      :metaclass         (((:type-name () :name foo :source #21#)))
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
  '(#6=(deftype #7=foo #8=(#9=a &key #10=b)
         "bla bli"
         (declare #11=(ignore #12=a))
         (declare #13=(ignore #14=b))
         (list a b))
    (:deftype
     ((:name . 1)        (((:type-name () :name foo :source #7#)))
      (:lambda-list . 1) (((:destructuring-lambda-list
                            (:required (((:required-parameter
                                          ((:name . 1) (((:variable-name
                                                          ()
                                                          :name a :source #9#))))
                                          :source #9#)))
                             :keyword  (((:keyword-parameter
                                          (:name (((:variable-name
                                                    ()
                                                    :name b :source #10#))))
                                          :source #10#))))
                            :source #8#)))
      :documentation     (("bla bli"))
      :declarations      (((:declaration
                            (:argument (((:variable-name () :name a :source #12#))))
                            :kind ignore :source #11#))
                          ((:declaration
                            (:argument (((:variable-name () :name b :source #14#))))
                            :kind ignore :source #13#)))
      :forms             (((list a b))))
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
  '(#14=(defgeneric #15=foo #16=(#17=a #18=b)
         (:documentation "foo")
         (:generic-function-class #19=clazz))
    (:defgeneric
     ((:name . 1)                (((:function-name () :name foo :source #15#)))
      (:lambda-list . 1)         (((:generic-function-lambda-list
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
                                    :source #16#)))
      :generic-function-class    (((:type-name () :name clazz :source #19#)))
      :documentation             (("foo")))
     :source #14#))
  '(#20=(defgeneric #21=foo #22=(#23=a #24=b)
          (:argument-precedence-order b a))
    (:defgeneric
     ((:name . 1)                (((:function-name () :name foo :source #21#)))
      (:lambda-list . 1)         (((:generic-function-lambda-list
                                    (:required (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name a :source #21#))))
                                                  :source #21#))
                                                ((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #22#))))
                                                  :source #22#))))
                                    :source #20#)))
      :argument-precedence-order (((b a))))
     :source #18#))
  '(#25=(defgeneric #26=foo #27=(#28=a)
          #29=(:method :custom 1 "foo" #30=(#31=a)))
    (:defgeneric
     (:name        (((:function-name () :name foo :source #26#)))
      :lambda-list (((:generic-function-lambda-list
                      (:required (((:required-parameter
                                    (:name ((a)))
                                    :source #28#))))
                      :source #27#)))
      :methods     (((:method-description
                      (:qualifiers  ((:custom) (1) ("foo"))
                       :lambda-list (((:specialized-lambda-list
                                       (:required (((:specialized-parameter
                                                     (:name (((:required-parameter
                                                               (:name ((a)))
                                                               :source #31#))))
                                                     :source #31#))))
                                       :source #30#))))
                      :source #29#))))
     :source #25#)))

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
  '(#7=(defmethod #8=foo #9=())
    (:defmethod
     ((:name . 1)        (((:function-name () :name foo :source #8#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #9#))))
     :source #7#))
  '(#10=(defmethod #11=foo :around #12=())
    (:defmethod
     ((:name . 1)        (((:function-name () :name foo :source #9#)))
      :qualifiers        ((:around))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #10#))))
     :source #7#))
  '(#13=(defmethod #14=foo :custom 1 "foo" #15=())
    (:defmethod
     (:name        (((:function-name () :name foo :source #14#)))
      :qualifiers  ((:custom) (1) ("foo"))
      :lambda-list (((:specialized-lambda-list () :source #15#))))
     :source #13#))
  '(#16=(defmethod #17=foo #18=(#19=(#20=x #21=t)))
    (:defmethod
     ((:name . 1)        (((:function-name () :name foo :source #17#)))
      (:lambda-list . 1) (((:specialized-lambda-list
                            (:required (((:specialized-parameter
                                          (:name        (((:variable-name
                                                           ()
                                                           :name x :source #20#)))
                                           :specializer (((:type-name
                                                           ()
                                                           :name t :source #21#))))
                                          :source #19#))))
                            :source #18#))))
     :source #16#))
  '(#22=(defmethod #23=foo #24=()
          "foo" (declare #25=(ignore)) 1)
    (:defmethod
     ((:name . 1)        (((:function-name () :name foo :source #23#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #24#)))
      :documentation     (("foo"))
      :declarations      (((:declaration () :kind ignore :source #25#)))
      :forms             ((1)))
      :source #22#)))

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
  '(#8=(defpackage #9=foo
         (:documentation "bla")
         (:use #10=:bar #11="bar")
         (:size 1)
         #12=(:import-from #13=:foo #14=#\c #15=:bar)
         #16=(:shadowing-import-from #17=:foo2 #18="BAZ2" #19=:bar2))
    (:defpackage
     ((:name . 1)            (((:string-designator
                                ()
                                :string "FOO" :source #9#)))
      :documentation         (("bla"))
      :use                   (((:string-designator
                                ()
                                :string "BAR" :source #10#))
                              ((:string-designator
                                ()
                                :string "bar" :source #11#)))
      :shadowing-import-from (((:import-from
                                ((:package . 1) (((:string-designator
                                                   ()
                                                   :string "FOO2" :source #17#)))
                                 :names         (((:string-designator
                                                   ()
                                                   :string "BAZ2" :source #18#))
                                                 ((:string-designator
                                                   ()
                                                   :string "BAR2" :source #19#))))
                                :source #16#)))
      :import-from           (((:import-from
                                ((:package . 1) (((:string-designator
                                                   ()
                                                   :string "FOO" :source #13#)))
                                 :names         (((:string-designator
                                                   ()
                                                   :string "c" :source #14#))
                                                 ((:string-designator
                                                   ()
                                                   :string "BAR" :source #15#))))
                                :source #12#)))
      :size                  ((1)))
     :source #8#))

  '(#20=(defpackage #21=foo
          (:nicknames #22="f" #23=:fo)
          (:export #24="bar" #25=:baz))
    (:defpackage
     ((:name . 1)(((:string-designator () :string "FOO" :source #21#)))
      :nicknames (((:string-designator
                    ()
                    :string "f" :source #22#))
                  ((:string-designator
                    ()
                    :string "FO" :source #23#)))
      :export    (((:string-designator
                    ()
                    :string "bar" :source #24#))
                  ((:string-designator
                    ()
                    :string "BAZ" :source #25#))))
     :source #20#)))

(define-macro-test (in-package)
  '((in-package)          syn:invalid-syntax-error)
  '((in-package 1)        syn:invalid-syntax-error)
  '((in-package foo 1)    syn:invalid-syntax-error)

  '(#4=(in-package #5=foo)
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #5#))))
     :source #4#))
  '(#6=(in-package #7="FOO")
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #7#))))
     :source #6#)))

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
         (bar (#6=x) (baz))
         (:no-error #7=(#8=y #9=z) (fez)))
    (:handler-case
     ((:form . 1)           (((foo)))
      :types                ((bar))
      :variables            (((:required-parameter
                               ((:name . 1) (((:variable-name
                                               ()
                                               :name x :source #6#))))
                               :source #6#)))
      :forms                ((((baz))))
      :no-error-lambda-list (((:ordinary-lambda-list
                               (:required (((:required-parameter
                                             ((:name . 1) (((:variable-name
                                                             ()
                                                             :name y :source #8#))))
                                             :source #8#))
                                           ((:required-parameter
                                             ((:name . 1) (((:variable-name
                                                             ()
                                                             :name z :source #9#))))
                                             :source #9#))))
                               :source #7#)))
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
  '(#6=(restart-bind ((#7=foo bar :report-function baz)) 1)
    (:restart-bind
     (:names            (((:variable-name () :naem foo :source #7#)))
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
  '((restart-case 1 (nil ()))
    syn:invalid-syntax-error nil "for an unnamed restart, the :REPORT option must be supplied")
  ;; Valid syntax
  '(#5=(restart-case 1)
    (:restart-case (:form ((1))) :source #5#))
  '(#6=(restart-case 1 (#7=foo #8=(x) :report "bar" :test baz))
    (:restart-case
     (:form         ((1))
      :names        (((:variable-name () :name foo :source #7#)))
      :lambda-lists (((:ordinary-lambda-list
                       (:required ((x)))
                       :source #8#)))
      :reports      (("bar"))
      :tests        ((baz)))
     :source #6#)))
