;;;; standard-macros.lisp --- Tests for standard macro rules.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
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

  '(#1=(defconstant #2=foo
         (bar 1)
         "bla")
    (:defconstant
     ((:name          . 1) (((:variable-name () :name foo :source #2#)))
      (:initial-value . 1) (((bar 1) :evaluation t))
      (:documentation . 1) (("bla")))
     :source #1#)))

(define-macro-test (defvar)
  '((defvar)            syn:invalid-syntax-error)
  '((defvar 1)          syn:invalid-syntax-error)
  '((defvar foo 1 2)    syn:invalid-syntax-error)
  '((defvar foo 1 "" 2) syn:invalid-syntax-error)

  '(#1=(defvar #2=foo)
    (:defvar
     ((:name . 1) (((:variable-name () :name foo :source #2#))))
     :source #1#))
  '(#3=(defvar #4=foo
         (bar 1)
         "bla")
    (:defvar
     ((:name          . 1) (((:variable-name () :name foo :source #4#)))
      (:initial-value . 1) (((bar 1) :evaluation t))
      (:documentation . 1) (("bla")))
     :source #3#)))

(define-macro-test (defparameter)
  '((defparameter)            syn:invalid-syntax-error)
  '((defparameter 1)          syn:invalid-syntax-error)
  '((defparameter foo)        syn:invalid-syntax-error)
  '((defparameter foo 1 2)    syn:invalid-syntax-error)
  '((defparameter foo 1 "" 2) syn:invalid-syntax-error)

  '(#1=(defparameter #2=foo
         (bar 1)
         "bla")
    (:defparameter
     ((:name          . 1) (((:variable-name () :name foo :source #2#)))
      (:initial-value . 1) (((bar 1) :evaluation t))
      (:documentation . 1) (("bla")))
     :source #1#)))

(define-macro-test (defun)
  '((defun (setf 1) ())
    syn:invalid-syntax-error)
  '((defun foo 1)
    syn:invalid-syntax-error 1 "must be an ordinary lambda list")
  '((defun foo (x #1=x))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  #+TODO '((defun foo ((a b)))
           syn:invalid-syntax-error) ; this should fail but doesn't because of the way grammars use each other
  '((defun foo () (declare 1))
    syn:invalid-syntax-error)
  ;; Valid syntax
  '(#2=(defun #3=foo #4=(#5=bar #6=baz)
         "bla"
         (declare #7=(type #8=integer #9=bar #10=baz))
         (+ 1 2))
    (:defun
     ((:name          . 1) (((:function-name () :name foo :source #3#)))
      (:lambda-list   . 1) (((:ordinary-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name bar :source #5#)
                                                                 :evaluation nil)))
                                                  :source #5#))
                                                ((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name baz :source #6#)
                                                                 :evaluation nil)))
                                                  :source #6#))))
                              :source #4#)
                             :evaluation :compound))
      (:documentation . 1)     (("bla"))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:atomic-type-specifier
                                                  ((:name . 1) (((:type-name
                                                                  ()
                                                                  :name integer :source #8#))))
                                                  :source #8#))
                                                ((:variable-name () :name bar :source #9#))
                                                ((:variable-name () :name baz :source #10#))))
                              :kind type :source #7#)))
      (:form          . *) (((+ 1 2) :evaluation t)))
     :source #2#)))

(define-macro-test (defmacro)
  '((defmacro)       syn:invalid-syntax-error)
  '((defmacro 1)     syn:invalid-syntax-error)
  '((defmacro foo 1)
    syn:invalid-syntax-error 1 "must be a destructuring lambda list")
  '((defmacro foo (x (y #1=x)))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  `(#2=(defmacro #3=foo #4=(#5=a #6=b)
         "bla"
         (declare #7=(ignore #8=a))
         (list 'cons b b))
    (:defmacro
     ((:name          . 1) (((:function-name () :name foo :source #3#)))
      (:lambda-list   . 1) (((:destructuring-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name a :source #5#)
                                                                 :evaluation nil)))
                                                  :source #5#)
                                                 :evaluation :compound)
                                                ((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #6#)
                                                                 :evaluation nil)))
                                                  :source #6#)
                                                 :evaluation :compound)))
                              :source #4#)
                             :evaluation :compound))
      (:documentation . 1) (("bla"))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:variable-name () :name a :source #8#))))
                              :kind ignore :source #7#)))
      (:form          . *) (((list 'cons b b) :evaluation t)))
     :source #2#)))

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
  '(#7=(#8=foo 1 :type #9=bit :read-only t)
    (:slot-description
     ((:name      . 1) (((:variable-name () :name foo :source #8#)))
      (:initform  . 1) ((1 :evaluation t))
      (:read-only . 1) ((t))
      (:type      . 1) (((:atomic-type-specifier
                          ((:name . 1) (((:type-name () :name bit :source #9#))))
                          :source #9#))))
     :source #7#)))

(define-macro-test (defstruct)
  '((defstruct)
    syn:invalid-syntax-error)
  '((defstruct #1=1)
    syn:invalid-syntax-error #1# "must be a class name")
  '((defstruct (foo (:constructor foo #2=1)))
    syn:invalid-syntax-error #2# "must be an ordinary lambda list")
  '((defstruct (foo (:constructor foo (x #3=x))))
    syn:invalid-syntax-error #3# "the variable name X occurs more than once")
  '((defstruct (foo (:constructor nil) #4=(:constructor bar)))
    syn:invalid-syntax-error #4# "(:constructor nil) and named constructors are mutually exclusive")
  '((defstruct (foo (:constructor . #5=(foo () 1))))
    syn:invalid-syntax-error #5# ":CONSTRUCTOR option accepts one value")
  ;; Repeated options
  '((defstruct (foo (:include bar) #6=(:include bar)))
    syn:invalid-syntax-error #6# ":INCLUDE option must not be repeated")
  '((defstruct (foo (:initial-offset 1) #7=(:initial-offset 1)))
    syn:invalid-syntax-error #7# ":INITIAL-OFFSET option must not be repeated")
  '((defstruct (foo (:type list) #8=(:type list)))
    syn:invalid-syntax-error #8# ":TYPE option must not be repeated")
  ;; Valid
  '(#9=(defstruct #10=foo)
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #10#))))
     :source #9#))
  '(#11=(defstruct (#12=foo))
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #12#))))
     :source #11#))
  '(#13=(defstruct (#14=foo #15=(:constructor nil)))
    (:defstruct
     ((:name        . 1) (((:type-name () :name foo :source #14#)))
      (:constructor . *) (((:structure-constructor () :source #15#)
                           :evaluation :compound)))
     :source #13#))
  '(#16=(defstruct (#17=foo #18=(:constructor #19=foo #20=(#21=a #22=b))))
    (:defstruct
     ((:name        . 1) (((:type-name () :name foo :source #17#)))
      (:constructor . *) (((:structure-constructor
                            ((:name        . 1) (((:function-name () :name foo :source #19#)))
                             (:lambda-list . 1) (((:ordinary-lambda-list
                                                   ((:required . *) (((:required-parameter
                                                                       ((:name . 1) (((:variable-name
                                                                                       ()
                                                                                       :name a :source #21#)
                                                                                      :evaluation nil)))
                                                                       :source #21#))
                                                                     ((:required-parameter
                                                                       ((:name . 1) (((:variable-name
                                                                                       ()
                                                                                       :name b :source #22#)
                                                                                      :evaluation nil)))
                                                                       :source #22#))))
                                                   :source #20#)
                                                  :evaluation :compound)))
                            :source #18#)
                           :evaluation :compound)))
     :source #16#))
  '(#23=(defstruct #24=foo "doc")
    (:defstruct
     ((:name          . 1) (((:type-name () :name foo :source #24#)))
      (:documentation . 1) (("doc")))
     :source #23#)))

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
    '((foo 1)                                     :fatal 1          "option name must be a symbol")
    ;; Repeated options
    '((foo :allocation :class :allocation :class) :fatal :class     ":ALLOCATION option must not be repeated")
    '((foo :initform 1 :initform 1)               :fatal 1          ":INITFORM option must not be repeated")
    '((foo :type bit :type bit)                   :fatal bit        ":TYPE option must not be repeated")
    '((foo :documentation "" :documentation "")   :fatal ""         ":DOCUMENTATION option must not be repeated")
    ;; Valid syntax
    '(#1=(#2=foo :initform (+ 1) :custom-option :foo :reader #3=bar)
      t t (:slot-specifier
           ((:name         . 1) (((:variable-name () :name foo :source #2#)))
            (:reader       . *) (((:function-name () :name bar :source #3#)))
            (:initform     . 1) (((+ 1) :evaluation t))
            (:option-name  . *) ((:custom-option))
            (:option-value . *) ((:foo)))
           :source #1#))))

(define-macro-test (defclass)
  '((defclass)
    syn:invalid-syntax-error)
  '((defclass 1)
    syn:invalid-syntax-error)
  #+TODO '((defclass foo #3=1)
    syn:invalid-syntax-error #3# "must be a list")
  '((defclass foo () () (:default-initargs #1=1))
    syn:invalid-syntax-error #1# "default initarg must be a symbol followed by an expression")
  '((defclass foo () () (:default-initargs :foo))
    syn:invalid-syntax-error nil "default initarg must be a symbol followed by an expression")
  '((defclass foo () () (:metaclass #2=1))
    syn:invalid-syntax-error #2# "metaclass must be a class name")
  '((defclass foo () () (:metaclass . #3=(foo 1)))
    syn:invalid-syntax-error #3# ":METACLASS option accepts one value")
  '((defclass foo () () (:documentation #4=1))
    syn:invalid-syntax-error #4# "must be a documentation string")
  '((defclass foo () () (:documentation . #5=("" 1)))
    syn:invalid-syntax-error #5# ":DOCUMENTATION option accepts one value")
  ;; Repeated options
  '((defclass foo () () (:metaclass bar) #6=(:metaclass bar))
    syn:invalid-syntax-error #6# ":METACLASS option must not be repeated")
  '((defclass foo () () (:documentation "") #7=(:documentation ""))
    syn:invalid-syntax-error #7# ":DOCUMENTATION option must not be repeated")
  ;; Slot options
  '((defclass foo () ((a :documentation #8=1)))
    syn:invalid-syntax-error #8# "must be a documentation string")
  ;; Repeated slot options
  '((defclass foo () ((a :type t . #9=(:type t))))
    syn:invalid-syntax-error #9# ":TYPE option must not be repeated")
  ;; Valid syntax
  '(#10=(defclass #11=foo (#12=bar #13=baz)
         (#14=(#15=foo :initform (+ 1) :custom-option :foo :reader #16=bar))
         (:metaclass #17=foo)
         (:documentation "foo")
         (:default-initargs
          :bar 1)
         (:my-class-option 1))
    (:defclass
     ((:name             . 1) (((:type-name () :name foo :source #11#)))
      (:superclass       . *) (((:type-name () :name bar :source #12#))
                               ((:type-name () :name baz :source #13#)))
      (:slot             . *) (((:slot-specifier
                                 ((:name         . 1) (((:variable-name () :name foo :source #15#)))
                                  (:reader       . *) (((:function-name () :name bar :source #16#)))
                                  (:initform     . 1) (((+ 1) :evaluation t))
                                  (:option-name  . *) ((:custom-option))
                                  (:option-value . *) ((:foo)))
                                 :source #14#)
                                :evaluation :compound))
      (:default-initarg  . *) ((:bar))
      (:default-initform . *) ((1 :evaluation t))
      (:metaclass        . 1) (((:type-name () :name foo :source #17#)))
      (:documentation    . 1)     (("foo"))
      (:option-name      . *)      ((:my-class-option))
      (:option-value     . *)     (((1))))
     :source #10#)))

(define-macro-test (deftype)
  '((deftype)       syn:invalid-syntax-error)
  '((deftype 1)     syn:invalid-syntax-error)
  '((deftype foo)   syn:invalid-syntax-error)
  '((deftype foo 1) syn:invalid-syntax-error 1 "must be a DEFTYPE lambda list")
  '((deftype foo (x #1=x))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#2=(deftype #3=foo #4=(#5=a &key #6=b)
         "bla bli"
         (declare #7=(ignore #8=a))
         (declare #9=(ignore #10=b))
         (list a b))
    (:deftype
     ((:name          . 1) (((:type-name () :name foo :source #3#)))
      (:lambda-list   . 1) (((:deftype-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name a :source #5#)
                                                                 :evaluation nil)))
                                                  :source #5#)
                                                 :evaluation :compound))
                               (:keyword  . *) (((:keyword-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #6#))))
                                                  :source #6#)
                                                 :evaluation :compound)))
                              :source #4#)
                             :evaluation :compound))
      (:documentation . 1) (("bla bli"))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:variable-name () :name a :source #8#))))
                              :kind ignore :source #7#))
                            ((:declaration
                              ((:argument . *) (((:variable-name () :name b :source #10#))))
                              :kind ignore :source #9#)))
      (:form          . *) (((list a b) :evaluation t)))
     :source #2#)))

(define-macro-test (defgeneric)
  '((defgeneric foo #1=1)
    syn:invalid-syntax-error #1# "must be a generic function lambda list")
  #+TODO '((defgeneric foo #2=(&key (a 1)))
           syn:invalid-syntax-error #2# "must be a generic function lambda list")
  '((defgeneric foo (x #2=x))
    syn:invalid-syntax-error #2# "the variable name X occurs more than once")
  '((defgeneric foo ()
      (:generic-function-class))
    syn:invalid-syntax-error nil ":GENERIC-FUNCTION-CLASS option accepts one value")
  '((defgeneric foo ()
      (:generic-function-class . #3=(bar 1)))
    syn:invalid-syntax-error #3# ":GENERIC-FUNCTION-CLASS option accepts one value")
  '((defgeneric foo ()
      (:generic-function-class #4="foo"))
    syn:invalid-syntax-error #4# "must be a class name")
  '((defgeneric foo ()
      (:argument-precedence-order #5=1))
    syn:invalid-syntax-error #5# "variable name must be a symbol")
  '((defgeneric foo ()
      (:method-combination #6=1))
    syn:invalid-syntax-error #6# "method combination name must be a symbol")
  '((defgeneric foo ()
      (:method-class #7=1))
    syn:invalid-syntax-error #7# "must be a class name")
  '((defgeneric foo ()
      #8=(:method))
    syn:invalid-syntax-error #8# "must be of the for (:method [QUALIFIERS] LAMBDA-LIST [DECLARATION] FORM*)")
  ;; Repeated options
  '((defgeneric foo ()
      (:generic-function-class bar)
      #9=(:generic-function-class bar))
    syn:invalid-syntax-error #9# ":GENERIC-FUNCTION-CLASS option must not be repeated")
  '((defgeneric foo (a)
      (:argument-precedence-order a)
      #10=(:argument-precedence-order a))
    syn:invalid-syntax-error #10# ":ARGUMENT-PRECEDENCE-ORDER option must not be repeated")
  '((defgeneric foo ()
      (:documentation "foo")
      #11=(:documentation "foo"))
    syn:invalid-syntax-error #11# ":DOCUMENTATION option must not be repeated")
  ;; Valid syntax
  '(#12=(defgeneric #13=foo #14=(#15=a #16=b)
         (:documentation "foo")
         (:generic-function-class #17=clazz))
    (:defgeneric
     ((:name                   . 1) (((:function-name () :name foo :source #13#)))
      (:lambda-list            . 1) (((:generic-function-lambda-list
                                       ((:required . *) (((:required-parameter
                                                           ((:name . 1) (((:variable-name
                                                                           ()
                                                                           :name a :source #15#)
                                                                          :evaluation nil)))
                                                           :source #15#))
                                                         ((:required-parameter
                                                           ((:name . 1) (((:variable-name
                                                                           ()
                                                                           :name b :source #16#)
                                                                          :evaluation nil)))
                                                           :source #16#))))
                                       :source #14#)
                                      :evaluation :compound))
      (:generic-function-class . 1) (((:type-name () :name clazz :source #17#)))
      (:documentation          . 1) (("foo")))
     :source #12#))
  '(#18=(defgeneric #19=foo #20=(#21=a #22=b)
          (:argument-precedence-order #23=b #24=a))
    (:defgeneric
     ((:name                      . 1) (((:function-name () :name foo :source #19#)))
      (:lambda-list               . 1) (((:generic-function-lambda-list
                                          ((:required . *) (((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name a :source #21#)
                                                                             :evaluation nil)))
                                                              :source #21#))
                                                            ((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name b :source #22#)
                                                                             :evaluation nil)))
                                                              :source #22#))))
                                          :source #20#)
                                         :evaluation :compound))
      (:argument-precedence-order . 1) ((((:variable-name () :name b :source #23#)
                                          (:variable-name () :name a :source #24#)))))
     :source #18#))
  '(#25=(defgeneric #26=foo #27=(#28=a)
          #29=(:method :custom 1 "foo" #30=(#31=a)))
    (:defgeneric
     ((:name        . 1) (((:function-name () :name foo :source #26#)))
      (:lambda-list . 1) (((:generic-function-lambda-list
                            ((:required . *) (((:required-parameter
                                                ((:name . 1) (((:variable-name
                                                                ()
                                                                :name a :source #28#)
                                                               :evaluation nil)))
                                                :source #28#))))
                            :source #27#)
                           :evaluation :compound))
      (:method      . *) (((:method-description
                            ((:qualifier   . *) ((:custom) (1) ("foo"))
                             (:lambda-list . 1) (((:specialized-lambda-list
                                                   ((:required . *) (((:specialized-parameter
                                                                       ((:name . 1) (((:variable-name
                                                                                       ()
                                                                                       :name a :source #31#))))
                                                                       :source #31#))))
                                                   :source #30#)
                                                  :evaluation :compound)))
                            :source #29#)
                           :evaluation :compound)))
     :source #25#)))

(define-macro-test (defmethod)
  '((defmethod 1)
    syn:invalid-syntax-error 1 "must be a function name")
  #+TODO '((defmethod foo)
           syn:invalid-syntax-error nil "must be a specialized lambda list")
  '((defmethod foo (1))
    syn:invalid-syntax-error 1 "variable name must be a symbol")
  '((defmethod foo (x #1=x))
    syn:invalid-syntax-error #1# "the variable name X occurs more than once")
  '((defmethod foo ((x 1)))
    syn:invalid-syntax-error 1 "must be a class name")
  '((defmethod foo (#2=(x t 1)))
    syn:invalid-syntax-error #2# "must be of the form (NAME SPECIALIZER)")
  ;; Valid syntax
  '(#3=(defmethod #4=foo #5=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #4#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #5#))))
     :source #3#))
  '(#6=(defmethod #7=foo :around #8=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #7#)))
      (:qualifier   . *) ((:around))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #8#))))
     :source #6#))
  '(#9=(defmethod #10=foo :custom 1 "foo" #11=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #10#)))
      (:qualifier   . *) ((:custom) (1) ("foo"))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #11#))))
     :source #9#))
  '(#12=(defmethod #13=foo #14=(#15=(#16=x #17=t)))
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #13#)))
      (:lambda-list . 1) (((:specialized-lambda-list
                            ((:required . *) (((:specialized-parameter
                                                ((:name        . 1) (((:variable-name
                                                                       ()
                                                                       :name x :source #16#)))
                                                 (:specializer . 1) (((:type-name
                                                                       ()
                                                                       :name t :source #17#))))
                                            :source #15#))))
                            :source #14#))))
     :source #12#))
  '(#18=(defmethod #19=foo #20=()
          "foo" (declare #21=(ignore)) 1)
    (:defmethod
     ((:name          . 1) (((:function-name () :name foo :source #19#)))
      (:lambda-list   . 1) (((:specialized-lambda-list () :source #20#)))
      (:documentation . 1) (("foo"))
      (:declaration   . *) (((:declaration () :kind ignore :source #21#)))
      (:form          . *) ((1 :evaluation t)))
      :source #18#)))

(define-macro-test (defpackage)
  '((defpackage #1=1)
    syn:invalid-syntax-error #1# "must be a string designator")
  '((defpackage foo #2=2)
    syn:invalid-syntax-error #2# "option must be a list")
  '((defpackage foo (:documentation #3=1))
    syn:invalid-syntax-error #3# "must be a documentation string")
  '((defpackage foo (:size #4="a"))
    syn:invalid-syntax-error #4# "must be a non-negative integer")
  '((defpackage foo (:size . #5=(1 2)))
    syn:invalid-syntax-error #5# ":SIZE option accepts one value")
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
     ((:name                  . 1) (((:string-designator
                                      ()
                                      :string "FOO" :source #9#)))
      (:documentation         . 1) (("bla"))
      (:use                   . *) (((:string-designator
                                      ()
                                      :string "BAR" :source #10#))
                                    ((:string-designator
                                      ()
                                      :string "bar" :source #11#)))
      (:shadowing-import-from . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO2" :source #17#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "BAZ2" :source #18#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR2" :source #19#))))
                                      :source #16#)))
      (:import-from           . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO" :source #13#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "c" :source #14#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR" :source #15#))))
                                      :source #12#)))
      (:size                  . 1) ((1)))
     :source #8#))

  '(#20=(defpackage #21=foo
          (:nicknames #22="f" #23=:fo)
          (:export #24="bar" #25=:baz))
    (:defpackage
     ((:name     . 1) (((:string-designator () :string "FOO" :source #21#)))
      (:nickname . *) (((:string-designator () :string "f" :source #22#))
                       ((:string-designator () :string "FO" :source #23#)))
      (:export   . *) (((:string-designator () :string "bar" :source #24#))
                       ((:string-designator () :string "BAZ" :source #25#))))
     :source #20#)))

(define-macro-test (in-package)
  '((in-package)          syn:invalid-syntax-error)
  '((in-package 1)        syn:invalid-syntax-error)
  '((in-package foo 1)    syn:invalid-syntax-error)

  '(#1=(in-package #2=foo)
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #2#))))
     :source #1#))
  '(#3=(in-package #4="FOO")
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #4#))))
     :source #3#)))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-macro-test (handler-bind)
  '((handler-bind)
    syn:invalid-syntax-error)
  '((handler-bind (#1=1))
    syn:invalid-syntax-error #1# "must be of the form (TYPE HANDLER-FORM)")
  '((handler-bind ((#2=1 (lambda (x)))))
    syn:invalid-syntax-error #2# "must be a type specifier")
  ;; Valid syntax
  '(#3=(handler-bind (#4=(#5=foo (lambda (x) (bar))))
         (baz))
    (:handler-bind
     ((:binding . *) (((:handler-binding
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name
                                                        ()
                                                        :name foo :source #5#))))
                                        :source #5#)))
                         (:form . 1) (((lambda (x) (bar)) :evaluation t)))
                        :source #4#)
                       :evaluation :compound))
      (:form    . *) (((baz) :evaluation t)))
     :source #3#)))

(define-macro-test (handler-case)
  '((handler-case nil (#1=1))
    syn:invalid-syntax-error #1# "must be a type specifier")
  '((handler-case nil (foo #2=1))
    syn:invalid-syntax-error #2# "must be a lambda list with zero or one required parameter")
  '((handler-case nil (foo (#3=1)))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  ;; Repeated option
  '((handler-case nil (:no-error ()) #4=(:no-error ()))
    syn:invalid-syntax-error #4# "NO-ERROR must not be repeated")
  ;; Valid syntax
  '(#5=(handler-case (foo)
         #6=(#7=bar (#8=x) (baz))
         #9=(:no-error #10=(#11=y #12=z) (fez)))
    (:handler-case
     ((:form   . 1)          (((foo) :evaluation t))
      (:clause . *)          (((:handler-clause
                                ((:type     . 1) (((:atomic-type-specifier
                                                    ((:name . 1) (((:type-name
                                                                    ()
                                                                    :name bar :source #7#))))
                                                    :source #7#)))
                                 (:variable . 1) (((:required-parameter
                                                    ((:name . 1) (((:variable-name
                                                                    ()
                                                                    :name x :source #8#))))
                                                    :source #8#)
                                                   :evaluation :binding))
                                 (:form     . *) (((baz) :evaluation t)))
                                :source #6#)
                               :evaluation :compound))
      (:no-error-clause . 1) (((:no-error-clause
                                ((:lambda-list . 1) (((:ordinary-lambda-list
                                                       ((:required . *) (((:required-parameter
                                                                           ((:name . 1) (((:variable-name
                                                                                           ()
                                                                                           :name y :source #11#)
                                                                                          :evaluation nil)))
                                                                           :source #11#))
                                                                         ((:required-parameter
                                                                           ((:name . 1) (((:variable-name
                                                                                           ()
                                                                                           :name z :source #12#)
                                                                                          :evaluation nil)))
                                                                           :source #12#))))
                                                       :source #10#)
                                                      :evaluation :compound))
                                 (:form        . *) (((fez) :evaluation t)))
                                :source #9#)
                               :evaluation :compound)))
     :source #5#)))

(define-macro-test (restart-bind)
  '((restart-bind 1)
    syn:invalid-syntax-error)
  '((restart-bind (#1=1))
    syn:invalid-syntax-error #1# "must be of the form (NAME FUNCTION [OPTIONS])")
  '((restart-bind ((#2=1 foo)))
    syn:invalid-syntax-error #2# "variable name must be a symbol")
  '((restart-bind ((foo bar :test-function baz . #3=(:test-function fez))))
    syn:invalid-syntax-error #3# ":TEST-FUNCTION option must not be repeated")
  ;; Valid syntax
  '(#4=(restart-bind () 1)
    (:restart-bind ((:form . *) ((1 :evaluation t))) :source #4#))
  '(#5=(restart-bind (#6=(#7=foo #8=bar :report-function #9=baz)) 1)
    (:restart-bind
     ((:binding . *) (((:restart-binding
                        ((:name            . 1) (((:variable-name () :name foo :source #7#)))
                         (:function        . 1) ((bar :evaluation t))
                         (:report-function . 1) ((baz :evaluation t)))
                        :source #6#)
                       :evaluation :compound))
      (:form    . *) ((1 :evaluation t)))
     :source #5#)))

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
    (:restart-case ((:form . 1) ((1 :evaluation t))) :source #5#))
  '(#6=(restart-case 1 #7=(#8=foo #9=(#10=x) :report "bar" :test #11=baz))
    (:restart-case
     ((:form   . 1) ((1 :evaluation t))
      (:clause . *) (((:restart-clause
                       ((:name        . 1) (((:variable-name () :name foo :source #8#)))
                        (:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required . *) (((:required-parameter
                                                                  ((:name . 1) (((:variable-name
                                                                                  ()
                                                                                  :name x :source #10#)
                                                                                 :evaluation nil)))
                                                                  :source #10#))))
                                              :source #9#)))
                        (:report-string . 1) (("bar"))
                        (:test-name     . 1) (((:function-name
                                                ()
                                                :name baz :source #11#))))
                       :source #7#)
                      :evaluation :compound)))
     :source #6#)))
