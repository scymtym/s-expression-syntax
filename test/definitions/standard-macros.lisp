;;;; standard-macros.lisp --- Tests for standard macro rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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
  '(#1=(defconstant)
    syn:invalid-syntax-error #1#)
  '((defconstant #2=1)
    syn:invalid-syntax-error #2# "variable name must be a symbol")
  '((defconstant foo 1 #3=2)
    syn:invalid-syntax-error #3# "must be a documentation string")
  '(#4=(defconstant foo 1 "" 2)
    syn:invalid-syntax-error #4#)
  ;; Valid syntax
  '(#5=(defconstant #6=foo
         #7=(bar 1)
         #8="bla")
    (:defconstant
     ((:name          . 1) (((:variable-name () :name foo :source #6#)))
      (:initial-value . 1) (((:unparsed
                              ()
                              :expression #7# :context :form :source #7#)
                             :evaluation t))
      (:documentation . 1) (((:documentation () :string "bla" :source #8#))))
     :source #5#)))

(define-macro-test (defvar)
  '(#1=(defvar)
    syn:invalid-syntax-error #1#)
  '((defvar #2=1)
    syn:invalid-syntax-error #2# "variable name must be a symbol")
  '((defvar foo 1 #3=2)
    syn:invalid-syntax-error #3# "must be a documentation string")
  '(#4=(defvar foo 1 "" 2)
    syn:invalid-syntax-error #4#)
  ;; Valid syntax
  '(#5=(defvar #6=foo)
    (:defvar
     ((:name . 1) (((:variable-name () :name foo :source #6#))))
     :source #5#))
  '(#7=(defvar #8=foo
         #9=(bar 1)
         #10="bla")
    (:defvar
     ((:name          . 1) (((:variable-name () :name foo :source #8#)))
      (:initial-value . 1) (((:unparsed
                              ()
                              :expression #9# :context :form :source #9#)
                             :evaluation t))
      (:documentation . 1) (((:documentation () :string "bla" :source #10#))))
     :source #7#)))

(define-macro-test (defparameter)
  '(#1=(defparameter)
    syn:invalid-syntax-error #1#)
  '((defparameter #2=1)
    syn:invalid-syntax-error #2# "variable name must be a symbol")
  '(#3=(defparameter foo)
    syn:invalid-syntax-error #3#)
  '((defparameter foo 1 #4=2)
    syn:invalid-syntax-error #4# "must be a documentation string")
  '(#5=(defparameter foo 1 "" 2)
    syn:invalid-syntax-error #5#)
  ;; valid syntax
  '(#6=(defparameter #7=foo
         #8=(bar 1)
         #9="bla")
    (:defparameter
     ((:name          . 1) (((:variable-name () :name foo :source #7#)))
      (:initial-value . 1) (((:unparsed
                              ()
                              :expression #8# :context :form :source #8#)
                             :evaluation t))
      (:documentation . 1) (((:documentation () :string "bla" :source #9#))))
     :source #6#)))

(define-macro-test (defun)
  '((defun (setf #1=1) ())
    syn:invalid-syntax-error #1# "second element of SETF function name must be a symbol")
  '((defun foo #2=1)
    syn:invalid-syntax-error #2# "must be an ordinary lambda list")
  '((defun foo (x #3=x))
    syn:invalid-syntax-error #3# "the variable name X occurs more than once")
  #+TODO '((defun foo ((a b)))
           syn:invalid-syntax-error) ; this should fail but doesn't because of the way grammars use each other
  '((defun foo () (declare #4=1))
    syn:invalid-syntax-error #4# "must be a declaration")
  ;; Valid syntax
  '(#5=(defun #6=foo #7=(#8=bar #9=baz)
         #10="bla"
         (declare #11=(type #12=integer #13=bar #14=baz))
         #15=(+ 1 2))
    (:defun
     ((:name          . 1) (((:function-name () :name foo :source #6#)))
      (:lambda-list   . 1) (((:ordinary-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name bar :source #8#)
                                                                 :evaluation nil)))
                                                  :source #8#))
                                                ((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name baz :source #9#)
                                                                 :evaluation nil)))
                                                  :source #9#))))
                              :source #7#)
                             :evaluation :compound))
      (:documentation . 1) (((:documentation () :string "bla" :source #10#)))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:atomic-type-specifier
                                                  ((:name . 1) (((:type-name
                                                                  ()
                                                                  :name integer :source #12#))))
                                                  :source #12#))
                                                ((:variable-name () :name bar :source #13#))
                                                ((:variable-name () :name baz :source #14#))))
                              :kind type :source #11#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #15# :context :form :source #15#)
                             :evaluation t)))
     :source #5#)))

(define-macro-test (defmacro)
  '((defmacro)
    syn:invalid-syntax-error)
  '((defmacro #1=1)
    syn:invalid-syntax-error #1# "function name must be a symbol")
  '((defmacro foo #2=1)
    syn:invalid-syntax-error #2# "must be a destructuring lambda list")
  '((defmacro foo (x (y #3=x)))
    syn:invalid-syntax-error #3# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#4=(defmacro #5=foo #6=(#7=a #8=b)
         #9="bla"
         (declare #10=(ignore #11=a))
         #12=(list 'cons b b))
    (:defmacro
     ((:name          . 1) (((:function-name () :name foo :source #5#)))
      (:lambda-list   . 1) (((:destructuring-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name a :source #7#)
                                                                 :evaluation nil)))
                                                  :source #7#)
                                                 :evaluation :compound)
                                                ((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #8#)
                                                                 :evaluation nil)))
                                                  :source #8#)
                                                 :evaluation :compound)))
                              :source #6#)
                             :evaluation :compound))
      (:documentation . 1) (((:documentation () :string "bla" :source #9#)))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:variable-name () :name a :source #11#))))
                              :kind ignore :source #10#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #12# :context :form :source #12#)
                             :evaluation t)))
     :source #4#)))

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
  ;; Valid syntax
  '(#6=foo
    (:slot-description
     ((:name . 1) (((:variable-name () :name foo :source #6#))))
     :source #6#))
  '(#7=(#8=foo #9=1 :type #10=bit :read-only #11=t)
    (:slot-description
     ((:name      . 1) (((:variable-name () :name foo :source #8#)))
      (:initform  . 1) (((:unparsed
                          ()
                          :expression #9# :context :form :source #9#)
                         :evaluation t))
      (:read-only . 1) (((:unparsed
                          ()
                          :expression t :context :slot-read-only :source #11#)))
      (:type      . 1) (((:atomic-type-specifier
                          ((:name . 1) (((:type-name () :name bit :source #10#))))
                          :source #10#))))
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
  ;; Valid syntax
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
  '(#23=(defstruct #24=foo #25="doc")
    (:defstruct
     ((:name          . 1) (((:type-name () :name foo :source #24#)))
      (:documentation . 1) (((:documentation () :string #25# :source #25#))))
     :source #23#)))

;;; `defclass' including slots

(test slot-specifier
  "Test for `slot-specifier' rule."
  (rule-test-cases ((syn::slot-specifier syn::special-operators))
    ;; Invalid syntax
    '((foo :reader #1=1)                                  :fatal #1#  "reader must be a symbol function name")
    '((foo :reader #2=(setf foo))                         :fatal #2#  "reader must be a symbol function name")
    '((foo :writer #3=1)                                  :fatal #3#  "writer must be an extended function name")
    '((foo :writer (setf #4=1))                           :fatal #4#  "second element of SETF function name must be a symbol")
    '((foo :accessor #5=1)                                :fatal #5#  "accessor must be a symbol function name")
    '((foo :accessor #6=(setf foo))                       :fatal #6#  "accessor must be a symbol function name")
    '((foo :initarg #7=1)                                 :fatal #7#  "initarg name must be a symbol")
    '((foo :initform #8=(declare))                        :fatal #8#  "declare is not allowed here")
    '((foo :type #9=1)                                    :fatal #9#  "must be a type specifier")
    '((foo :documentation #10=1)                          :fatal #10# "must be a documentation string")
    '((foo #11=1)                                         :fatal #11# "option name must be a symbol")
    ;; Repeated options
    '((foo :allocation :class . #12=(:allocation :class)) :fatal #12# ":ALLOCATION option must not be repeated")
    '((foo :initform 1 . #13=(:initform 1))               :fatal #13# ":INITFORM option must not be repeated")
    '((foo :type bit . #14=(:type bit))                   :fatal #14# ":TYPE option must not be repeated")
    '((foo :documentation "" . #15=(:documentation ""))   :fatal #15# ":DOCUMENTATION option must not be repeated")
    ;; Valid syntax
    '(#16=(#17=foo :initform #18=(+ 1) #19=:custom-option #20=:foo :reader #21=bar)
      t t (:slot-specifier
           ((:name         . 1) (((:variable-name () :name foo :source #17#)))
            (:reader       . *) (((:function-name () :name bar :source #21#)))
            (:initform     . 1) (((:unparsed
                                   ()
                                   :expression #18# :context :form :source #18#)
                                  :evaluation t))
            (:option-name  . *) (((:option-name () :name :custom-option :source #19#)))
            (:option-value . *) (((:unparsed
                                   ()
                                   :expression :foo
                                   :context    :non-standard-slot-option
                                   :source     #20#))))
           :source #16#))))

(define-macro-test (defclass)
  '(#1=(defclass)
    syn:invalid-syntax-error #1#)
  '((defclass #2=1)
    syn:invalid-syntax-error #2#)
  #+TODO '((defclass foo #3=1)
    syn:invalid-syntax-error #3# "must be a list")
  '((defclass foo () () (:default-initargs #3=1))
    syn:invalid-syntax-error #3# "initarg name must be a symbol")
  '((defclass foo () () (:default-initargs :foo))
    syn:invalid-syntax-error nil "default initarg must be a symbol followed by a form")
  '((defclass foo () () (:metaclass #4=1))
    syn:invalid-syntax-error #4# "metaclass must be a class name")
  '((defclass foo () () (:metaclass . #5=(foo 1)))
    syn:invalid-syntax-error #5# ":METACLASS option accepts one value")
  '((defclass foo () () (:documentation #6=1))
    syn:invalid-syntax-error #6# "must be a documentation string")
  '((defclass foo () () (:documentation . #7=("" 1)))
    syn:invalid-syntax-error #7# ":DOCUMENTATION option accepts one value")
  ;; Repeated options
  '((defclass foo () () (:metaclass bar) #8=(:metaclass bar))
    syn:invalid-syntax-error #8# ":METACLASS option must not be repeated")
  '((defclass foo () () (:documentation "") #9=(:documentation ""))
    syn:invalid-syntax-error #9# ":DOCUMENTATION option must not be repeated")
  ;; Slot options
  '((defclass foo () ((a :documentation #10=1)))
    syn:invalid-syntax-error #10# "must be a documentation string")
  ;; Repeated slot options
  '((defclass foo () ((a :type t . #11=(:type t))))
    syn:invalid-syntax-error #11# ":TYPE option must not be repeated")
  ;; Valid syntax
  '(#12=(defclass #13=foo (#14=bar #15=baz)
         (#16=(#17=foo :initform #18=(+ 1) #19=:custom-option :foo :reader #20=bar))
         (:metaclass #21=foo)
         (:documentation #22="foo")
         (:default-initargs
          #23=:bar #24=1)
         (#25=:my-class-option . #26=(1)))
    (:defclass
     ((:name             . 1) (((:type-name () :name foo :source #13#)))
      (:superclass       . *) (((:type-name () :name bar :source #14#))
                               ((:type-name () :name baz :source #15#)))
      (:slot             . *) (((:slot-specifier
                                 ((:name         . 1) (((:variable-name () :name foo :source #17#)))
                                  (:reader       . *) (((:function-name () :name bar :source #20#)))
                                  (:initform     . 1) (((:unparsed
                                                         ()
                                                         :expression #18# :context :form :source #18#)
                                                        :evaluation t))
                                  (:option-name  . *) (((:option-name ()
                                                         :name   :custom-option
                                                         :source #19#)))
                                  (:option-value . *) (((:unparsed
                                                         ()
                                                         :expression :foo
                                                         :context    :non-standard-slot-option
                                                         :source     :foo))))
                                 :source #16#)
                                :evaluation :compound))
      (:default-initarg  . *) (((:default-initarg
                                 ((:name     . 1) (((:initarg-name
                                                     ()
                                                     :name :bar :source #23#)))
                                  (:initform . 1) (((:unparsed
                                                     ()
                                                     :expression 1 :context :form :source #24#)
                                                    :evaluation t)))
                                 :source #23#)
                                :evaluation :compound))
      (:metaclass        . 1) (((:type-name () :name foo :source #21#)))
      (:documentation    . 1) (((:documentation () :string "foo" :source #22#)))
      (:option-name      . *) (((:option-name () :name :my-class-option :source #25#)))
      (:option-value     . *) (((:unparsed
                                 ()
                                 :expression (1)
                                 :context    :non-standard-defclass-option
                                 :source     #26#))))
     :source #12#)))

(define-macro-test (deftype)
  '(#1=(deftype)
    syn:invalid-syntax-error #1#)
  '((deftype #2=1)
    syn:invalid-syntax-error #2# "must be a class name")
  '(#3=(deftype foo)
    syn:invalid-syntax-error #3#)
  '((deftype foo #4=1)
    syn:invalid-syntax-error #4# "must be a DEFTYPE lambda list")
  '((deftype foo (x #5=x))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#6=(deftype #7=foo #8=(#9=a &key #10=b)
         #11="bla bli"
         (declare #12=(ignore #13=a))
         (declare #14=(ignore #15=b))
         #16=(list a b))
    (:deftype
     ((:name          . 1) (((:type-name () :name foo :source #7#)))
      (:lambda-list   . 1) (((:deftype-lambda-list
                              ((:required . *) (((:required-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name a :source #9#)
                                                                 :evaluation nil)))
                                                  :source #9#)
                                                 :evaluation :compound))
                               (:keyword  . *) (((:keyword-parameter
                                                  ((:name . 1) (((:variable-name
                                                                  ()
                                                                  :name b :source #10#))))
                                                  :source #10#)
                                                 :evaluation :compound)))
                              :source #8#)
                             :evaluation :compound))
      (:documentation . 1) (((:documentation () :string "bla bli" :source #11#)))
      (:declaration   . *) (((:declaration
                              ((:argument . *) (((:variable-name () :name a :source #13#))))
                              :kind ignore :source #12#))
                            ((:declaration
                              ((:argument . *) (((:variable-name () :name b :source #15#))))
                              :kind ignore :source #14#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #16# :context :form :source #16#)
                             :evaluation t)))
     :source #6#)))

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
         (:documentation #17="foo")
         (:generic-function-class #18=clazz))
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
      (:generic-function-class . 1) (((:type-name () :name clazz :source #18#)))
      (:documentation          . 1) (((:documentation () :string #17# :source #17#))))
     :source #12#))
  '(#19=(defgeneric #20=foo #21=(#22=a #23=b)
          (:argument-precedence-order #24=b #25=a))
    (:defgeneric
     ((:name                      . 1) (((:function-name () :name foo :source #20#)))
      (:lambda-list               . 1) (((:generic-function-lambda-list
                                          ((:required . *) (((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name a :source #22#)
                                                                             :evaluation nil)))
                                                              :source #22#))
                                                            ((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name b :source #23#)
                                                                             :evaluation nil)))
                                                              :source #23#))))
                                          :source #21#)
                                         :evaluation :compound))
      (:argument-precedence-order . *) (((:variable-name () :name b :source #24#))
                                        ((:variable-name () :name a :source #25#))))
     :source #19#))
  '(#26=(defgeneric #27=foo #28=(#29=a)
          #30=(:method #31=:custom #32=1 #33="foo" #34=(#35=a)))
    (:defgeneric
     ((:name        . 1) (((:function-name () :name foo :source #27#)))
      (:lambda-list . 1) (((:generic-function-lambda-list
                            ((:required . *) (((:required-parameter
                                                ((:name . 1) (((:variable-name
                                                                ()
                                                                :name a :source #29#)
                                                               :evaluation nil)))
                                                :source #29#))))
                            :source #28#)
                           :evaluation :compound))
      (:method      . *) (((:method-description
                            ((:qualifier   . *) (((:unparsed
                                                   ()
                                                   :expression :custom
                                                   :context    :method-qualifier
                                                   :source     #31#))
                                                 ((:unparsed
                                                   ()
                                                   :expression 1
                                                   :context    :method-qualifier
                                                   :source     #32#))
                                                 ((:unparsed
                                                   ()
                                                   :expression "foo"
                                                   :context    :method-qualifier
                                                   :source     #33#)))
                             (:lambda-list . 1) (((:specialized-lambda-list
                                                   ((:required . *) (((:specialized-parameter
                                                                       ((:name . 1) (((:variable-name
                                                                                       ()
                                                                                       :name a :source #35#))))
                                                                       :source #35#))))
                                                   :source #34#)
                                                  :evaluation :compound)))
                            :source #30#)
                           :evaluation :compound)))
     :source #26#)))

(define-macro-test (defmethod)
  '((defmethod #1=1)
    syn:invalid-syntax-error #1# "must be a function name")
  #+TODO '((defmethod foo)
           syn:invalid-syntax-error nil "must be a specialized lambda list")
  '(#2=(defmethod foo 1)
    syn:invalid-syntax-error #2#)
  '((defmethod foo (#3=1))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((defmethod foo (x #4=x))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
  '((defmethod foo ((x #5=1)))
    syn:invalid-syntax-error #5# "must be a class name")
  '((defmethod foo (#6=(x t 1)))
    syn:invalid-syntax-error #6# "must be of the form (NAME SPECIALIZER)")
  ;; Valid syntax
  '(#7=(defmethod #8=foo #9=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #8#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #9#))))
     :source #7#))
  '(#10=(defmethod #11=foo #12=:around #13=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #11#)))
      (:qualifier   . *) (((:unparsed
                            ()
                            :expression :around
                            :context    :method-qualifier
                            :source     #12#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #13#))))
     :source #10#))
  '(#14=(defmethod #15=foo #16=:custom #17=1 #18="foo" #19=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #15#)))
      (:qualifier   . *) (((:unparsed
                            ()
                            :expression :custom
                            :context    :method-qualifier
                            :source     #16#))
                          ((:unparsed
                            ()
                            :expression 1
                            :context    :method-qualifier
                            :source     #17#))
                          ((:unparsed
                            ()
                            :expression "foo"
                            :context    :method-qualifier
                            :source     #18#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #19#))))
     :source #14#))
  '(#20=(defmethod #21=foo #22=(#23=(#24=x #25=t)))
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #21#)))
      (:lambda-list . 1) (((:specialized-lambda-list
                            ((:required . *) (((:specialized-parameter
                                                ((:name        . 1) (((:variable-name
                                                                       ()
                                                                       :name x :source #24#)))
                                                 (:specializer . 1) (((:type-name
                                                                       ()
                                                                       :name t :source #25#))))
                                            :source #23#))))
                            :source #22#))))
     :source #20#))
  '(#26=(defmethod #27=foo #28=()
          #29="foo" (declare #30=(ignore)) #31=1)
    (:defmethod
     ((:name          . 1) (((:function-name () :name foo :source #27#)))
      (:lambda-list   . 1) (((:specialized-lambda-list () :source #28#)))
      (:documentation . 1) (((:documentation () :string #29# :source #29#)))
      (:declaration   . *) (((:declaration () :kind ignore :source #30#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #31# :context :form :source #31#)
                             :evaluation t)))
      :source #26#)))

(define-macro-test (defpackage)
  '((defpackage #1=1)
    syn:invalid-syntax-error #1# "must be a string designator")
  '((defpackage foo #2=2)
    syn:invalid-syntax-error #2# "option must be a list")
  '((defpackage foo (:documentation #3=1))
    syn:invalid-syntax-error #3# "must be a documentation string")
  '((defpackage foo (:size #4="a"))
    syn:invalid-syntax-error #4# "package size must be a non-negative integer")
  '((defpackage foo (:size . #5=(1 2)))
    syn:invalid-syntax-error #5# ":SIZE option accepts one value")
  ;; Repeated options
  '((defpackage foo (:documentation "") #6=(:documentation ""))
    syn:invalid-syntax-error #6# ":DOCUMENTATION option must not be repeated")
  '((defpackage foo (:size 1) #7=(:size 2))
    syn:invalid-syntax-error #7# ":SIZE option must not be repeated")
  ;; Valid syntax
  '(#8=(defpackage #9=foo
         (:documentation #10="bla")
         (:use #11=:bar #12="bar")
         (:size #13=1)
         #14=(:import-from #15=:foo #16=#\c #17=:bar)
         #18=(:shadowing-import-from #19=:foo2 #20="BAZ2" #21=:bar2))
    (:defpackage
     ((:name                  . 1) (((:string-designator
                                      ()
                                      :string "FOO" :source #9#)))
      (:documentation         . 1) (((:documentation () :string #10# :source #10#)))
      (:use                   . *) (((:string-designator
                                      ()
                                      :string "BAR" :source #11#))
                                    ((:string-designator
                                      ()
                                      :string "bar" :source #12#)))
      (:shadowing-import-from . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO2" :source #19#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "BAZ2" :source #20#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR2" :source #21#))))
                                      :source #18#)))
      (:import-from           . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO" :source #15#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "c" :source #16#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR" :source #17#))))
                                      :source #14#)))
      (:size                  . 1) (((:unparsed
                                      ()
                                      :expression 1
                                      :context    :package-size
                                      :source     #13#))))
     :source #8#))

  '(#22=(defpackage #23=foo
          (:nicknames #24="f" #25=:fo)
          (:export #26="bar" #27=:baz))
    (:defpackage
     ((:name     . 1) (((:string-designator () :string "FOO" :source #23#)))
      (:nickname . *) (((:string-designator () :string "f" :source #24#))
                       ((:string-designator () :string "FO" :source #25#)))
      (:export   . *) (((:string-designator () :string "bar" :source #26#))
                       ((:string-designator () :string "BAZ" :source #27#))))
     :source #22#)))

(define-macro-test (in-package)
  '((in-package)
    syn:invalid-syntax-error nil)
  '((in-package #1=1)
    syn:invalid-syntax-error #1# "must be a string designator")
  '(#2=(in-package foo 1)
    syn:invalid-syntax-error #2#)
  ;; Valid syntax
  '(#3=(in-package #4=foo)
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #4#))))
     :source #3#))
  '(#5=(in-package #6="FOO")
    (:in-package
     ((:name . 1) (((:string-designator () :string "FOO" :source #6#))))
     :source #5#)))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-macro-test (handler-bind)
  '((handler-bind . #1=())
    syn:invalid-syntax-error #1# "must be a list of handler bindings")
  '((handler-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of handler bindings")
  '((handler-bind (#3=1))
    syn:invalid-syntax-error #3# "must be of the form (TYPE HANDLER-FORM)")
  '((handler-bind ((#4=1 (lambda (x)))))
    syn:invalid-syntax-error #4# "must be a type specifier")
  ;; Valid syntax
  '(#5=(handler-bind (#6=(#7=foo #8=(lambda (x) (bar))))
         #9=(baz))
    (:handler-bind
     ((:binding . *) (((:handler-binding
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name
                                                        ()
                                                        :name foo :source #7#))))
                                        :source #7#)))
                         (:form . 1) (((:unparsed
                                        ()
                                        :expression (lambda (x) (bar))
                                        :context    :form
                                        :source     #8#)
                                       :evaluation t)))
                        :source #6#)
                       :evaluation :compound))
      (:form    . *) (((:unparsed
                        ()
                        :expression (baz) :context :form :source #9#)
                       :evaluation t)))
     :source #5#)))

(define-macro-test (handler-case)
  '((handler-case)
    syn:invalid-syntax-error)
  '((handler-case nil . #1=(1))
    syn:invalid-syntax-error #1# "must be a list of handler clauses")
  '((handler-case nil (#2=1))
    syn:invalid-syntax-error #2# "must be a type specifier")
  '((handler-case nil (foo #3=1))
    syn:invalid-syntax-error #3# "must be a lambda list with zero or one required parameter")
  '((handler-case nil (foo (#4=1)))
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  ;; Repeated option
  '((handler-case nil (:no-error ()) #5=(:no-error ()))
    syn:invalid-syntax-error #5# "NO-ERROR must not be repeated")
  ;; Valid syntax
  '(#6=(handler-case #7=(foo)
         #8=(#9=bar (#10=x) #11=(baz))
         #12=(:no-error #13=(#14=y #15=z) #16=(fez)))
    (:handler-case
     ((:form   . 1)          (((:unparsed
                                ()
                                :expression (foo) :context :form :source #7#)
                               :evaluation t))
      (:clause . *)          (((:handler-clause
                                ((:type     . 1) (((:atomic-type-specifier
                                                    ((:name . 1) (((:type-name
                                                                    ()
                                                                    :name bar :source #9#))))
                                                    :source #9#)))
                                 (:variable . 1) (((:required-parameter
                                                    ((:name . 1) (((:variable-name
                                                                    ()
                                                                    :name x :source #10#))))
                                                    :source #10#)
                                                   :evaluation (:binding :namespace variable
                                                                         :scope     :lexical)))
                                 (:form     . *) (((:unparsed
                                                    ()
                                                    :expression (baz) :context :form :source #11#)
                                                   :evaluation t)))
                                :source #8#)
                               :evaluation :compound))
      (:no-error-clause . 1) (((:no-error-clause
                                ((:lambda-list . 1) (((:ordinary-lambda-list
                                                       ((:required . *) (((:required-parameter
                                                                           ((:name . 1) (((:variable-name
                                                                                           ()
                                                                                           :name y :source #14#)
                                                                                          :evaluation nil)))
                                                                           :source #14#))
                                                                         ((:required-parameter
                                                                           ((:name . 1) (((:variable-name
                                                                                           ()
                                                                                           :name z :source #15#)
                                                                                          :evaluation nil)))
                                                                           :source #15#))))
                                                       :source #13#)
                                                      :evaluation :compound))
                                 (:form        . *) (((:unparsed
                                                       ()
                                                       :expression (fez) :context :form :source #16#)
                                                      :evaluation t)))
                                :source #12#)
                               :evaluation :compound)))
     :source #6#)))

(define-macro-test (restart-bind)
  '((restart-bind . #1=())
    syn:invalid-syntax-error #1# "must be a list of restart bindings")
  '((restart-bind #2=1)
    syn:invalid-syntax-error #2# "must be a list of restart bindings")
  '((restart-bind (#3=1))
    syn:invalid-syntax-error #3# "must be of the form (NAME FUNCTION [OPTIONS])")
  '((restart-bind ((#4=1 foo)))
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  '((restart-bind ((foo bar :test-function baz . #5=(:test-function fez))))
    syn:invalid-syntax-error #5# ":TEST-FUNCTION option must not be repeated")
  ;; Valid syntax
  '(#6=(restart-bind () #7=1)
    (:restart-bind
     ((:form . *) (((:unparsed
                     ()
                     :expression #7# :context :form :source #7#)
                    :evaluation t)))
     :source #6#))
  '(#8=(restart-bind (#9=(#10=foo #11=bar :report-function #12=baz)) #13=1)
    (:restart-bind
     ((:binding . *) (((:restart-binding
                        ((:name            . 1) (((:variable-name () :name foo :source #10#)))
                         (:function        . 1) (((:unparsed
                                                   ()
                                                   :expression #11# :context :form :source #11#)
                                                  :evaluation t))
                         (:report-function . 1) (((:unparsed
                                                   ()
                                                   :expression #12# :context :form :source #12#)
                                                  :evaluation t)))
                        :source #9#)
                       :evaluation :compound))
      (:form    . *) (((:unparsed
                        ()
                        :expression #13# :context :form :source #13#)
                       :evaluation t)))
     :source #8#)))

(define-macro-test (restart-case)
  '((restart-case #1=(declare))
    syn:invalid-syntax-error #1# "declare is not allowed here")
  '((restart-case 1 . #2=(1))
    syn:invalid-syntax-error #2# "must be a list of restart clauses")
  '((restart-case 1 (#3=1))
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((restart-case 1 (foo #4=1))
    syn:invalid-syntax-error #4# "must be an ordinary lambda list")
  '((restart-case 1 (nil ()))
    syn:invalid-syntax-error nil "for an unnamed restart, the :REPORT option must be supplied")
  ;; Valid syntax
  '(#5=(restart-case #6=1)
    (:restart-case
     ((:form . 1) (((:unparsed
                     ()
                     :expression #6# :context :form :source #6#)
                    :evaluation t)))
     :source #5#))
  '(#7=(restart-case #8=1 #9=(#10=foo #11=(#12=x)
                               :report #13="bar"
                               :test #14=baz
                               #15=1))
    (:restart-case
     ((:form   . 1) (((:unparsed
                       ()
                       :expression #8# :context :form :source #8#)
                      :evaluation t))
      (:clause . *) (((:restart-clause
                       ((:name        . 1) (((:variable-name () :name foo :source #10#)))
                        (:lambda-list . 1) (((:ordinary-lambda-list
                                              ((:required . *) (((:required-parameter
                                                                  ((:name . 1) (((:variable-name
                                                                                  ()
                                                                                  :name x :source #12#)
                                                                                 :evaluation nil)))
                                                                  :source #12#))))
                                              :source #11#)))
                        (:report-string . 1) (((:unparsed
                                                ()
                                                :expression "bar"
                                                :context    :restart-report-string
                                                :source     #13#)))
                        (:test-name     . 1) (((:function-name
                                                ()
                                                :name baz :source #14#)))
                        (:form          . *) (((:unparsed
                                                ()
                                                :expression 1
                                                :context    :form
                                                :source     #15#)
                                               :evaluation t)))
                       :source #9#)
                      :evaluation :compound)))
     :source #7#)))

;;; `[ec]case'

(define-macro-test (case)
  ;; Invalid syntax
  '(#1=(case)
    syn:invalid-syntax-error #1#)
  '((case x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")
  '((case x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")
  '((case x (otherwise 1) #4=(otherwise 2))
    syn:invalid-syntax-error #4# "otherwise clause must not be repeated")
  '((case x (otherwise 1) #5=(:normal 2))
    syn:invalid-syntax-error #5#
    "normal clause must not follow otherwise clause")
  ;; Valid syntax
  '(#6=(case #7=x)
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #7#)
                       :evaluation t)))
     :source #6#))
  '(#8=(case #9=x #10=(#11=y #12=1))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #9#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #11#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #12#)
                                       :evaluation t)))
                        :source #10#)
                       :evaluation :compound)))
     :source #8#))
  '(#13=(case #14=x #15=(#16=y #17=1) #18=(#19=z #20=2))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #14#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #16#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #17#)
                                       :evaluation t)))
                        :source #15#)
                       :evaluation :compound)
                      ((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression z :context :key :source #19#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #20#)
                                       :evaluation t)))
                        :source #18#)
                       :evaluation :compound)))
     :source #13#))
  '(#21=(case #22=x #23=((#24=y #25=z) #26=1 #27=2))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #22#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y :context :key :source #24#))
                                      ((:unparsed
                                        ()
                                        :expression z :context :key :source #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #26#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2 :context :form :source #27#)
                                       :evaluation t)))
                        :source #23#)
                       :evaluation :compound)))
     :source #21#))
  '(#28=(case #29=x #30=(otherwise #31=1))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #29#)
                       :evaluation t))
      (:clause  . *) (((:case-otherwise-clause
                        ((:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #31#)
                                       :evaluation t)))
                        :source #30#)
                       :evaluation :compound)))
     :source #28#))
  '(#32=(case #33=x #34=((#35=otherwise) #36=1))
    (:case
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #33#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression otherwise :context :key :source #35#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #36#)
                                       :evaluation t)))
                        :source #34#)
                       :evaluation :compound)))
     :source #32#)))

(define-macro-test (ccase)
  ;; Invalid syntax
  '(#1=(ccase)
    syn:invalid-syntax-error #1#)
  '((ccase #2=1)
    syn:invalid-syntax-error #2# "place must be a cons or a variable name")
  '((ccase x #3=1)
    syn:invalid-syntax-error #3#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  '((ccase x #4=())
    syn:invalid-syntax-error #4#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  ;; Valid syntax
  '(#5=(ccase #6=x)
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #6#)
                        :evaluation t)))
     :source #5#))
  '(#7=(ccase #8=x #9=(#10=otherwise #11=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #8#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression otherwise
                                         :context    :key
                                         :source     #10#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #11#)
                                        :evaluation t)))
                         :source #9#)
                        :evaluation :compound)))
     :source #7#))
  '(#12=(ccase #13=x #14=(#15=t #16=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #13#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression t
                                         :context    :key
                                         :source     #15#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #16#)
                                        :evaluation t)))
                         :source #14#)
                        :evaluation :compound)))
     :source #12#))
  '(#17=(ccase #18=x #19=(#20=y #21=1 #22=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #18#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #20#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #21#)
                                        :evaluation t)
                                       ((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #22#)
                                        :evaluation t)))
                         :source #19#)
                        :evaluation :compound)))
     :source #17#))
  '(#23=(ccase #24=x #25=(#26=y #27=1) #28=(#29=z #30=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #24#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #26#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #27#)
                                        :evaluation t)))
                         :source #25#)
                        :evaluation :compound)
                       ((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression z
                                         :context    :key
                                         :source     #29#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #30#)
                                        :evaluation t)))
                         :source #28#)
                        :evaluation :compound)))
     :source #23#))
  '(#31=(ccase #32=x #33=((#34=y #35=z) #36=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #32#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression y
                                         :context    :key
                                         :source     #34#))
                                       ((:unparsed
                                         ()
                                         :expression z
                                         :context    :key
                                         :source     #35#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #36#)
                                        :evaluation t)))
                         :source #33#)
                        :evaluation :compound)))
     :source #31#))
  '(#37=(ccase #38=x #39=((#40=otherwise) #41=1))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #38#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression otherwise
                                         :context    :key
                                         :source     #40#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :form
                                         :source     #41#)
                                        :evaluation t)))
                         :source #39#)
                        :evaluation :compound)))
      :source #37#))
  '(#42=(ccase #43=x #44=((#45=1 #46=t) #47=2))
    (:ccase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #43#)
                        :evaluation t))
      (:clause   . *) (((:case-normal-clause
                         ((:key  . *) (((:unparsed
                                         ()
                                         :expression 1
                                         :context    :key
                                         :source     #45#))
                                       ((:unparsed
                                         ()
                                         :expression t
                                         :context    :key
                                         :source     #46#)))
                          (:form . *) (((:unparsed
                                         ()
                                         :expression 2
                                         :context    :form
                                         :source     #47#)
                                        :evaluation t)))
                         :source #44#)
                        :evaluation :compound)))
      :source #42#)))

(define-macro-test (ecase)
  ;; Invalid syntax
  '(#1=(ecase)
    syn:invalid-syntax-error #1#)
  '((ecase x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  '((ecase x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of the form (KEY-OR-KEYS FORM*)")
  ;; Valid syntax
  '(#4=(ecase #5=x)
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #5#)
                       :evaluation t)))
     :source #4#))
  '(#6=(ecase #7=x #8=(#9=otherwise #10=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #7#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression otherwise
                                        :context    :key
                                        :source     #9#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #10#)
                                       :evaluation t)))
                        :source #8#)
                       :evaluation :compound)))
     :source #6#))
  '(#11=(ecase #12=x #13=(#14=t #15=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #12#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression t
                                        :context    :key
                                        :source     #14#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #15#)
                                       :evaluation t)))
                        :source #13#)
                       :evaluation :compound)))
     :source #11#))
  '(#16=(ecase #17=x #18=(#19=y #20=1 #21=2))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #17#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #19#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #20#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #21#)
                                       :evaluation t)))
                        :source #18#)
                       :evaluation :compound)))
     :source #16#))
  '(#22=(ecase #23=x #24=(#25=y #26=1) #27=(#28=z #29=2))
    (:ecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #23#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #26#)
                                       :evaluation t)))
                        :source #24#)
                       :evaluation :compound)
                      ((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression z
                                        :context    :key
                                        :source     #28#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #29#)
                                       :evaluation t)))
                        :source #27#)
                       :evaluation :compound)))
     :source #22#))
  '(#30=(ecase #31=x #32=((#33=y #34=z) #35=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #31#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression y
                                        :context    :key
                                        :source     #33#))
                                      ((:unparsed
                                        ()
                                        :expression z
                                        :context    :key
                                        :source     #34#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #35#)
                                       :evaluation t)))
                        :source #32#)
                       :evaluation :compound)))
     :source #30#))
  '(#36=(ecase #37=x #38=((#39=otherwise) #40=1))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #37#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression otherwise
                                        :context    :key
                                        :source     #39#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :form
                                        :source     #40#)
                                       :evaluation t)))
                        :source #38#)
                       :evaluation :compound)))
     :source #36#))
  '(#41=(ecase #42=x #43=((#44=1 #45=t) #46=2))
    (:ecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #42#)
                       :evaluation t))
      (:clause  . *) (((:case-normal-clause
                        ((:key  . *) (((:unparsed
                                        ()
                                        :expression 1
                                        :context    :key
                                        :source     #44#))
                                      ((:unparsed
                                        ()
                                        :expression t
                                        :context    :key
                                        :source     #45#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2
                                        :context    :form
                                        :source     #46#)
                                       :evaluation t)))
                        :source #43#)
                       :evaluation :compound)))
     :source #41#)))

;;; `[ec]typecase'

(define-macro-test (typecase)
  ;; Invalid syntax
  '(#1=(typecase)
    syn:invalid-syntax-error #1#)
  '((typecase x #2=1)
    syn:invalid-syntax-error #2#
    "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")
  '((typecase x #3=())
    syn:invalid-syntax-error #3#
    "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")
  '((typecase x (#4=(otherwise) 1))
    syn:invalid-syntax-error #4# "CL:OTHERWISE does not name a compound type")
  '((typecase x (otherwise 1) #5=(otherwise 2))
    syn:invalid-syntax-error #5#
    "otherwise clause must not be repeated")
  '((typecase x (otherwise 1) #6=(:normal 2))
    syn:invalid-syntax-error #6#
    "normal clause must not follow otherwise clause")
  ;; Valid syntax
  '(#7=(typecase #8=x)
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #8#)
                       :evaluation t)))
     :source #7#))
  '(#9=(typecase #10=x #11=(#12=y #13=1))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #10#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #12#))))
                                        :source #12#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #13#)
                                       :evaluation t)))
                        :source #11#)
                       :evaluation :compound)))
      :source #9#))
  '(#14=(typecase #15=x #16=(#17=y #18=1) #19=(#20=z #21=2))
    (:typecase
     ((:keyform . 1) (((:unparsed
                        ()
                        :expression x :context :form :source #15#)
                       :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #17#))))
                                        :source #17#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #18#)
                                       :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name z :source #20#))))
                                        :source #20#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #21#)
                                       :evaluation t)))
                        :source #19#)
                       :evaluation :compound)))
      :source #14#))
  '(#22=(typecase #23=x #24=(#25=(#26=y) #27=1 #28=2))
    (:typecase
        ((:keyform . 1) (((:unparsed
                           ()
                           :expression x :context :form :source #23#)
                          :evaluation t))
         (:clause  . *) (((:typecase-normal-clause
                           ((:type . 1) (((:compound-type-specifier
                                           ((:name . 1) (((:type-name () :name y :source #26#))))
                                           :source #25#)))
                            (:form . *) (((:unparsed
                                           ()
                                           :expression 1 :context :form :source #27#)
                                          :evaluation t)
                                         ((:unparsed
                                           ()
                                           :expression 2 :context :form :source #28#)
                                          :evaluation t)))
                           :source #24#)
                          :evaluation :compound)))
      :source #22#))
  '(#29=(typecase #30=x #31=(otherwise #32=1))
    (:typecase
        ((:keyform . 1) (((:unparsed
                           ()
                           :expression x :context :form :source #30#)
                          :evaluation t))
         (:clause  . *) (((:typecase-otherwise-clause
                           ((:form . *) (((:unparsed
                                           ()
                                           :expression 1 :context :form :source #32#)
                                          :evaluation t)))
                           :source #31#)
                          :evaluation :compound)))
      :source #29#)))

(define-macro-test (ctypecase)
  ;; Invalid syntax
  '(#1=(ctypecase)
    syn:invalid-syntax-error #1#)
  '((ctypecase #2=1)
    syn:invalid-syntax-error #2# "place must be a cons or a variable name")
  '((ctypecase x #3=1)
    syn:invalid-syntax-error #3# "must be a clause of the form (TYPE FORM*)")
  '((ctypecase x #4=())
    syn:invalid-syntax-error #4# "must be a clause of the form (TYPE FORM*)")
  '((ctypecase x (#5=otherwise 1))
    syn:invalid-syntax-error #5# "CL:OTHERWISE does not name a type")
  '((ctypecase x (#6=(otherwise) 1))
    syn:invalid-syntax-error #6# "CL:OTHERWISE does not name a compound type")
  ;; Valid syntax
  '(#7=(ctypecase #8=x)
    (:ctypecase
     ((:keyplace . 1) (((:unparsed
                         ()
                         :expression x :context :place :source #8#)
                        :evaluation t)))
     :source #7#))
  '(#9=(ctypecase #10=x #11=(#12=y #13=1 #14=2))
    (:ctypecase
     ((:keyplace . 1) (( (:unparsed
                          ()
                          :expression x :context :place :source #10#)
                         :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #12#))))
                                         :source #12#)))
                          (:form . *) (( (:unparsed
                                          ()
                                          :expression 1 :context :form :source #13#)
                                         :evaluation t)
                                       ( (:unparsed
                                          ()
                                          :expression 2 :context :form :source #14#)
                                         :evaluation t)))
                         :source #11#)
                        :evaluation :compound)))
     :source #9#))
  '(#15=(ctypecase #16=x #17=(#18=y #19=1) #20=(#21=z #22=2))
    (:ctypecase
     ((:keyplace . 1) (( (:unparsed
                          ()
                          :expression x :context :place :source #16#)
                         :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #18#))))
                                         :source #18#)))
                          (:form . *) (( (:unparsed
                                          ()
                                          :expression 1 :context :form :source #19#)
                                         :evaluation t)))
                         :source #17#)
                        :evaluation :compound)
                       ((:typecase-normal-clause
                         ((:type . 1) (((:atomic-type-specifier
                                         ((:name . 1) (((:type-name () :name z :source #21#))))
                                         :source #21#)))
                          (:form . *) (( (:unparsed
                                          ()
                                          :expression 2 :context :form :source #22#)
                                         :evaluation t)))
                         :source #20#)
                        :evaluation :compound)))
     :source #15#))
  '(#23=(ctypecase #24=x #25=(#26=(#27=y) #28=1))
    (:ctypecase
     ((:keyplace . 1) (( (:unparsed
                          ()
                          :expression x :context :place :source #24#)
                         :evaluation t))
      (:clause   . *) (((:typecase-normal-clause
                         ((:type . 1) (((:compound-type-specifier
                                         ((:name . 1) (((:type-name () :name y :source #27#))))
                                         :source #26#)))
                          (:form . *) (( (:unparsed
                                          ()
                                          :expression 1 :context :form :source #28#)
                                         :evaluation t)))
                         :source #25#)
                        :evaluation :compound)))
     :source #23#)))

(define-macro-test (etypecase)
  ;; Invalid syntax
  '(#1=(etypecase)
    syn:invalid-syntax-error #1#)
  '((etypecase x #2=1)
    syn:invalid-syntax-error #2# "must be a clause of the form (TYPE FORM*)")
  '((etypecase x #3=())
    syn:invalid-syntax-error #3# "must be a clause of the form (TYPE FORM*)")
  '((etypecase x (#4=otherwise 1))
    syn:invalid-syntax-error #4# "CL:OTHERWISE does not name a type")
  '((etypecase x (#5=(otherwise) 1))
    syn:invalid-syntax-error #5# "CL:OTHERWISE does not name a compound type")
  ;; Valid syntax
  '(#6=(etypecase #7=x)
    (:etypecase
     ((:keyform . 1) (((:unparsed () :expression x :context :form :source #7#)
                       :evaluation t)))
     :source #6#))
  '(#8=(etypecase #9=x #10=(#11=y #12=1 #13=2))
    (:etypecase
      ((:keyform . 1) (((:unparsed
                         ()
                         :expression x :context :form :source #9#)
                        :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #11#))))
                                        :source #11#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #12#)
                                       :evaluation t)
                                      ((:unparsed
                                        ()
                                        :expression 2 :context :form :source #13#)
                                       :evaluation t)))
                        :source #10#)
                       :evaluation :compound)))
     :source #8#))
  '(#14=(etypecase #15=x #16=(#17=y #18=1) #19=(#20=z #21=2))
    (:etypecase
      ((:keyform . 1) (((:unparsed
                         ()
                         :expression x :context :form :source #15#)
                        :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #17#))))
                                        :source #17#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #18#)
                                       :evaluation t)))
                        :source #16#)
                       :evaluation :compound)
                      ((:typecase-normal-clause
                        ((:type . 1) (((:atomic-type-specifier
                                        ((:name . 1) (((:type-name () :name z :source #20#))))
                                        :source #20#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 2 :context :form :source #21#)
                                       :evaluation t)))
                        :source #19#)
                       :evaluation :compound)))
      :source #14#))
  '(#22=(etypecase #23=x #24=(#25=(#26=y) #27=1))
    (:etypecase
      ((:keyform . 1) (((:unparsed
                         ()
                         :expression x :context :form :source #23#)
                        :evaluation t))
      (:clause  . *) (((:typecase-normal-clause
                        ((:type . 1) (((:compound-type-specifier
                                        ((:name . 1) (((:type-name () :name y :source #26#))))
                                        :source #25#)))
                         (:form . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source #27#)
                                       :evaluation t)))
                        :source #24#)
                       :evaluation :compound)))
     :source #22#)))
