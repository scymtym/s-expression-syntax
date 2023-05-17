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
    syn:invalid-syntax-error #4# "must be a declaration specifier")
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
      (:declaration   . *) (((:declaration-specifier
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
      (:declaration   . *) (((:declaration-specifier
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
  (syntax-test-cases (syn::slot-specifier)
    ;; Invalid syntax
    '(#1=1
      syn:invalid-syntax-error #1# "slot name must be a symbol that is a valid variable name")
    '(#2=()
      syn:invalid-syntax-error #2# "slot must have a name")
    '((#3=1)
      syn:invalid-syntax-error #3# "slot name must be a symbol that is a valid variable name")
    '((foo :reader #4=1)
      syn:invalid-syntax-error #4# "reader must be a symbol function name")
    '((foo :reader #5=(setf foo))
      syn:invalid-syntax-error #5# "reader must be a symbol function name")
    '((foo :writer #6=1)
      syn:invalid-syntax-error #6# "writer must be an extended function name")
    '((foo :writer (setf #7=1))
      syn:invalid-syntax-error #7# "second element of SETF function name must be a symbol")
    '((foo :accessor #8=1)
      syn:invalid-syntax-error #8# "accessor must be a symbol function name")
    '((foo :accessor #9=(setf foo))
      syn:invalid-syntax-error #9# "accessor must be a symbol function name")
    '((foo :initarg #10=1)
      syn:invalid-syntax-error #10# "initarg name must be a symbol")
    '((foo :initform #11=(declare))
      syn:invalid-syntax-error #11# "declare is not allowed here")
    '((foo :type #12=1)
      syn:invalid-syntax-error #12# "must be a type specifier")
    '((foo :documentation #13=1)
      syn:invalid-syntax-error #13# "must be a documentation string")
    '((foo #14=1)
      syn:invalid-syntax-error #14# "option name must be a symbol")
    ;; Repeated options
    '((foo :allocation :class . #15=(:allocation :class))
      syn:invalid-syntax-error #15# ":ALLOCATION option must not be repeated")
    '((foo :initform 1 . #16=(:initform 1))
      syn:invalid-syntax-error #16# ":INITFORM option must not be repeated")
    '((foo :type bit . #17=(:type bit))
      syn:invalid-syntax-error #17# ":TYPE option must not be repeated")
    '((foo :documentation "" . #18=(:documentation ""))
      syn:invalid-syntax-error #18# ":DOCUMENTATION option must not be repeated")
    ;; Valid syntax
    '(#19=(#20=foo :initform #21=(+ 1) #22=:custom-option #23=:foo :reader #24=bar)
      (:slot-specifier
       ((:name     . 1) (((:variable-name () :name foo :source #20#)))
        (:reader   . *) (((:function-name () :name bar :source #24#)))
        (:initform . 1) (((:unparsed
                           ()
                           :expression #21# :context :form :source #21#)
                          :evaluation t))
        (:option  . *)  (((:slot-option
                           ((:name  . 1) (((:option-name
                                            ()
                                            :name   :custom-option
                                            :source #22#)))
                            (:value . 1) (((:unparsed
                                            ()
                                            :expression :foo
                                            :context    :non-standard-slot-option
                                            :source     #23#))))
                           :source #22#))))
       :source #19#))))

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
         (#16=(#17=foo :initform #18=(+ 1) #19=:custom-option #20=:foo :reader #21=bar))
         (:metaclass #22=foo)
         (:documentation #23="foo")
         (:default-initargs
          #24=:bar #25=1)
         #26=(#27=:my-class-option . #28=(1)))
    (:defclass
     ((:name             . 1) (((:type-name () :name foo :source #13#)))
      (:superclass       . *) (((:type-name () :name bar :source #14#))
                               ((:type-name () :name baz :source #15#)))
      (:slot             . *) (((:slot-specifier
                                 ((:name     . 1) (((:variable-name () :name foo :source #17#)))
                                  (:reader   . *) (((:function-name () :name bar :source #21#)))
                                  (:initform . 1) (((:unparsed
                                                         ()
                                                         :expression #18# :context :form :source #18#)
                                                        :evaluation t))
                                  (:option   . *) (((:slot-option
                                                     ((:name  . 1) (((:option-name
                                                                      ()
                                                                      :name   :custom-option
                                                                      :source #19#)))
                                                      (:value . 1) (((:unparsed
                                                                      ()
                                                                      :expression :foo
                                                                      :context    :non-standard-slot-option
                                                                      :source     #20#))))
                                                     :source #19#))))
                                 :source #16#)
                                :evaluation :compound))
      (:default-initarg  . *) (((:default-initarg
                                 ((:name     . 1) (((:initarg-name
                                                     ()
                                                     :name :bar :source #24#)))
                                  (:initform . 1) (((:unparsed
                                                     ()
                                                     :expression 1 :context :form :source #25#)
                                                    :evaluation t)))
                                 :source #24#)
                                :evaluation :compound))
      (:metaclass        . 1) (((:type-name () :name foo :source #22#)))
      (:documentation    . 1) (((:documentation () :string "foo" :source #23#)))
      (:option           . *) (((:class-option
                                 ((:name  . 1) (((:option-name
                                                  ()
                                                  :name   :my-class-option
                                                  :source #27#)))
                                  (:value . 1) (((:unparsed
                                                  ()
                                                  :expression (1)
                                                  :context    :non-standard-defclass-option
                                                  :source     #28#))))
                                 :source #26#))))
     :source #12#)))

(define-syntax-test (syn::condition-slot-specifier)
  ;; Invalid syntax
  '(#1=1
    syn:invalid-syntax-error #1# "slot name must be a symbol that is a valid variable name")
  '(#2=()
    syn:invalid-syntax-error #2# "slot must have a name")
  '((#3=2)
    syn:invalid-syntax-error #3# "slot name must be a symbol that is a valid variable name")
  '((%foo #4=1)
    syn:invalid-syntax-error #4# "option name must be a symbol")
  '((%foo :reader #5=3)
    syn:invalid-syntax-error #5# "reader must be a symbol function name")
  '((%foo :writer #6=3)
    syn:invalid-syntax-error #6# "writer must be an extended function name")
  '((%foo :writer (setf #7=4))
    syn:invalid-syntax-error #7# "second element of SETF function name must be a symbol")
  '((%foo :accessor #8=5)
    syn:invalid-syntax-error #8# "accessor must be a symbol function name")
  '((%foo :allocation #9=6)
    syn:invalid-syntax-error #9# "allocation must be :INSTANCE or :CLASS")
  '((%foo :initarg #10=6)
    syn:invalid-syntax-error #10# "initarg name must be a symbol")
  '((%foo :initform #11=(declare))
    syn:invalid-syntax-error #11# "declare is not allowed here")
  '((%foo :type #12=7)
    syn:invalid-syntax-error #12# "must be a type specifier")
  '((%foo :documentation #13=8)
    syn:invalid-syntax-error #13# "must be a documentation string")
  ;; Repeated options
  '((%foo :documentation "foo" . #14=(:documentation "bar"))
    syn:invalid-syntax-error #14# ":DOCUMENTATION option must not be repeated")
  ;; Valid syntax
  '(#15=%foo
    (:condition-slot-specifier
     ((:name . 1) (((:variable-name () :name %foo :source #15#))))
     :source #15#))
  '(#16=(%foo :initform #17=pi :type #18=bit)
    (:condition-slot-specifier
     ((:name     . 1) (((:variable-name () :name %foo :source #15#)))
      (:initform . 1) (((:unparsed
                         ()
                         :expression pi
                         :context    :form
                         :source     #17#)
                        :evaluation t))
      (:type     . 1) (((:atomic-type-specifier
                         ((:name . 1) (((:type-name
                                         ()
                                         :name   bit
                                         :source #18#))))
                         :source #18#))))
     :source #16#)))

(define-macro-test (define-condition)
  ;; Invalid syntax
  '(#1=(define-condition)
    syn:invalid-syntax-error #1#)
  '((define-condition #2=1)
    syn:invalid-syntax-error #2# "must be a class name")
  '(#3=(define-condition foo)
    syn:invalid-syntax-error #3#)
  '(#4=(define-condition foo ())
    syn:invalid-syntax-error #4#)
  '((define-condition foo (#5=1))
    syn:invalid-syntax-error #5# "superclass must be a class name" )
  '(#6=(define-condition foo ()
         1)
    syn:invalid-syntax-error #6#)
  '((define-condition foo ()
      ()
      (:default-initargs :foo . #7=()))
    syn:invalid-syntax-error #7# "default initarg must be a symbol followed by a form")
  ;; Repeated options
  '((define-condition foo ()
      ()
      (:documentation "foo") #8=(:documentation "bar"))
    syn:invalid-syntax-error #8# ":DOCUMENTATION option must not be repeated")
  ;; Valid syntax
  '(#9=(define-condition #10=foo () ())
    (:define-condition
     ((:name . 1) (((:type-name () :name foo :source #10#))))
     :source #9#))
  '(#11=(define-condition #12=bar () () (:default-initargs #13=:foo #14=1))
    (:define-condition
     ((:name             . 1) (((:type-name () :name bar :source #12#)))
      (:default-initarg  . *) (((:default-initarg
                                 ((:name     . 1) (((:initarg-name
                                                     ()
                                                     :name :foo :source #13#)))
                                  (:initform . 1) (((:unparsed
                                                     ()
                                                     :expression 1
                                                     :context    :form
                                                     :source     #14#)
                                                    :evaluation t)))
                                 :source #13#)
                                :evaluation :compound)))
     :source #11#))
  '(#15=(define-condition #16=baz () () (:documentation #17="bar"))
    (:define-condition
     ((:name . 1) (((:type-name () :name baz :source #16#)))
      (:documentation . 1) (((:documentation () :string "bar" :source #17#))))
     :source #15#))
  '(#18=(define-condition #19=fez () () (:report #20="bar"))
    (:define-condition
     ((:name   . 1) (((:type-name () :name fez :source #19#)))
      (:report . 1) (((:condition-report
                       ((:string . 1) (((:unparsed
                                         ()
                                         :expression "bar"
                                         :context    :condition-report
                                         :source     #17#))))
                       :source #20#)
                      :evaluation :compound)))
        :source #18#)))

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
      (:declaration   . *) (((:declaration-specifier
                              ((:argument . *) (((:variable-name () :name a :source #13#))))
                              :kind ignore :source #12#))
                            ((:declaration-specifier
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
    syn:invalid-syntax-error #8# "must be of the form (:method QUALIFIER* LAMBDA-LIST DECLARATION* FORM*)")
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
  ;; Argument precedence order mismatch
  '((defgeneric foo (a b)
      (:argument-precedence-order . #12=(a c)))
    syn:invalid-syntax-error #12# "(C A) must match the set of required parameters (A B)")
  ;; Valid syntax
  '(#13=(defgeneric #14=foo #15=(#16=a #17=b)
         (:documentation #18="foo")
         (:generic-function-class #19=clazz))
    (:defgeneric
     ((:name                   . 1) (((:function-name () :name foo :source #14#)))
      (:lambda-list            . 1) (((:generic-function-lambda-list
                                       ((:required . *) (((:required-parameter
                                                           ((:name . 1) (((:variable-name
                                                                           ()
                                                                           :name a :source #16#)
                                                                          :evaluation nil)))
                                                           :source #16#))
                                                         ((:required-parameter
                                                           ((:name . 1) (((:variable-name
                                                                           ()
                                                                           :name b :source #17#)
                                                                          :evaluation nil)))
                                                           :source #17#))))
                                       :source #15#)
                                      :evaluation :compound))
      (:generic-function-class . 1) (((:type-name () :name clazz :source #19#)))
      (:documentation          . 1) (((:documentation () :string #18# :source #18#))))
     :source #13#))
  '(#20=(defgeneric #21=foo #22=(#23=a #24=b)
          (:argument-precedence-order #25=b #26=a))
    (:defgeneric
     ((:name                      . 1) (((:function-name () :name foo :source #21#)))
      (:lambda-list               . 1) (((:generic-function-lambda-list
                                          ((:required . *) (((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name a :source #23#)
                                                                             :evaluation nil)))
                                                              :source #23#))
                                                            ((:required-parameter
                                                              ((:name . 1) (((:variable-name
                                                                              ()
                                                                              :name b :source #24#)
                                                                             :evaluation nil)))
                                                              :source #24#))))
                                          :source #22#)
                                         :evaluation :compound))
      (:argument-precedence-order . *) (((:variable-name () :name b :source #25#))
                                        ((:variable-name () :name a :source #26#))))
     :source #20#))
  '(#27=(defgeneric #28=foo #29=(#30=a)
          #31=(:method #32=:custom #33=1 #34="foo" #35=(#36=a)))
    (:defgeneric
     ((:name        . 1) (((:function-name () :name foo :source #28#)))
      (:lambda-list . 1) (((:generic-function-lambda-list
                            ((:required . *) (((:required-parameter
                                                ((:name . 1) (((:variable-name
                                                                ()
                                                                :name a :source #30#)
                                                               :evaluation nil)))
                                                :source #30#))))
                            :source #29#)
                           :evaluation :compound))
      (:method      . *) (((:method-description
                            ((:qualifier   . *) (((:unparsed
                                                   ()
                                                   :expression :custom
                                                   :context    :method-qualifier
                                                   :source     #32#))
                                                 ((:unparsed
                                                   ()
                                                   :expression 1
                                                   :context    :method-qualifier
                                                   :source     #33#))
                                                 ((:unparsed
                                                   ()
                                                   :expression "foo"
                                                   :context    :method-qualifier
                                                   :source     #34#)))
                             (:lambda-list . 1) (((:specialized-lambda-list
                                                   ((:required . *) (((:specialized-parameter
                                                                       ((:name . 1) (((:variable-name
                                                                                       ()
                                                                                       :name a :source #36#))))
                                                                       :source #36#))))
                                                   :source #35#)
                                                  :evaluation :compound)))
                            :source #31#)
                           :evaluation :compound)))
     :source #27#))
  '(#37=(defgeneric #38=foo #39=()
           (:method-combination #40=progn))
    (:defgeneric
     ((:name               . 1) (((:function-name () :name foo :source #38#)))
      (:lambda-list        . 1) (((:generic-function-lambda-list () :source #39#)
                                  :evaluation :compound))
      (:method-combination . 1) (((:method-combination-name () :name progn :source #40#))))
     :source #37#))
  '(#41=(defgeneric #42=baz #43=()
          #44=(#45=:custom . #46=(1 2)))
    (:defgeneric
     ((:name        . 1) (((:function-name ():name baz :source #42#)))
      (:lambda-list . 1) (((:generic-function-lambda-list () :source #43#)
                           :evaluation :compound))
      (:option      . *) (((:generic-function-option
                            ((:name  . 1) (((:option-name () :name :custom :source #45#)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression (1 2)
                                             :context    :non-standard-defgeneric-option
                                             :source     #46#))))
                            :source #44#))))
      :source #41#)))

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
      (:declaration   . *) (((:declaration-specifier () :kind ignore :source #30#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #31# :context :form :source #31#)
                             :evaluation t)))
      :source #26#)))

(define-macro-test (defpackage)
  ;; Invalid syntax
  '((defpackage #1=1)
    syn:invalid-syntax-error #1# "must be a string designator")
  '((defpackage foo #2=2)
    syn:invalid-syntax-error #2# "option must be a list of the form (:NAME . VALUE)")
  '((defpackage foo #3=(:unknown 1))
    syn:invalid-syntax-error #3# "unknown option")
  '((defpackage foo (:documentation #4=1))
    syn:invalid-syntax-error #4# "must be a documentation string")
  '((defpackage foo (:size #5="a"))
    syn:invalid-syntax-error #5# "package size must be a non-negative integer")
  '((defpackage foo (:size . #6=(1 2)))
    syn:invalid-syntax-error #6# ":SIZE option accepts one value")
  ;; Repeated options
  '((defpackage foo (:documentation "") #7=(:documentation ""))
    syn:invalid-syntax-error #7# ":DOCUMENTATION option must not be repeated")
  '((defpackage foo (:size 1) #8=(:size 2))
    syn:invalid-syntax-error #8# ":SIZE option must not be repeated")
  '((defpackage foo (:locked t) #9=(:locked nil))
    syn:invalid-syntax-error #9# ":LOCKED option must not be repeated")
  ;; Valid syntax
  '(#10=(defpackage #11=foo
         (:documentation #12="bla")
         (:use #13=:bar #14="bar")
         (:size #15=1)
         #16=(:import-from #17=:foo #18=#\c #19=:bar)
         #20=(:shadowing-import-from #21=:foo2 #22="BAZ2" #23=:bar2))
    (:defpackage
     ((:name                  . 1) (((:string-designator
                                      ()
                                      :string "FOO" :source #11#)))
      (:documentation         . 1) (((:documentation () :string #12# :source #12#)))
      (:use                   . *) (((:string-designator
                                      ()
                                      :string "BAR" :source #13#))
                                    ((:string-designator
                                      ()
                                      :string "bar" :source #14#)))
      (:shadowing-import-from . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO2" :source #21#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "BAZ2" :source #22#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR2" :source #23#))))
                                      :source #20#)))
      (:import-from           . *) (((:import-from
                                      ((:package . 1) (((:string-designator
                                                         ()
                                                         :string "FOO" :source #17#)))
                                       (:name    . *) (((:string-designator
                                                         ()
                                                         :string "c" :source #18#))
                                                       ((:string-designator
                                                         ()
                                                         :string "BAR" :source #19#))))
                                      :source #16#)))
      (:size                  . 1) (((:literal () :value 1 :source #15#))))
     :source #10#))
  '(#24=(defpackage #25=foo
          (:nicknames #26="f" #27=:fo)
          (:export #28="bar" #29=:baz))
    (:defpackage
     ((:name     . 1) (((:string-designator () :string "FOO" :source #25#)))
      (:nickname . *) (((:string-designator () :string "f" :source #26#))
                       ((:string-designator () :string "FO" :source #27#)))
      (:export   . *) (((:string-designator () :string "bar" :source #28#))
                       ((:string-designator () :string "BAZ" :source #29#))))
     :source #24#))
  ;; Non-standard options
  '(#30=(defpackage #31="baz"
      (:locked #32=nil)
      #33=(:local-nicknames #34=(#35="foo" #36="bar")))
    (:defpackage
     ((:name            . 1) (((:string-designator
                                () :string "baz" :source #31#)))
      (:local-nicknames . *) (((:local-nicknames
                                ((:local-nickname . *) (((:local-nickname
                                                          ((:local-nickname . 1) (((:string-designator
                                                                                    ()
                                                                                    :string "foo"
                                                                                    :source #35#)))
                                                           (:package-name   . 1) (((:string-designator
                                                                                    ()
                                                                                    :string "bar"
                                                                                    :source #36#))))
                                                          :source #34#))))
                                :source #33#)))
      (:locked          . 1) (((:literal () :value nil :source #32#))))
      :source #30#)))

(define-macro-test (in-package)
  ;; Invalid syntax
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

;;; `cond'

(define-macro-test (cond)
  ;; Invalid syntax
  '((cond #1=1)
    syn:invalid-syntax-error #1# "must be a clause of the form (TEST-FORM FORM*)")
  '((cond #2=())
    syn:invalid-syntax-error #2# "must be a clause of the form (TEST-FORM FORM*)")
  '((cond (1 #3=(declare)))
    syn:invalid-syntax-error #3# "declare is not allowed here")
  ;; Valid syntax
  '(#4=(cond)
    (:cond () :source #4#))
  '(#5=(cond #6=(#7=1))
    (:cond
      ((:clause . *) (((:cond-clause
                        ((:test-form . 1) (((:unparsed
                                             ()
                                             :expression 1 :context :form :source #7#)
                                            :evaluation t)))
                        :source #6#)
                       :evaluation :compound)))
      :source #5#))
  '(#8=(cond #9=(#10=1 #11=2))
    (:cond
      ((:clause . *) (((:cond-clause
                        ((:test-form . 1) (((:unparsed
                                             ()
                                             :expression 1 :context :form :source #10#)
                                            :evaluation t))
                         (:form      . *) (((:unparsed
                                             ()
                                             :expression 2 :context :form :source #11#)
                                            :evaluation t)))
                        :source #9#)
                       :evaluation :compound)))
      :source #8#))
  '(#12=(cond #13=(#14=1) #15=(#16=2))
    (:cond
      ((:clause . *) (((:cond-clause
                        ((:test-form . 1) (((:unparsed
                                             ()
                                             :expression 1 :context :form :source #14#)
                                            :evaluation t)))
                        :source #13#)
                       :evaluation :compound)
                      ((:cond-clause
                        ((:test-form . 1) (((:unparsed
                                             ()
                                             :expression 2 :context :form :source #16#)
                                            :evaluation t)))
                        :source #15#)
                       :evaluation :compound)))
      :source #12#)))
