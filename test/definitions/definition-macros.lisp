;;;; definition-macros.lisp --- Tests for definition macro rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.definition-macros
  :in :s-expression-syntax)

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
                              ((:required-section . 1) (((:required-section
                                                          ((:parameter . *) (((:required-parameter
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
                                                                               :source #9#))))))))
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
    syn:invalid-syntax-error #2# "must be a macro lambda list")
  '((defmacro foo (x (y #3=x)))
    syn:invalid-syntax-error #3# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#4=(defmacro #5=foo #6=(#7=a #8=b)
         #9="bla"
         (declare #10=(ignore #11=a))
         #12=(list 'cons b b))
    (:defmacro
     ((:name          . 1) (((:function-name () :name foo :source #5#)))
      (:lambda-list   . 1) (((:macro-lambda-list
                              ((:required-section . 1)
                               (((:required-section
                                  ((:parameter . *) (((:required-parameter
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
                                                      :evaluation :compound))))
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

(define-syntax-test (syn:slot-description)
  '((#1=1)
    syn:invalid-syntax-error #1# "slot name must be a symbol that is a valid variable name")
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
                                                   ((:required-section . 1) (((:required-section
                                                                               ((:parameter . *) (((:required-parameter
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
                                                                                                    :source #22#))))))))
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
  (syntax-test-cases (syn:slot-specifier)
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

(define-syntax-test (syn:condition-slot-specifier)
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
    syn:invalid-syntax-error #2# "must be a type name")
  '(#3=(deftype foo)
    syn:invalid-syntax-error #3#)
  '((deftype foo #4=1)
    syn:invalid-syntax-error #4# "must be a DEFTYPE lambda list")
  '((deftype foo (x #5=x))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
  ;; Valid syntax
  '(#6=(deftype #7=foo #8=(#9=a #10=&key #11=b)
         #12="bla bli"
         (declare #13=(ignore #14=a))
         (declare #15=(ignore #16=b))
         #17=(list a b))
    (:deftype
     ((:name          . 1) (((:type-name () :name foo :source #7#)))
      (:lambda-list   . 1) (((:deftype-lambda-list
                              ((:required-section . 1)
                               (((:required-section
                                  ((:parameter . *) (((:required-parameter
                                                       ((:name . 1) (((:variable-name
                                                                       ()
                                                                       :name a :source #9#)
                                                                      :evaluation nil)))
                                                       :source #9#)
                                                      :evaluation :compound))))
                                 :evaluation :compound))
                               (:keyword-section  . 1)
                               (((:keyword-section
                                  ((:keyword    . 1) (((:lambda-list-keyword () :keyword &key :source #10#)))
                                   (:parameter  . *) (((:keyword-parameter
                                                        ((:name . 1) (((:variable-name
                                                                        ()
                                                                        :name b :source #11#))))
                                                        :source #11#)
                                                       :evaluation :compound))))
                                 :evaluation :compound)))
                              :source #8#)
                             :evaluation :compound))
      (:documentation . 1) (((:documentation () :string "bla bli" :source #12#)))
      (:declaration   . *) (((:declaration-specifier
                              ((:argument . *) (((:variable-name () :name a :source #14#))))
                              :kind ignore :source #13#))
                            ((:declaration-specifier
                              ((:argument . *) (((:variable-name () :name b :source #16#))))
                              :kind ignore :source #15#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #17# :context :form :source #17#)
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
      (:argument-precedence-order . #5=()))
    syn:invalid-syntax-error #5# "at least one name must follow :ARGUMENT-PRECEDENCE-ORDER")
  '((defgeneric foo ()
      (:argument-precedence-order #6=1))
    syn:invalid-syntax-error #6# "variable name must be a symbol")
  '((defgeneric foo ()
      (:method-combination #7=1))
    syn:invalid-syntax-error #7# "method combination name must be a symbol")
  '((defgeneric foo ()
      (:method-class #8=1))
    syn:invalid-syntax-error #8# "must be a class name")
  '((defgeneric foo ()
      (declare . #9=()))
    syn:invalid-syntax-error #9# "at least one OPTIMIZE declaration specifier must follow DECLARE")
  '((defgeneric foo ()
      (declare #10=(type)))
    syn:invalid-syntax-error #10# "must be an OPTIMIZE declaration")
  '((defgeneric foo ()
      #11=(:method))
    syn:invalid-syntax-error #11# "must be of the form (:method QUALIFIER* LAMBDA-LIST DECLARATION* FORM*)")
  ;; Repeated options
  '((defgeneric foo ()
      (:generic-function-class bar)
      #12=(:generic-function-class bar))
    syn:invalid-syntax-error #12# ":GENERIC-FUNCTION-CLASS option must not be repeated")
  '((defgeneric foo (a)
      (:argument-precedence-order a)
      #13=(:argument-precedence-order a))
    syn:invalid-syntax-error #13# ":ARGUMENT-PRECEDENCE-ORDER option must not be repeated")
  '((defgeneric foo ()
      (:documentation "foo")
      #14=(:documentation "foo"))
    syn:invalid-syntax-error #14# ":DOCUMENTATION option must not be repeated")
  ;; Argument precedence order mismatch
  '((defgeneric foo (a b)
      (:argument-precedence-order . #15=(a c)))
    syn:invalid-syntax-error #15# "(A C) must match the set of required parameters (A B)")
  ;; Valid syntax
  '(#16=(defgeneric #17=foo #18=(#19=a #20=b)
         (:documentation #21="foo")
         (:generic-function-class #22=clazz))
    (:defgeneric
     ((:name                   . 1) (((:function-name () :name foo :source #17#)))
      (:lambda-list            . 1) (((:generic-function-lambda-list
                                       ((:required-section . 1) (((:required-section
                                                                   ((:parameter . *) (((:required-parameter
                                                                                        ((:name . 1) (((:variable-name
                                                                                                        ()
                                                                                                        :name a :source #19#)
                                                                                                       :evaluation nil)))
                                                                                        :source #19#))
                                                                                      ((:required-parameter
                                                                                        ((:name . 1) (((:variable-name
                                                                                                        ()
                                                                                                        :name b :source #20#)
                                                                                                       :evaluation nil)))
                                                                                        :source #20#))))))))
                                       :source #18#)
                                      :evaluation :compound))
      (:generic-function-class . 1) (((:type-name () :name clazz :source #22#)))
      (:documentation          . 1) (((:documentation () :string #21# :source #21#))))
     :source #16#))
  '(#23=(defgeneric #24=foo #25=(#26=a #27=b)
          (:argument-precedence-order #28=b #29=a))
    (:defgeneric
     ((:name                      . 1) (((:function-name () :name foo :source #24#)))
      (:lambda-list               . 1) (((:generic-function-lambda-list
                                          ((:required-section . 1) (((:required-section
                                                                      ((:parameter . *) (((:required-parameter
                                                                                           ((:name . 1) (((:variable-name
                                                                                                           ()
                                                                                                           :name a :source #26#)
                                                                                                          :evaluation nil)))
                                                                                           :source #26#))
                                                                                         ((:required-parameter
                                                                                           ((:name . 1) (((:variable-name
                                                                                                           ()
                                                                                                           :name b :source #27#)
                                                                                                          :evaluation nil)))
                                                                                           :source #27#))))))))
                                          :source #25#)
                                         :evaluation :compound))
      (:argument-precedence-order . *) (((:variable-name () :name b :source #28#))
                                        ((:variable-name () :name a :source #29#))))
     :source #23#))
  '(#30=(defgeneric #31=foo #32=(#33=a)
          #34=(:method #35=:custom #36=1 #37="foo" #38=(#39=a)))
    (:defgeneric
     ((:name        . 1) (((:function-name () :name foo :source #31#)))
      (:lambda-list . 1) (((:generic-function-lambda-list
                            ((:required-section . 1) (((:required-section
                                                        ((:parameter . *) (((:required-parameter
                                                                             ((:name . 1) (((:variable-name
                                                                                             ()
                                                                                             :name a :source #33#)
                                                                                            :evaluation nil)))
                                                                             :source #33#))))))))
                            :source #32#)
                           :evaluation :compound))
      (:method      . *) (((:method-description
                            ((:qualifier   . *) (((:unparsed
                                                   ()
                                                   :expression :custom
                                                   :context    :method-qualifier
                                                   :source     #35#))
                                                 ((:unparsed
                                                   ()
                                                   :expression 1
                                                   :context    :method-qualifier
                                                   :source     #36#))
                                                 ((:unparsed
                                                   ()
                                                   :expression "foo"
                                                   :context    :method-qualifier
                                                   :source     #37#)))
                             (:lambda-list . 1) (((:specialized-lambda-list
                                                   ((:required-section . 1) (((:required-section
                                                                               ((:parameter . *) (((:specialized-parameter
                                                                                                    ((:name . 1) (((:variable-name
                                                                                                                    ()
                                                                                                                    :name a :source #39#))))
                                                                                                    :source #39#)
                                                                                                   :evaluation :compound))))
                                                                              :evaluation :compound)))
                                                   :source #38#)
                                                  :evaluation :compound)))
                            :source #34#)
                           :evaluation :compound)))
     :source #30#))
  '(#40=(defgeneric #41=foo #42=()
           (:method-combination #43=progn))
    (:defgeneric
     ((:name               . 1) (((:function-name () :name foo :source #41#)))
      (:lambda-list        . 1) (((:generic-function-lambda-list () :source #42#)
                                  :evaluation :compound))
      (:method-combination . 1) (((:method-combination-name () :name progn :source #43#))))
     :source #40#))
  '(#44=(defgeneric #45=baz #46=()
          #47=(#48=:custom . #49=(1 2)))
    (:defgeneric
     ((:name        . 1) (((:function-name ():name baz :source #45#)))
      (:lambda-list . 1) (((:generic-function-lambda-list () :source #46#)
                           :evaluation :compound))
      (:option      . *) (((:generic-function-option
                            ((:name  . 1) (((:option-name () :name :custom :source #48#)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression (1 2)
                                             :context    :non-standard-defgeneric-option
                                             :source     #49#))))
                            :source #47#))))
      :source #44#)))

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
      (:lambda-list . 1) (((:specialized-lambda-list () :source #9#)
                           :evaluation :compound)))
     :source #7#))
  '(#10=(defmethod #11=foo #12=:around #13=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #11#)))
      (:qualifier   . *) (((:unparsed
                            ()
                            :expression :around
                            :context    :method-qualifier
                            :source     #12#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #13#)
                           :evaluation :compound)))
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
      (:lambda-list . 1) (((:specialized-lambda-list () :source #19#)
                           :evaluation :compound)))
     :source #14#))
  '(#20=(defmethod #21=foo #22=(#23=(#24=x #25=t)))
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #21#)))
      (:lambda-list . 1) (((:specialized-lambda-list
                            ((:required-section  . 1) (((:required-section
                                                         ((:parameter . *) (((:specialized-parameter
                                                                              ((:name        . 1) (((:variable-name
                                                                                                     ()
                                                                                                     :name x :source #24#)))
                                                                               (:specializer . 1) (((:type-name
                                                                                                     ()
                                                                                                     :name t :source #25#)
                                                                                                    :evaluation :compound)))
                                                                              :source #23#)
                                                                             :evaluation :compound))))
                                                        :evaluation :compound)))
                            :source #22#)
                           :evaluation :compound)))
     :source #20#))
  '(#26=(defmethod #27=foo #28=()
          #29="foo" (declare #30=(ignore)) #31=1)
    (:defmethod
     ((:name          . 1) (((:function-name () :name foo :source #27#)))
      (:lambda-list   . 1) (((:specialized-lambda-list () :source #28#)
                             :evaluation :compound))
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
      (:shadowing-import-from . *) (((:shadowing-import-from
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
