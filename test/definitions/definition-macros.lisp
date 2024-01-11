;;;; definition-macros.lisp --- Tests for definition macro rules.
;;;;
;;;; Copyright (C) 2018-2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.definition-macros
  :in :s-expression-syntax)

;;; `defconstant', `defvar', `defparameter' and `define-symbol-macro'

(define-macro-test (defconstant)
  ;; Invalid syntax
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
  ;; Invalid syntax
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
  ;; Invalid syntax
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
  ;; Valid syntax
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

(define-syntax-test (define-symbol-macro)
  ;; Invalid syntax
  '(#1=(define-symbol-macro)
    syn:invalid-syntax-error #1#)
  '((define-symbol-macro #2=1)
    syn:invalid-syntax-error #2# "variable name must be a symbol")
  '(#3=(define-symbol-macro foo)
    syn:invalid-syntax-error #3#)
  '(#4=(define-symbol-macro foo 1 2)
    syn:invalid-syntax-error #4#)
  ;; Valid syntax
  '(#5=(define-symbol-macro #6=foo
         #7=(bar 1))
    (:define-symbol-macro
     ((:name      . 1) (((:variable-name () :name foo :source #6#)
                         :evaluation (:assignment :namespace variable)))
      (:expansion . 1) (((:unparsed
                          ()
                          :expression #7#
                          :context    :symbol-macro-expansion
                          :source     #7#))))
      :source #5#)))

;;; `defun', `defmacro', `define-modify-macro'

(define-macro-test (defun)
  '((defun (setf #1=1) ())
    syn:invalid-syntax-error #1# "second element of SETF function name must be a symbol")
  '((defun foo #2=1)
    syn:invalid-syntax-error #2# "must be an ordinary lambda list")
  '((defun foo (#3=(a b))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((defun foo (x #4=x))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
  '((defun foo () (declare #5=1))
    syn:invalid-syntax-error #5# "must be a declaration specifier")
  ;; Valid syntax
  '(#6=(defun #7=foo #8=(#9=bar #10=baz)
         #11="bla"
         #12=(declare #13=(type #14=integer #15=bar #16=baz))
         #17=(+ 1 2))
    (:defun
     ((:name          . 1) (((:function-name () :name foo :source #7#)))
      (:lambda-list   . 1) (((:ordinary-lambda-list
                              ((:required-section . 1)
                               (((:required-section
                                  ((:parameter . *) (((:required-parameter
                                                       ((:name . 1) (((:variable-name
                                                                       ()
                                                                       :name bar :source #9#))))
                                                       :source #9#))
                                                     ((:required-parameter
                                                       ((:name . 1) (((:variable-name
                                                                       ()
                                                                       :name baz :source #10#))))
                                                       :source #10#))))))))
                              :source #8#)
                             :evaluation :compound))
      (:declaration   . *) (((:declaration
                              ((:declaration-specifier . *)
                               (((:declaration-specifier
                                  ((:argument . *) (((:atomic-type-specifier
                                                      ((:name . 1) (((:type-name
                                                                      ()
                                                                      :name integer :source #14#))))
                                                      :source #14#))
                                                    ((:variable-name () :name bar :source #15#))
                                                    ((:variable-name () :name baz :source #16#))))
                                  :kind type :source #13#))))
                              :source #12#)))
      (:documentation . 1) (((:documentation () :string "bla" :source #11#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #17# :context :form :source #17#)
                             :evaluation t)))
     :source #6#)))

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
         #10=(declare #11=(ignore #12=a))
         #13=(list 'cons b b))
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
      (:declaration   . *) (((:declaration
                              ((:declaration-specifier . *)
                               (((:declaration-specifier
                                  ((:argument . *) (((:variable-name () :name a :source #12#))))
                                  :kind ignore :source #11#))))
                              :source #10#)))
      (:documentation . 1) (((:documentation () :string "bla" :source #9#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #13# :context :form :source #13#)
                             :evaluation t)))
     :source #4#))
  '(#14=(defmacro #15=bar #16=(#17=(#18=a #19=b))) ; test pattern in lambda list
    (:defmacro
     ((:name        . 1) (((:function-name () :name bar :source #15#)))
      (:lambda-list . 1) (((:macro-lambda-list
                            ((:required-section . 1)
                             (((:required-section
                                ((:parameter . *) (((:required-parameter
                                                     ((:name . 1)
                                                      (((:pattern
                                                         ((:required-section . 1)
                                                          (((:required-section
                                                             ((:parameter . *)
                                                              (((:required-parameter
                                                                 ((:name . 1)
                                                                  (((:variable-name
                                                                     ()
                                                                     :name a :source #18#)
                                                                    :evaluation nil)))
                                                                 :source #18#)
                                                                :evaluation :compound)
                                                               ((:required-parameter
                                                                 ((:name . 1)
                                                                  (((:variable-name
                                                                     ()
                                                                     :name b :source #19#)
                                                                    :evaluation nil)))
                                                                 :source #19#)
                                                                :evaluation :compound))))
                                                            :evaluation :compound)))
                                                         :source #17#)
                                                        :evaluation :compound)))
                                                     :source #17#)
                                                    :evaluation :compound))))
                               :evaluation :compound)))
                            :source #16#)
                           :evaluation :compound)))
     :source #14#)))


(define-syntax-test (define-modify-macro)
  ;; Invalid syntax
  '(#1=(define-modify-macro)
    syn:invalid-syntax-error #1#)
  '((define-modify-macro #2=1)
    syn:invalid-syntax-error #2# "must be a function name")
  '(#3=(define-modify-macro foo)
    syn:invalid-syntax-error #3#)
  '((define-modify-macro foo #4=1)
    syn:invalid-syntax-error #4# "must be an ordinary lambda list")
  '((define-modify-macro foo (#5=(a b))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #5# "variable name must be a symbol")
  '((define-modify-macro foo () #6=2)
    syn:invalid-syntax-error #6# "function name must be a symbol")
  '((define-modify-macro foo () foo #7=2)
    syn:invalid-syntax-error #7# "must be a documentation string")
  '(#8=(define-modify-macro foo () foo "" 2)
    syn:invalid-syntax-error #8#)
  ;; Valid syntax
  '(#9=(define-modify-macro #10=foo #11=() #12=foo #13="bar")
    (:define-modify-macro
     ((:name          . 1) (((:function-name () :name foo :source #10#)
                             :evaluation (:assignment :namespace function)))
      (:lambda-list   . 1) (((:ordinary-lambda-list
                              ()
                              :source #11#)
                             :evaluation :compound))
      (:function      . 1) (((:function-name () :name foo :source #12#)))
      (:documentation . 1) (((:documentation () :string "bar" :source #13#))))
     :source #9#)))

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
  '((defstruct (foo (:conc-name #2=1)))
    syn:invalid-syntax-error #2# "function name must be a symbol")
  '((defstruct (foo (:constructor foo #3=1)))
    syn:invalid-syntax-error #3# "must be an ordinary lambda list")
  '((defstruct (foo (:constructor foo (#4=(a b))))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #4# "variable name must be a symbol")
  '((defstruct (foo (:constructor foo (x #5=x))))
    syn:invalid-syntax-error #5# "the variable name X occurs more than once")
  '((defstruct (foo (:constructor nil) #6=(:constructor bar)))
    syn:invalid-syntax-error #6# "(:constructor nil) and named constructors are mutually exclusive")
  '((defstruct (foo (:constructor . #7=(foo () 1))))
    syn:invalid-syntax-error #7# ":CONSTRUCTOR option accepts one value")
  ;; Repeated options
  '((defstruct (foo (:include bar) #8=(:include bar)))
    syn:invalid-syntax-error #8# ":INCLUDE option must not be repeated")
  '((defstruct (foo (:initial-offset 1) #9=(:initial-offset 1)))
    syn:invalid-syntax-error #9# ":INITIAL-OFFSET option must not be repeated")
  '((defstruct (foo (:type list) #10=(:type list)))
    syn:invalid-syntax-error #10# ":TYPE option must not be repeated")
  ;; Valid syntax
  '(#11=(defstruct #12=foo)
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #12#))))
     :source #11#))
  '(#13=(defstruct (#14=foo))
    (:defstruct
     ((:name . 1) (((:type-name () :name foo :source #14#))))
     :source #13#))
  '(#15=(defstruct (#16=foo #17=(:constructor nil)))
    (:defstruct
     ((:name        . 1) (((:type-name () :name foo :source #16#)))
      (:constructor . *) (((:structure-constructor () :source #17#)
                           :evaluation :compound)))
     :source #15#))
  '(#18=(defstruct (#19=foo #20=(:constructor #21=foo #22=(#23=a #24=b))))
    (:defstruct
     ((:name        . 1) (((:type-name () :name foo :source #19#)))
      (:constructor . *) (((:structure-constructor
                            ((:name        . 1) (((:function-name () :name foo :source #21#)))
                             (:lambda-list . 1) (((:ordinary-lambda-list
                                                   ((:required-section . 1)
                                                    (((:required-section
                                                       ((:parameter . *) (((:required-parameter
                                                                            ((:name . 1) (((:variable-name
                                                                                            ()
                                                                                            :name a :source #23#))))
                                                                            :source #23#))
                                                                          ((:required-parameter
                                                                            ((:name . 1) (((:variable-name
                                                                                            ()
                                                                                            :name b :source #24#))))
                                                                            :source #24#))))))))
                                                   :source #22#)
                                                  :evaluation :compound)))
                            :source #20#)
                           :evaluation :compound)))
     :source #18#))
  '(#25=(defstruct #26=foo #27="doc")
    (:defstruct
     ((:name          . 1) (((:type-name () :name foo :source #26#)))
      (:documentation . 1) (((:documentation () :string #27# :source #27#))))
     :source #25#))
  '(#28=(defstruct (#29=bar (:conc-name #30=bar-)))
    (:defstruct
     ((:name      . 1) (((:type-name () :name bar :source #29#)))
      (:conc-name . 1) (((:function-name () :name bar- :source #30#))))
     :source #28#))
  '(#31=(defstruct (#32=baz #33=(:constructor #34=a) #35=(:constructor #36=b)))
    (:defstruct
     ((:name        . 1) (((:type-name () :name baz :source #32#)))
      (:constructor . *) (((:structure-constructor
                            ((:name . 1) (((:function-name () :name a :source #34#))))
                            :source #33#)
                           :evaluation :compound)
                          ((:structure-constructor
                            ((:name . 1) (((:function-name () :name b :source #36#))))
                            :source #35#)
                           :evaluation :compound)))
      :source #31#))
  '(#37=(defstruct (#38=fez (:include #39=woo #40=a #41=(#42=b))))
    (:defstruct
     ((:name         . 1) (((:type-name () :name fez :source #38#)))
      (:include      . 1) (((:type-name () :name woo :source #39#)))
      (:include-slot . *) (((:slot-description
                             ((:name . 1) (((:variable-name () :name a :source #40#))))
                             :source #40#))
                           ((:slot-description
                             ((:name . 1) (((:variable-name () :name b :source #42#))))
                             :source #41#))))
      :source #37#)))

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
                                 ; :source #24#
                                 )
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
                                 ; TODO :source #13#
                                 )
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
        :source #18#))
  '(#21=(define-condition fez () () (:report #22=(lambda #23=())))
    (:define-condition
     ((:name   . 1) (((:type-name () :name fez :source #19#)))
      (:report . 1) (((:condition-report
                       ((:lambda . 1) (((:lambda-expression
                                         ((:lambda-list . 1) (((:ordinary-lambda-list
                                                                ()
                                                                :source #23#)
                                                               :evaluation :compound)))
                                         :source #22#)
                                        :evaluation :compound)))
                       :source #22#)
                      :evaluation :compound)))
     :source #21#)))

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
         #13=(declare #14=(ignore #15=a))
         #16=(declare #17=(ignore #18=b))
         #19=(list a b))
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
      (:declaration   . *) (((:declaration
                              ((:declaration-specifier . *)
                               (((:declaration-specifier
                                  ((:argument . *) (((:variable-name () :name a :source #15#))))
                                  :kind ignore :source #14#))))
                              :source #13#))
                            ((:declaration
                              ((:declaration-specifier . *)
                               (((:declaration-specifier
                                  ((:argument . *) (((:variable-name () :name b :source #18#))))
                                  :kind ignore :source #17#))))
                              :source #16#)))
      (:documentation . 1) (((:documentation () :string "bla bli" :source #12#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #19# :context :form :source #19#)
                             :evaluation t)))
     :source #6#)))

(define-macro-test (defgeneric)
  '((defgeneric foo #1=1)
    syn:invalid-syntax-error #1# "must be a generic function lambda list")
  '((defgeneric foo (&key (a #2=(list))))
    syn:invalid-syntax-error #2# "keyword parameter initializer form is not allowed in a generic function lambda list")
  '((defgeneric foo (#3=(a b))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #3# "variable name must be a symbol")
  '((defgeneric foo (x #4=x))
    syn:invalid-syntax-error #4# "the variable name X occurs more than once")
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
      (:argument-precedence-order . #7=()))
    syn:invalid-syntax-error #7# "at least one name must follow :ARGUMENT-PRECEDENCE-ORDER")
  '((defgeneric foo ()
      (:argument-precedence-order #8=1))
    syn:invalid-syntax-error #8# "variable name must be a symbol")
  '((defgeneric foo ()
      (:method-combination #9=1))
    syn:invalid-syntax-error #9# "method combination name must be a symbol")
  '((defgeneric foo ()
      (:method-class #10=1))
    syn:invalid-syntax-error #10# "must be a class name")
  '((defgeneric foo ()
      (declare . #11=()))
    syn:invalid-syntax-error #11# "at least one OPTIMIZE declaration specifier must follow DECLARE")
  '((defgeneric foo ()
      (declare #12=(type)))
    syn:invalid-syntax-error #12# "must be an OPTIMIZE declaration")
  '((defgeneric foo ()
      #13=(:method))
    syn:invalid-syntax-error #13# "must be of the form (:method QUALIFIER* LAMBDA-LIST DECLARATION* FORM*)")
  '((defgeneric foo (&key x)
      (:method (&optional (#14=(x y) nil)))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #14# "variable name must be a symbol")
  '((defgeneric foo (&key x)
      (:method (&key (x nil #15=(y z))))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #15# "variable name must be a symbol")
  ;; Repeated options
  '((defgeneric foo ()
      (:generic-function-class bar)
      #16=(:generic-function-class bar))
    syn:invalid-syntax-error #16# ":GENERIC-FUNCTION-CLASS option must not be repeated")
  '((defgeneric foo (a)
      (:argument-precedence-order a)
      #17=(:argument-precedence-order a))
    syn:invalid-syntax-error #17# ":ARGUMENT-PRECEDENCE-ORDER option must not be repeated")
  '((defgeneric foo ()
      (:documentation "foo")
      #18=(:documentation "foo"))
    syn:invalid-syntax-error #18# ":DOCUMENTATION option must not be repeated")
  ;; Argument precedence order mismatch
  '((defgeneric foo (a b)
      (:argument-precedence-order . #19=(a c)))
    syn:invalid-syntax-error #19# "(A C) must match the set of required parameters (A B)")
  ;; Valid syntax
  '(#20=(defgeneric #21=foo #22=(#23=a #24=b)
         (:documentation #25="foo")
         (:generic-function-class #26=clazz))
    (:defgeneric
     ((:name                   . 1) (((:function-name () :name foo :source #21#)))
      (:lambda-list            . 1) (((:generic-function-lambda-list
                                       ((:required-section . 1) (((:required-section
                                                                   ((:parameter . *) (((:required-parameter
                                                                                        ((:name . 1) (((:variable-name
                                                                                                        ()
                                                                                                        :name a :source #23#))))
                                                                                        :source #23#))
                                                                                      ((:required-parameter
                                                                                        ((:name . 1) (((:variable-name
                                                                                                        ()
                                                                                                        :name b :source #24#))))
                                                                                        :source #24#))))))))
                                       :source #22#)
                                      :evaluation :compound))
      (:generic-function-class . 1) (((:type-name () :name clazz :source #26#)))
      (:documentation          . 1) (((:documentation () :string #25# :source #25#))))
     :source #20#))
  '(#27=(defgeneric #28=foo #29=(#30=a #31=b)
          (:argument-precedence-order #32=b #33=a))
    (:defgeneric
     ((:name                      . 1) (((:function-name () :name foo :source #28#)))
      (:lambda-list               . 1) (((:generic-function-lambda-list
                                          ((:required-section . 1) (((:required-section
                                                                      ((:parameter . *) (((:required-parameter
                                                                                           ((:name . 1) (((:variable-name
                                                                                                           ()
                                                                                                           :name a :source #30#))))
                                                                                           :source #30#))
                                                                                         ((:required-parameter
                                                                                           ((:name . 1) (((:variable-name
                                                                                                           ()
                                                                                                           :name b :source #31#))))
                                                                                           :source #31#))))))))
                                          :source #29#)
                                         :evaluation :compound))
      (:argument-precedence-order . *) (((:variable-name () :name b :source #32#))
                                        ((:variable-name () :name a :source #33#))))
     :source #27#))
  '(#34=(defgeneric #35=foo #36=(#37=a)
          #38=(:method #39=:custom #40=1 #41="foo" #42=(#43=a)))
    (:defgeneric
     ((:name        . 1) (((:function-name () :name foo :source #35#)))
      (:lambda-list . 1) (((:generic-function-lambda-list
                            ((:required-section . 1) (((:required-section
                                                        ((:parameter . *) (((:required-parameter
                                                                             ((:name . 1) (((:variable-name
                                                                                             ()
                                                                                             :name a :source #37#))))
                                                                             :source #37#))))))))
                            :source #36#)
                           :evaluation :compound))
      (:method      . *) (((:method-description
                            ((:qualifier   . *) (((:unparsed
                                                   ()
                                                   :expression :custom
                                                   :context    :method-qualifier
                                                   :source     #39#))
                                                 ((:unparsed
                                                   ()
                                                   :expression 1
                                                   :context    :method-qualifier
                                                   :source     #40#))
                                                 ((:unparsed
                                                   ()
                                                   :expression "foo"
                                                   :context    :method-qualifier
                                                   :source     #41#)))
                             (:lambda-list . 1) (((:specialized-lambda-list
                                                   ((:required-section . 1) (((:required-section
                                                                               ((:parameter . *) (((:specialized-parameter
                                                                                                    ((:name . 1) (((:variable-name
                                                                                                                    ()
                                                                                                                    :name a :source #43#))))
                                                                                                    :source #43#)
                                                                                                   :evaluation :compound))))
                                                                              :evaluation :compound)))
                                                   :source #42#)
                                                  :evaluation :compound)))
                            :source #38#)
                           :evaluation :compound)))
     :source #34#))
  '(#44=(defgeneric #45=foo #46=()
           (:method-combination #47=progn))
    (:defgeneric
     ((:name               . 1) (((:function-name () :name foo :source #45#)))
      (:lambda-list        . 1) (((:generic-function-lambda-list () :source #46#)
                                  :evaluation :compound))
      (:method-combination . 1) (((:method-combination-name () :name progn :source #47#))))
     :source #44#))
  '(#48=(defgeneric #49=baz #50=()
          #51=(#52=:custom . #53=(1 2)))
    (:defgeneric
     ((:name        . 1) (((:function-name ():name baz :source #49#)))
      (:lambda-list . 1) (((:generic-function-lambda-list () :source #50#)
                           :evaluation :compound))
      (:option      . *) (((:generic-function-option
                            ((:name  . 1) (((:option-name () :name :custom :source #52#)))
                             (:value . 1) (((:unparsed
                                             ()
                                             :expression (1 2)
                                             :context    :non-standard-defgeneric-option
                                             :source     #53#))))
                            :source #51#))))
      :source #48#)))

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
  '((defmethod foo ((#5=(a b) foo))) ; ensure parameter is not parsed as pattern
    syn:invalid-syntax-error #5# "variable name must be a symbol")
  '((defmethod foo (&optional (#6=(x y) nil)))
    syn:invalid-syntax-error #6# "variable name must be a symbol")
  '((defmethod foo (&key (x nil #7=(y z))))
    syn:invalid-syntax-error #7# "variable name must be a symbol")
  '((defmethod foo ((x #8=1)))
    syn:invalid-syntax-error #8# "must be a class name")
  '((defmethod foo (#9=(x t 1)))
    syn:invalid-syntax-error #9# "must be of the form (NAME SPECIALIZER)")
  ;; Valid syntax
  '(#10=(defmethod #11=foo #12=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #11#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #12#)
                           :evaluation :compound)))
     :source #10#))
  '(#13=(defmethod #14=foo #15=:around #16=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #14#)))
      (:qualifier   . *) (((:unparsed
                            ()
                            :expression :around
                            :context    :method-qualifier
                            :source     #15#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #16#)
                           :evaluation :compound)))
     :source #13#))
  '(#17=(defmethod #18=foo #19=:custom #20=1 #21="foo" #22=())
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #18#)))
      (:qualifier   . *) (((:unparsed
                            ()
                            :expression :custom
                            :context    :method-qualifier
                            :source     #19#))
                          ((:unparsed
                            ()
                            :expression 1
                            :context    :method-qualifier
                            :source     #20#))
                          ((:unparsed
                            ()
                            :expression "foo"
                            :context    :method-qualifier
                            :source     #21#)))
      (:lambda-list . 1) (((:specialized-lambda-list () :source #22#)
                           :evaluation :compound)))
     :source #17#))
  '(#23=(defmethod #24=foo #25=(#26=(#27=x #28=t)))
    (:defmethod
     ((:name        . 1) (((:function-name () :name foo :source #24#)))
      (:lambda-list . 1) (((:specialized-lambda-list
                            ((:required-section  . 1)
                             (((:required-section
                                ((:parameter . *) (((:specialized-parameter
                                                     ((:name        . 1) (((:variable-name
                                                                            ()
                                                                            :name x :source #27#)))
                                                      (:specializer . 1) (((:type-name
                                                                            ()
                                                                            :name t :source #28#)
                                                                           :evaluation :compound)))
                                                     :source #26#)
                                                    :evaluation :compound))))
                               :evaluation :compound)))
                            :source #25#)
                           :evaluation :compound)))
     :source #23#))
  '(#29=(defmethod #30=foo #31=()
          #32="foo" #33=(declare #34=(ignore)) #35=1)
    (:defmethod
     ((:name          . 1) (((:function-name () :name foo :source #30#)))
      (:lambda-list   . 1) (((:specialized-lambda-list () :source #31#)
                             :evaluation :compound))
      (:declaration   . *) (((:declaration
                              ((:declaration-specifier . *)
                               (((:declaration-specifier () :kind ignore :source #34#))))
                              :source #33#)))
      (:documentation . 1) (((:documentation () :string #32# :source #32#)))
      (:form          . *) (((:unparsed
                              ()
                              :expression #35# :context :form :source #35#)
                             :evaluation t)))
      :source #29#)))

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
  '((defpackage foo (:lock t) #9=(:lock nil))
    syn:invalid-syntax-error #9# ":LOCK option must not be repeated")
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
  '(#30=(defpackage #31=fez
           (:shadow #32="a" #33=:b #34=#:c))
    (:defpackage
     ((:name   . 1) (((:string-designator () :string "FEZ" :source #31#)))
      (:shadow . *) (((:string-designator () :string "a" :source #32#))
                     ((:string-designator () :string "B" :source #33#))
                     ((:string-designator () :string "C" :source #34#))))
      :source #30#))
  ;; Non-standard options
  '(#35=(defpackage #36="baz"
      (:lock #37=nil)
      #38=(:local-nicknames #39=(#40="foo" #41="bar")))
    (:defpackage
     ((:name            . 1) (((:string-designator
                                () :string "baz" :source #36#)))
      (:local-nicknames . *) (((:local-nicknames
                                ((:local-nickname . *) (((:local-nickname
                                                          ((:local-nickname . 1) (((:string-designator
                                                                                    ()
                                                                                    :string "foo"
                                                                                    :source #40#)))
                                                           (:package-name   . 1) (((:string-designator
                                                                                    ()
                                                                                    :string "bar"
                                                                                    :source #41#))))
                                                          :source #39#))))
                                :source #38#)))
      (:lock            . 1) (((:literal () :value nil :source #37#))))
      :source #35#)))

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
