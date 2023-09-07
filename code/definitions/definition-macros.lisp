;;;; definition-macros.lisp --- Standard macros for definitions.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macros `defconstant', `defvar' and `defparameter'

(define-macro defconstant
    (list (<- name ((variable-name! names)))
          (<- initial-value ((form! forms)))
          (? (<- documentation ((documentation-string! forms)))))
    `(,name ,initial-value ,@(? documentation))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

(define-macro defvar
    (list (<- name ((variable-name! names)))
          (? (seq (<- initial-value ((form! forms)))
                  (? (<- documentation ((documentation-string! forms)))))))
    `(,name ,@(? initial-value-supplied? initial-value) ,@(? documentation))
  ((name          1)
   (initial-value ? :evaluation t)
   (documentation ?)))

(define-macro defparameter
    (list (<- name ((variable-name! names)))
          (<- initial-value ((form! forms)))
          (? (<- documentation ((documentation-string! forms)))))
    `(,name ,initial-value ,@(? documentation))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

;;; Standard macro `define-symbol-macro'

(define-macro define-symbol-macro
    (list (<- name ((variable-name! names)))
          (<- expansion ((unparsed-expression forms) :symbol-macro-expansion)))
  `(,name ,expansion)
  ((name      1 :evaluation (make-instance 'assignment-semantics
                                           :namespace 'variable))
   (expansion 1)))

;;; Standard macro `defun'

(define-macro defun
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (declaration documentation form) ((docstring-body forms))))
    `(,name ,lambda-list ,@(? documentation) ,@declaration ,@form)
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

;;; Standard macro `define-compiler-macro'

(define-macro define-compiler-macro
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((macro-lambda-list! macro-lambda-list)))
           (<- (declaration documentation form) ((docstring-body forms))))
    `(,name ,lambda-list ,@(? documentation) ,@declaration ,@form)
  ((name          1)
   (lambda-list   1)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

;;; Standard macro `defmacro'

(define-macro defmacro
    (list* (<- name ((function-name/symbol! names)))
           (<- lambda-list ((macro-lambda-list! macro-lambda-list)))
           (<- (declaration documentation form) ((docstring-body forms))))
    `(,name ,lambda-list ,@(? documentation) ,@declaration ,@form)
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

;;; Standard macro `define-modify-macro'

(define-macro define-modify-macro
    (list (<- name ((function-name! names)))
          (<- lambda-list ((ordinary-lambda-list! lambda-lists))) ; TODO define-modify-macro-lambda-list
          (<- function ((function-name/symbol! names)))
          (? (<- documentation ((documentation-string! forms)))))
    `(,name ,lambda-list ,function ,@(? documentation))
  ((name          1 :evaluation (make-instance 'assignment-semantics
                                               :namespace 'function))
   (lambda-list   1 :evaluation :compound)
   (function      1)
   (documentation ?)))

;;; Standard macro `defstruct' (including slot description, constructor, ...)

(define-syntax slot-description
    (or (and (list* :any)
             (must (list (<- name ((slot-name! names)))
                         (? (seq (<- initform ((form! forms)))
                                 (* (or (eg:poption :read-only (<- read-only ((unparsed-expression forms) ':slot-read-only)))
                                        (eg:poption :type      (<- type ((type-specifier! type-specifiers)))))))))
                   "must be of the form (NAME [INITFORM] ...)"))
        (<- name ((variable-name! names))))
    (if initform-supplied?
        `(,name ,@(? initform-supplied? initform)
                ,@(? read-only-supplied? :read-only read-only)
                ,@(? type-supplied?      :type      type))
        name)
  ((name      1)
   (initform  ? :evaluation t)
   (read-only ?)
   (type      ?)))

(defrule structure-constructor-name ()
  ((function-name/symbol! names)))

(define-syntax (structure-constructor :arguments ((other-constructors '())))
    (:transform
       (eg:option* :constructor
                   (? (or 'nil ; NAME remains `nil'
                          (seq (<- name (structure-constructor-name))
                               (? (<- lambda-list ((ordinary-lambda-list! lambda-lists))))))))  ; TODO boa-lambda-list
     (let ((names (list* name (map 'list (lambda (constructor)
                                           (bp:node-relation* '(:name . 1) constructor))
                                   other-constructors))))
       (when (and (not (= (length names) 1)) (member nil names))
         (:fatal "(:constructor nil) and named constructors are mutually exclusive"))))
    `(:constructor ,name ,@(? lambda-list-supplied? lambda-list))
  ((name        1)
   (lambda-list ? :evaluation :compound)))

(macrolet ((define-function-option (name option &optional (symbol? t))
             `(defrule ,name ()
                  ,(let ((list-syntax `(eg:option* ; TODO can these really be repeated?
                                        ,option
                                        (? (<- name (or 'nil
                                                        ((function-name/symbol! names))))))))
                     (if symbol?
                         `(or ,option ,list-syntax)
                         list-syntax))
                (or name t))))
  (define-function-option structure-conc-name      :conc-name)
  (define-function-option structure-copier         :copier)
  (define-function-option structure-predicate      :predicate)
  (define-function-option structure-print-object   :print-object   nil)
  (define-function-option structure-print-function :print-function nil))

(defrule structure-name ()
    (or (list (and (<- name ((class-name! names)))
                   (<- constructors (:transform :any '()))) ; HACK
              (:transform
                 (* (or (<- conc-name               (structure-conc-name))
                        (<<- constructors           (structure-constructor constructors))
                        (<- copier                  (structure-copier))
                        (eg:option :include         (<- include ((class-name! names)))
                                                    (* (<<- include-slots (slot-description))))
                        (eg:option :initial-offset  (must (guard offset (typep '(integer 0)))
                                                          "must be a valid offset"))
                        (<- named                   :named)
                        (<- predicate               (structure-predicate))
                        (<- print-object            (structure-print-object))
                        (<- print-function          (structure-print-function))
                        (eg:option :type            (<- type ((type-specifier! type-specifiers))))
                        (:transform :any
                          (:fatal "must be a DEFSTRUCT option"))))
                 (when (and offset (not type))
                   (:fatal (format nil "Cannot specify ~S without ~S"
                                   :initial-offset :type)))
                 (when (and type (or print-object print-function))
                   (:fatal (format nil "The options ~S and ~S are mutually exclusive"
                                   :type (cond (print-object   :print-object)
                                               (print-function :print-function)))))))
        (<- name ((class-name! names))))
    (list name conc-name (nreverse constructors) include (nreverse include-slots)))

(define-macro defstruct
    (list (<- (name conc-name constructor include include-slot)
              (structure-name))
          (? (<- documentation ((documentation-string forms))))
          (* (<<- slot (slot-description))))
    `(,(if (or constructor-supplied? include-supplied?)
           `(,name
             ,@(? conc-name-supplied? `(:conc-name   ,conc-name))
             ,@constructor
             ,@(? include-supplied?   `(:include     ,include     ,@include-slot)))
           name)
      ,@(? documentation)
      ,@slot)
  ((name          1)
   (conc-name     ?)
   (constructor   *> :evaluation :compound)
   (include       ?)
   (include-slot  *>)
   (documentation ?)
   (slot          *  :evaluation :compound)))

;;; Standard macros `defclass' and `define-condition' (including slots, ...)

(defrule allocation-type! ()
    (value (source)
      (must (<- value (or :instance :class))
            "allocation must be :INSTANCE or :CLASS"))
  (let ((value (eg::%naturalize value))) ; TODO make a literal rule
    (bp:node* (:literal :value value :source source))))

(defrule (slot-option :environment (make-instance 'eg::expression-environment)) ()
    (value (source)
      (seq (<- name  ((option-name! names)))
           (<- value ((unparsed-expression forms) ':non-standard-slot-option))))
  (bp:node* (:slot-option :source source)
    (1 (:name  . 1) name)
    (1 (:value . 1) value)))

(defrule (slot-option! :environment (make-instance 'eg::expression-environment)) ()
  (must (slot-option)
        "slot option must be a symbol followed by an expression"))

(define-syntax slot-specifier
    (or (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- reader       (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writer       (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessor     (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (allocation-type!)))
                     (eg:poption* :initarg       (<<- initarg      ((initarg-name! names))))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))
                     (and :any (<<- option (slot-option!))))))
        (<- name ((slot-name! names))))
    `(,name
      ,@(a:mappend (a:curry #'list :initarg)  initarg)
      ,@(a:mappend (a:curry #'list :reader)   reader)
      ,@(a:mappend (a:curry #'list :writer)   writer)
      ,@(a:mappend (a:curry #'list :accessor) accessor)
      ,@(? allocation-supplied?    :allocation    allocation)
      ,@(? initform-supplied?      :initform      initform)
      ,@(? type-supplied?          :type          type)
      ,@(? documentation           :documentation documentation)
      ,@option)
  ((name          1)
   ;; Options
   (initarg       *)
   (reader        *)
   (writer        *)
   (accessor      *)
   (allocation    ?)
   (initform      ? :evaluation t)
   (type          ?)
   (documentation ?)
   ;; Non-standard options
   (option        *)))

(defrule superclasses ()
    (list (* (and :any (must (<<- superclasses ((class-name names)))
                             "superclass must be a class name"))))
  (nreverse superclasses))

(defrule (default-initarg :environment (make-instance 'eg::expression-environment)) ()
    (value (source)
      (seq (<- name     ((initarg-name! names)))
           (<- initform ((form! forms)))))
  source
  (bp:node* (:default-initarg ; :source source
             )
    (1 (:name     . 1) name)
    (1 (:initform . 1) initform :evaluation t)))

(defrule (default-initarg! :environment (make-instance 'eg::expression-environment)) ()
  (must (default-initarg)
        "default initarg must be a symbol followed by a form"))

(defrule class-option ()
    (value (source)
      (list* (<- name  ((option-name! names)))
             (<- value ((unparsed-expression forms) ':non-standard-defclass-option))))
  (bp:node* (:class-option :source source)
    (1 (:name  . 1) name)
    (1 (:value . 1) value)))

(define-macro defclass
    (list (<- name ((class-name! names)))
          (<- superclass (superclasses))
          (list (* (<<- slot (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (eg:option :default-initargs (* (and :any (<<- default-initarg (default-initarg!)))))
                 (eg:option :metaclass        (must (<- metaclass ((class-name names)))
                                                    "metaclass must be a class name"))
                 (eg:option :documentation    (<- documentation ((documentation-string! forms))))
                 ;; Non-standard options are basically free-form
                 (<<- option (class-option)))))
    `(,name (,@superclass)
        (,@slot)
      ,@default-initarg
      ,@(? metaclass     `(:metaclass ,metaclass))
      ,@(? documentation `(:documentation ,documentation))
      ,@option)
  ((name            1)
   (superclass      *>)
   (slot            *  :evaluation :compound)
   ;; Standard options
   (default-initarg *  :evaluation :compound)
   (metaclass       ?)
   (documentation   ?)
   ;; Non-standard options
   (option          *)))

(define-syntax condition-slot-specifier
    (or (and (not (list* :any)) (<- name ((slot-name! names))))
        (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- reader       (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writer       (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessor     (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (allocation-type!)))
                     (eg:poption* :initarg       (<<- initarg      ((initarg-name! names))))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))
                     (and :any (must (guard (typep 'symbol)) "option name must be a symbol"))))))
    `(,name
      ,@(a:mappend (a:curry #'list :initarg)  initarg)
      ,@(a:mappend (a:curry #'list :reader)   reader)
      ,@(a:mappend (a:curry #'list :writer)   writer)
      ,@(a:mappend (a:curry #'list :accessor) accessor)
      ,@(? allocation-supplied? :allocation    allocation)
      ,@(? initform-supplied?   :initform      initform)
      ,@(? type-supplied?       :type          type)
      ,@(? documentation        :documentation documentation))
  ((name          1)
   ;; Options
   (initarg       *)
   (reader        *)
   (writer        *)
   (accessor      *)
   (allocation    ?)
   (initform      ? :evaluation t)
   (type          ?)
   (documentation ?)))

(define-syntax condition-report
    (or (<- string   (and (guard (typep 'string))
                          ((unparsed-expression forms) :condition-report)))
        (<- function ((function-name/symbol names)))
        (<- lambda   (lambda-expression :lambda-expression)))
  (or string function lambda)
  ((string   ?)
   (function ?)
   (lambda   ? :evaluation :compound)))

(defrule condition-report! ()
  (must (condition-report)
        "report must be a string, a symbol naming a function or a lambda expression"))

(define-macro define-condition
    (list (<- name ((class-name! names)))
          (<- parent-type (superclasses))
          (list (* (<<- slot (condition-slot-specifier))))
          (* (or (eg:option :default-initargs (* (and :any (<<- default-initarg (default-initarg!)))))
                 (eg:option :documentation    (<- documentation ((documentation-string! forms))))
                 (eg:option :report           (<- report (condition-report!))))))
    `(,name (,@parent-type)
        (,@slot)
      ,@default-initarg
      ,@(? documentation `(:documentation ,documentation))
      ,@(? report        `(:report        ,report)))
  ((name             1)
   (parent-type      *>)
   (slot             *  :evaluation :compound)
   (default-initarg  *  :evaluation :compound)
   (documentation    ?)
   (report           ?  :evaluation :compound)))

;;; Standard macro `deftype'

(define-macro deftype
    (list* (<- name ((type-name! names)))
           (<- lambda-list ((deftype-lambda-list! deftype-lambda-list)))
           (<- (declaration documentation form) ((docstring-body forms))))
    `(,name ,lambda-list ,@declaration ,@(? documentation) ,@form)
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

;;; Standard macro `defgeneric'

(defrule qualifier ()
  (and (not (guard listp))
       ((unparsed-expression forms) :method-qualifier)))

(define-syntax method-description
    (and (list* :method :any)
         (must (list* :method
                      (* (<<- qualifier (qualifier)))
                      (<- lambda-list ((specialized-lambda-list! lambda-lists)))
                      (<- (declaration documentation form) ((docstring-body forms))))
               "must be of the form (:method QUALIFIER* LAMBDA-LIST DECLARATION* FORM*)"))
    `(:method ,@qualifier ,lambda-list
      ,@declaration ,@(? documentation) ,@form)
  ((qualifier     *)
   (lambda-list   1  :evaluation :compound)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

(defun verify-precedence-order (precedence-order lambda-list)
  (let* ((required-section (bp:node-relation* :required-section lambda-list))
         (required-names   (map 'list (lambda (parameter)
                                        (let ((name (bp:node-relation* :name parameter)))
                                          (getf (bp:node-initargs* name) :name)))
                                (bp:node-relation* :parameter required-section)))
         (order-names      (map 'list (lambda (name)
                                        (getf (bp:node-initargs* name) :name))
                                precedence-order)))
    (values (a:set-equal order-names required-names :test #'eg::%eql)
            required-names
            order-names)))

(define-syntax generic-function-option
    (list* (<- name  ((option-name! names)))
           (<- value ((unparsed-expression forms) ':non-standard-defgeneric-option)))
    `(,name ,value)
  ((name  1)
   (value 1)))

(define-macro defgeneric
    (list (<- name ((function-name! names)))
          (<- lambda-list ((generic-function-lambda-list! lambda-lists)))
          (* (or ;; Standard options
                 (eg:option  :generic-function-class
                             (<- generic-function-class ((class-name! names))))
                 (eg:option  :argument-precedence-order
                             (<- argument-precedence-order
                                 (:transform (* (<<- names ((lambda-list-variable-name! lambda-lists))))
                                  (when (null names)
                                    (:fatal (format nil "at least one name must follow ~S"
                                                    ':argument-precedence-order)))
                                  ;; Collect names of required parameters in
                                  ;; LAMBDA-LIST and ensure that all names
                                  ;; following :ARGUMENT-PRECEDENCE-ORDER
                                  ;; occur in that set of names.
                                  (let ((names (nreverse names)))
                                    (multiple-value-bind (compatiblep required-names order-names)
                                        (verify-precedence-order names lambda-list)
                                      (unless compatiblep
                                        (:fatal (format nil "~S must match the set of required parameters ~:S"
                                                        order-names required-names))))
                                    names))))
                 (eg:option  :method-combination
                             (<- method-combination ((method-combination-name! names)))
                             (* (<<- method-combination-argument)))
                 (eg:option  :method-class
                             (<- method-class ((class-name! names))))
                 (eg:option* declare
                             (:transform
                                (* (and :any
                                        (must (list* 'optimize :any) "must be an OPTIMIZE declaration")
                                        (<<- declaration ((declaration-specifier! declarations)))))
                              (when (null declaration)
                                (:fatal (format nil "at least one ~S declaration specifier must follow ~S"
                                                'optimize 'declare)))
                              declaration))
                 (eg:option  :documentation
                             (<- documentation ((documentation-string! forms))))
                 (<<- method (method-description))
                 ;; Non-standard options are or the form (:NON-STANDARD-NAME . VALUE).
                 (<<- option (generic-function-option)))))
    `(,name ,lambda-list
      ,@(? generic-function-class    `(:generic-function-class    ,generic-function-class))
      ,@(? argument-precedence-order `(:argument-precedence-order ,@argument-precedence-order))
      ,@(? method-combination        `(:method-combination        ,method-combination
                                                                  ,@method-combination-argument))
      ,@(? method-class              `(:method-class              ,method-class))
      ,@(? documentation             `(:documentation             ,documentation))
      ,@(mapcar (a:curry 'list* 'declare) declaration)
      ,@method
      ,@option)
  ((name                        1)
   (lambda-list                 1 :evaluation :compound)
   ;; Standard options
   (generic-function-class      ?)
   (argument-precedence-order   *>)
   (method-combination          ?)
   (method-combination-argument *)
   (method-class                ?)
   (declaration                 *)
   (documentation               ?)
   (method                      * :evaluation :compound)
   ;; Non-standard options
   (option                      *)))

;;; Standard macro `defmethod'

(define-macro defmethod
    (list* (<- name ((function-name! names)))
           (* (<<- qualifier (qualifier)))
           (<- lambda-list ((specialized-lambda-list! lambda-lists)))
           (<- (declaration documentation form) ((docstring-body forms))))
    `(,name ,@qualifier ,lambda-list
      ,@declaration ,@(? documentation) ,@form)
  ((name          1)
   (qualifier     *)
   (lambda-list   1 :evaluation :compound)
   (declaration   *>)
   (documentation ?)
   (form          *> :evaluation t)))

;;; Standard macros `defpackage' and `in-package'

(defrule string-designator ()
    (value (source)
      (or (<- string (:transform (guard character (typep 'character))
                       (string (eg::%naturalize character))))
          (<- string (:transform (guard string (typep 'string))
                       (eg::%naturalize string)))
          (structure 'symbol (symbol-name string))))
  (bp:node* (:string-designator :string string :source source)))

(defrule string-designator! ()
  (must (string-designator) "must be a string designator"))

(defrule package-designator ()
  (or (string-designator) (guard (typep 'package))))

(defrule package-designator! ()
  (must (package-designator) "must be a package designator"))

(macrolet ((define (name keyword)
             `(define-syntax ,name
                  (list* ',keyword
                         (must (list (<- package (package-designator!))
                                     (* (<<- name (and :any (string-designator!)))))
                               ,(format nil "~A accepts a package designator ~
                                             followed by string designators"
                                        keyword)))
                `(,,keyword ,package ,@name)
                ((package 1)
                 (name    *)))))
  (define import-from           :import-from)
  (define shadowing-import-from :shadowing-import-from))

(defrule package-size ()
    (value (source)
      (guard size (typep '(integer 0))))
  (let ((size (eg::%naturalize size)))
    (bp:node* (:literal :value size :source source))))

(defrule package-size! ()
  (must (package-size) "package size must be a non-negative integer"))

(define-syntax local-nickname
    (list (<- local-nickname (string-designator!))
          (<- package-name   (string-designator!)))
    `(,local-nickname ,package-name)
  ((local-nickname 1)
   (package-name   1)))

(define-syntax local-nicknames
    (list :local-nicknames
          (* (and :any (must (<<- local-nickname (local-nickname))
                             "local nickname must be of the form (LOCAL-NICKNAME PACKAGE-NAME)"))))
    `(:local-nicknames ,@local-nickname)
  ((local-nickname *)))

(defrule package-lock ()
    (value (source)
      (guard locked (typep 'boolean)))
  (let ((locked (eg::%naturalize locked)))
    (bp:node* (:literal :value locked :source source))))

(defrule package-lock! ()
  (must (package-lock) "package lock must be a Boolean"))

(define-macro defpackage
    (list (must (<- name (string-designator!)) "name is required")
          (* (or (eg:option* :nicknames     (* (<<- nickname (string-designator!))))
                 (eg:option  :documentation (<- documentation ((documentation-string! forms))))
                 (eg:option* :use           (* (<<- use (package-designator!))))
                 (eg:option* :shadow        (* (<<- shadow (string-designator!))))
                 (<<- shadowing-import-from (shadowing-import-from))
                 (<<- import-from           (import-from))
                 (eg:option* :export        (* (<<- export (string-designator!))))
                 (eg:option* :intern        (* (<<- intern (string-designator!))))
                 (eg:option  :size          (<- size (package-size!)))
                 ;; Non-standard options
                 (<<- local-nicknames       (local-nicknames))
                 (eg:option :lock           (<- lock (package-lock!)))
                 (list* (must (not :any) "unknown option") :any)
                 (and :any (must (list* :any :any) "option must be a list of the form (:NAME . VALUE)")))))
    `(,name
      ,@(? nickname      `(:nicknames ,@nickname))
      ,@(? documentation `(:documentation ,documentation))
      ,@(? use           `(:use ,@use))
      ,@(? shadow        `(:shadow ,@shadow))
      ,@shadowing-import-from
      ,@import-from
      ,@(? export        `(:export ,@export))
      ,@(? intern        `(:intern ,@intern))
      ,@(? size          `(:size ,size))
      ,@local-nicknames
      ,@(? lock          `(:lock ,lock)))
  ((name                  1)
   (nickname              *)
   (documentation         ?)
   (use                   *) ; TODO cannot distinguish empty option, i.e. (:use), from absent
   (shadow                *)
   (shadowing-import-from *)
   (import-from           *)
   (export                *)
   (intern                *)
   (size                  ?)
   ;; Non-standard options
   (local-nicknames       *)
   (lock                  ?)))

(define-macro in-package
    (list (<- name (must (string-designator!) "name is required")))
  `(,name)
  ((name 1)))
