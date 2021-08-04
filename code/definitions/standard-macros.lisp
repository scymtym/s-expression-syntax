;;;; standard-macros.lisp ---  Syntax of various Common Lisp standard macros.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; `defconstant', `defvar' and `defparameter'

(define-macro defconstant
    (list (<- name ((variable-name! names)))
          (<- initial-value ((form! forms)))
          (? (<- documentation ((documentation-string! forms)))))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

(define-macro defvar
    (list (<- name ((variable-name! names)))
          (? (seq (<- initial-value ((form! forms)))
                  (? (<- documentation ((documentation-string! forms)))))))
  ((name          1)
   (initial-value ? :evaluation t)
   (documentation ?)))

(define-macro defparameter
    (list (<- name ((variable-name! names)))
          (<- initial-value ((form! forms)))
          (? (<- documentation ((documentation-string! forms)))))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

;;; `defun'

(define-macro defun
    (list* (<- name ((function-name! names)))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

;;; `defmacro'

(define-macro defmacro
    (list* (<- name ((function-name/symbol! names)))
           (<- lambda-list ((destructuring-lambda-list! destructuring-lambda-list))) ; TODO macro lambda list
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

;;; `defstruct' including slots

(define-syntax slot-description
    (or (and (list* :any)
             (must (list (<- name ((variable-name! names)))
                         (? (seq (<- initform ((form! forms)))
                                 (* (or (eg:poption :read-only (<- read-only))
                                        (eg:poption :type      (<- type ((type-specifier! type-specifiers)))))))))
                   "must be of the form (NAME [INITFORM] ...)"))
        (<- name ((variable-name! names))))
  ((name      1)
   (initform  ? :evaluation t)
   (read-only ?)
   (type      ?)))

(defrule structure-constructor (other-constructors)
    (value (source)
      (eg:option* :constructor
                  (? (or 'nil   ; NAME remains `nil'
                         (seq (<- name ((function-name/symbol! names)))
                              (? (<- lambda-list ((ordinary-lambda-list! lambda-lists))))))))) ; TODO boa-lambda-list
  (let ((names (list* name (map 'list (lambda (constructor)
                                        (bp:node-relation* '(:name . 1) constructor))
                                other-constructors))))
    (when (and (not (= (length names) 1)) (member nil names))
      (:fatal "(:constructor nil) and named constructors are mutually exclusive")))
  (bp:node* (:structure-constructor :source source)
    (1    (:name        . 1) name)
    (bp:? (:lambda-list . 1) lambda-list :evaluation :compound)))

(macrolet ((define-function-option (name option &optional (symbol? t))
             `(defrule ,name ()
                  ,(let ((list-syntax `(eg:option*
                                        ,option
                                        (? (<- name (or 'nil
                                                        ((function-name/symbol! names))))))))
                     (if symbol?
                         `(or ,option ,list-syntax)
                         list-syntax))
                (or name t))))
  (define-function-option structure-copier         :copier)
  (define-function-option structure-predicate      :predicate)
  (define-function-option structure-print-object   :print-object   nil)
  (define-function-option structure-print-function :print-function nil))

(defrule structure-name ()
    (or (list (and (<- name ((class-name! names)))
                   (<- constructors (:transform :any '()))) ; HACK
              (:transform
                 (* (or (<<- constructors           (structure-constructor constructors))
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
    (list name constructors include include-slots))

(define-macro defstruct
    (list (<- (name constructor include include-slot)
              (structure-name))
          (? (<- documentation ((documentation-string forms))))
          (* (<<- slot (slot-description))))
  ((name          1)
   (constructor   *> :evaluation :compound)
   (include       ?)
   (include-slot  *>)
   (documentation ?)
   (slot          *  :evaluation :compound)))

;;; `defclass' and `define-condition' including slots

(defrule allocation-type! ()
  (must (or :instance :class)
        "allocation must be :INSTANCE or :CLASS"))

(defrule superclasses ()
    (list (* (and :any (must (<<- superclasses ((class-name names)))
                             "superclass must be a class name"))))
  (nreverse superclasses))

(define-syntax slot-specifier
    (or (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- reader       (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writer       (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessor     (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (allocation-type!)))
                     (eg:poption* :initarg       (<<- initarg      (must (guard (typep 'symbol))
                                                                         "initarg must be a symbol")))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))
                     (seq (<<- option-name (guard symbolp))
                          (<<- option-value)))))
        (<- name ((slot-name! names))))
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
   (option-name   *)
   (option-value  *)))

(define-macro defclass
    (list (<- name ((class-name! names)))
          (<- superclass (superclasses))
          (list (* (<<- slot (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (eg:option :default-initargs
                            (* (and :any
                               (must (seq (<<- default-initarg  (guard (typep 'symbol)))
                                          (<<- default-initform ((form! forms))))
                                     "default initarg must be a symbol followed by an expression"))))
                 (eg:option :metaclass     (must (<- metaclass ((class-name names)))
                                                 "metaclass must be a class name"))
                 (eg:option :documentation (<- documentation ((documentation-string! forms))))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-name (must (guard symbolp) "option name must be a symbol"))
                        (<<- option-value)))))
  ((name             1)
   (superclass       *>)
   (slot             *  :evaluation :compound)
   ;; Standard options
   (default-initarg  *)
   (default-initform *  :evaluation t)
   (metaclass        ?)
   (documentation    ?)
   ;; Non-standard options
   (option-name      *)
   (option-value     *)))

(define-syntax condition-slot-specifier
    (or (and (not (list* :any)) (<- name ((slot-name! names))))
        (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- reader       (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writer       (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessor     (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (must (allocation-type))))
                     (eg:poption* :initarg       (<<- initarg      (must (guard (typep 'symbol))
                                                                         "initarg must be a symbol")))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))))))
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
    (or (<- string   (guard (typep 'string)))
        (<- function ((function-name/symbol names)))
        (<- lambda   (lambda-expression)))
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
          (* (or (eg:option :default-initargs
                            (* (and :any
                                    (must (seq (<<- default-initarg  (guard (typep 'symbol)))
                                               (<<- default-initform ((form! forms))))
                                          "default initarg must be a symbol followed by an expression"))))
                 (eg:option :documentation    (<- documentation ((documentation-string! forms))))
                 (eg:option :report           (<- report (condition-report!))))))
  ((name             1)
   (parent-type      *>)
   (slot             *  :evaluation :compound)
   (default-initarg  *)
   (default-initform *  :evaluation t)
   (documentation    ?)
   (report           ?  :evaluation :compound)))

;;; `deftype'

(define-macro deftype
    (list* (<- name ((class-name! names)))
           (<- lambda-list ((deftype-lambda-list! deftype-lambda-list)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1  :evaluation :compound)
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

;;; `defgeneric'

(defrule qualifier ()
  (not (guard listp)))

(define-syntax method-description
    (must (list* :method
                 (* (<<- qualifier (qualifier)))
                 (<- lambda-list ((specialized-lambda-list! lambda-lists)))
                 (<- (documentation declaration form) ((docstring-body forms))))
          "must be of the for (:method [QUALIFIERS] LAMBDA-LIST [DECLARATION] FORM*)")
  ((qualifier     *)
   (lambda-list   1  :evaluation :compound)
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

(defun verify-precedence-order (precedence-order lambda-list)
  (let ((required-names (map 'list (lambda (parameter)
                                     (let ((name (bp:node-relation* :name parameter)))
                                       (getf (bp:node-initargs* name) :name)))
                             (bp:node-relation* :required lambda-list)))
        (order-names    (map 'list (lambda (name)
                                     (getf (bp:node-initargs* name) :name))
                             precedence-order)))
    (a:set-equal order-names required-names :test #'eg::%eql)))

(define-macro defgeneric
    (list (<- name ((function-name! names)))
          (<- lambda-list ((generic-function-lambda-list! lambda-lists)))
          (* (or ;; Standard options
                 (eg:option  :generic-function-class    (<- generic-function-class ((class-name! names))))
                 (and (eg:option :argument-precedence-order (* (<<- names ((lambda-list-variable-name! lambda-lists)))))
                      (<- argument-precedence-order
                          (:transform :any
                            ;; Collect names of required parameters in
                            ;; LAMBDA-LIST and ensure that all names
                            ;; following :ARGUMENT-PRECEDENCE-ORDER
                            ;; occur in that set of names.
                            (multiple-value-bind (compatiblep required-names order-names)
                                (verify-precedence-order names lambda-list)
                              (unless compatiblep
                                (:fatal (format nil "~S must match the set of required parameters ~S"
                                                order-names required-names))))
                            (nreverse names))))
                 (eg:option  :method-combination        (<- method-combination (must (guard (typep 'symbol))
                                                                                     "method combination name must be a symbol"))
                             (* (<<- method-combination-argument)))
                 (eg:option  :method-class              (<- method-class ((class-name! names))))
                 (eg:option* declare                    (and (must (list* 'optimize :any) "must be an OPTIMIZE declaration")
                                                             (<<- declarations ((declaration! declarations)))))
                 (eg:option  :documentation             (<- documentation ((documentation-string! forms))))
                 (<<- method (method-description))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-name (must (guard (typep 'symbol)) "option name must be a symbol"))
                        (<<- option-value)))))
  ((name                        1)
   (lambda-list                 1 :evaluation :compound)
   ;; Standard options
   (generic-function-class      ?)
   (argument-precedence-order   ?)
   (method-combination          ?)
   (method-combination-argument *)
   (method-class                ?)
   (declarations                *)
   (documentation               ?)
   (method                      * :evaluation :compound)
   ;; Other options
   (option-name                 *)
   (option-value                *)))

;;; `defmethod'

(define-macro defmethod
    (list* (<- name ((function-name! names)))
           (* (<<- qualifier (qualifier)))
           (<- lambda-list ((specialized-lambda-list! lambda-lists)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((name          1)
   (qualifier     *)
   (lambda-list   1)
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

;;; `defpackage' and `in-package'

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

(defrule non-standard-package-option ()
  (or (eg:option  :locked          (must (guard (typep 'boolean)) "must be a Boolean")) ; TODO the `once' within this does not work
      (eg:option* :local-nicknames (* (list (must (string-designator!) "expected TODO")
                                            (must (string-designator!) "expected TODO"))))))

(define-syntax import-from
    (list* (or :import-from :shadowing-import-from)
           (must (list (<- package (package-designator!))
                       (* (<<- name (and :any (string-designator!)))))
                 "import from options accept a package designator followed by string designators"))
  ((package 1)
   (name    *)))

(define-macro defpackage
    (list (must (<- name (string-designator!)) "name is required")
          ;; TODO (* (and :any (must (or …) "unknown options"))
          (* (or (eg:option* :nicknames     (* (<<- nickname (and :any (string-designator!)))))
                 (eg:option  :documentation (<- documentation ((documentation-string! forms))))
                 (eg:option* :use           (* (<<- use (package-designator!))))
                 (eg:option* :shadow        (* (<<- shadow (guard symbolp))))
                 (<<- shadowing-import-from (and (list* :shadowing-import-from :any)
                                                 (import-from)))
                 (<<- import-from           (and (list* :import-from :any)
                                                 (import-from)))
                 (eg:option* :export (* (<<- export (string-designator!))))
                 (eg:option* :intern (* (<<- intern (string-designator!))))
                 (eg:option  :size   (<- size (must (guard (typep '(integer 0)))
                                                    "must be a non-negative integer")))
                 (non-standard-package-option)
                 (list* (must (not :any) "unknown option") :any)
                 (and :any (must (guard (not :any) atom) "option must be a list")))))
  ((name                           1)
   (nickname                       *)
   (documentation                  ?)
   (use                            *) ; TODO cannot distinguish empty option, i.e. (:use), from absent
   (shadow                         *)
   ;; (shadowing-import-from-packages *)
   ;; (shadowing-import-from-names    *)
   (shadowing-import-from          *)
   ;; (import-from-packages           *)
   ;; (import-from-names              *)
   (import-from                    *)
   (export                         *)
   (intern                         *)
   (size                           ?)))

(define-macro in-package
    (list (<- name (must (string-designator!) "name is required")))
  ((name 1)))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(define-syntax handler-binding
    (list (<- type ((type-specifier! type-specifiers)))
          (<- form ((form! forms))))
  ((type 1)
   (form 1 :evaluation t)))

(defrule handler-binding! ()
  (must (handler-binding) "must be of the form (TYPE HANDLER-FORM)"))

(define-macro handler-bind
    (list* (list (* (<<- binding (handler-binding!))))
           (<- form (forms)))
  ((binding *  :evaluation :compound)
   (form    *> :evaluation t)))

(defrule handler-clause ()
    (list* (<- type ((type-specifier! type-specifiers)))
           (must (list (? (<- variable ((required-parameter! lambda-lists) 'nil))))
                 "must be a lambda list with zero or one required parameter")
           (<- (declarations forms) ((body forms))))
  (list type variable declarations forms))

(define-macro handler-case
    (list (<- form ((form! forms)))
          (* (or (list* (eg:once :no-error)
                        (<- no-error-lambda-list
                            ((ordinary-lambda-list! lambda-lists)))
                        (<- (no-error-declaration no-error-form)
                            ((body forms))))
                 (<<- (type variable declaration handler-form)
                      (handler-clause)))))
  (;; Body form
   (form                 1  :evaluation t)
   ;; Handler clauses
   (type                 *)
   (variable             *  :evaluation (make-instance 'binding-semantics
                                                       :namespace 'variable
                                                       :scope     :lexical
                                                       :values    nil))
   (declaration          *)
   (handler-form         *> :evaluation t)
   ;; No-error clause
   (no-error-lambda-list ?)
   (no-error-declaration *)
   (no-error-form        *  :evaluation t)))

(define-syntax restart-binding
    (list (or 'nil (<- name     ((variable-name! names))))
          (<- function ((form! forms)))
          (* (or (eg:poption :interactive-function
                             (<- interactive-function ((form! forms))))
                 (eg:poption :report-function
                             (<- report-function ((form! forms))))
                 (eg:poption :test-function
                             (<- test-function ((form! forms)))))))
  ((name                 1)
   (function             1 :evaluation t)
   (interactive-function ? :evaluation t)
   (report-function      ? :evaluation t)
   (test-function        ? :evaluation t)))

(defrule restart-binding! ()
  (must (restart-binding) "must be of the form (NAME FUNCTION [OPTIONS])"))

(define-macro restart-bind
    (list* (list (* (<<- binding (restart-binding!))))
           (<- form ((forms forms))))
  ((binding * :evaluation :compound)
   (form    * :evaluation t)))

(define-syntax restart-clause
    (list* (or 'nil (<- name ((variable-name! names))))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (* (or (eg:poption :interactive (or (<- interactive-name   ((function-name/symbol names)))
                                               (<- interactive-lambda (lambda-expression))))
                  (eg:poption :report      (or (<- report-string      (guard (typep 'string)))
                                               (<- report-name        ((function-name/symbol names)))
                                               (<- report-lambda      (lambda-expression))))
                  (eg:poption :test        (or (<- test-name          ((function-name/symbol forms)))
                                               (<- test-lambda        (lambda-expression))))))
           (:transform (<- (declarations forms) ((body forms)))
             (when (not (or name report-string report-name report-lambda))
               (:fatal "for an unnamed restart, the :REPORT option must be supplied"))))
  ((name               1)
   (lambda-list        1)
   (interactive-name   ?)
   (interactive-lambda ?)
   (report-string      ?)
   (report-name        ?)
   (report-lambda      ?)
   (test-name          ?)
   (test-lambda        ?)
   (declarations       *>)
   (forms              *> :evaluation t)))

(define-macro restart-case
    (list (<- form ((form forms)))
          (* (<<- clause (restart-clause))))
  ((form   1 :evaluation t)
   (clause * :evaluation :compound)))

;;; `[ec]case' and `cond'

(define-syntax case-normal-clause
    (list (or (list (* (<<- keys))) (<<- keys))
          (* (<<- forms ((form! forms)))))
  ((keys  *)
   (forms * :evaluation t)))

(define-syntax case-otherwise-clause
    (list (or 'otherwise 't) (* (<<- forms ((form! forms)))))
  ((forms * :evaluation t)))

(define-macro case
    (list (<- keyform ((form! forms)))
          (* (guard ; HACK guard forces the value context
              (<<- clauses
                   (or (eg:once (case-otherwise-clause)
                                :flag otherwise? :name "otherwise clause")
                       (:transform (<- clause (case-normal-clause))
                         (when otherwise?
                           (:fatal "normal clause cannot follow otherwise clause"))
                         clause)
                       (:transform :any
                         (:fatal "must be a clause of one of the forms (KEY FORM*), (otherwise FORM*) or (t FORM*)"))))
              (typep 't))))
  ((keyform 1 :evaluation t)
   (clauses * :evaluation :compound)))

(define-macro ccase
    (list (<- keyform ((form! forms)))
          (* (guard ; HACK
              (<<- clauses (must (case-normal-clause)
                                 "must be a clause of the form (KEY FORM*)"))
              (typep 't))))
  ((keyform 1 :evaluation t)
   (clauses * :evaluation :compound)))

(define-macro ecase
    (list (<- keyform ((form! forms)))
          (* (guard ; HACK
              (<<- clauses (must (case-normal-clause)
                                 "must be a clause of the form (KEY FORM*)"))
              (typep 't))))
  ((keyform 1 :evaluation t)
   (clauses * :evaluation :compound)))

(define-syntax cond-clause
    (list* (<- test-form ((form! forms))) (<- forms ((forms forms))))
  ((test-form 1  :evaluation t)
   (forms     *> :evaluation t)))

(defrule cond-clause! ()
  (must (cond-clause) "must be a clause of the form (TEST-FORM FORM*)"))

(define-macro cond
    (list (* (<<- clauses (cond-clause!))))
  ((clauses * :evaluation :compound)))
