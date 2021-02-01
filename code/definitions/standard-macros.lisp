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
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defmacro'

(define-macro defmacro
    (list* (<- name ((function-name/symbol! names)))
           (<- lambda-list ((destructuring-lambda-list! destructuring-lambda-list))) ; TODO macro lambda list
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

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
    (eg:option* :constructor
                (? (or 'nil ; NAME remains `nil'
                       (seq (<- name ((function-name/symbol! names)))
                            (? (<- lambda-list ((ordinary-lambda-list! lambda-lists)))))))) ; TODO boa-lambda-list
  (let ((names (list* name (map 'list #'first other-constructors))))
    (when (and (not (= (length names) 1)) (member nil names))
      (:fatal "(:constructor nil) and named constructors are mutually exclusive")))
  (list name lambda-list))

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
    (list (<- (name constructors include include-slots)
              (structure-name))
          (? (<- documentation ((documentation-string forms))))
          (* (<<- slots (slot-description))))
  ((name          1)
   (constructors  *>)
   (include       ?)
   (include-slots *>)
   (documentation ?)
   (slots         *)))

;;; `defclass' and `define-condition' including slots

(defrule allocation-type ()
  (or :instance :class))

(defrule superclasses ()
    (list (* (and :any (must (<<- superclasses ((class-name names)))
                             "superclass must be a class name"))))
  (nreverse superclasses))

(define-syntax slot-specifier
    (or (and (not (list* :any)) (<- name ((slot-name! names))))
        (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- readers      (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writers      (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessors    (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (must (allocation-type))))
                     (eg:poption* :initarg       (<<- initargs     (must (guard symbolp)
                                                                         "initarg must be a symbol")))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))
                     (seq (<<- option-names  (guard symbolp))
                          (<<- option-values))))))
  ((name          1)
   ;; Options
   (initargs      *)
   (readers       *)
   (writers       *)
   (accessors     *)
   (allocation    ?)
   (initform      ? :evaluation t)
   (type          ?)
   (documentation ?)
   ;; Non-standard options
   (option-names  *)
   (option-values *)))

(define-macro defclass
    (list (<- name ((class-name! names)))
          (<- superclasses (superclasses))
          (list (* (<<- slots (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (eg:option :default-initargs
                            (* (and :any
                               (must (seq (<<- default-initargs  (guard symbolp))
                                          (<<- default-initforms ((form! forms))))
                                     "default initarg must be a symbol followed by an expression"))))
                 (eg:option :metaclass     (must (<- metaclass ((class-name names)))
                                                 "metaclass must be a class name"))
                 (eg:option :documentation (<- documentation ((documentation-string! forms))))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (must (guard symbolp) "option name must be a symbol"))
                        (<<- option-values)))))
  ((name              1)
   (superclasses      *>)
   (slots             *)
   ;; Standard options
   (default-initargs  *)
   (default-initforms * :evaluation t)
   (metaclass         ?)
   (documentation     ?)
   ;; Non-standard options
   (option-names      *)
   (option-values     *)))

(define-syntax condition-slot-specifier
    (or (and (not (list* :any)) (<- name ((slot-name! names))))
        (list (must (<- name ((slot-name! names))) "slot must have a name")
              (* (or (eg:poption* :reader        (<<- readers      (must ((function-name/symbol names))
                                                                         "reader must be a symbol function name")))
                     (eg:poption* :writer        (<<- writers      (must ((function-name names))
                                                                         "writer must be an extended function name")))
                     (eg:poption* :accessor      (<<- accessors    (must ((function-name/symbol names))
                                                                         "accessor must be a symbol function name")))
                     (eg:poption  :allocation    (<- allocation    (must (allocation-type))))
                     (eg:poption* :initarg       (<<- initargs     (must (guard symbolp)
                                                                         "initarg must be a symbol")))
                     (eg:poption  :initform      (<- initform      ((form! forms))))
                     (eg:poption  :type          (<- type          ((type-specifier! type-specifiers))))
                     (eg:poption  :documentation (<- documentation ((documentation-string! forms))))))))
  ((name          1)
   ;; Options
   (initargs      *)
   (readers       *)
   (writers       *)
   (accessors     *)
   (allocation    ?)
   (initform      ? :evaluation t)
   (type          ?)
   (documentation ?)))

(defrule condition-report ()
    (or (guard (typep 'string))
        ((function-name/symbol names))
        (lambda-expression)))

(defrule condition-report! ()
  (must (condition-report)
        "report must be a string, a symbol naming a function or a lambda expression"))

(define-macro define-condition
    (list (<- name ((class-name! names)))
          (<- parent-types (superclasses))
          (list (* (<<- slots (condition-slot-specifier))))
          (* (or (eg:option :default-initargs
                            (* (and :any
                                    (must (seq (<<- default-initargs  (guard symbolp))
                                               (<<- default-initforms ((form! forms))))
                                          "default initarg must be a symbol followed by an expression"))))
                 (eg:option :documentation    (<- documentation ((documentation-string! forms))))
                 (eg:option :report           (<- report (condition-report!))))))
  ((name              1)
   (parent-types      *>)
   (slots             *)
   (default-initargs  *)
   (default-initforms * :evaluation t)
   (documentation     ?)
   (report            ? :evaluation :depends))) ; TODO report is not quite evaluated

;;; `deftype'

(define-macro deftype
    (list* (<- name ((class-name! names)))
           (<- lambda-list ((deftype-lambda-list! deftype-lambda-list)))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defgeneric'

(defrule qualifier ()
  (not (guard listp)))

(define-syntax method-description
    (must (list* :method
                 (* (<<- qualifiers (qualifier)))
                 (<- lambda-list ((specialized-lambda-list! lambda-lists)))
                 (<- (documentation declarations forms) ((docstring-body forms))))
          "must be of the for (:method [QUALIFIERS] LAMBDA-LIST [DECLARATION] FORM*)")
  ((qualifiers    *)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

(define-macro defgeneric
    (list (<- name ((function-name! names)))
          (<- lambda-list ((generic-function-lambda-list! lambda-lists)))
          (* (or ;; Standard options
                 (eg:option  :generic-function-class    (<- generic-function-class ((class-name! names))))
                 (and (eg:option  :argument-precedence-order (* (<<- names ((lambda-list-variable-name! lambda-lists)))))
                      (<- argument-precedence-order (:transform :any (nreverse names))))
                 (eg:option  :method-combination        (<- method-combination (must (guard symbolp)
                                                                                     "method combination name must be a symbol"))
                             (* (<<- method-combination-arguments)))
                 (eg:option  :method-class              (<- method-class ((class-name! names))))
                 (eg:option* declare                    (and (must (list* 'optimize :any) "must be an OPTIMIZE declaration")
                                                             (<<- declarations ((declaration! declarations)))))
                 (eg:option  :documentation             (<- documentation ((documentation-string! forms))))
                 (<<- methods (method-description))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (must (guard symbolp) "option name must be a symbol"))
                        (<<- option-values)))))
  ((name                         1)
   (lambda-list                  1)
   ;; Standard options
   (generic-function-class       ?)
   (argument-precedence-order    ?)
   (method-combination           ?)
   (method-combination-arguments *)
   (method-class                 ?)
   (declarations                 *)
   (documentation                ?)
   (methods                      *)
   ;; Other options
   (option-names                 *)
   (option-values                *)))

;;; `defmethod'

(define-macro defmethod
    (list* (<- name ((function-name! names)))
           (* (<<- qualifiers (qualifier)))
           (<- lambda-list ((specialized-lambda-list! lambda-lists)))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (qualifiers    *)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defpackage' and `in-package'

(defrule string-designator ()
  (guard (typep '(or character string symbol))))

(defrule string-designator! ()
  (must (string-designator) "must be a string designator"))

(defrule package-designator ()
  (or (string-designator) (guard packagep)))

(defrule package-designator! ()
  (must (package-designator) "must be a package designator"))

(defrule non-standard-package-option ()
  (or (eg:option  :locked (must (guard (typep 'boolean)) "must be a Boolean")) ; TODO the `once' within this does not work
      (eg:option* :local-nicknames (* (list (must (string-designator!) "expected TODO")
                                      (must (string-designator!) "expected TODO"))))))

(define-macro defpackage
    (list (must (<- name (string-designator!)) "name is required")
          ;; TODO (* (and :any (must (or …) "unknown options"))
          (* (or (eg:option* :nicknames     (* (<<- nicknames (and :any (string-designator!)))))
                 (eg:option  :documentation (<- documentation ((documentation-string! forms))))
                 (eg:option* :use           (* (<<- use (and :any (package-designator!)))))
                 (eg:option* :shadow        (* (<<- shadow (guard symbolp))))
                 (eg:option* :shadowing-import-from
                             (<<- shadowing-import-from-packages (package-designator!))
                             (<<- shadowing-import-from-names    (:transform
                                                                     (* (<<- temp (and :any (string-designator!))))
                                                                   (prog1
                                                                       (nreverse temp)
                                                                     (setf temp '())))))
                 (eg:option* :import-from
                             (<<- import-from-packages (package-designator!))
                             (<<- import-from-names    (:transform
                                                           (* (<<- temp (and :any (string-designator!))))
                                                         (prog1
                                                             (nreverse temp)
                                                           (setf temp '())))))
                 (eg:option* :export (* (<<- export (and :any (string-designator!)))))
                 (eg:option* :intern (* (<<- intern (and :any (string-designator!)))))
                 (eg:option  :size   (<- size (must (guard (typep '(integer 0)))
                                                    "must be a non-negative integer")))
                 (non-standard-package-option)
                 (list* (must (not :any) "unknown option") :any)
                 (and :any (must (guard (not :any) atom) "option must be a list")))))
  ((name                           1)
   (nicknames                      *)
   (documentation                  ?)
   (use                            *) ; TODO cannot distinguish empty option, i.e. (:use), from absent
   (shadow                         *)
   (shadowing-import-from-packages *)
   (shadowing-import-from-names    *)
   (import-from-packages           *)
   (import-from-names              *)
   (export                         *)
   (intern                         *)
   (size                           ?)))

(define-macro in-package
    (list (<- name (must (string-designator!) "name is required")))
  ((name 1)))

;;; `handler-{bind,case}' and  `restart-{bind,case}'

(defrule handler-binding ()
  (list (<- type    ((type-specifier! type-specifiers)))
        (<- handler ((form! forms)))))

(defrule handler-binding! ()
  (must (handler-binding) "must be of the form (TYPE HANDLER-FORM)"))

(define-macro handler-bind
    (list* (list (* (<<- (types handler-forms) (handler-binding!))))
           (<- forms (forms)))
  (;; Handler bindings
   (types         *)
   (handler-forms *  :evaluation t)
   ;; Body forms
   (forms         *> :evaluation t)))

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
                        (<- (no-error-declarations no-error-forms)
                            ((body forms))))
                 (<<- (types variables declarations forms)
                      (handler-clause)))))
  (;; Body form
   (form                  1 :evaluation t)
   ;; Handler clauses
   (types                 *)
   (variables             * :evaluation (make-instance 'binding-semantics
                                                       :namespace 'variable
                                                       :scope     :lexical
                                                       :values    nil))
   (declarations          *)
   (forms                 * :evaluation t)
   ;; No-error clause
   (no-error-lambda-list  ?)
   (no-error-declarations *)
   (no-error-forms        * :evaluation t)))

(defrule restart-binding ()
    (list (<- name     ((variable-name! names)))
          (<- function ((form! forms)))
          (* (or (eg:poption :interactive-function
                             (<- interactive-function ((form! forms))))
                 (eg:poption :report-function
                             (<- report-function ((form! forms))))
                 (eg:poption :test-function
                             (<- test-function ((form! forms)))))))
  (list name function interactive-function report-function test-function))

(defrule restart-binding! ()
  (must (restart-binding) "must be of the form (NAME FUNCTION [OPTIONS])"))

(define-macro restart-bind
    (list* (list (* (<<- (names functions
                          interactive-functions report-functions test-functions)
                         (restart-binding!))))
           (<- forms ((forms forms))))
  (;; Handler bindings
   (names                 *)
   (functions             * :evaluation t)
   (interactive-functions * :evaluation t)
   (report-functions      * :evaluation t)
   (test-functions        * :evaluation t)
   ;; Body forms
   (forms                 * :evaluation t)))

(defrule restart-clause ()
    (list* (<- name        ((variable-name! names)))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (* (or (eg:poption :interactive (<- interactive ((form! forms))))
                  (eg:poption :report      (<- report ((form! forms))))
                  (eg:poption :test        (<- test ((form! forms))))))
           (<- (declarations forms) ((body forms))))
  (list name lambda-list interactive report test declarations forms))

(define-macro restart-case
    (list (<- form ((form forms)))
          (* (<<- (names lambda-lists interactives reports tests declarations forms)
                  (restart-clause))))
  (;; Body form
   (form         1 :evaluation t)
   ;; Handler clauses
   (names        *)
   (lambda-lists *)
   (interactives *)
   (reports      *)
   (tests        *)
   (declarations *)
   (forms        *)))
