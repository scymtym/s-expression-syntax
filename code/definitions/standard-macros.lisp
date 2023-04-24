;;;; standard-macros.lisp ---  Syntax of various Common Lisp standard macros.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
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
                                 (* (or (eg:poption :read-only (<- read-only ((unparsed-expression forms) ':slot-read-only)))
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
  (bp:node* (:default-initarg :source source)
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
  (and (not (guard listp))
       ((unparsed-expression forms) :method-qualifier)))

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
                 (eg:option  :method-combination        (<- method-combination ((method-combination-name! names)))
                                                        (* (<<- method-combination-argument)))
                 (eg:option  :method-class              (<- method-class ((class-name! names))))
                 (eg:option* declare                    (and (must (list* 'optimize :any) "must be an OPTIMIZE declaration")
                                                             (<<- declarations ((declaration-specifier! declarations)))))
                 (eg:option  :documentation             (<- documentation ((documentation-string! forms))))
                 (<<- method (method-description))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-name (must (guard (typep 'symbol)) "option name must be a symbol"))
                        (<<- option-value ((unparsed-expression forms) ':non-standard-defgeneric-option))))))
  ((name                        1)
   (lambda-list                 1 :evaluation :compound)
   ;; Standard options
   (generic-function-class      ?)
   (argument-precedence-order   *>)
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

(defrule package-size ()
    (value (source)
      (guard size (typep '(integer 0))))
  (bp:node* (:unparsed :expression size
                       :context    :package-size
                       :source     source)))

(defrule package-size! ()
  (must (package-size) "package size must be a non-negative integer"))

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
                 (eg:option  :size   (<- size (package-size!)))
                 (non-standard-package-option)
                 (list* (must (not :any) "unknown option") :any)
                 (and :any (must (guard (not :any) atom) "option must be a list")))))
  ((name                           1)
   (nickname                       *)
   (documentation                  ?)
   (use                            *) ; TODO cannot distinguish empty option, i.e. (:use), from absent
   (shadow                         *)
   (shadowing-import-from          *)
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
    (list* (must (list (* (<<- binding (handler-binding!))))
                 "must be a list of handler bindings")
           (<- form (forms)))
  ((binding *  :evaluation :compound)
   (form    *> :evaluation t)))

(define-syntax handler-clause
    (list* (<- type ((type-specifier! type-specifiers)))
           (must (list (? (<- variable ((required-parameter! lambda-lists) '()))))
                 "must be a lambda list with zero or one required parameter")
           (<- (declaration form) ((body forms))))
  ((type        1)
   (variable    ?  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil))
   (declaration *>)
   (form        *> :evaluation t)))

(define-syntax no-error-clause
    (list* :no-error
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (declaration form) ((body forms))))
  ((lambda-list 1  :evaluation :compound)
   (declaration *>)
   (form        *> :evaluation t)))

(define-macro handler-case
    (list (<- form ((form! forms)))
          (* (or (and (list* (eg:once :no-error) :any)
                      (<- no-error-clause (no-error-clause)))
                 (<<- clause (handler-clause))))
          (must (not :any)
                "must be a list of handler clauses"))
  ((form            1 :evaluation t)
   (clause          * :evaluation :compound)
   (no-error-clause ? :evaluation :compound)))

(define-syntax restart-binding
    (list (or 'nil (<- name ((variable-name! names))))
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
    (list* (must (list (* (<<- binding (restart-binding!))))
                 "must be a list of restart bindings")
           (<- form ((forms forms))))
  ((binding * :evaluation :compound)
   (form    * :evaluation t)))

(define-syntax restart-clause
    (list* (or 'nil (<- name ((variable-name! names))))
           (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (* (or (eg:poption :interactive (or (<- interactive-name   ((function-name/symbol names)))
                                               (<- interactive-lambda (lambda-expression))))
                  (eg:poption :report      (or (<- report-string      (and (guard (typep 'string))
                                                                           ((unparsed-expression forms)
                                                                            ':restart-report-string)))
                                               (<- report-name        ((function-name/symbol names)))
                                               (<- report-lambda      (lambda-expression))))
                  (eg:poption :test        (or (<- test-name          ((function-name/symbol forms)))
                                               (<- test-lambda        (lambda-expression))))))
           (:transform (<- (declarations form) ((body forms)))
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
   (form               *> :evaluation t)))

(define-macro restart-case
    (list (<- form ((form forms)))
          (* (<<- clause (restart-clause)))
          (must (not :any)
                "must be a list of restart clauses"))
  ((form   1 :evaluation t)
   (clause * :evaluation :compound)))

;;; `[ec]case'

(defrule otherwise-key ()
  (or 'otherwise 't))

(defrule normal-key (allow-otherwise?)
  (and (or (:transform (otherwise-key)
             (unless allow-otherwise? (:fail)))
           (not (otherwise-key)))
       ((unparsed-expression forms) ':key)))

(define-syntax (case-normal-clause :arguments ((allow-otherwise? t)))
    (list (or (list (* (<<- key ((unparsed-expression forms) ':key))))
              (<<- key (normal-key allow-otherwise?)))
          (* (<<- form ((form! forms)))))
  ((key  *)
   (form * :evaluation t)))

(defrule case-normal-clause! (allow-otherwise?)
  (must (case-normal-clause allow-otherwise?)
        "must be a clause of the form (KEY-OR-KEYS FORM*)"))

(define-syntax case-otherwise-clause
    (list (otherwise-key) (* (<<- form ((form! forms)))))
  ((form * :evaluation t)))

(defrule case-clauses ()
    (list (* (<<- clauses
                  (eg:element
                   (or (eg:once (case-otherwise-clause)
                                :flag otherwise? :name "otherwise clause")
                       (:transform (<- clause (case-normal-clause 'nil))
                         (when otherwise?
                           (:fatal "normal clause must not follow otherwise clause"))
                         clause)
                       (:transform :any
                         (:fatal "must be a clause of one of the forms (KEY-OR-KEYS FORM*), (otherwise FORM*) or (t FORM*)")))))))
  (nreverse clauses))

(define-macro case
    (list* (<- keyform ((form! forms))) (<- clause (case-clauses)))
  ((keyform 1  :evaluation t)
   (clause  *> :evaluation :compound)))

(define-macro ccase
    (list (<- keyplace ((place! forms)))
          (* (<<- clause (case-normal-clause! 't))))
  ((keyplace 1 :evaluation t)
   (clause   * :evaluation :compound)))

(define-macro ecase
    (list (<- keyform ((form! forms)))
          (* (<<- clause (case-normal-clause! 't))))
  ((keyform 1 :evaluation t)
   (clause  * :evaluation :compound)))

;;; `[ec]typecase'

(define-syntax typecase-normal-clause
    (list (or (eg:element
               (or (:transform 'otherwise
                     (:fatal "CL:OTHERWISE does not name a type"))
                   (:transform (list* 'otherwise :any)
                     (:fatal "CL:OTHERWISE does not name a compound type"))))
              (<- type ((type-specifier! type-specifiers))))
          (* (<<- form ((form! forms)))))
  ((type 1)
   (form * :evaluation t)))

(defrule typecase-normal-clause! ()
  (must (typecase-normal-clause)
        "must be a clause of the form (TYPE FORM*)"))

(define-syntax typecase-otherwise-clause
    (list 'otherwise (* (<<- form ((form! forms)))))
  ((form * :evaluation t)))

(defrule typecase-clauses ()
    (list (* (<<- clauses
                  (eg:element
                   (or (eg:once (typecase-otherwise-clause)
                                :flag otherwise? :name "otherwise clause")
                       (:transform (<- clause (typecase-normal-clause))
                         (when otherwise?
                           (:fatal "normal clause must not follow otherwise clause"))
                         clause)
                       (:transform :any
                         (:fatal "must be a clause of the form (TYPE FORM*) or (otherwise FORM*)")))))))
  (nreverse clauses))

(define-macro typecase
    (list* (<- keyform ((form! forms))) (<- clause (typecase-clauses)))
  ((keyform 1  :evaluation t)
   (clause  *> :evaluation :compound)))

(define-macro ctypecase
    (list (<- keyplace ((place! forms)))
          (* (<<- clause (typecase-normal-clause!))))
  ((keyplace 1 :evaluation t)
   (clause   * :evaluation :compound)))

(define-macro etypecase
    (list (<- keyform ((form! forms)))
          (* (<<- clause (typecase-normal-clause!))))
  ((keyform 1 :evaluation t)
   (clause  * :evaluation :compound)))

;;; `cond'

(define-syntax cond-clause
    (list* (<- test-form ((form! forms))) (<- form ((forms forms))))
  ((test-form 1  :evaluation t)
   (form      *> :evaluation t)))

(defrule cond-clause! ()
  (must (cond-clause) "must be a clause of the form (TEST-FORM FORM*)"))

(define-macro cond
    (list (* (<<- clause (cond-clause!))))
  ((clause * :evaluation :compound)))
