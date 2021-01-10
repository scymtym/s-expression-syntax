;;;; standard-macros.lisp ---  Syntax of various Common Lisp standard macros.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

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
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defmacro'

(define-macro defmacro
    (list* (<- name ((function-name/symbol! names)))
           (<- lambda-list ((destructuring-lambda-list destructuring-lambda-list) 'nil)) ; TODO macro lambda list
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defstruct' including slots

#+maybe (defrule defstruct-slot-options
            ())

(define-syntax slot-description
    (or (list (<- name ((variable-name! names)))
              (? (seq (<- initform ((form! forms)))
                       (* (or (seq :read-only (<- read-only))
                              (seq :type      (<- type ((type-specifier! type-specifiers)))))))))
        (<- name ((variable-name! names))))
  ((name      1)
   (initform  ? :evaluation t)
   (read-only ?)
   (type      ?)))

(defrule structure-constructor ()
    (list :constructor
          (? (or (<- name 'nil)
                 (seq (<- name ((function-name/symbol! names)))
                       (? (<- lambda-list ((ordinary-lambda-list lambda-lists) '()))))))) ; TODO boa-lambda-list
  (list name lambda-list))

(macrolet ((define-function-option (name option &optional (symbol? t))
             `(defrule ,name ()
                ,(let ((list-syntax `(list ,option (? (<- name (or 'nil ((function-name/symbol! names))))))))
                   (if symbol?
                       `(or ,option ,list-syntax)
                       list-syntax))
                (or name t))))
  (define-function-option structure-copier         :copier)
  (define-function-option structure-predicate      :predicate)
  (define-function-option structure-print-object   :print-object   nil)
  (define-function-option structure-print-function :print-function nil))

(defrule structure-include ()
    (list :include (<- name ((class-name! names)))
          (* (<<- slots (slot-description))))
    (list name slots))

(defrule structure-initial-offset ()
    (list :initial-offset (must (guard offset (typep '(integer 0))) "a valid offset"))
  offset)

(defrule structure-type ()
    (list :type (<- type ((type-specifier! type-specifiers))))
  type)

(defrule structure-name ()
    (or (list (<- name ((class-name! names)))
              (* (or (<- constructor             (structure-constructor))
                     (<- copier                  (structure-copier))
                     (<- (include include-slots) (structure-include))
                     (<- offset                  (structure-initial-offset))
                     (<- named                   :named)
                     (<- predicate               (structure-predicate))
                     (<- print-object            (structure-print-object))
                     (<- print-function          (structure-print-function))
                     (<- type                    (structure-type)))))
        (<- name ((class-name! names))))
  (when (and offset (not type))
    (:fail ; "Cannot specify ~S without ~S" :initial-offset :type
     ))
  (when (and type (or print-object print-function))
    (:fail ; "The options ~S and ~S are mutually exclusive"
     ))
  (list name constructor))

(define-macro defstruct
    (list (<- (name constructor) (structure-name))
          (? (<- documentation ((documentation-string forms))))
          (* (<<- slots (slot-description))))
  ((name          1)
   (constructor   ?)
   (documentation ?)
   (slots         *)))

;;; `defclass' including slots

(defrule allocation-type ()
  (or :instance :class))

#+unused (define-syntax slot-options
    (list (* (or (seq :reader        (<<- readers       (must (function-name/symbol)))) ; TODO fail for something like :reader 1
                 (seq :writer        (<<- writers       (must (function-name))))
                 (seq :accessor      (<<- accessor      (must (function-name/symbol))))
                 (seq :allocation    (<-  allocation    (must (allocation-type))))
                 (seq :initarg       (<<- initargs      (must (guard symbolp))))
                 (seq :initform      (<-  initform      :any)) ; TODO rule for form?
                 (seq :type          (<-  type          (type-specifier)))
                 (seq :documentation (<-  documentation (must (guard stringp))) ; TODO rule for documentation
                       )
                 (seq (<<- option-names  (guard symbolp))
                       (<<- option-values)))))
  ((initargs      *)
   (readers       *)
   (writers       *)
   (accessor      *)
   (allocation    ?)
   (initform      ? :evaluation t)
   (type          ?)
   (documentation ?)
   ;; Non-standard options
   (option-names  *)
   (option-values *)))

(define-syntax slot-specifier
    (or (and (not (list* :any)) (<- name ((slot-name! names))))
        (list (must (<- name ((slot-name! names))) "slot must have a name")
              #+no (<- options (slot-options))
              (* (or (seq :reader        (<<- readers       (must ((function-name/symbol names)) "reader must be a symbol function name")))
                     (seq :writer        (<<- writers       (must ((function-name names))        "writer must be an extended function name")))
                     (seq :accessor      (<<- accessors     (must ((function-name/symbol names)) "accessor must be a symbol function name")))
                     (seq :allocation    (<-  allocation    (must (allocation-type))))
                     (seq :initarg       (<<- initargs      (must (guard symbolp)       "initarg must be a symbol")))
                     (seq :initform      (<-  initform      :any)) ; TODO rule for form?
                     (seq :type          (<-  type          ((type-specifier! type-specifiers))))
                     (seq :documentation (<-  documentation ((documentation-string! forms)))
                           )
                     (seq (<<- option-names  (guard symbolp))
                          (<<- option-values))))))
  ((name    1)
   ;; Options
   #+no (options 1)
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
          (list (* (and :any (must (<<- superclasses ((class-name names)))
                                   "superclass must be a class name"))))
          (list (* (<<- slots (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (list :default-initargs
                       (* (and :any
                               (must (seq (<<- default-initargs  (guard symbolp))
                                          (<<- default-initforms :any))
                                     "default initarg must a symbol followed by an expression"))))
                 (list :metaclass
                       (must (<- metaclass ((class-name names))) "metaclass must be a class name"))
                 (list :documentation (<- documentation ((documentation-string! forms))))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (must (guard symbolp) "option name must be a symbol"))
                        (<<- option-values)))))
  ((name              1)
   (superclasses      *)
   (slots             *)
   ;; Standard options
   (default-initargs  *)
   (default-initforms * :evaluation t)
   (metaclass         ?)
   (documentation     ?)
   ;; Non-standard options
   (option-names      *)
   (option-values     *)))

;;; `deftype'

(define-macro deftype
    (list* (<- name ((class-name! names)))
           (<- lambda-list ((deftype-lambda-list deftype-lambda-list) 'nil))
           (<- (documentation declarations forms) ((docstring-body forms))))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defgeneric'

(define-macro defgeneric
    (list (<- name ((function-name! names)))
          (<- lambda-list ((generic-function-lambda-list lambda-lists) 'nil))
          (* (or ;; Standard options
                 (list :generic-function-class    (<- generic-function-class ((class-name! names))))
                 (list :argument-precedence-order (<- argument-precedence-order (guard (must (or :most-specific-first ; TODO how to force this `must' to work on the value context?
                                                                                                 :most-specific-last))
                                                                                        symbolp)))
                 (list :documentation             (<- documentation ((documentation-string! forms))))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (guard keywordp))
                        (<<- option-values)))))
  ((name                      1)
   (lambda-list               1)
   ;; Standard options
   (generic-function-class    ?)
   (argument-precedence-order ?)
   (documentation             ?)
   ;; Other options
   (option-names              *)
   (option-values             *)))

;;; `defmethod'

(define-macro defmethod
    (list* (<- name ((function-name! names)))
           (* (<<- qualifiers (guard (not 'nil) symbolp)))
           (must (<- lambda-list ((specialized-lambda-list lambda-lists) 'nil)) "expected lambda list")
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
  (or (list :locked (must (guard (typep 'boolean)) "must be a Boolean"))
      (list :local-nicknames (* (list (must (string-designator!) "expected TODO")
                                      (must (string-designator!) "expected TODO"))))))

(define-macro defpackage
    (list (must (<- name (string-designator!)) "name is required")
          ;; TODO (* (and :any (must (or …) "unknown options"))
          (* (or (list :nicknames     (* (<<- nicknames (and :any (string-designator!)))))
                 (list :documentation (<- documentation ((documentation-string! forms))))
                 (list :use           (* (<<- use (and :any (package-designator!)))))
                 (list :shadow        (* (<<- shadow (guard symbolp))))
                 (list :shadowing-import-from
                       (<<- shadowing-import-from-packages (package-designator!))
                       (<<- shadowing-import-from-names    (:transform
                                                            (* (<<- temp (and :any (string-designator!))))
                                                            (prog1
                                                                (nreverse temp)
                                                              (setf temp '())))))
                 (list :import-from
                       (<<- import-from-packages (package-designator!))
                       (<<- import-from-names    (:transform
                                                  (* (<<- temp (and :any (string-designator!))))
                                                  (prog1
                                                      (nreverse temp)
                                                    (setf temp '())))))
                 (list :export (* (<<- export (and :any (string-designator!)))))
                 (list :intern (* (<<- intern (and :any (string-designator!)))))
                 (list :size   (<- size (guard (typep '(integer 0)))))
                 (non-standard-package-option)
                 (list* (must (not :any) "unknown option") :any)
                 (and :any (must (guard (not :any) atom) "options must be list")))))
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
