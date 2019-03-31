(cl:in-package #:syntax)

(parser:in-grammar special-operators)

;;; `defconstant', `defvar' and `defparameter'

(define-macro defconstant
    (list (<- name (variable-name))
          (<- initial-value (form))
          (? (:guard documentation stringp)))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

(define-macro defvar
    (list (<- name (variable-name))
          (? (:seq (<- initial-value (form))
                   (? (:guard documentation stringp)))) )
  ((name          1)
   (initial-value ? :evaluation t)
   (documentation ?)))

(define-macro defparameter
    (list (<- name (variable-name))
          (<- initial-value (form))
          (? (:guard documentation stringp)))
  ((name          1)
   (initial-value 1 :evaluation t)
   (documentation ?)))

;;; `defun'

(define-macro defun
    (list* (<- name (function-name))
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
           (:compose (docstring-body) (list documentation declarations forms)))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defmacro'

(define-macro defmacro
    (list* (<- name (function-name/symbol))
           (<- lambda-list ((destructuring-lambda-list lambda-lists) 'nil)) ; TODO macro lambda list
           (:compose (docstring-body) (list documentation declarations forms)))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defclass' including slots

(parser:defrule slot-name ()
  (:guard symbolp))

(define-syntax slot-options
    (list (* (or (:seq :reader        (<<- readers    (function-name/symbol))) ; TODO fail for something like :reader 1
                 (:seq :writer        (<<- writers    (function-name)))
                 (:seq :accessor      (<<- accessor   (function-name/symbol)))
                 (:seq :allocation    (<-  allocation allocation-type))
                 (:seq :initform      (<-  initform   :any)) ; TODO rule for form?
                 (:seq :type          (<-  type       (type-specifier)))
                 (:seq :documentation (:guard documentation string) ; TODO rule for documentation
                       )
                 (:seq (<<- option-names  (:guard symbolp))
                       (<<- option-values :any)))))
  ((readers       *)
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
    (or (<- name (slot-name))
        (list* (<- name (slot-name)) (<- options (slot-options))))
  ((name    1)
   (options 1)))

(define-macro defclass
    (list (<- name (class-name))
          (list (* (<<- superclasses (class-name))))
          (list (* (<<- slots (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (list :default-initargs
                       (* (:seq (<<- default-initargs  (:guard symbolp))
                                (<<- default-initforms :any))))
                 (list :metaclass
                       (<- metaclass (class-name)))
                 (list :documentation
                       (:guard documentation stringp))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (:guard symbolp))
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
    (list* (<- name (class-name))
           (<- lambda-list ((deftype-lambda-list lambda-lists) 'nil))
           (:compose (docstring-body) (list documentation declarations forms)))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *> :evaluation t)))

;;; `defgeneric'

(define-macro defgeneric
    (list (<- name (function-name))
          (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
          (* (or ;; Standard options
                 (list :generic-function-class    (:guard generic-function-class symbolp))
                 (list :argument-precedence-order (<- argument-precedence-order (or :most-specific-first
                                                                                    :most-specific-last)))
                 (list :documentation             (:guard documentation stringp))
                 ;; Non-standard options are basically free-form
                 (list* (<<- option-names (:guard keywordp)) (<<- option-values)))))
  ((name                      1)
   (lambda-list               1)
   ;; Standard options
   (generic-function-class    ?)
   (argument-precedence-order ?)
   (documentation             ?)
   ;; Other options
   (option-names              *)
   (option-values             *)))

;;; `defpackage' and `in-package'

(parser:defrule string-designator ()
  (:guard (typep '(or character string symbol))))

(parser:defrule package-designator ()
  (or (string-designator) (:guard packagep)))

(define-macro defpackage
    (list (<- name (string-designator))
          (* (or (list :nicknames (* (<<- nicknames (string-designator))))
                 (list :documentation (:guard documentation stringp))
                 (list :use (* (<<- use (package-designator))))
                 (list :shadow (* (<<- shadow (:guard symbolp))))
                 (list :shadowing-import-from
                       (<<- shadowing-import-from-packages (package-designator))
                       (<<- shadowing-import-from-names    (:transform
                                                            (* (<<- temp (string-designator)))
                                                            (prog1
                                                                (nreverse temp)
                                                              (setf temp '())))))
                 (list :import-from
                       (<<- import-from-packages (package-designator))
                       (<<- import-from-names    (:transform
                                                  (* (<<- temp (string-designator)))
                                                  (prog1
                                                      (nreverse temp)
                                                    (setf temp '())))))
                 (list :export (* (<<- export (string-designator))))
                 (list :intern (* (<<- intern (string-designator))))
                 (list :size (:guard size (typep '(integer 0)))))))
  ((name                           1)
   (nicknames                      *)
   (documentation                  ?)
   (use                            *)
   (shadow                         *)
   (shadowing-import-from-packages *)
   (shadowing-import-from-names    *)
   (import-from-packages           *)
   (import-from-names              *)
   (export                         *)
   (intern                         *)
   (size                           ?)))

(define-macro in-package
    (list (<- name (string-designator)))
  ((name 1)))
