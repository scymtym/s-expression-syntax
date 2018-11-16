(cl:in-package #:syntax)

(parser:in-grammar special-operators)

;;; `defun'

(define-macro defun
    (list* (:guard name symbolp)
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
           (:compose (docstring-body) (list documentation declarations forms)))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *>)))

;;; `defclass' including slots

(parser:defrule slot-name ()
  (:guard symbolp))

(define-syntax slot-options
    (list (* (or (:seq :reader        (<<- readers    (:must (function-name/symbol)))) ; TODO fail for something like :reader 1
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
   (initform      ?)
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
    (list (:guard name symbolp)
          (list (* (<<- superclasses (:guard symbolp))))
          (list (* (<<- slots (slot-specifier))))
          (* (or ;; Standard options have their respective syntax.
                 (list :default-initargs
                       (* (:seq (<<- default-initargs  (:guard symbolp))
                                (<<- default-initforms :any))))
                 (list :metaclass
                       (:guard metaclass symbolp))
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
   (default-initforms *)
   (metaclass         ?)
   (documentation     ?)
   ;; Non-standard options
   (option-names      *)
   (option-values     *)))

;;; `defgeneric'

(define-macro defgeneric
    (list name
          (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
          (* (or (list :generic-function-class    (:guard generic-function-class symbolp))
                 (list :argument-precedence-order (<- argument-precedence-order (or :most-specific-first
                                                                                    :most-specific-last)))
                 (list :documentation             (:guard documentation stringp))
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
