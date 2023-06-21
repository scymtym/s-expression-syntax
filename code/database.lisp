;;;; database.lisp --- Storage and lookup of syntax descriptions.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; `named-mixin'

(defclass named-mixin ()
  ((%name :initarg  :name
          :reader   name))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name)))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name "~A" ,(name object))))

;;; `documentation-mixin'

(defclass documentation-mixin ()
  ((%documentation :initarg  :documentation
                   :type     (or null string)
                   :reader   %documentation
                   :initform nil)))

;;; `parser-mixin'

(defclass parser-mixin ()
  ((%rule    :reader   rule
             :accessor %rule)
   (%grammar :reader   grammar
             :accessor %grammar
             :initform nil)
   (%parser  :initarg  :parser
             :accessor %parser)))

(defmethod shared-initialize :after ((instance   parser-mixin)
                                     (slot-names t)
                                     &key (rule    nil rule-supplied?)
                                          (grammar nil grammar-supplied?))
  (when rule-supplied?
    (setf (%rule instance) rule))
  (when grammar-supplied?
    (setf (%grammar instance) grammar))
  (when (or rule-supplied? grammar-supplied?)
    (setf (%parser instance)
          (compile-parser (%rule instance) (%grammar instance)))))

(defun compile-parser (rule grammar)
  (destructuring-bind (rule &rest arguments) (a:ensure-list rule)
    (let* ((grammar              (parser.packrat.grammar:find-grammar
                                  (or grammar 'special-operators)))
           (argument-expressions (mapcar (lambda (argument)
                                           (make-instance 'parser.packrat.grammar.base:constant-expression
                                                          :value (second argument)))
                                         arguments))
           (expression           (make-instance 'parser.packrat.grammar.base:rule-invocation-expression
                                                :rule      rule
                                                :arguments argument-expressions))
           (rule                 (compile nil (parser.packrat.compiler:compile-rule
                                               grammar rule '() expression))))
      (declare (type function rule))
      (lambda (form)
        (let ((context (parser.packrat.grammar::make-context
                        grammar expression form)))
          (funcall rule context form))))))

(defmethod parse ((client t) (syntax parser-mixin) (expression t))
  (multiple-value-bind (success? result value)
      (bp:with-builder (client)
        (funcall (%parser syntax) expression))
    (if (eq success? t)
        result
        (invalid-syntax-error
         syntax result (if (stringp value) value "invalid expression")))))

;;; `syntax-description'

(defclass syntax-description (named-mixin
                              documentation-mixin
                              parser-mixin
                              print-items:print-items-mixin)
  ((%parts :initarg :parts
           :type    list
           :reader  parts))
  (:default-initargs
   :parts (missing-required-initarg 'special-operator :parts)))

(defmethod find-part ((name t) (container syntax-description)
                      &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (find name (parts container) :key #'name :test #'eq))

;;; `part'

(defclass part (named-mixin
                print-items:print-items-mixin)
  ((%cardinality :initarg :cardinality
                 :type    (member 1 bp:? *)
                 :reader  cardinality)
   (%evaluation :initarg  :evaluation   ; TODO change this to semantic
                :reader   evaluation))
  (:default-initargs
   :cardinality (missing-required-initarg 'part :cardinality)
   :evaluation  (missing-required-initarg 'part :evaluation)))

(defmethod print-items:print-items append ((object part))
  (let ((semantics (a:when-let ((semantics (evaluation object)))
                     (print-items:print-items semantics))))
    `((:name                         "~A"  ,(name object))
      ((:cardinality (:after :name)) " ~A" ,(cardinality object))
      ,@(when semantics
          `(((:semantics (:after :cardinality)) " ~/print-items:format-print-items/" ,semantics))))))

;;; `special-operator-syntax'

(defclass special-operator-syntax (syntax-description)
  ())

;;; `macro-synax'

(defclass macro-syntax (syntax-description)
  ())

;;; Database of named syntax definitions

(defvar *syntaxes* (make-hash-table :test #'eq))

(defun syntaxes ()
  (a:hash-table-values *syntaxes*))

(defun syntaxes/alist ()
  (a:hash-table-alist *syntaxes*))

(defmethod find-syntax ((name t) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  ;; Error signaling is handled in an `:around' method.
  (gethash name *syntaxes*))

(defmethod (setf find-syntax) ((new-value t) (name t) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name *syntaxes*) new-value))

(defmethod ensure-syntax ((name t) (class t) &rest initargs)
  (let ((initargs (list* :name name initargs)))
    (a:if-let ((existing (find-syntax name :if-does-not-exist nil)))
      (apply #'reinitialize-instance existing initargs)
      (setf (find-syntax name) (apply #'make-instance class initargs)))))
