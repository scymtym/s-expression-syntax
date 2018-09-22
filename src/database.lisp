;;;; database.lisp --- TODO.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

;;; `named-mixin'

(defclass named-mixin ()
  ((%name :initarg  :name
          :reader   name))
  (:default-initargs
   :name (error "Missing required initarg ~S for class ~S"
                :name 'named-mixin)))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name ,(name object) "~A")))

;;; `documentation-mixin'

(defclass documentation-mixin ()
  ((%documentation :initarg  :documentation
                   :type     (or null string)
                   :reader   %documentation
                   :initform nil)))

;;; `parser-mixin'

(defclass parser-mixin ()
  ((%rule   :reader   rule
            :accessor %rule)
   (%parser :accessor %parser)))

(defmethod shared-initialize :after ((instance   parser-mixin)
                                     (slot-names t)
                                     &key
                                     (rule nil rule-supplied?))
  (when rule-supplied?
    (setf (%rule instance) rule)))

(defmethod (setf %rule) :after ((new-value t) (syntax parser-mixin))
  (setf (%parser syntax) (compile-parser new-value)))

(defun compile-parser (rule)
  (let* ((grammar    (parser.packrat.grammar:find-grammar 'special-operators))
         (expression (make-instance 'parser.packrat.grammar.base:rule-invocation-expression
                                    :rule rule))
         (rule       (compile nil (parser.packrat.compiler:compile-rule
                                   grammar '() expression))))
    (declare (type function rule))
    (lambda (form)
      (let ((context (parser.packrat.grammar::make-context
                      grammar expression form)))
        (funcall rule context form)))))

(defmethod parse ((client t) (syntax parser-mixin) (form t))
  (multiple-value-bind (success? components) (funcall (%parser syntax) form)
    (when success?
      components)))

;;; `special-operator'

(defclass special-operator (named-mixin
                            documentation-mixin
                            parser-mixin
                            print-items:print-items-mixin)
  ((%components :initarg :components
                :reader  components)))

(defmethod find-component ((name t) (container special-operator))
  (find name (components container) :key #'name :test #'eq))

;;; `component'

(defclass component (named-mixin
                     print-items:print-items-mixin)
  ((%cardinality :initarg :cardinality
                 :reader  cardinality)
   (%evaluation :initarg  :evaluation   ; TODO change this to semantic
                :reader   evaluation)))

(defmethod print-items:print-items append ((object component))
  (let ((semantics (when-let ((semantics (evaluation object)))
                    (print-items:print-items semantics))))
    `((:name      ,(name object) "~A")
      (:sematnics ,semantics     " ~/print-items:format-print-items/" ((:after :name))))))

;;;

(defvar *syntaxes* (make-hash-table :test #'eq))

(defun syntaxes ()
  (hash-table-values *syntaxes*))

(defun syntaxes/alist ()
  (hash-table-alist *syntaxes*))

(defun find-syntax (name &key (if-does-not-exist #'error))
  (or (gethash name *syntaxes*)
      (typecase if-does-not-exist
        (function
         (funcall if-does-not-exist (make-condition 'syntax-not-found-error
                                                    :name name)))
        (t
         if-does-not-exist))))

(defun (setf find-syntax) (new-value name)
  (setf (gethash name *syntaxes*) new-value))

(defun ensure-syntax (name class &rest initargs)
  (let ((initargs (list* :name name initargs)))
    (if-let ((existing (find-syntax name :if-does-not-exist nil)))
      (apply #'reinitialize-instance existing initargs)
      (setf (find-syntax name) (apply #'make-instance class
                                      initargs)))))
