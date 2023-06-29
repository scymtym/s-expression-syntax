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
   (%parser  :accessor %parser)))

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

;;; `unparser-mixin'

(defclass unparser-mixin ()
  ((%unparser :initarg  :unparser
              :accessor %unparser)))

(defmethod unparse ((client t) (syntax unparser-mixin) (node t))
  (multiple-value-bind (initargs sub-expressions)
      (destructure-and-unparse-node client node)
    (apply (%unparser syntax) initargs sub-expressions)))

(defun destructure-and-unparse-node (client node)
  (bp:with-unbuilder (client) ; TODO this does too much work
    (let ((initargs  (bp:node-initargs* node))
          (relations (bp:node-relations* node)))
      (labels ((sub-expression (right)
                 (unparse client t right))
               (sub-expressions ()
                 (loop :for relation           :in relations
                       :for (name cardinality) = (multiple-value-list
                                                  (bp:normalize-relation relation))
                       :for right              = (bp:node-relation* relation node)
                       :collect name
                       :collect (bp:cardinality-ecase cardinality
                                  ((1 bp:?)
                                   (sub-expression right))
                                  (*
                                   (map 'list #'sub-expression right))
                                  (:map
                                   (error "not implemented"))))))
        (values initargs (sub-expressions))))))

(defmethod unparse ((client t) (syntax (eql :unparsed)) (node t))
  (getf (bp:node-initargs client node) :expression))

(defmethod unparse ((client t) (syntax (eql :variable-name)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :function-name)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :type-name)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :block-name)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :initarg-name)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :tag)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :documentation)) (node t))
  (getf (bp:node-initargs client node) :string))

(defmethod unparse ((client t) (syntax (eql :string-designator)) (node t))
  (getf (bp:node-initargs client node) :string))

(defmethod unparse ((client t) (syntax (eql :keyword)) (node t))
  (getf (bp:node-initargs client node) :name))

(defmethod unparse ((client t) (syntax (eql :lambda-list-keyword)) (node t))
  (getf (bp:node-initargs client node) :keyword))

(defmethod unparse ((client t) (syntax (eql :literal)) (node t))
  (getf (bp:node-initargs client node) :value))

(defmethod unparse ((client t) (syntax (eql :atomic-type-specifier)) (node t))
  (let ((name (bp:node-relation client :name node)))
    (unparse client t name)))

(defmethod unparse ((client t) (syntax (eql :declaration-specifier)) (node t))
  (destructuring-bind ((&key kind &allow-other-keys) (&key argument))
      (multiple-value-list (destructure-and-unparse-node client node)) ; TODO don't cons the list
    `(,kind ,@argument)))

(defmethod unparse ((client t) (syntax (eql 'required-section)) (node t))
  (destructuring-bind (&key parameter)
      (nth-value 1 (destructure-and-unparse-node client node))
    parameter))

(defmethod unparse ((client t) (syntax (eql 'optional-section)) (node t))
  (destructuring-bind (&key keyword parameter)
      (nth-value 1 (destructure-and-unparse-node client node))
    (list* keyword parameter)))

(defmethod unparse ((client t) (syntax (eql 'rest-section)) (node t))
  (destructuring-bind (&key keyword parameter)
      (nth-value 1 (destructure-and-unparse-node client node))
    (list keyword parameter)))

(defmethod unparse ((client t) (syntax (eql 'keyword-section)) (node t))
  (destructuring-bind (&key keyword parameter allow-other-keys)
      (nth-value 1 (destructure-and-unparse-node client node))
    `(,keyword ,@parameter ,@(if allow-other-keys `(,allow-other-keys) '()))))

(defmethod unparse ((client t) (syntax (eql 'aux-section)) (node t))
  (destructuring-bind (&key keyword parameter)
      (nth-value 1 (destructure-and-unparse-node client node))
    (list* keyword parameter)))

(defmethod unparse ((client t) (syntax (eql 'tagbody-segment)) (node t))
  (destructuring-bind (&key label statement)
      (nth-value 1 (destructure-and-unparse-node client node))
    (list* label statement)))

(defmethod unparse ((client t) (syntax (eql t)) (node t))
  (let* ((kind   (bp:node-kind client node))
         (name   (or (find-symbol (symbol-name kind) (find-package '#:cl))
                     (find-symbol (symbol-name kind) (find-package '#:s-expression-syntax))))
         (syntax (case kind
                   ((:unparsed
                     :variable-name :function-name :type-name :block-name :initarg-name
                     :keyword :lambda-list-keyword
                     :tag :string-designator :documentation :literal
                     :declaration-specifier :atomic-type-specifier)
                    kind)
                   ((:required-section :optional-section :rest-section :keyword-section :aux-section
                     :tagbody-segment)
                    name)
                   (t
                    (find-syntax name)))))
    (unparse client syntax node)))

;;; `syntax-description'

(defclass syntax-description (named-mixin
                              documentation-mixin
                              parser-mixin
                              unparser-mixin
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
