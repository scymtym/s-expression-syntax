;;;; protocol.lisp --- Protocol functions provided by the syntax system.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; Name protocol

;;; in conditions.lisp
#+(or)(defgeneric name (thing))
(setf (documentation 'name 'function)
      "Return the name of THING.

If THING is a syntax description that describes a standard special
operator, macro, class or type, the returned name is the symbol in the
COMMON-LISP package which names the special operator, macro, class or
type.

If THING is a part, the name is a symbol which uniquely identifies the
part within the containing syntax description.")

;;; Part protocol

(defgeneric cardinality (part)
  (:documentation
   "Return cardinality of sub-expression(s) described by PART.

The following values may be returned

? The described sub-expression occurs zero or one times in the
  containing expression.

1 The described sub-expression occurs exactly once in the containing
  expression.

* The described sub-expression occurs zero or more times in the
  containing expression."))

(defgeneric evaluation (part)
  (:documentation
   "Return evaluation semantics of sub-expressions described by PART."))

;;; Syntax description protocol

(defgeneric parts (container)
  (:documentation
   "Return a sequence of parts belonging to CONTAINER.

"))

(defgeneric find-part (name container &key if-does-not-exist)
  (:documentation
   "Return the part of CONTAINER named NAME.

IF-DOES-NOT-EXIST controls the behavior in case a part named NAME does
not exist in CONTAINER.

If the value of IF-DOES-NOT-EXIST is a function, that function is
called with a single argument, a condition of type
`part-not-found-error'.

If the value of IF-DOES-NOT-EXIST is not a function, that value is
returned in place of the missing part."))

;;; Default behavior

(defmethod find-part ((name t) (container symbol) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (find-part name (find-syntax container)))

(defmethod find-part :around ((name t) (container t) &key if-does-not-exist) ; TODO should be outermost call only
  (or (call-next-method)
      (typecase if-does-not-exist
        (function
         (funcall if-does-not-exist (make-condition 'part-not-found-error
                                                    :syntax container
                                                    :name   name)))
        (t
         if-does-not-exist))))

;;; Backwards compatibility

(defun components (container)
  (parts container))

(defun find-component (name container
                       &key (if-does-not-exist nil if-does-not-exist-p))
  (apply #'find-part name container (when if-does-not-exist-p
                                      (list :if-does-not-exist if-does-not-exist))))

#+sbcl
(declaim (sb-ext:deprecated :early ("s-expression-syntax" "0.1")
                            (function components) (function find-component)))

;;; Syntax description repository protocol

(defgeneric find-syntax (name &key if-does-not-exist)
  (:documentation
   "Return the syntax description named NAME, if any.

IF-DOES-NOT-EXIST controls the behavior in case a syntax description
named NAME does not exist. The following values are allowed:

#'ERROR

  Signal an error if a syntax description named NAME does not exist.

OBJECT

  Return OBJECT if a syntax description named NAME does not exist."))

(defgeneric (setf find-syntax) (new-value name &key if-does-not-exist)
  (:documentation
   "Set the syntax description associated with NAME to NEW-VALUE.

An existing association for NAME, if any, is replaced.

IF-DOES-NOT-EXISTS is accepted for parity with FIND-SYNTAX but
ignored."))

(defgeneric ensure-syntax (name class &rest initargs)
  (:documentation
   "Associate NAME with a syntax description based on CLASS and INITARGS.

Return the new or updated syntax description object associated with
NAME.

If the database of syntax descriptions already contains a syntax
description for NAME, the existing syntax description object is
reinitialized with INITARGS.

If the database of syntax descriptions does not contain a syntax
description for NAME, a new association is created by making an
instance of CLASS, initializing it with INITARGS and registering the
new object for NAME."))

;;; Default behavior

(defmethod find-syntax :around ((name t) &key (if-does-not-exist #'error))
  (or (call-next-method)
      (typecase if-does-not-exist
        (function
         (funcall if-does-not-exist (make-condition 'syntax-not-found-error
                                                    :name name)))
        (t
         if-does-not-exist))))

;;; Parse protocol

(defgeneric classify (client expression)
  (:documentation
   "Classify EXPRESSION, possibly according to specialized behavior of CLIENT.

Return a syntax description object that roughly reflects the kind of
EXPRESSION. Note that a precise classification would have to take into
account aspects beyond the syntax, such as the environment, to, for
example, distinguish function and macro application or variable
references and symbol macro applications. It is always possible to
find an appropriate syntax description:

+ If EXPRESSION is a special form, this function returns the syntax
  description for the corresponding special operator.

+ If EXPRESSION is an application of a standard macro, this function
  returns the syntax description for that macro.

+ If EXPRESSION a list not covered by the above cases, this function
  returns the syntax description for a generic (that is, function or
  macro) application. Note that this case also covers invalid
  applications such as (1 2 3).

+ If EXPRESSION is a symbol but not a keyword, this function returns a
  syntax description for a variable reference.

+ If EXPRESSION is any object that is not covered by the above cases,
  this function returns a syntax description for a self-evaluating
  object."))

(defgeneric parse (client syntax expression)
  (:documentation
   "Parse EXPRESSION according to SYNTAX, possibly specialized to CLIENT.

SYNTAX is a designator for a syntax description:

+ If SYNTAX is `t', `classify' is applied to CLIENT and EXPRESSION to
  obtain an appropriate syntax description object.

+ If SYNTAX is any other symbol, `find-syntax' is called to obtain the
  syntax description named by SYNTAX. An error is signaled if SYNTAX
  does not name a syntax description.

+ Otherwise SYNTAX must be a syntax description object.

EXPRESSION is either one of the kinds of expressions that make up
Common Lisp programs (such as forms, type specifiers and declarations)
or a particular non-standard representation of such expressions which
is specific to CLIENT. For example, a client may choose to represent
every sub-expression contained in an expression as a standard object
in order to store additional information. If CLIENT employs such a
non-standard representation, the protocol named by symbols exported
from the `s-expression-syntax.expression-grammar' package has to be
implemented by defining appropriate methods.

If EXPRESSION does not conform to the syntax described by SYNTAX, an
error of type `invalid-syntax-error' is signaled.

If EXPRESSION does conform to the syntax described by SYNTAX, a parse
result that associates the parts of SYNTAX with sub-expressions of
EXPRESSION is returned. The type and structure of the return value
depends on CLIENT as the parse result is constructed using the builder
protocol with CLIENT as the builder."))

;;; Default behavior

(defmethod parse ((client t) (syntax symbol) (expression t))
  (parse client (find-syntax syntax) expression))

;;; Unparse protocol

(defgeneric unparse (client syntax node)
  (:documentation
   "Unparse NODE according to SYNTAX and return the resulting expression.

SYNTAX is a designator for a syntax description:

+ If SYNTAX is `t', this function uses the kind of NODE to obtain an
  appropriate syntax description object.

+ If SYNTAX is any other symbol, this function calls `find-syntax' to
  obtain the syntax description named by SYNTAX. An error is signaled if
  SYNTAX does not name a syntax description.

+ Otherwise SYNTAX must be a syntax description object.

TODO"))

;;; Default behavior

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
