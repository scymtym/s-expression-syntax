;;;; protocol.lisp --- Protocol functions provided by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

;;; Name protocol

;;; in conditions.lisp
#+(or)(defgeneric name (thing)
        (:documentation
         "Return the name of THING.

If THING is syntax description that describes a standard special
operator, macro, class or type, the returned name is the symbol in the
COMMON-LISP package which names the special operator, macro, class or
type.

If THING is a part, the name is a symbol which uniquely identifies the
part within the containing syntax description."))

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

(defgeneric find-part (naem container &key if-does-not-exist))

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

(defgeneric (setf find-syntax) (new-value name &key if-does-not-exist))

(defgeneric ensure-syntax (name class &rest initargs))

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
references and symbol macro applications. It should always be possible
to find an appropriate syntax description:

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
   "Parse EXPRESSION according to SYNTAX, possibly specialized behavior of CLIENT.

TODO"))

;;; Default behavior

(defmethod parse ((client t) (syntax symbol) (expression t))
  (parse client (find-syntax syntax) expression))
