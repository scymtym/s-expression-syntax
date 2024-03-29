;;;; meta-grammar.lisp --- Meta grammar used by the expression-grammar module.
;;;;
;;;; Copyright (C) 2020-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

(parser.packrat:defgrammar meta-grammar
  (:class   sexp:sexp-grammar)
  (:cached? nil)
  (:use     sexp::meta-grammar))

(parser.packrat:in-grammar meta-grammar)

(defmacro define-macro-rule (name ()
                             expression)
  (a:with-unique-names (context-var)
    `(parser.packrat:defrule ,name (,context-var)
       (:compose ,expression (base::expression ,context-var)))))

;;; Force element context
;;;
;;; Force a sub-expression of a sequence context to use the element
;;; context.

(define-macro-rule element ()
  ;; Transform into a GUARD expression with a predicate that is always
  ;; true. The presence of the GUARD expression forces the element
  ;; context for EXPRESSION. Later, when compiling, the %ALWAYS-TRUE
  ;; predicate is recognized and no code is emitted for the predicate
  ;; call.
  (:transform (list 'element expression)
    `(guard ,expression (%always-true))))

;;; Once
;;;
;;; Make sure an expression (or one expression in a group of
;;; expressions) only matches once.

(define-macro-rule once ()
  (:transform (list* 'once (must (list expression (* (or (seq:seq :name name)
                                                         (seq:seq :flag flag))))
                                 "must be of the form EXPRESSION {:name NAME|:flag FLAG}"))
    (let ((result  (gensym "RESULT"))
          (flag    (or flag (gensym)))
          (message (format nil "~A must not be repeated"
                           (or name expression))))
      `(or (and (not :any) (<- ,flag :any))
           (:transform (<- ,result ,expression)
             (if ,flag
                 (:fatal ,message)
                 (setf ,flag t))
             ,result)))))

;;; List-shaped options

(defun emit-list-option-expression (name values repeated &key repeat?)
  (let* ((required        (- (length values) (length repeated)))
         (message         (format nil "~S option accepts~:[~; at least~] ~R ~
                                       value~:P"
                                  name repeated required))
         (name-expression (if repeat?
                              `',name
                              (let ((name* (format nil "~S option" name)))
                                `(once ',name :name ,name*)))))
    `(list* ,name-expression (must (list ,@(reverse values)) ,message))))

(define-macro-rule option ()
    ;; Transform (option NAME PARAMETER₁ … PARAMETERₖ) into an
    ;; expression that produces a parse error unless exactly K
    ;; arguments are supplied to the option. However, as a
    ;; hack/exception, expressions of the form (* …) do not count
    ;; towards the required number of arguments.
  (:transform (list 'option name (* (and (seq:? (<<- repeated (list* '* :any)))
                                         (<<- values))))
    (emit-list-option-expression name values repeated)))

(define-macro-rule option* ()
  ;; Like `option' but allow repeated occurrences
  (:transform (list 'option* name (* (and (seq:? (<<- repeated (list* '* :any)))
                                          (<<- values))))
    (emit-list-option-expression name values repeated :repeat? t)))

;;; Property-shaped options

(defun emit-property-option-expression (name value &key repeat?)
  (let ((name-expression (if repeat?
                             `',name
                             (let ((name* (format nil "~S option" name)))
                               `(once ',name :name ,name*)))))
    `(seq:seq ,name-expression ,value)))

(define-macro-rule poption ()
  ;; Transform (poption NAME PARAMETER) into an expression that
  ;; matches the option.
  (:transform (list 'poption name value)
    (emit-property-option-expression name value)))

(define-macro-rule poption* ()
  ;; Like `poption' but allow repeated occurrences
  (:transform (list 'poption* name value)
    (emit-property-option-expression name value :repeat? t)))

(parser.packrat:defrule base::expression (context)
  (or (element context)

      (once context)

      (option context)  (option* context)
      (poption context) (poption* context)

      ((base::expression sexp::meta-grammar) context)))
