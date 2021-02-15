;;;; meta-grammar.lisp --- Meta grammar used by the expression-grammar module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

(parser.packrat:defgrammar meta-grammar
  (:class   sexp:sexp-grammar)
  (:cached? nil)
  (:use     sexp::meta-grammar))

(parser.packrat:in-grammar meta-grammar)

;;; Once
;;;
;;; Make sure an expression (or one expression in a group of
;;; expressions) only matches once.

(parser.packrat:defrule once (context)  ; TODO define-macro-rule
    (:compose (:transform (list* 'once (must (list expression (* (or (seq:seq :name name)
                                                                     (seq:seq :flag flag))))
                                             "must be of the form EXPRESSION {:name NAME|:flag FLAG}"))
                (let ((result  (gensym "RESULT"))
                      (flag    (or flag (gensym)))
                      (message (format nil "~A must not be repeated"
                                       (or name expression))))
                  `(:compose (<- ,flag (:transform (<- ,result ,expression)
                                         (when ,flag
                                           (:fatal ,message))
                                         t))
                             (:transform :any ,result))))
              (base::expression context)))

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

(parser.packrat:defrule option (context)
    ;; Transform (option NAME PARAMETER₁ … PARAMETERₖ) into an
    ;; expression that produces a parse error unless exactly K
    ;; arguments are supplied to the option. However, as a
    ;; hack/exception, expressions of the form (* …) do not count
    ;; towards the required number of arguments.
    (:compose
     (:transform (list 'option name (* (and (seq:? (<<- repeated (list* '* :any)))
                                            (<<- values))))
       (emit-list-option-expression name values repeated))
     (base::expression context)))

(parser.packrat:defrule option* (context)
  ;; Like `option' but allow repeated occurrences
  (:compose
   (:transform (list 'option* name (* (and (seq:? (<<- repeated (list* '* :any)))
                                           (<<- values))))
     (emit-list-option-expression name values repeated :repeat? t))
   (base::expression context)))

;;; Property-shaped options

(defun emit-property-option-expression (name value &key repeat?)
  (let ((name-expression (if repeat?
                             `',name
                             (let ((name* (format nil "~S option" name)))
                               `(once ',name :name ,name*)))))
    `(seq:seq ,name-expression ,value)))

(parser.packrat:defrule poption (context)
  ;; Transform (poption NAME PARAMETER) into an expression that
  ;; matches the option.
  (:compose
   (:transform (list 'poption name value)
     (emit-property-option-expression name value))
   (base::expression context)))

(parser.packrat:defrule poption* (context)
  ;; Like `poption' but allow repeated occurrences
  (:compose
   (:transform (list 'poption* name value)
     (emit-property-option-expression name value :repeat? t))
   (base::expression context)))

(parser.packrat:defrule base::expression (context)
  (or (once context)

      (option context)  (option* context)
      (poption context) (poption* context)

      ((base::expression sexp::meta-grammar) context)))
