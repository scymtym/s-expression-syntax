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

(parser.packrat:defrule once (context)  ; TODO define-macro-rule
    (:compose (:transform (list 'once expression (seq:? name))
                (let ((temp    (gensym))
                      (message (format nil "~S option must not be repeated"
                                       (or name expression))))
                  `(and ,expression
                        (<- ,temp (:transform :any
                                    (when ,temp
                                      (:fatal ,message))
                                    t)))))
              (base::expression context)))

;;; List-shaped options

(defun emit-list-option-expression (name values repeated &key repeat?)
  (let* ((required (- (length values) (length repeated)))
         (message (format nil "~S option accepts~:[~; at least~] ~R ~
                               value~:P"
                          name repeated required)))
    `(list* ,(if repeat? `',name `(once ',name ,name))
            (must (list ,@(reverse values)) ,message))))

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
  `(seq:seq ,(if repeat? `',name `(once ',name ,name)) ,value))

(parser.packrat:defrule poption (context)
  ;; Transform (poption NAME PARAMETER) into an expression that parses
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
