;;;; loop.lisp ---  Syntax of the Common Lisp loop macro.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

(defmacro define-loop-clause (name (&rest arguments) &body body)
  (let ((rule-name (a:symbolicate '#:loop- name '#:-clause)))
    `(defrule (,rule-name :environment (make-instance 'eg::expression-environment)) ,arguments
       ,@body)))

(define-macro loop
    (list* (<- clauses (or (simple-loop) (extended-loop))))
  ((clauses *)))

(defrule simple-loop ()
    (list)
  '(:loop))

(defrule extended-loop ()
    (list (? (<- name (loop-name-clause)))
          (* (<<- variable-clauses (loop-variable-clause)))
          (* (<<- main-clauses (and :any
                                    (must (loop-main-clause) "must be a LOOP main clause (i.e. do, collect, until, initially, etc.)")))))
  (list :loop :name             name
              :variable-clauses (nreverse variable-clauses)
              :main-clauses     (nreverse main-clauses)))

(defrule (loop-name-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name "NAMED"))
         (<- name (must (guard symbolp) "name must be a symbol")))
  name)

;;; Variable clauses

(defrule (loop-variable-clause :environment (make-instance 'eg::expression-environment)) ()
    (or (loop-with-clause)
        (loop-initial-final-clause)
        (loop-for-as-clause)))

(defrule (loop-with-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name "WITH")) (<- name ((variable-name! names))))
  (list :with name))

(defrule (loop-for-as-clause :environment (make-instance 'eg::expression-environment)) ()
    ;; TODO destructuring
    (seq (<- variables (structure 'symbol (symbol-name (or "FOR" "AS"))))
         (loop-for-as-in-list-clause))
  (list :for variables))

(define-loop-clause for-as-in-list ()
    (seq (<- name ((variable-name! names))) ; (? (loop-type-spec-clause))
         (must (structure 'symbol (symbol-name (or "IN" "ON"))) "must be IN or ON")
         (<- form ((form! forms)))))

;;; Main clauses

(defrule (loop-main-clause :environment (make-instance 'eg::expression-environment)) ()
    (or (loop-unconditional-clause :unconditional)
        (loop-accumulation-clause :unconditional)
        (loop-conditional-clause)
        (loop-termination-test-clause)
        (loop-initial-final-clause)))

(defrule (loop-unconditional-clause :environment (make-instance 'eg::expression-environment)) (context)
    (or (loop-do-clause)
        (loop-return-clause context)))

(defrule (loop-do-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name (or "DO" "DOING")))
         ((compound-form! forms))))

(defrule (loop-return-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (structure 'symbol (symbol-name "RETURN"))
         (loop-form-or-it context)))

(defrule (loop-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (or (loop-list-accumulation-clause context)
        (loop-numeric-accumulation-clause context)))

(defrule (loop-list-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (structure 'symbol (symbol-name (or "COLLECT" "COLLECTING" "APPEND" "APPENDING" ; TODO
                                              )))
         (<- value (loop-form-or-it context))
         (? (seq (structure 'symbol (symbol-name "INTO"))
                 (<- into ((variable-name! names))))))
  (list :list-accum :value value :into into))

(defrule (loop-numeric-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (structure 'symbol (symbol-name (or "COUNT" "COUNTING" "SUM" "SUMMING" ; TODO
                                              )))
         (<- value (loop-form-or-it context))
         (? (seq (structure 'symbol (symbol-name "INTO"))
                 (<- into ((variable-name! names))))))
  (list :numeric-accum :value value :into into))

(defrule (loop-conditional-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name (or "IF" "WHEN" "UNLESS")))
         (<- test ((form! forms)))
         (must (loop-selectable-clause) "must be a selectable clause")))

;;; Selectable clauses

(defrule (loop-selectable-clause :environment (make-instance 'eg::expression-environment)) ()
    (or (loop-unconditional-clause :conditional)
        (loop-accumulation-clause :conditional)
        (loop-conditional-clause)))

(defrule (loop-termination-test-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name (or "WHILE" "UNTIL" "REPEAT" "ALWAYS" "NEVER" "THEREIS")))
         (<- form ((form! forms))))
  (list :while form))

(defrule (loop-initial-final-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (structure 'symbol (symbol-name (or "INITIALLY" "FINALLY")))
         (+ (<<- forms ((compound-form! forms)))))
  (list :initially forms))

;;; Variables and types

(defrule loop-form-or-it (context)
    (or (and (structure 'symbol (symbol-name "IT"))
             (must (:transform :any (unless (eq context :conditional) (:fail)))
                   "IT is only allowed in a conditional context"))
        (form!)))

(define-loop-clause type-spec ()
    (or (seq (structure 'symbol (symbol-name "OF-TYPE"))
             ((type-specifier! type-specifiers)))
        (and :any (must (or 'fixnum 'float 't 'nil) "must be FIXNUM, FLOAT, T or NIL"))))
