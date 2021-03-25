;;;; loop.lisp ---  Syntax of the Common Lisp loop macro.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Utilities

(defmacro define-loop-clause (name (&rest arguments) &body body)
  (let ((rule-name (a:symbolicate '#:loop- name '#:-clause)))
    `(defrule (,rule-name :environment (make-instance 'eg::expression-environment)) ,arguments
       ,@body)))

(define-macro loop
    (list* (<- clauses (or (simple-loop) (extended-loop))))
  ((clauses *>)))

(defrule simple-loop ()
    (list (* (<<- forms ((form! forms)))))
  (list :loop (nreverse forms)))

(parse 'list (find-syntax 'loop) '(loop (foo) (bar)))

(defrule extended-loop ()
    (list (? (<- name (loop-name-clause)))
          (* (<<- variable-clauses (loop-variable-clause)))
          (* (<<- main-clauses (and :any
                                    (must (loop-main-clause) "must be a LOOP main clause (i.e. do, collect, until, initially, etc.)")))))
  (list :loop :name             name
              :variable-clauses (nreverse variable-clauses)
              :main-clauses     (nreverse main-clauses)))

(defrule (loop-name-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "NAMED")
         (<- name (must (guard (typep 'symbol)) "name must be a symbol")))
  name)

;;; Variable clauses

(defrule (loop-variable-clause :environment (make-instance 'eg::expression-environment)) ()
    (or (loop-with-clause)
        (loop-initial-final-clause)
        (loop-for-as-clause)))

(defrule (loop-with-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "WITH") (<<- clauses (loop-with-variable-clause))
         (* (seq (eg::loop-keyword "AND") (<<- clauses (loop-with-variable-clause)))))
  (list :with :clauses (nreverse clauses)))

(define-loop-clause with-variable ()
    (seq (<- name ((variable-name! names)))
         ;; (? (loop-type-spec))
         (? (seq (eg::loop-keyword "=") (<- form ((form! forms))))))
  (list name form))

(parse 'list (find-syntax 'loop) '(loop :with a = 1 and b = 2))

(defrule (loop-for-as-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "FOR" "AS")
         (<<- clauses (loop-for-as-subclause))
         (* (seq (eg::loop-keyword "AND") (<<- clauses (loop-for-as-subclause)))))
  (nreverse clauses))

(defrule (loop-for-as-subclause :environment (make-instance 'eg::expression-environment)) ()
  (or (loop-for-as-in-list-clause) ; must be first
      (loop-for-as-arithmetic-clause)
      (loop-for-as-equals-then-clause)
      (loop-for-as-across-clause)
      (loop-for-as-hash-clause)
      (loop-for-as-package-clause)))

(define-loop-clause for-as-arithmetic ()
  (seq (<- name ((variable-name! names))) ; TODO loop-typed-variable
       ;; (? (loop-type-spec))
       (or ;; arithmetic-downto
           (:transform
               (+ (or (seq (eg:once (eg::loop-keyword "FROM"))
                           (<- from ((form! forms))))
                      (seq (eg:once (eg::loop-keyword "DOWNTO" "ABOVE"))
                           (<- to ((form! forms))))
                      (seq (eg:once (eg::loop-keyword "BY"))
                           (<- by ((form! forms))))))
             (unless (and from to)
               (:fail)))
           ;; arithmetic-up
           (+ (or (seq (eg:once (eg::loop-keyword "FROM" "UPFROM"))
                       (<- from ((form! forms))))
                  (seq (eg:once (eg::loop-keyword "TO" "UPTO" "BELOW"))
                       (<- to ((form! forms))))
                  (seq (eg:once (eg::loop-keyword "BY"))
                       (<- by ((form! forms))))))
           ;; arithmetic-downfrom
           (:transform
              (+ (or (seq (eg:once (eg::loop-keyword "DOWNFROM"))
                          (<- from ((form! forms))))
                     (seq (eg:once (eg::loop-keyword "TO" "DOWNTO" "ABOVE"))
                          (<- from ((form! forms))))))
            (unless from
              (:fail)))))
  (list :arithmetic name from to by))

(define-loop-clause for-as-in-list ()
  (seq (<- name ((variable-name! names))) ; TODO destructuring
       ;; (? (loop-type-spec))
       (eg::loop-keyword "IN" "ON") ; (must (eg::loop-keyword "IN" "ON") "must be IN or ON")
       (<- list-form ((form! forms)))
       (? (seq (eg::loop-keyword "BY") (<- step-form ((form! forms))))))
  (list :in-list name list-form step-form))

(define-loop-clause for-as-equals-then ()
  (seq (<- name ((variable-name! names)))
       ;; (? (loop-type-spec))
       (eg::loop-keyword "=")
       (<- form ((form! forms)))
       (? (seq (eg::loop-keyword "THEN")
               (<- then ((form! forms)))))))

(define-loop-clause for-as-across ()
  (seq (<- name ((variable-name! names)))
       ;; (? (loop-type-spec))
       (eg::loop-keyword "ACROSS")
       (<- form ((form! forms)))))

(define-loop-clause for-as-hash ()
    (seq (<- name ((variable-name! names)))
         ;; (? (loop-type-spec))
         (eg::loop-keyword "BEING") (eg::loop-keyword "EACH" "THE")
         (or (seq (eg::loop-keyword "HASH-KEY" "HASH-KEYS")
                  (eg::loop-keyword "IN" "OF")
                  (<- hash-table ((form! forms)))
                  (? (seq (eg::loop-keyword "USING")
                          (list (eg::loop-keyword "HASH-VALUE")
                                (<- other-var ((variable-name! names)))))))
             (seq (eg::loop-keyword "HASH-VALUE" "HASH-VALUES")
                  (eg::loop-keyword "IN" "OF")
                  (<- hash-table ((form! forms)))
                  (? (seq (eg::loop-keyword "USING")
                          (list (eg::loop-keyword "HASH-KEY")
                                (<- other-var ((variable-name! names)))))))))
  (list :hash name hash-table other-var))

(define-loop-clause for-as-package ()
    (seq (<- name ((variable-name! names)))
         ;; (? (loop-type-spec))
         (eg::loop-keyword "BEING") (eg::loop-keyword "EACH" "THE")
         (or (eg::loop-keyword "SYMBOL" "SYMBOLS")
             (eg::loop-keyword "PRESENT-SYMBOL" "PRESENT-SYMBOLS")
             (eg::loop-keyword "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS"))
         (eg::loop-keyword "IN" "OF")
         (<- form ((form! forms))))
  (list :package name form))

(parse 'list (find-syntax 'loop) '(loop :for a :in foo :by #'cdr
                                        and b :upfrom baz :to 10 :by (random 10)
                                   and c :from a downto #:foo))

(parse 'list (find-syntax 'loop) '(loop :for a :being the hash-keys in (make-hash-table) using (hash-value b)))

(parse 'list (find-syntax 'loop) '(loop :for a :being the symbols in (find-package "CLOUSEAU") :collect a))

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
    (seq (eg::loop-keyword "DO" "DOING")
         ((compound-form! forms))))

(defrule (loop-return-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (eg::loop-keyword "RETURN")
         (loop-form-or-it context)))

(defrule (loop-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (or (loop-list-accumulation-clause context)
        (loop-numeric-accumulation-clause context)))

(defrule (loop-list-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (eg::loop-keyword "COLLECT" "COLLECTING" "APPEND" "APPENDING" ; TODO
                           )
         (<- value (loop-form-or-it context))
         (? (seq (eg::loop-keyword "INTO")
                 (<- into ((variable-name! names))))))
  (list :list-accum :value value :into into))

(defrule (loop-numeric-accumulation-clause :environment (make-instance 'eg::expression-environment)) (context)
    (seq (eg::loop-keyword "COUNT" "COUNTING" "SUM" "SUMMING" ; TODO
                           )
         (<- value (loop-form-or-it context))
         (? (seq (eg::loop-keyword "INTO")
                 (<- into ((variable-name! names))))))
  (list :numeric-accum :value value :into into))

(defrule (loop-conditional-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "IF" "WHEN" "UNLESS")
         (<- test ((form! forms)))
         (must (loop-selectable-clause) "must be a selectable clause")))

;;; Selectable clauses

(defrule (loop-selectable-clause :environment (make-instance 'eg::expression-environment)) ()
    (or (loop-unconditional-clause :conditional)
        (loop-accumulation-clause :conditional)
        (loop-conditional-clause)))

(defrule (loop-termination-test-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "WHILE" "UNTIL" "REPEAT" "ALWAYS" "NEVER" "THEREIS")
         (<- form ((form! forms))))
  (list :while form))

(defrule (loop-initial-final-clause :environment (make-instance 'eg::expression-environment)) ()
    (seq (eg::loop-keyword "INITIALLY" "FINALLY")
         (+ (<<- forms ((compound-form! forms)))))
  (list :initially forms))

;;; Variables and types

(defrule loop-form-or-it (context)
    (or (and (eg::loop-keyword "IT")
             (:transform :any
               (unless (eq context :conditional)
                 (:fatal "IT is only allowed in a conditional context"))))
        ((form! forms))))

(define-loop-clause type-spec ()
    (or (seq (eg::loop-keyword "OF-TYPE")
             ((type-specifier! type-specifiers))) ; TODO destructuring-type-spec
        (and :any (must (or 'fixnum 'float 't 'nil) "must be FIXNUM, FLOAT, T or NIL"))))
