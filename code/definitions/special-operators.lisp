;;;; special-operators.lisp --- Standard special operators supported by the syntax system.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Special operators for control

(define-special-operator progn
    (list* (<- form ((forms forms))))
  ((form *> :evaluation t)))

(define-special-operator if
    (list (<- test ((form! forms)))
          (<- then ((form! forms)))
          (? (<- else ((form! forms)))))
  ((test 1 :evaluation t)
   (then 1 :evaluation t)
   (else ? :evaluation t)))

;;; Special operators `block', `return-from', `return'

(define-special-operator block
    (list* (<- name ((block-name! names))) (<- form ((forms forms))))
  ((name 1  :evaluation (make-instance 'binding-semantics
                                       :namespace 'block
                                       :scope     :lexical
                                       :values    nil))
   (form *> :evaluation t)))

(define-special-operator return-from
    (list (<- name ((block-name! names))) (? (<- result ((form! forms)))))
  ((name   1 :evaluation (make-instance 'reference-semantics
                                        :namespace 'block))
   (result ? :evaluation t)))

(define-special-operator return
    (list (? (<- result ((form! forms)))))
  ((result ? :evaluation t)))

;;; Special operators `tagbody' and `go'

(defrule (tagbody-segment :environment (make-instance 'eg::expression-environment)) (seen first?)
    (value (source)
      (seq (<- label (or (and (:transform :any (unless first? (:fail)))
                              (or (and ((integer-or-symbol names)) ((unique-tag! names) seen))
                                  (seq)))
                         ((unique-tag! names) seen)))
           (* (<<- statements (and (not ((integer-or-symbol names)))
                                   ((form! forms)))))))
  (bp:node* (:tagbody-segment :source source)
    (bp:? (:label     . 1) label)
    (*    (:statement . *) (nreverse statements) :evaluation (a:circular-list t))))

(define-special-operator tagbody
    (list (? (and (<- seen (:transform :any (make-hash-table)))
                  (<<- segment (tagbody-segment seen 't))))
          (* (<<- segment (tagbody-segment seen 'nil))))
  ((segment * :evaluation :compound)))

(define-special-operator go
    (list* (must (list (<- tag ((tag! names)))) "must be a single tag"))
  ((tag 1 :evaluation (make-instance 'reference-semantics
                                     :namespace 'tag))))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(a:define-constant +eval-when-situations+
    '(:compile-toplevel compile
      :load-toplevel    load
      :execute          eval)
  :test #'equal)

(defrule eval-when-situation ()
    (value (source)
      (<- situation (or . #.(mapcar (lambda (situation)
                                      `',situation)
                                    +eval-when-situations+))))
  (bp:node* (:eval-when-situation :situation situation :source source)))

(defrule eval-when-situation! ()
  (must (eval-when-situation) #.(format nil "must be one of ~{~S~^, ~}"
                                        +eval-when-situations+)))

(define-special-operator eval-when
    (list (must (list (* (<<- situation (eval-when-situation!))))
                "must be a list of situations")
          (* (<<- form ((form! forms)))))
  ((situation * :evaluation nil)
   (form      * :evaluation t)))

(defrule read-only-p ()
    (value (source)
      (guard value (typep 'boolean)))
  (bp:node* (:unparsed :expression value :source source)))

(defrule read-only-p! ()
  (must (<- read-only-p (read-only-p))
        "READ-ONLY-P must be either T or NIL, not a generalized boolean"))

(define-special-operator load-time-value
    (list (<- form ((form! forms))) (? (<- read-only-p (read-only-p!))))
  ((form        1 :evaluation t)
   (read-only-p ? :evaluation nil)))

(define-special-operator quote
    (list (<- material ((unparsed-expression forms) ':quote)))
  ((material 1 :evaluation nil)))

(define-special-operator (lambda-expression :operator lambda)
    (list* (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((lambda-list   1  :evaluation :compound) ; TODO binding
   (documentation ?)
   (declaration   *>)
   (form          *> :evaluation t)))

(define-special-operator function
    (list* (must (list (or (<- name ((function-name names)))
                           (must (<- lambda (lambda-expression))
                                 "must be a function name or lambda expression")))
                 "nothing may follow function name or lambda expression"))
  ((name   ? :evaluation nil)
   (lambda ? :evaluation :compound)))

;;; This handles the `lambda' macro by accepting a `lambda-expression'
;;; and wrapping it in an AST node of kind `:lambda'.
(define-syntax (lambda)
    (<- lambda (lambda-expression))
  ((lambda 1 :evaluation :compound)))

;;; Special operators `symbol-macrolet', `let[*]', `locally' and `progv'
;;;
;;; Lexically scoped bindings of values declarations.

(define-special-operator symbol-macrolet
    (list* (<- binding (symbol-macro-bindings!))
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator let ; TODO macro for this and let* and maybe symbol-macrolet
    (list* (<- binding (value-bindings!))
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound) ; :order     :parallel
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator let*
    (list* (<- binding (value-bindings!)) ; :order     :sequential
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator locally
    (list* (<- (declaration form) ((body forms))))
  ((declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator progv
    (list* (<- symbols ((form! forms))) (<- values ((form! forms)))
           (<- (declaration form) ((body forms))))
  ((symbols     1  :evaluation t)
   (values      1  :evaluation t)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

;;; Special operators `macrolet', `flet' and `labels'
;;;
;;; Lexically scope bindings of function-ish things.

(define-special-operator macrolet
    (list* (<- binding (macro-function-bindings!))
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator flet
    (list* (<- binding (function-bindings!))
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

(define-special-operator labels
    (list* (<- binding (function-bindings!))
           (<- (declaration form) ((body forms))))
  ((binding     *> :evaluation :compound)
   (declaration *> :evaluation nil)
   (form        *> :evaluation t)))

;;; Special operators `declaim' and `the'

(define-special-operator declaim
    (list (* (<<- declaration (declaration!)))) ; TODO only free declarations
  ((declaration *> :evaluation nil)))

(define-special-operator the
    (list (<- type ((type-specifier! type-specifiers)))
          (<- form ((form! forms))))
  ((type 1 :evaluation nil)
   (form 1 :evaluation t)))

;;; Special operators `[p]setq'

(define-special-operator setq
    (list (* (and :any
                  (must (seq (<<- name       (variable-name!))
                             (<<- value-form ((form! forms))))
                        "must be a variable name followed by a form"))))
  ((name       * :evaluation (make-instance 'assignment-semantics
                                            :namespace 'variable))
   (value-form * :evaluation t)))

(defrule unique-assignment-pairs ()
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (list (* (and :any
                       (must (seq (<<- names       ((unique-variable-name! lambda-lists)
                                                    seen))
                                  (<<- value-forms ((form! forms))))
                             "must be a variable name followed by a form")))))
  (list names value-forms))

(define-special-operator psetq
    (list* (<- (name value-form) (unique-assignment-pairs)))
  ((name       * :evaluation (make-instance 'assignment-semantics
                                            :namespace 'variable))
   (value-form * :evaluation t)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-special-operator throw
    (list (<- tag-form ((form! forms))) (<- result-form ((form! forms))))
  ((tag-form    1 :evaluation t)
   (result-form 1 :evaluation t)))

(define-special-operator catch
    (list* (<- tag-form ((form! forms))) (<- form ((forms forms))))
  ((tag-form 1  :evaluation t)
   (form     *> :evaluation t)))

(define-special-operator unwind-protect
    (list* (<- protected ((form! forms))) (<- cleanup ((forms forms))))
  ((protected 1  :evaluation t)
   (cleanup   *> :evaluation t)))

;;; `destructuring-bind'

(define-special-operator destructuring-bind
    (list* (<- lambda-list        ((destructuring-lambda-list! destructuring-lambda-list)))
           (<- expression         ((form! forms)))
           (<- (declaration form) ((body forms))))
  ((lambda-list 1  :evaluation :compound)
   (expression  1  :evaluation t)
   (declaration *>)
   (form        *> :evaluation t)))

;;; Special operators for multiple values

(define-special-operator multiple-value-bind
    (list* (must (list (* (<<- name ((variable-name! names)))))
                 "must be a list of variable names") ; TODO unique variable name
           (must (<- values-form ((form! forms)))
                 "a value form must follow the list of variable names")
           (<- (declaration form) ((body forms))))
  ((name        *  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :order     :parallel
                                              :values    'values))
   (values-form 1  :evaluation t)
   (declaration *>)
   (form        *> :evaluation t)))

(define-special-operator multiple-value-call
    (list* (<- function-form ((form! forms))) (<- argument ((forms forms))))
  ((function-form 1  :evaluation t)
   (argument      *> :evaluation t)))

(define-special-operator multiple-value-prog1
    (list* (<- values-form ((form! forms))) (<- form ((forms forms))))
  ((values-form 1  :evaluation t)
   (form        *> :evaluation t)))
