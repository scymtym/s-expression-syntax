;;;; special-operators.lisp --- Standard special operators supported by the syntax system.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
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

;;; Special operators `block', `return-from', `return', `tagbody' and `go'

(defrule block-name ()
    (guard name (typep 'symbol))
  name)

(defrule block-name! ()
    (must (block-name) "block name must be a symbol"))

(define-special-operator block
    (list* (<- name (block-name!)) (<- forms ((forms forms))))
  ((name  1  :evaluation (make-instance 'binding-semantics
                                        :namespace 'block
                                        :scope     :lexical
                                        :values    nil))
   (forms *> :evaluation t)))

(define-special-operator return-from
    (list (<- name (block-name!)) (? (<- value ((form! forms)))))
  ((name  1 :evaluation (make-instance 'reference-semantics
                                       :namespace 'block))
   (value ? :evaluation t)) ; TODO value-form? result?
  )

(define-special-operator return
    (list (? (<- value ((form! forms)))))
  ((value ? :evaluation t))) ; TODO result

;;; Note: we don't have `tag!' or `new-tag!' anything that is not a
;;; valid tag will be treated as a form.
(defrule tag ()
  (guard name (typep '(or symbol integer))))

(defrule new-tag (seen)
    (<- name (tag))
  (if (position name seen :test #'eq)
      (:fail)
      name)
  #+no (cond ((not seen)
         name)
        ((not (gethash name seen))
         (setf (gethash name seen) t)
         name)
        (t
         nil)))

(defrule new-tag! (seen)
  (must (new-tag seen) "must be a unique tag name"))

;;; TODO make a rule for parsing segments
(define-special-operator tagbody
    (list (* (seq (* (<<- forms (and (not (tag)) ((form! forms)))))
                  (* (<<- tags 'foo)) ; HACK to bind tags
                  (or (<<- segments
                           (:transform
                              (<<- tags (new-tag! tags))
                            (prog1
                                (nreverse forms)
                              (print forms *trace-output*)
                              (setf forms '()))))
                      (:transform
                         (seq)
                       (unless forms (:fail))
                       (push (nreverse forms) segments)
                       (setf forms '()))))))
  ((tags     * :evaluation nil) #+later (make-instance 'binding-semantics
                                               :namespace 'tag
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    nil)
   (segments * :evaluation t)))

(define-special-operator go
    (list (<- tag (tag)))
  ((tag 1 :evaluation nil)))

;;; Special operators `eval-when', `load-time-value', `quote' and `function'

(a:define-constant +eval-when-situations+
    '(:compile-toplevel compile
      :load-toplevel    load
      :execute          eval)
  :test #'equal)

(defrule eval-when-situation ()
  (or . #.(mapcar (lambda (situation)
                    `',situation)
                  +eval-when-situations+)))

(defrule eval-when-situation! ()
  (must (eval-when-situation) #.(format nil "must be one of ~{~S~^, ~}"
                                        +eval-when-situations+)))

(define-special-operator eval-when
    (list (must (list (* (<<- situations (eval-when-situation!))))
                "must be a list of situations")
          (* (<<- forms ((form! forms)))))
  ((situations * :evaluation nil)
   (forms      * :evaluation t)))

(define-special-operator load-time-value
    (list (<- form ((form! forms))) (? (guard read-only-p (typep 'boolean))))
  ((form        1 :evaluation t)
   (read-only-p ? :evaluation nil)))

(define-special-operator quote
    (list material)
  ((material 1 :evaluation nil)))

(define-special-operator (lambda-expression :operator lambda)
    (list* (<- lambda-list ((ordinary-lambda-list! lambda-lists)))
           (<- (documentation declaration form) ((docstring-body forms))))
  ((lambda-list   1  :evaluation :compound) ; TODO binding
   (documentation ?  :evaluation nil)
   (declaration   *> :evaluation nil)
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
    (list* (<- (names expansions) (symbol-macro-bindings))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace :symbol-macro
                                               :scope     :lexical
                                               :values    'values))
   (expansions   *> :evaluation nil)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator let ; TODO macro for this and let* and maybe symbol-macrolet
    (list* (<- (names values) (value-bindings!))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'variable
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    'values))
   (values       *> :evaluation t)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator let*
    (list* (<- (names values) (value-bindings!))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'variable
                                               :scope     :lexical
                                               :order     :sequential
                                               :values    'values))
   (values       *> :evaluation t)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator locally
    (list* (<- (declarations forms) ((body forms))))
  ((declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator progv
    (list* (<- symbols ((form! forms))) (<- values ((form! forms)))
           (<- (declarations forms) ((body forms))))
  ((symbols      1  :evaluation (make-instance 'binding-semantics
                                               :namespace 'variable
                                               :scope     :dynamic
                                               :values    'values))
   (values       1  :evaluation t)
   (declarations 1  :evaluation nil)
   (forms        *> :evaluation t)))

;;; Special operators `macrolet', `flet' and `labels'
;;;
;;; Lexically scope bindings of function-ish things.

(define-special-operator macrolet
    (list* (<- (names functions) (macro-function-bindings))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :values    'functions))
   (functions    *> :evaluation :compound)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator flet
    (list* (<- (names functions) (function-bindings))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    'functions))
   (functions    *> :evaluation :compound)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator labels
    (list* (<- (names functions) (function-bindings))
           (<- (declarations forms) ((body forms))))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :order     :recursive
                                               :values    'functions))
   (functions    *> :evaluation :compound)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

;;; Special operators `declaim' and `the'

(define-special-operator declaim
    (list (* (<<- declarations (declaration!)))) ; TODO only free declarations
  ((declarations *> :evaluation nil)))

(define-special-operator the
    (list (<- type ((type-specifier! type-specifiers)))
          (<- form ((form! forms))))
  ((type 1 :evaluation nil)
   (form 1 :evaluation t)))

;;; Special operator `setq'

(define-special-operator setq
    (list (* (and :any
                  (must (seq (<<- names       (variable-name))
                             (<<- value-forms ((form! forms))))
                        "must be a variable name followed by an expression"))))
  ((names       * :evaluation nil       ; :type symbol :access :write
                )
   (value-forms * :evaluation t)))

;;; Special operators `throw', `catch' and `unwind-protect'

(define-special-operator throw
    (list (<- tag-form ((form! forms))) (<- result-form ((form! forms))))
  ((tag-form    1 :evaluation t)
   (result-form 1 :evaluation t)))

(define-special-operator catch
    (list* (<- tag-form ((form! forms))) (<- forms ((forms forms))))
  ((tag-form 1  :evaluation t)
   (forms    *> :evaluation t)))

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
    (list* (must (list (* (<<- names ((variable-name! names)))))
                 "must be a list of variable names") ; TODO unique variable name
           (<- values-form ((form! forms)))
           (<- (declarations forms) ((body forms))))
  ((names        *) ; TODO binding semantics
   (values-form  1  :evaluation t)
   (declarations *>)
   (forms        *> :evaluation t)))

(define-special-operator multiple-value-call
    (list* (<- function-form ((form! forms))) (<- arguments ((forms forms))))
  ((function-form 1  :evaluation t)
   (arguments     *> :evaluation t)))

(define-special-operator multiple-value-prog1
    (list* (<- values-form ((form! forms))) (<- forms ((forms forms))))
  ((values-form 1  :evaluation t)
   (forms       *> :evaluation t)))
