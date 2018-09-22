;;;; special-operators.lisp --- Standard special operators supported by the syntax system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

;;;; Special operators for control

(define-special-operator progn
    (list* forms)
  ((forms * :evaluation t))
  (:documentation
   "PROGN form*

    Evaluates each FORM in order, returning the values of the last
    form. With no forms, returns NIL."))

(define-special-operator if
    (list test then (? else))
  ((test 1 :evaluation t)
   (then 1 :evaluation t)
   (else ? :evaluation t))
  (:documentation
   "IF test then [else]

If TEST evaluates to true, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to
NIL."))

;;;; `block', `return-from' and `tagbody'

(parser:defrule block-name ()
    (or (:guard name symbolp)
        (:transform thing (error "~@<Block name must be a symbol, not ~S~@:>"
                                 thing)))
  name)

(define-special-operator block
    (list* (<- name (block-name)) (<- forms (forms)))
  ((name  1 :evaluation (make-instance 'binding-semantics
                                       :namespace 'block
                                       :scope     :lexical
                                       :values    nil))
   (forms * :evaluation t))
  (:documentation
   "BLOCK name form*

    Evaluate the FORMS as a PROGN. Within the lexical scope of the
    body, RETURN-FROM can be used to exit the form."))

(define-special-operator return-from
    (list (<- name (block-name)) (? value))
  ((name  1 :evaluation (make-instance 'reference-semantics
                                       :namespace 'block))
   (value 1 #+later ? :evaluation t))
  (:documentation
   "RETURN-FROM block-name value-form

    Evaluate the VALUE-FORM, returning its values from the lexically
    enclosing block BLOCK-NAME. This is constrained to be used only
    within the dynamic extent of the block."))

(parser:defrule tag ()
    (:guard name (typep '(or symbol integer))))

(parser:defrule new-tag (seen)
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

;; TODO for parser.packrat tests
#+test (parser:defrule test ()
    (list (:seq (* (<<- tags 'foo)) (new-tag tags))))

#+test (parser:parse '(test) '(foo foo foz))

(define-special-operator tagbody
    (list (* (:seq (* (<<- forms (and (not (tag)) :any)))
                   (* (<<- tags 'foo)) ; HACK to bind tags
                   (or (<<- segments
                             (:transform
                              (<<- tags (new-tag tags))
                              (prog1
                                  forms
                                (setf forms '()))))
                       (:transform
                        (:seq)
                        (unless forms (:fail))
                        (push forms segments)
                        (setf forms '()))))))
  ((tags     * :evaluation nil)
   (segments * :evaluation t))
  (:documentation
   "TAGBODY {tag | statement}*

    Define tags for use with GO. The STATEMENTS are evaluated in
    order, skipping TAGS, and NIL is returned. If a statement contains
    a GO to a defined TAG within the lexical scope of the form, then
    control is transferred to the next statement following that tag. A
    TAG must be an integer or a symbol. A STATEMENT must be a
    list. Other objects are illegal within the body.")
  #+no (:parser
        ;; Collect tags and segments in an alternating manner. We must
        ;; allow empty segments to not get confused by adjacent tags.
        (collect ((tags) (segments))
          (let ((current body))
            (loop
              (let ((next-segment (member-if #'atom current)))
                (unless next-segment
                  (segments `(progn ,@current))
                  (return))
                (let ((tag (car next-segment)))
                  (when (member tag (tags))
                    (compiler-error
                     "~@<The tag ~S appears more than once in a ~S.~@:>"
                     tag 'tagbody))
                  (tags tag)
                  (segments `(progn ,@(ldiff current next-segment))))
                (setf current (rest next-segment)))))
          (component :tags     (tags))
          (component :segments (segments))))
  #+no (:unparser
        ;; Temporarily prepend a NIL tag for ease of implementation.
        (rest (mapcan (lambda (tag segment)
                        `(,tag ,@(if (typep segment '(cons (eql progn)))
                                     (rest segment)
                                     (list segment))))
                      (list* nil (component :tags))
                      (component :segments)))))

(define-special-operator go
    (list (<- tag (tag)))
  ((tag 1 :evaluation nil))
  (:documentation
   "GO tag

    Transfer control to the named TAG in the lexically enclosing
    TAGBODY. This is constrained to be used only within the dynamic
    extent of the TAGBODY."))

;;;; `eval-when', `load-time-value', `quote' and `function'

(define-constant +eval-when-situations+
    '(:compile-toplevel compile
      :load-toplevel    load
      :execute          eval)
  :test #'equal)

(deftype eval-when-situation ()
  `(member ,@+eval-when-situations+))

;;; Parse an EVAL-WHEN situations list, returning three flags, (VALUES
;;; COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE), indicating the types of
;;; situations present in the list.
#+no-used (defun parse-eval-when-situations (situations)
  (awhen (intersection situations '(compile load eval))
    (style-warn "~@<Using deprecated ~S situation name~P ~{~S~^, ~}.~@:>"
                'eval-when (length it) it))
  (values (intersection '(:compile-toplevel compile) situations)
          (intersection '(:load-toplevel load) situations)
          (intersection '(:execute eval) situations)))

(define-special-operator eval-when
    (list* (list (* (<<- situations (:guard (typep 'eval-when-situation)))))
           forms)
  ((situations 1 :evaluation nil)
   (forms      * :evaluation t))
  (:documentation
   "EVAL-WHEN (situation*) form*

    Evaluate the FORMS in the specified SITUATIONS (any of
    :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL, or :EXECUTE, or (deprecated)
    COMPILE, LOAD, or EVAL)."))

(define-special-operator load-time-value
    (list form (? (:guard read-only-p (typep 'boolean))))
  ((form        1 :evaluation t)
   (read-only-p ? :evaluation nil))
  (:documentation
   "Arrange for FORM to be evaluated at load-time and use the value
    produced as if it were a constant.

    If READ-ONLY-P is non-NIL, then the resultant object is guaranteed
    to never be modified, so it can be put in read-only storage."))

(define-special-operator quote ; TODO doesn't work because bootstrap-parse interprets quote
    (list material)
  ((material 1 :evaluation nil))
  (:documentation
   "Return MATERIAL without evaluating it."))

;; There are two cases for this special operator:
;; 1) (function NAME)
;; 2) (function (lambda LAMBDA-LIST [DECLARATIONS] [DOCUMENTATION] BODY))
;; which produce disjoint sets of components, namely:
;; 1) :name NAME
;; 2) :lambda-list LAMBDA-LIST :declarations DECLARATIONS ...
;; In 2), the :declarations and :documentation components are omitted
;; if they are not present in the form being processed.
#+not-now (define-special-operator function (thing)
  (;; Components for (function NAME) case.
   (:name          ? :evaluated nil)
   ;; Components for (function (lambda (...) ...)) case.
   (:lambda-list   ? :evaluated nil)
   (:declarations  t :evaluated nil)
   (:documentation ? :evaluated nil)
   (:body          t))
  #!+sb-doc
  (:documentation
   "FUNCTION name
Return the lexically apparent definition of the function NAME. NAME
may also be a lambda expression.")
  (:parser
   (flet ((process-lambda-like (lambda-list body)
            ;; Components in reverse evaluation-order.
            (multiple-value-bind (body declarations documentation)
                (parse-body body nil)
              (component :body body)
              (when documentation
                (component :documentation documentation))
              (component :declarations declarations))
            (component :lambda-list lambda-list)))
     (cond
       ((legal-fun-name-p thing)
        (component :name thing))
       ((typep thing '(cons (eql lambda) (cons list)))
        (process-lambda-like (second thing) (nthcdr 2 thing)))
       ((typep thing '(cons (eql sb!int:named-lambda) (cons list)))
        (process-lambda-like (third thing) (nthcdr 3 thing)))
       (t
        (compiler-error "~@<Invalid argument to ~S special operator: ~
                       ~S.~@:>"
                        'function thing)))))
  (:unparser
   (list (cond
           ((componentp :name)
            (component :name))
           ((componentp :lambda-list)
            `(lambda ,(component :lambda-list)
               ,@(component :declarations)
               ,@(when (componentp :documentation)
                   `(,(component :documentation)))
               ,@(component :body)))
           (t
            (error "~@<Exactly one of ~S and ~S must be supplied.~@:>"
                   :name :lambda-list))))))

(define-special-operator function
    (list (or (<- name (function-name))
              (list* 'lambda (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
                     (:compose (docstring-body) (list docstring declarations body)))))
  ((name         1 :evaluation nil)
   (lambda-list  1 :evaluation nil)     ; TODO binding
   (docstring    1 :evaluation nil)
   (declarations * :evaluation nil)
   (body         * :evaluation nil)))

;;;; SYMBOL-MACROLET, LET[*], LOCALLY and PROGV

#+no (defun parse-symbol-macrolet-binding (context spec)
  (unless (proper-list-of-length-p spec 2)
    (compiler-error "~@<Malformed symbol-expansion pair in ~S: ~S~@:>"
                    context spec)) ; TODO should be compiler-error or simple-program-error depending on the context
  (values (check-variable-name (first spec) "local symbol-macro name")
          (second spec)))

#+no (defun parse-let-binding (context spec)
  (multiple-value-bind (name value suppliedp)
      (cond
        ((atom spec)
         spec)
        ((not (proper-list-of-length-p spec 1 2))
         (compiler-error "~@<The ~S binding spec ~S is malformed.~@:>"
                         context spec))
        (t
         (values (first spec) (second spec) t)))
    (values (check-variable-name name "local variable") value suppliedp)))

#+no (defun unparse-let-binding (name value suppliedp)
  (if suppliedp
      (list name value)
      name))

(define-special-operator symbol-macrolet
    (list* (:compose (symbol-macro-bindings) (list names values))
           (:compose (body) (list declarations forms)))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace :symbol-macro
                                               :scope     :lexical
                                               :values    'values))
   (values       *> :evaluation nil)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t))
  (:documentation
   "SYMBOL-MACROLET ({(name expansion)}*) decl* form*

    Define the NAMES as symbol macros with the given
    EXPANSIONS. Within the body, references to a NAME will effectively
    be replaced with the EXPANSION."))

(define-special-operator let ; TODO macro for this and let* and maybe symbol-macrolet
    (list* (:compose (value-bindings) (list names values))
           (:compose (body) (list declarations forms)))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'variable
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    'values))
   (values       *> :evaluation t)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t))
  (:documentation
   "LET ({(var [value]) | var}*) declaration* form*

    During evaluation of the FORMS, bind the VARS to the result of
    evaluating the VALUE forms. The variables are bound in parallel
    after all of the VALUES forms have been evaluated."))


(define-special-operator let*
    (list* (:compose (value-bindings) (list names values))
           (:compose (body) (list declarations forms)))
  ((names        *> :evaluation '(:binding :namespace :variable
                                  :scope     :lexical
                                  :order     :sequential
                                  :values    values))
   (values       *> :evaluation t)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t))
  (:documentation
   "LET* ({(var [value]) | var}*) declaration* form*

    Similar to LET, but the variables are bound sequentially, allowing
    each VALUE form to reference any of the previous VARS."))

(define-special-operator locally
    (list* (:compose (body) (list declarations forms)))
  ((declarations *> :evaluation t)
   (forms        *> :evaluation t))
  (:documentation
   "LOCALLY declaration* form*

    Sequentially evaluate the FORMS in a lexical environment where the
    DECLARATIONS have effect. If LOCALLY is a top level form, then the
    FORMS are also processed as top level forms."))

(define-special-operator progv
    (list* symbols values (:compose (body) (list declarations forms)))
  ((symbols      1 :evaluation (make-instance 'binding-semantics
                                              :namespace :variable
                                              :scope     :dynamic))
   (values       1 :evaluation t)
   (declarations 1 :evaluation nil)
   (forms        t :evaluation t))
  (:documentation
   "PROGV symbols values form*

    Evaluate SYMBOLS producing a list of symbols and VALUES producing
    a list of values, then dynamically bind the symbols to the values
    while evaluating FORMS. Excess values produced by VALUES are
    discarded, excess symbols produced SYMBOLS are made unbound. All
    bindings \(including making variables unbound) are undone on exit
    from the PROGV form."))

;;;; `macrolet', `flet' and `labels'

(define-special-operator parsed-lambda
    (list lambda-list declarations documentation-string body)
  ((lambda-list          1 :evaluation nil)
   (declarations         1 :evaluation nil)
   (documentation-string 1 :evaluation nil)
   (body                 * :evaluation t)))

(define-special-operator macrolet
    (list* (:compose (function-bindings) (list names functions))
           (:compose (body) (list declarations forms)))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace :function
                                               :scope     :lexical
                                               :values    'functions))
   (functions    *> :evaluation nil)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator flet
    (list* (:compose (function-bindings) (list names functions))
           (:compose (body) (list declarations forms)))
  ((names        *> :evaluation (make-instance 'binding-semantics
                                               :namespace 'function
                                               :scope     :lexical
                                               :order     :parallel
                                               :values    'functions))
   (functions    *> :evaluation nil)
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

(define-special-operator labels
    (list* (:compose (function-bindings) (list names functions))
           (:compose (body) (list declarations forms)))
  ((functions    *> :evaluation '(:binding :namespace :function
                                  :scope     :lexical
                                  :order     :recursive))
   (declarations *> :evaluation nil)
   (forms        *> :evaluation t)))

;;;; `the'

(define-special-operator the
    (list (<- type (type-specifier)) form)
  ((type 1 :evaluation nil)
   (form 1 :evaluation t)))

;;; SETQ

(define-special-operator setq
    (list (* (:seq (<<- names       (:guard symbolp)) ; TODO (variable-name)
                   (<<- value-forms))))
  ((names       * :evaluation nil       ; :type symbol :access :write
                )
   (value-forms *))
  (:documentation
   "SETQ {var form}*

    Assign the value of each FORM to the variable name by the
    preceding VAR.")
  #+no (:parser
   (multiple-value-bind (names value-forms)
       (parse-setq-contents form names-and-value-forms)
     (component :names       names)
     (component :value-forms value-forms)))
  #+no (:unparser
   (nconc (mapcan #'list (component :names) (component :value-forms)))))

;;;; `throw', `catch' and `unwind-protect'

(define-special-operator throw
    (list tag-form result-form)
  ((tag-form    1 :evaluation t)
   (result-form 1 :evaluation t))
  (:documentation
   "THROW tag result-form

    Do a non-local exit, return the values of RESULT-FORM from the
    CATCH whose tag is EQ to TAG."))

(define-special-operator catch
    (list* tag-form (<- forms (forms)))
  ((tag-form  1 :evaluation (make-instance 'binding-semantics
                                           :namespace 'tag
                                           :scope     'lexical
                                           :values    nil))
   (forms     * :evaluation t))
  (:documentation
   "CATCH tag form*

    Evaluate TAG and instantiate it as a catcher while the body forms
    are evaluated in an implicit PROGN. If a THROW is done to TAG
    within the dynamic scope of the body, then control will be
    transferred to the end of the body and the thrown values will be
    returned."))

(define-special-operator unwind-protect
    (list* protected (<- cleanup (forms)))
  ((protected 1  :evaluation t)
   (cleanup   *> :evaluation t))
  (:documentation
   "UNWIND-PROTECT protected cleanup*

    Evaluate the form PROTECTED, returning its values. The CLEANUP
    forms are evaluated whenever the dynamic scope of the PROTECTED
    form is exited (either due to normal completion or a non-local
    exit such as THROW)."))

;;;; Multiple value stuff

(define-special-operator multiple-value-call
    (list* function-form (<- args (forms)))
  ((function-form 1  :evaluation t)
   (args          *> :evaluation t)) ; TODO arguments? make this consistent
  (:documentation
   "MULTIPLE-VALUE-CALL function-form values-form*

    Call FUNCTION-FORM, passing all the values of each VALUES-FORM as
    arguments, values from the first VALUES-FORM making up the first
    argument, etc."))

(define-special-operator multiple-value-prog1
    (list* values-form (<- forms (forms)))
  ((values-form 1  :evaluation t)
   (forms       *> :evaluation t))
  (:documentation
   "MULTIPLE-VALUE-PROG1 values-form form*

    Evaluate VALUES-FORM and then the FORMS, but return all the values
    of VALUES-FORM."))

;;;

#+no (parser:defrule lambda ()
  (list* 'lambda '() (<- body (docstring-body)))
  (bp:node* ('lambda :documentation-string (first body))
    (* :declaration (second body))
    (* :form        (third body))))

(parser:defrule application ()
    (list (<- abstraction (or (reference) (lambda)))
          (* (<<- arguments)))
  (bp:node* (:application)
    (1 :abstraction abstraction)
    (* :argument    arguments)))
