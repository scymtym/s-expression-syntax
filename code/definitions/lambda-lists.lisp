;;;; lambda-lists.lisp --- Rules for parsing different kinds of lambda lists.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar lambda-lists
  (:class eg::expression-grammar)
  (:use names
        forms))

(parser:in-grammar lambda-lists)

;;; Shared rules

;; TODO could use lambda-list-keywords
(defrule not-lambda-list-keyword ()
  (not (or '&whole '&environment
           '&optional '&rest '&body '&key '&aux '&allow-other-keys)))

(defrule lambda-list-variable-name ()
  (and (not-lambda-list-keyword)
       ((variable-name names))))

(defrule lambda-list-variable-name! ()
  (must (and (must (not-lambda-list-keyword)
                   "must not be a lambda list keyword")
             ((variable-name! names)))
        "must be a lambda list variable name"))

(defrule unique-name (seen)
    name
  (let ((key (getf (bp:node-initargs* name) :name)))
    (cond ((not seen)
           name)
          ((not (gethash key seen))
           (setf (gethash key seen) t)
           name)
          (t
           (:fail) ; (:fatal (format nil "the variable name ~S occurs more than once" key))
           ))))

(defrule unique-variable-name (seen)
  (and (lambda-list-variable-name)
       (:compose ((variable-name/unchecked names)) (unique-name seen))))

(defrule unique-variable-name! (seen)
  (and (<- name (lambda-list-variable-name!))
       (or (:compose (:transform :any name) (unique-name seen))
           (:transform :any
             (let ((key (getf (bp:node-initargs* name) :name)))
               (:fatal (format nil "the variable name ~S occurs more than once" key)))))))

(define-syntax (required-parameter :arguments ((seen nil)))
    (<- name (unique-variable-name seen))
  ((name 1)))

(defrule required-parameter! (seen) ; TODO this should use required-parameter
    (value (source)
      (and (not-lambda-list-keyword)
           (<- name (unique-variable-name! seen))))
  (bp:node* (:required-parameter :source source)
    (1 (:name . 1) name)))

(define-syntax (optional-parameter :arguments ((seen nil)))
    (or (list (<- name (unique-variable-name! seen))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not-lambda-list-keyword)
                      (unique-variable-name! seen))))
  ((name     1)
   (default  ? :evaluation t)
   (supplied ?)))

(define-syntax (rest-parameter :arguments ((seen nil)))
    (<- name (unique-variable-name! seen))
  ((name 1)))

(define-syntax (keyword-parameter :arguments ((seen nil)))
    (or (list (or (list (<- keyword ((parameter-keyword! names)))
                        (<- name (unique-variable-name! seen)))
                  (<- name (unique-variable-name! seen)))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not-lambda-list-keyword)
                      (unique-variable-name! seen))))
  ((name     1)
   (keyword  ?)
   (default  ? :evaluation t)
   (supplied ?)))

(define-syntax (aux-parameter :arguments ((seen nil)))
    (or (list (<- name (and (not-lambda-list-keyword)
                            (unique-variable-name! seen)))
              (? (<- value ((form! forms)))))
        (<- name (and (not-lambda-list-keyword)
                      (unique-variable-name! seen))))
  ((name  1)
   (value ? :evaluation t)))

;;; Reusable sections of lambda lists

(defrule the-lambda-list-keyword (which)
    (and which keyword)
  (bp:node* (:lambda-list-keyword :keyword (eg::%naturalize keyword)
                                  :source  keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (required-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (+ (<<- parameters (required-parameter! seen))))
    source
    (bp:node* (:required-section ; :source source
               )
      (* (:parameter . *) (nreverse parameters))))

  (defrule (optional-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq    (<-  keyword    (the-lambda-list-keyword '&optional))
             (* (<<- parameters (optional-parameter seen)))))
    source
    (bp:node* (:optional-section ; :source source
               )
      (1 (:keyword   . 1) keyword)
      (* (:parameter . *) (nreverse parameters) :evaluation :compound)))

  (defrule (rest-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq (<- keyword   (the-lambda-list-keyword '&rest))
             (<- parameter (rest-parameter seen))))
    source
    (bp:node* (:rest-section ; :source source
               )
      (1 (:keyword   . 1) keyword)
      (1 (:parameter . 1) parameter)))

  (defrule (keyword-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq    (<-  keyword            (the-lambda-list-keyword '&key))
             (* (<<- keyword-parameters (keyword-parameter seen)))
             (? (<-  allow-other-keys   (the-lambda-list-keyword '&allow-other-keys)))))
    source
    (bp:node* (:keyword-section ; :source source
               )
      (1    (:keyword          . 1) keyword)
      (*    (:parameter        . *) (nreverse keyword-parameters) :evaluation :compound)
      (bp:? (:allow-other-keys . 1) allow-other-keys)))

  (defrule (aux-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq    (<-  keyword    (the-lambda-list-keyword '&aux))
             (* (<<- parameters (aux-parameter seen)))))
    source
    (bp:node* (:aux-section ; :source source
               )
      (1 (:keyword   . 1) keyword)
      (* (:parameter . *) (nreverse parameters) :evaluation :compound))))

;;; 3.4.1 Ordinary Lambda Lists

(defrule %ordinary-lambda-list (seen)
    (list (? (<- required (required-section seen)))
          (? (<- optional (optional-section seen)))
          (? (<- rest     (rest-section seen)))
          (? (<- keyword  (keyword-section seen)))
          (? (<- aux      (aux-section seen))))
  (list required optional rest keyword aux))

(define-syntax ordinary-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required-section optional-section rest-section keyword-section aux-section)
             (%ordinary-lambda-list seen)))
  ((required-section  ?)
   (optional-section  ? :evaluation :compound)
   (rest-section      ?)
   (keyword-section   ? :evaluation :compound)
   (aux-section       ? :evaluation :compound)))

(defrule ordinary-lambda-list! ()
  (must (ordinary-lambda-list) "must be an ordinary lambda list"))

;;; 3.4.2 Generic Function Lambda Lists

(defrule %generic-function-lambda-list (seen)
    (list (? (<- required (required-section seen)))
          (? (<- optional (optional-section seen))) ; TODO disallow defaults
          (? (<- rest     (rest-section seen)))
          (? (<- keyword  (keyword-section seen)))) ; TODO disallow defaults
  (list required optional rest keyword))

(define-syntax generic-function-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required-section optional-section rest-section keyword-section)
             (%generic-function-lambda-list seen)))
  ((required-section ?)
   (optional-section ?)
   (rest-section     ?)
   (keyword-section  ?)))

(defrule generic-function-lambda-list! ()
  (must (generic-function-lambda-list)
        "must be a generic function lambda list"))

;;; 3.4.3 Specialized Lambda Lists

(define-syntax eql-specializer
    (list* 'eql (must (list (<- object ((form! forms))))
                      "must be a single object"))
  ((object 1 :evaluation t)))

(defrule specializer ()
  (or (eql-specializer) ((class-name! names))))

(define-syntax (specialized-parameter :arguments ((seen nil)))
    (or (and (list* :any)
             (must (list (<- name (unique-variable-name! seen))
                         (<- specializer (specializer)))
                   "must be of the form (NAME SPECIALIZER)"))
        (and (not-lambda-list-keyword)
             (<- name (unique-variable-name! seen))))
  ((name        1)
   (specializer ? :evaluation :compound)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (specialized-required-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (+ (<<- parameters (specialized-parameter seen))))
    source
    (bp:node* (:required-section ; :source source
               )
      (* (:parameter . *) (nreverse parameters) :evaluation :compound))))

(defrule %specialized-lambda-list (seen)
    (list (? (<- required (specialized-required-section seen)))
          (? (<- optional (optional-section seen)))
          (? (<- rest     (rest-section seen)))
          (? (<- keyword  (keyword-section seen)))
          (? (<- aux      (aux-section seen))))
  (list required optional rest keyword aux))

(define-syntax specialized-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required-section optional-section rest-section keyword-section aux-section)
             (%specialized-lambda-list seen)))
  ((required-section  ? :evaluation :compound)
   (optional-section  ? :evaluation :compound)
   (rest-section      ?)
   (keyword-section   ? :evaluation :compound)
   (aux-section       ? :evaluation :compound)))

(defrule specialized-lambda-list! ()
  (must (specialized-lambda-list)
        "must be a specialized lambda list"))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

(defun compute-name-evaluation (name-node)
  (if (eq (bp:node-kind* name-node) :pattern) :compound nil))

(defun compute-parameter-evaluation (parameter-node)
  (compute-name-evaluation
   (bp:node-relation* '(:name . 1) parameter-node)))

(parser:defgrammar destructuring-lambda-list
  (:class eg::expression-grammar)
  (:use lambda-lists))

(parser:in-grammar destructuring-lambda-list)

(defrule unique-variable-name (seen) ; TODO maybe not all unique-variable-names?
  (or (and (guard (typep 'list)) (pattern seen))
      ((unique-variable-name lambda-lists) seen)))

(defrule unique-variable-name! (seen) ; TODO maybe not all unique-variable-names?
  (or (and (guard (typep 'list)) (pattern seen))
      ((unique-variable-name! lambda-lists) seen)))

(defrule required-parameter (seen)
    (value (source) (<- name (unique-variable-name seen)))
  (bp:node* (:required-parameter :source source)
    (1 (:name . 1) name :evaluation (compute-name-evaluation name))))

(defrule required-parameter! (seen) ; TODO this should use required-parameter
    (value (source)
      (and (not-lambda-list-keyword)
           (<- name (unique-variable-name! seen))))
  (bp:node* (:required-parameter :source source)
    (1 (:name . 1) name :evaluation (compute-name-evaluation name))))

(define-syntax (whole-parameter :arguments ((seen nil)))
    (<- name (unique-variable-name! seen))
  ((name 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (whole-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq (<- keyword   (the-lambda-list-keyword '&whole))
             (<- parameter (whole-parameter seen))))
    source
    (bp:node* (:whole-section ; :source source
               )
      (1 (:keyword   . 1) keyword)
      (1 (:parameter . 1) parameter))))

(define-syntax (environment-parameter :arguments ((seen nil)))
    (<- name (unique-variable-name! seen))
  ((name 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (environment-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq (<- keyword   (the-lambda-list-keyword '&environment))
             (<- parameter (environment-parameter seen))))
    source
    (bp:node* (:environment-section ; :source source
               )
      (1 (:keyword   . 1) keyword)
      (1 (:parameter . 1) parameter)))

  ;; This is like `required-section' in the ordinary lambda list
  ;; grammar except that the `:evaluation' of all `:parameter's is
  ;; `:compound'.
  ;; TODO should be able to use the name `required-section' here
  (defrule (destructuring-required-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (+ (<<- parameters (required-parameter! seen))))
    source
    (bp:node* (:required-section ; :source source
               )
      (* (:parameter . *) (nreverse parameters) :evaluation :compound)))

  ;; TODO should be able to use the name `rest-section' here
  (defrule (destructuring-rest-section :environment (make-instance 'eg::expression-environment)) (seen)
      (value (source)
        (seq (<- keyword   (or (the-lambda-list-keyword '&rest)
                               (the-lambda-list-keyword '&body)))
             (<- parameter (rest-parameter seen))))
    source
    (let ((evaluation (compute-parameter-evaluation parameter)))
      (bp:node* (:rest-section ; :source source
                 )
        (1 (:keyword   . 1) keyword)
        (1 (:parameter . 1) parameter :evaluation evaluation)))))

(define-syntax (pattern :arguments ((seen nil)))
    (list* (? (<- whole-section    (whole-section seen)))
           (? (<- required-section (destructuring-required-section seen)))
           (? (<- optional-section (optional-section seen)))
           (or (list (? (<- rest-section    (destructuring-rest-section seen)))
                     (? (<- keyword-section (keyword-section seen)))
                     (? (<- aux-section     (aux-section seen))))
               (<- cdr ((unique-variable-name! lambda-lists) seen))))
  ((whole-section    ?)
   (required-section ? :evaluation :compound)
   (optional-section ? :evaluation :compound)
   (rest-section     ? :evaluation :compound)
   (keyword-section  ? :evaluation :compound)
   (aux-section      ? :evaluation :compound)
   (cdr              ?)))

(defrule %destructuring-lambda-list (seen)
    (list* (? (<- whole    (whole-section seen)))
           (? (<- env      #1=(eg:once (environment-section seen)
                                       :flag env? :name &environment)))
           (? (<- required (destructuring-required-section seen)))
           (? (<- env      #1#))
           (? (<- optional (optional-section seen)))
           (? (<- env      #1#))
           (or (list (? (<- rest    (destructuring-rest-section seen)))
                     (? (<- env     #1#))
                     (? (<- keyword (keyword-section seen)))
                     (? (<- env     #1#))
                     (? (<- aux     (aux-section seen)))
                     (? (<- env     #1#)))
               (<- cdr ((unique-variable-name! lambda-lists) seen))))
  (list whole env required optional rest keyword aux cdr))

(define-syntax destructuring-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (whole-section env-section
              required-section optional-section rest-section keyword-section aux-section
              cdr)
             (%destructuring-lambda-list seen)))
  ((whole-section    ?)
   (env-section      ?)
   (required-section ? :evaluation :compound)
   (optional-section ? :evaluation :compound)
   (rest-section     ? :evaluation :compound)
   (keyword-section  ? :evaluation :compound)
   (aux-section      ? :evaluation :compound)
   (cdr              ?)))

(defrule destructuring-lambda-list! ()
  (must (destructuring-lambda-list)
        "must be a destructuring lambda list"))

;;; 3.4.8 Deftype Lambda Lists
;;;
;;; A deftype lambda list differs from a macro lambda list only in
;;; that if no init-form is supplied for an optional parameter or
;;; keyword parameter in the lambda-list, the default value for that
;;; parameter is the symbol * (rather than nil).

(parser:defgrammar deftype-lambda-list
  (:class eg::expression-grammar)
  (:use destructuring-lambda-list))

(parser:in-grammar deftype-lambda-list)

(define-syntax deftype-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (whole-section env-section
              required-section optional-section rest-section keyword-section aux-section
              cdr)
             ((%destructuring-lambda-list destructuring-lambda-list) seen)))
  ((whole-section    ?)
   (env-section      ?)
   (required-section ? :evaluation :compound)
   (optional-section ? :evaluation :compound)
   (rest-section     ? :evaluation :compound)
   (keyword-section  ? :evaluation :compound)
   (aux-section      ? :evaluation :compound)
   (cdr              ?)))

(defrule deftype-lambda-list! ()
  (must (deftype-lambda-list) "must be a DEFTYPE lambda list"))
