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
(defrule lambda-list-keyword ()
  (or '&whole '&environment '&optional '&rest '&key '&aux '&allow-other-keys))

(defrule lambda-list-variable-name ()
  (and (not (lambda-list-keyword))
       ((variable-name names))))

(defrule lambda-list-variable-name! ()
  (must (and (must (not (lambda-list-keyword))
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
      (and (not (lambda-list-keyword))
           (<- name (unique-variable-name! seen))))
  (bp:node* (:required-parameter :source source)
    (1 (:name . 1) name)))

(define-syntax (optional-parameter :arguments ((seen nil)))
    (or (list (<- name (unique-variable-name! seen))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not (lambda-list-keyword))
                      (unique-variable-name! seen))))
  ((name     1)
   (default  ? :evaluation t)
   (supplied ?)))

(defrule rest-parameter (seen)
  (unique-variable-name! seen))

(define-syntax (keyword-parameter :arguments ((seen nil)))
    (or (list (or (list (<- keyword ((parameter-keyword! names)))
                        (<- name (unique-variable-name! seen)))
                  (<- name (unique-variable-name! seen)))
              (? (seq (<- default ((form! forms)))
                      (? (<- supplied (unique-variable-name! seen))))))
        (<- name (and (not (lambda-list-keyword))
                      (unique-variable-name! seen))))
  ((name     1)
   (keyword  ?)
   (default  ? :evaluation t)
   (supplied ?)))

(define-syntax (aux-parameter :arguments ((seen nil)))
    (or (list (<- name (unique-variable-name! seen))
              (? (<- value ((form! forms)))))
        (<- name (unique-variable-name! seen)))
  ((name  1)
   (value ? :evaluation t)))

(defrule aux-parameter! (seen)
  (must (aux-parameter seen) "must be an aux parameter"))

;;; Reusable sections of lambda lists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (required-section :environment (make-instance 'eg::expression-environment)) (seen)
      (+ (<<- parameters (required-parameter! seen)))
    (nreverse parameters))

  (defrule (optional-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&optional (* (<<- parameters (optional-parameter seen))))
    (nreverse parameters))

  (defrule (rest-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&rest (<- parameter (rest-parameter seen)))
    parameter)

  (defrule (keyword-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&key
           (* (<<- keyword-parameters (keyword-parameter seen)))
           (? (<- allow-other-keys? '&allow-other-keys)))
    (list (nreverse keyword-parameters)
          (when allow-other-keys?
            (bp:node* (:lambda-list-keyword :keyword (eg::%naturalize allow-other-keys?)
                                            :source  allow-other-keys?)))))

  (defrule (aux-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&aux (* (<<- parameters (aux-parameter! seen))))
    (nreverse parameters)))

;;; 3.4.1 Ordinary Lambda Lists

(defrule %ordinary-lambda-list (seen)
    (list (? (<- required                    (required-section seen)))
          (? (<- optional                    (optional-section seen)))
          (? (<- rest                        (rest-section seen)))
          (? (<- (keyword allow-other-keys?) (keyword-section seen)))
          (? (<- aux                         (aux-section seen))))
  (list required optional rest keyword allow-other-keys? aux))

(define-syntax ordinary-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required optional rest keyword allow-other-keys? aux)
             (%ordinary-lambda-list seen)))
  ((required          *>)
   (optional          *> :evaluation :compound)
   (rest              ?)
   (keyword           *> :evaluation :compound)
   (allow-other-keys? ?)
   (aux               *> :evaluation :compound)))

(defrule ordinary-lambda-list! ()
  (must (ordinary-lambda-list) "must be an ordinary lambda list"))

;;; 3.4.2 Generic Function Lambda Lists

(defrule %generic-function-lambda-list (seen)
    (list (? (<- required                    (required-section seen)))
          (? (<- optional                    (optional-section seen))) ; TODO disallow defaults
          (? (<- rest                        (rest-section seen)))
          (? (<- (keyword allow-other-keys?) (keyword-section seen)))) ; TODO disallow defaults
  (list required optional rest keyword allow-other-keys?))

(define-syntax generic-function-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required optional rest keyword allow-other-keys?)
             (%generic-function-lambda-list seen)))
  ((required          *>)
   (optional          *>)
   (rest              ?)
   (keyword           *>)
   (allow-other-keys? ?)))

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
        (and (not (lambda-list-keyword))
             (<- name (unique-variable-name! seen))))
  ((name        1)
   (specializer ? :evaluation :compound)))

(defrule %specialized-lambda-list (seen)
    (list (* (<<- required                    (specialized-parameter seen)))
          (? (<-  optional                    (optional-section seen)))
          (? (<-  rest                        (rest-section seen)))
          (? (<-  (keyword allow-other-keys?) (keyword-section seen)))
          (? (<-  aux                         (aux-section seen))))
  (list (nreverse required) optional rest keyword allow-other-keys? aux))

(define-syntax specialized-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (required optional rest keyword allow-other-keys? aux)
             (%specialized-lambda-list seen)))
  ((required          *> :evaluation :compound)
   (optional          *> :evaluation :compound)
   (rest              ?)
   (keyword           *> :evaluation :compound)
   (allow-other-keys? ?)
   (aux               *> :evaluation :compound)))

(defrule specialized-lambda-list! ()
  (must (specialized-lambda-list)
        "must be a specialized lambda list"))

;;; 3.4.4 Macro Lambda Lists

;;; 3.4.5 Destructuring Lambda Lists

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
    (1 (:name . 1) name :evaluation (if (eq (bp:node-kind* name) :pattern) :compound nil))))

(defrule required-parameter! (seen) ; TODO this should use required-parameter
    (value (source)
      (and (not (lambda-list-keyword))
           (<- name (unique-variable-name! seen))))
  (bp:node* (:required-parameter :source source)
    (1 (:name . 1) name :evaluation (if (eq (bp:node-kind* name) :pattern) :compound nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrule (whole-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&whole (<- name (unique-variable-name! seen)))
    name)

  (defrule (environment-section :environment (make-instance 'eg::expression-environment)) (seen)
      (seq '&environment (<- name (unique-variable-name! seen)))
    name))

(define-syntax (pattern :arguments ((seen nil)))
    (list* (? (<- whole    (whole-section seen)))
           (? (<- required (required-section seen)))
           (? (<- optional (optional-section seen)))
           (or (list (? (<- rest                    (rest-section seen)))
                     (? (<- (key allow-other-keys?) (keyword-section seen)))
                     (? (<- aux                     (aux-section seen))))
               (<- cdr ((unique-variable-name! lambda-lists) seen))))
  ((whole             ?)
   (required          *> :evaluation :compound)
   (optional          *> :evaluation :compound)
   (rest              ?)
   (key               *> :evaluation :compound)
   (allow-other-keys? ?)
   (aux               *> :evaluation :compound)
   (cdr               ?)))

(defrule %destructuring-lambda-list (seen)
    (list* (? (<- whole    (whole-section seen)))
           (? (<- env      #1=(eg:once (environment-section seen)
                                       :flag env? :name &environment)))
           (? (<- required (required-section seen)))
           (? (<- env      #1#))
           (? (<- optional (optional-section seen)))
           (? (<- env      #1#))
           (or (list (? (<- rest                    (rest-section seen)))
                     (? (<- env                     #1#))
                     (? (<- (key allow-other-keys?) (keyword-section seen)))
                     (? (<- env                     #1#))
                     (? (<- aux                     (aux-section seen)))
                     (? (<- env                     #1#)))
               (<- cdr ((unique-variable-name! lambda-lists) seen))))
  (list whole env required optional rest key allow-other-keys? aux cdr))

(define-syntax destructuring-lambda-list
    (and (<- seen (:transform :any (make-hash-table :test #'eq)))
         (<- (whole env required optional rest keyword allow-other-keys? aux cdr)
             (%destructuring-lambda-list seen)))
  ((whole             ?)
   (env               ?)
   (required          *> :evaluation :compound)
   (optional          *> :evaluation :compound)
   (rest              ?  :evaluation :compound)
   (keyword           *> :evaluation :compound)
   (allow-other-keys? ?)
   (aux               *> :evaluation :compound)
   (cdr               ?)))

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
         (<- (whole env required optional rest keyword allow-other-keys? aux cdr)
             ((%destructuring-lambda-list destructuring-lambda-list) seen)))
  ((whole             ?)
   (env               ?)
   (required          *> :evaluation :compound)
   (optional          *> :evaluation :compound)
   (rest              ?  :evaluation :compound)
   (keyword           *> :evaluation :compound)
   (allow-other-keys? ?)
   (aux               *> :evaluation :compound)
   (cdr               ?)))

(defrule deftype-lambda-list! ()
  (must (deftype-lambda-list) "must be a DEFTYPE lambda list"))
