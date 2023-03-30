;;;; grammar.lisp --- Grammar class, compilation rules for the expression-grammar module.
;;;;
;;;; Copyright (C) 2020-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

;;; Grammar

(defclass expression-grammar (sexp::sexp-grammar)
  ()
  (:default-initargs
   :meta-grammar    'meta-grammar
   :meta-start-rule 'base::expression))

;;; Environment

(defclass expression-environment (; env:environment
                                  seq::list-environment ; TODO hack
                                  ; seq:sequential-environment-mixin
                                  )
  ((seq::tail :initarg :tail
              :reader  tail)))

(env:define-state-methods expression-environment
  (seq::tail)
  ())

;;; Base expression compilation

(macrolet ((define-method (function replacement)
             `(defmethod base::compile-call ((grammar     expression-grammar)
                                             (environment t)
                                             (expression  t)
                                             (function    (eql ',function))
                                             (arguments   t))
                `(,',replacement ,@arguments))))
  (define-method symbol-name    %symbol-name)
  (define-method symbol-package %symbol-package)
  (define-method package-name   %package-name))

(defmethod base::compile-test ((grammar      expression-grammar)
                               (environment  t)
                               (expression   t)
                               (predicate    t)
                               (value        t)
                               (arguments    t)
                               (success-cont function)
                               (failure-cont function))
  ;; If VALUE is natural, call PREDICATE on it, otherwise naturalize
  ;; VALUE and call PREDICATE on the result.
  `(if (,predicate (if (%natural? ,value)
                       ,value
                       (naturalize *client* ,value))
                   ,@arguments)
       ,(funcall success-cont environment)
       ,(funcall failure-cont environment)))

;;; Compile tests in which the predicate is `equal' or `eql' by
;;; emitting a call to `%equal' or `%eql' respectively. Those replacements
(macrolet ((define-method (predicate replacement)
             `(defmethod base::compile-test ((grammar      expression-grammar)
                                             (environment  t)
                                             (expression   t)
                                             (predicate    (eql ',predicate))
                                             (value        t)
                                             (arguments    t)
                                             (success-cont function)
                                             (failure-cont function))
                `(if (,',replacement ,value ,@arguments)
                     ,(funcall success-cont environment)
                     ,(funcall failure-cont environment)))))
  (define-method typep %typep)
  (define-method equal %equal)
  (define-method eql   %eql))

;;; Sequence expression compilation

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  env:environment)
                                 (expression   sexp::as-vector-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ ((value (env:value environment))
         ((&with-gensyms vector-function vector-value)))
    `(flet ((,vector-function (,vector-value)
              ,(call-next-method)))
       (if (%natural? ,value)
           (,vector-function ,value)
           (,vector-function (naturalize *client* ,value))))))

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  env:environment)
                                 (expression   sexp::as-list-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ ((value (env:value environment))
         ((&flet call-with-value-environment (cont parent-environment)
            (funcall cont (env:environment-at
                           parent-environment (list :value value)
                           :class 'env:value-environment
                           :state '()))))
         (list-environment (env:environment-at
                            environment (list :tail value)
                            :class 'expression-environment
                            :state '())))
    `(if ,(ecase (sexp::target-type expression)
            (list `(%listp ,value)))
         ,(c:compile-expression
           grammar list-environment (exp:sub-expression expression)
           (lambda (new-environment)
             (c:compile-expression
              grammar new-environment sexp::*just-test-bounds*
              (a:curry #'call-with-value-environment failure-cont)
              (a:curry #'call-with-value-environment success-cont)))
           (a:curry #'call-with-value-environment failure-cont))
         ,(funcall failure-cont environment))))

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  expression-environment)
                                 (expression   sexp::rest-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&with-gensyms tail-var end-var))
         (rest-environment (env:environment-at
                            environment (list :value tail-var)
                            :class 'env:value-environment
                            :state '())))
    `(let ((,tail-var ,(tail environment)))
       ,(c:compile-expression
         grammar rest-environment (exp:sub-expression expression)
         (lambda (new-environment)
           (let ((end-environment (env:environment-at
                                   new-environment (list :tail end-var)
                                   :class 'expression-environment
                                   :state '())))
             `(let ((,end-var '%null))
                ,(funcall success-cont end-environment))))
         (lambda (new-environment)
           (declare (ignore new-environment))
           (funcall failure-cont environment))))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  expression-environment)
                                 (expression   seq::bounds-test-expression)
                                 (success-cont function)
                                 (failure-cont function))
  `(if (and (%listp ,(tail environment)) ; TODO consp?
            (not (%null ,(tail environment))))
       ,(c:compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)
       ,(funcall failure-cont environment)))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  expression-environment)
                                 (expression   seq::element-access-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let* ((,element (%first ,(tail environment))))
       ,(c:compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  expression-environment)
                                 (expression   seq::advance-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let* ((amount (seq::amount expression))
         (tail   (tail environment)))
    (c:compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (let* ((new-environment (base::add-value
                                (env:environment-at environment :fresh
                                                    :parent element-environment)
                                (env:value element-environment)))
              (new-tail        (tail new-environment)))
         `(let ((,new-tail ,(ecase amount
                              (1 `(%rest ,tail)))))
            ,(funcall success-cont new-environment))))
     failure-cont)))

;;;; Structure expression compilation

;;; TODO compile symbol-name call as (if (%natural ) (cl:symbol-name ) (our-own:name ))
;;;      might require adding a c:compile-function-call

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  env:environment)
                                 (expression   sexp::structure-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (call-next-method)
  #+no (let+ ((value (env:value environment))
         ((&with-gensyms structure-function structure-value))
         (new-environment (env:environment-at
                           environment (list :value structure-value))))
    `(flet ((,structure-function (,structure-value)
              ,(call-next-method
                grammar new-environment expression
                (lambda (environment)
                  (funcall success-cont (env:environment-at
                                         environment (list :value value))))
                (lambda (environment)
                  (funcall failure-cont (env:environment-at
                                         environment (list :value value)))))))
       (if (%natural? ,value)
           (,structure-function ,value)
           (,structure-function (naturalize *client* ,value))))))
