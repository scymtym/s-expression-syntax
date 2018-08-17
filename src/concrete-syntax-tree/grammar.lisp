(cl:in-package #:syntax.concrete-syntax-tree)

;;; grammar

(defclass cst-grammar (parser.packrat.grammar.sexp::sexp-grammar)
  ())

(defmethod parser.packrat.grammar:default-environment ((grammar    cst-grammar)
                                                       (expression t))
  (call-next-method) #+previously (make-instance 'cst-environment))

;;; environment

(defclass cst-environment (env:environment
                           seq:sequential-environment-mixin)
  ((tail :initarg :tail
         :reader  tail)))

(env:define-state-methods cst-environment
  (tail)
  ())

;;; compiler

(defmethod c:compile-expression ((grammar      t) ; TODO how to specialize this properly?
                                 (environment  env:environment)
                                 (expression   parser.packrat.grammar.sexp::as-list-expression)
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
                            :class 'cst-environment
                            :state '())))
    `(if (typep ,value ',(ecase (parser.packrat.grammar.sexp::target-type expression)
                           (list '(or cst:atom-cst cst:cons-cst))))
         ,(c:compile-expression
           grammar list-environment (exp:sub-expression expression)
           (lambda (new-environment)
             (c:compile-expression
              grammar new-environment (make-instance 'seq::bounds-test-expression
                                                     :sub-expression (make-instance 'parser.packrat.grammar.base::ignored-expression))
              (curry #'call-with-value-environment failure-cont)
              (curry #'call-with-value-environment success-cont)))
           (curry #'call-with-value-environment failure-cont))
         ,(funcall failure-cont environment))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  cst-environment)
                                 (expression   parser.packrat.grammar.sexp::rest-expression)
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
                                   :class 'cst-environment
                                   :state '())))
             `(let ((,end-var (load-time-value (make-instance 'cst:atom-cst :raw nil))))
                ,(funcall success-cont end-environment))))
         (lambda (new-environment)
           (declare (ignore new-environment))
           (funcall failure-cont environment))))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  cst-environment)
                                 (expression   seq::bounds-test-expression)
                                 (success-cont function)
                                 (failure-cont function))
  `(if (cst:null ,(tail environment))
       ,(funcall failure-cont environment)
       ,(c:compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  cst-environment)
                                 (expression   seq::element-access-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,element (cst:raw (cst:first ,(tail environment)))))
       ,(c:compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  cst-environment)
                                 (expression   seq::advance-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let* ((amount (seq::amount expression))
         (tail   (tail environment)))
    (c:compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (let* ((new-environment (parser.packrat.grammar.base::add-value
                                (env:environment-at environment :fresh
                                                    :parent element-environment)
                                (env:value element-environment)))
              (new-tail        (tail new-environment)))
         `(let ((,new-tail ,(or nil #+maybe-later to
                                    (ecase amount
                                      (1 `(cst:rest ,tail))
                                      ; (2 `(cddr ,tail))
                                      ; (t `(nthcdr ,amount ,tail))
                                      ))))
            ,(funcall success-cont new-environment))))
     failure-cont)))
