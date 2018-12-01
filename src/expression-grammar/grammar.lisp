(cl:in-package #:syntax.expression-grammar)

;;; grammar

(defclass expression-grammar (parser.packrat.grammar.sexp::sexp-grammar)
  ())

;;; environment

(defclass expression-environment (env:environment
                           seq:sequential-environment-mixin)
  ((tail :initarg :tail
         :reader  tail)))

(env:define-state-methods expression-environment
  (tail)
  ())


;;; Base expression compilation

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  env:value-environment)
                                 (expression   base:predicate-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&accessors-r/o (sub-expression exp:sub-expression) (predicate base:predicate)) expression)
         ((predicate &rest arguments) (ensure-list predicate)))
    (c:compile-expression
     grammar environment sub-expression
     (lambda (new-environment)
       (let ((value (env:value new-environment)))
         `(if (if (%natural? ,value)
                  (,predicate ,value ,@arguments)
                  (,predicate (naturalize *client* ,value) ,@arguments))
              ,(funcall success-cont new-environment)
              ,(funcall failure-cont new-environment))))
     failure-cont)))

(defmethod c:compile-expression ((grammar      expression-grammar)
                                 (environment  env:value-environment)
                                 (expression   base:terminal-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let* ((value     (env:value environment))
         (expected  (exp:value expression))
         (predicate (typecase expected
                      (string '%equal)
                      (t      '%eql))))
    `(if (,predicate ,value ',expected)
         ,(funcall success-cont environment)
         ,(funcall failure-cont environment))))

;;; Sequence expression compilation

(defmethod c:compile-expression ((grammar      expression-grammar)
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
                            :class 'expression-environment
                            :state '())))
    `(if ,(ecase (parser.packrat.grammar.sexp::target-type expression)
            (list `(%listp ,value)))
         ,(c:compile-expression
           grammar list-environment (exp:sub-expression expression)
           (lambda (new-environment)
             (c:compile-expression
              grammar new-environment parser.packrat.grammar.sexp::*just-test-bounds*
              (curry #'call-with-value-environment failure-cont)
              (curry #'call-with-value-environment success-cont)))
           (curry #'call-with-value-environment failure-cont))
         ,(funcall failure-cont environment))))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  expression-environment)
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
  `(if (%null ,(tail environment))
       ,(funcall failure-cont environment)
       ,(c:compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)))

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
         `(let ((,new-tail ,(or nil #+maybe-later to
                                    (ecase amount
                                      (1 `(%rest ,tail))
                                      ; (2 `(cddr ,tail))
                                      ; (t `(nthcdr ,amount ,tail))
                                      ))))
            ,(funcall success-cont new-environment))))
     failure-cont)))
