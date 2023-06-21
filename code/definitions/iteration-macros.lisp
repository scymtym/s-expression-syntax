;;;; iteration-macros.lisp --- Standard macros for iteration (except `loop').
;;;;
;;;; Copyright (C) 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;; Standard macros `do' and `do*'

(define-syntax (do-iteration-variable :arguments ((seen nil)))
    (or (and (list* :any)
             (must (list (<- variable ((unique-variable-name! lambda-lists) seen))
                         (? (seq (<- init ((form! forms)))
                                 (? (<- step ((form! forms)))))))
                   "must be of the form (VARIABLE [INIT-FORM [STEP-FORM]])"))
        (<- variable ((unique-variable-name! lambda-lists) seen)))
    (if init
        `(,variable ,init ,@(? step))
        variable)
  ((variable 1 :evaluation (load-time-value
                            (make-instance 'binding-semantics
                                           :namespace 'variable
                                           :scope     :lexical
                                           :values    nil)))
   (init     ? :evaluation t)
   (step     ? :evaluation t)))

(macrolet
    ((define (name order)
       (declare (ignore order))
       `(define-macro ,name
            (list* (<- seen (:transform (seq) (make-hash-table :test #'eq)))
                   (must (list (* (<<- variable (do-iteration-variable seen)))) ; TODO allow duplicates for do*
                         "must be a list of iteration variable bindings")
                   (must (list (<- end-test ((form! forms)))
                               (* (<<- result ((form! forms)))))
                         "must be of the form (END-TEST RESULT*)")
                   (<- (declaration segment) ((tagbody-body forms))))
            `((,@variable) (,end-test ,@result)
              ,@declaration ,@(apply #'append segment)) ; TODO
          ((variable    *  :evaluation :compound) ; :order order
           (end-test    1  :evaluation t)
           (result      *  :evaluation t)
           (declaration *>)
           (segment     *> :evaluation :compound)))))
  (define do  :parallel)
  (define do* :sequential))

;;; Standard macros `do-{all,external,}-symbols'

(macrolet ((define (name)
             `(define-macro ,name
                  (list* (must (list (<- variable ((variable-name! names)))
                                     (? (seq (<- package ((form! forms)))
                                             (? (<- result ((form! forms)))))))
                               "must be of the form (VARIABLE [PACKAGE [RESULT]])")
                         (<- (declaration segment) ((tagbody-body forms))))
                  `((,variable ,@(? package) ,@(? result))
                    ,@declaration ,@ (apply #'append segment))
                ((variable    1 :evaluation (load-time-value
                                             (make-instance 'binding-semantics
                                                            :namespace 'variable
                                                            :scope     :lexical
                                                            :values    nil)))
                 (package     ?  :evaluation t)
                 (result      ?  :evaluation t)
                 (declaration *>)
                 (segment     *> :evaluation :compound)))))
  (define do-symbols)
  (define do-external-symbols))

(define-macro do-all-symbols
    (list* (must (list (<- variable ((variable-name! names)))
                       (? (<- result ((form! forms)))))
                 "must be of the form (VARIABLE [RESULT])")
           (<- (declaration segment) ((tagbody-body forms))))
    `((,variable ,@(? result)) ,@declaration ,@(apply #'append segment))
  ((variable    1 :evaluation (load-time-value
                               (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil)))
   (result      ?  :evaluation t)
   (declaration *>)
   (segment     *> :evaluation :compound)))

;;; Standard macros `dolist' and `dotimes'

(macrolet
    ((define (name source-name)
       `(define-macro ,name
            (list* (must (list (<- variable ((variable-name! names)))
                               (<- ,source-name ((form! forms)))
                               (? (<- result ((form! forms)))))
                         ,(format nil "must be of the form (VARIABLE ~A [RESULT])"
                                  source-name))
                   (<- (declaration segment) ((tagbody-body forms))))
            `((,variable ,,source-name ,@(? result))
              ,@declaration ,@(apply #'append segment))
          ((variable     1 :evaluation (load-time-value
                                        (make-instance 'binding-semantics
                                                       :namespace 'variable
                                                       :scope     :lexical
                                                       :values    nil)))
           (,source-name 1  :evaluation t)
           (result       ?  :evaluation t)
           (declaration  *>)
           (segment      *> :evaluation :compound)))))
  (define dolist  list)
  (define dotimes count))
