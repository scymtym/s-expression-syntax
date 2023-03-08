;;;; protocol.lisp --- Tests for protocol default behavior.
;;;;
;;;; Copyright (C) 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.protocol
  :in :s-expression-syntax)

;;; Syntax description repository protocol

(test find-syntax.non-existing
  "Test if-does-not-exist behavior of `find-syntax'."
  (signals syn:syntax-not-found-error
    (syn:find-syntax 'no-such-syntax))
  (signals syn:syntax-not-found-error
    (syn:find-syntax 'no-such-syntax :if-does-not-exist #'error))
  (is (eq :foo
          (block nil
            (syn:find-syntax
             'no-such-syntax :if-does-not-exist (lambda (condition)
                                                  (declare (ignore condition))
                                                  (block nil :foo))))))
  (eq nil (syn:find-syntax 'no-such-syntax :if-does-not-exist nil))
  (eq :foo (syn:find-syntax 'no-such-syntax :if-does-not-exist :foo)))

;;; Parse protocol

(test parse.auto-classify
  "Test automatic expression classification in `parse' calls."
  (is (equal '(:self-evaluating
               ((:value . 1) (((:unparsed
                                ()
                                :expression 1
                                :context    :self-evaluating
                                :source     1))))
               :source 1)
             (syn:parse 'list t '1)))
  (is (equal '(:application
               ((:function-name . 1) (((:function-name
                                        ()
                                        :name list :source list)))
                (:argument      . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source 1)
                                       :evaluation t)))
               :source (list 1))
             (syn:parse 'list t '(list 1)))))

(test parse.syntax-description-name
  "Test designating syntax descriptions by name in `parse' calls."
  (is (equal '(:self-evaluating
               ((:value . 1) (((:unparsed
                                ()
                                :expression 1
                                :context    :self-evaluating
                                :source     1))))
               :source 1)
             (syn:parse 'list 'syn::self-evaluating '1)))
  (is (equal '(:application
               ((:function-name . 1) (((:function-name
                                        ()
                                        :name list :source list)))
                (:argument      . *) (((:unparsed
                                        ()
                                        :expression 1 :context :form :source 1)
                                       :evaluation t)))
               :source (list 1))
             (syn:parse 'list 'syn::application '(list 1)))))
