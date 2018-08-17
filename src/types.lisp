;;;; types.lisp --- Rules for parsing type specifiers.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:syntax)

(parser:in-grammar special-operators)

#+TODO-maybe (parser:defrule compound-type-specifier ()
                 (list (:guard symbolp) (* :any))
               (bp:node* (:compound-type-specifier :head )
                 (* :argument )))

(parser:defrule type-specifier ()
    (or (:guard symbolp)
        (list (:guard symbolp) (* :any))))
