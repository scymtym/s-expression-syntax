(cl:in-package #:syntax.expression-grammar)

;;; TODO have the client return a bunch of functions at the beginning
;;;      of each rule and make them available lexically?

(defvar *client*)

(defgeneric natural? (client thing))

(defgeneric naturalize (client thing))

(defgeneric listp* (client maybe-list))

(defgeneric null* (client maybe-list))

(defgeneric first* (client maybe-list))

(defgeneric rest* (client maybe-list))

(defgeneric equal* (client left right))

(defgeneric eql* (client left right))
