;;;; names.lisp --- Rules for different kinds of names.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:defgrammar names
  (:class eg::expression-grammar))

(parser:in-grammar names)

;;; Constant

(defun constant? (name)
  (eq (sb-cltl2:variable-information name) :constant))

(defrule constant ()
    (structure 'symbol
               (symbol-name symbol-name)
               (symbol-package (structure 'package (package-name package-name))))
  (a:if-let ((package (find-package package-name)))
    (multiple-value-bind (symbol found?) (find-symbol symbol-name package)
      (unless (and found? (constant? symbol))
        (:fail)))
    (:fail)))

;;; Variable names

(defrule variable-name/unchecked ()
    (value (source)
      symbol)
  (bp:node* (:variable-name :name (eg::%naturalize symbol) :source source)))

(defrule variable-name ()
  (and (guard (typep 'symbol))
       (not (guard (typep 'keyword)))
       (not (constant))
       (variable-name/unchecked)))

(defrule variable-name! ()
  (and (must (guard (typep 'symbol))
             "variable name must be a symbol")
       (must (not (guard (typep 'keyword)))
             "variable name must not be a keyword")
       (must (not (constant))
             "variable name must not designate a constant")
       (variable-name/unchecked)))

;;; Function names

(defrule function-name/symbol/raw ()
  (and (guard (typep 'symbol))
       (not 't) (not 'nil)))

(defrule function-name/symbol ()
    (value (source)
      (<- symbol (function-name/symbol/raw)))
  (bp:node* (:function-name :name (eg::%naturalize symbol) :source source)))

(defrule function-name/symbol! ()
  (must (function-name/symbol) "function name must be a symbol"))

(defrule function-name/setf ()
    (value (source)
      (list 'setf (must (<- symbol (function-name/symbol/raw))
                        "second element of SETF function name must be a symbol")))
  (bp:node* (:function-name :name   `(setf ,(eg::%naturalize symbol))
                            :source source)))

(defrule function-name ()
  (or (function-name/symbol)
      (function-name/setf)))

(defrule function-name! ()
  (must (function-name) "must be a function name"))

(defrule class-name () ; TODO call this type name?
    (value (source)
      (and (guard (typep 'symbol)) (not 'nil) symbol))
  (bp:node* (:type-name :name (eg::%naturalize symbol) :source source)))

(defrule class-name! ()
  (must (class-name) "must be a class name"))

(defrule slot-name ()
  (variable-name))

(defrule slot-name! ()
  (must (slot-name) "slot name must be a symbol that is a valid variable name"))

;;; Initarg names

(defrule initarg-name ()
    (value (source)
      (guard name (typep 'symbol)))
  (bp:node* (:initarg-name :name (eg::%naturalize name) :source source)))

(defrule initarg-name! ()
  (must (initarg-name) "initarg name must be a symbol"))

;;; Custom class and slot options

(defrule option-name ()
    (value (source)
      (guard name (typep 'symbol)))
  (bp:node* (:option-name :name (eg::%naturalize name) :source source)))

(defrule option-name! ()
  (must (option-name) "option name must be a symbol"))

;;; Declaration identifiers

(defrule declaration-identifier ()
    (value (source)
      (guard name (typep 'symbol)))
  (bp:node* (:declaration-identifier :name (eg::%naturalize name) :source source)))

(defrule declaration-identifier! ()
  (must (declaration-identifier) "declaration identifier must be a symbol"))

;;; Block names

(defrule block-name ()
    (value (source)
      (guard name (typep 'symbol)))
  (bp:node* (:block-name :name (eg::%naturalize name) :source source)))

(defrule block-name! ()
  (must (block-name) "block name must be a symbol"))

;;; Tags

(defrule integer-or-symbol ()
  (or (guard (typep 'integer)) (guard (typep 'symbol))))

(defrule tag ()
    (value (source)
      (<- name (integer-or-symbol)))
  (let ((name (eg::%naturalize name)))
    (bp:node* (:tag :name name :source source))))

(defrule tag! ()
  (must (tag) "tag must be a symbol or an integer"))

(defrule unique-tag! (seen)
    (<- tag (tag!))
  (let ((name (getf (bp:node-initargs* tag) :name)))
    (cond ((not seen))
          ((not (nth-value 1 (gethash name seen)))
           (setf (gethash name seen) t))
          (t
           (:fatal (format nil "the tag ~S occurs more than once" name)))))
  tag)

;;; A symbol that is the keyword of a keyword parameter
;;;
;;; Doesn't fit here super well, but is used in multiple other
;;; grammars.

(defrule parameter-keyword ()
    (value (source)
      (guard keyword (typep 'symbol)))
  (let ((name (eg::%naturalize keyword)))
    (bp:node* (:keyword :name name :source source))))

(defrule parameter-keyword! ()
  (must (parameter-keyword) "must be a symbol"))

;;; References

(defrule function-reference () ; TODO does this make sense
    (list 'function (<- name (function-name!)))
  name)

(defrule function-reference! ()
  (must (function-reference) "must be a function reference"))
