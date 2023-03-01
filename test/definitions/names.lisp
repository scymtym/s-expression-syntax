;;;; names.lisp --- Tests for name-related rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.names
  :in :s-expression-syntax)

;;; Names

(test variable-name
  "Smoke test for the `variable-name' rule."
  (rule-test-cases ((syn::variable-name syn::names))
    '(#1=1    nil #1# nil)
    '(#2=:foo nil #2# nil)
    '(#3=nil  nil #3# nil)
    '(#4=t    nil #4# nil)
    '(#5=pi   nil #5# nil)
    '(#6=a    t   #6# (:variable-name () :name a :source #6#))))

(test variable-name!
  "Smoke test for the `variable-name!' rule."
  (rule-test-cases ((syn::variable-name! syn::names))
    '(#1=1    :fatal #1# "variable name must be a symbol")
    '(#2=:foo :fatal #2# "variable name must not be a keyword")
    '(#3=nil  :fatal #3# "variable name must not designate a constant")
    '(#4=t    :fatal #4# "variable name must not designate a constant")
    '(#5=pi   :fatal #5# "variable name must not designate a constant")
    ;;
    '(#6=a    t      #6# (:variable-name () :name a :source #6#))))

(test function-name
  "Smoke test for the `function-name' rule."
  (rule-test-cases ((syn::function-name syn::names))
    '(1           nil    1   nil)
    '(nil         nil    nil nil)
    '((setf #1=1) :fatal #1# "second element of SETF function name must be a symbol")
    ;; Valid syntax
    '(#2=foo
      t #2# (:function-name () :name foo :source #2#))
    '(#3=(setf foo)
      t #3# (:function-name () :name (setf foo) :source #3#))))

;;; References

(test function-reference
  "Smoke test for `function-reference' rule."
  (rule-test-cases ((syn::function-reference syn::names))
    '((function #1=1)        :fatal #1# "must be a function name")
    '((function #2=nil)      :fatal #2# "must be a function name")
    '((function (setf #3=1)) :fatal #3# "second element of SETF function name must be a symbol")

    '(#4=(function #5=foo)
      t #4# (:function-name () :name foo :source #5#))
    '(#6=(function #7=(setf foo))
      t #6# (:function-name () :name (setf foo) :source #7#))))
