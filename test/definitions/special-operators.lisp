;;;; special-operators.lisp --- Tests for special operator rules.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.special-operators
  :in :s-expression-syntax)

;;; Utilities

(defun check-roundtrip (special-operator form)
  (let ((syntax   (syn:find-syntax special-operator))
         ; (parser   (sb-c::special-operator-info-parser info))
         ; (unparser (sb-c::special-operator-info-unparser info))
        )
    (finishes (syn:parse nil syntax form))
    ; (assert (equal form (syn:parse nil syntax form)))
    ))

(defun check-roundtrip-cases (special-operator &rest forms)
  (dolist (form forms) (check-roundtrip special-operator form)))

(defun check-error (special-operator form expected-error)
  (let ((syntax (syn:find-syntax special-operator))
         ; (parser (sb-c::special-operator-info-parser info))
        )
    (signals syn:invalid-syntax-error
      (syn:parse nil syntax form))))

(defun check-error-cases (special-operator &rest specs)
  (loop :for (form expected-error) :in specs
        :do (check-error special-operator form expected-error)))

;;;; Special operators for control

(test progn
  "Test for `progn' special operator."

  (check-error-cases
   'progn
   '((progn . 1) foo))

  (is (equal '(syn::forms ())
             (syn:parse nil (syn:find-syntax 'progn) '(progn)))))

(test if
  "Test for `if' special operator."

  (check-error-cases 'if
                     '((if)                 sb-kernel::arg-count-error)
                     '((if foo)             sb-kernel::arg-count-error)
                     '((if foo bar baz fez) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'if
                         '(if foo bar)
                         '(if foo bar baz)))

;;; BLOCK, RETURN-FROM and TAGBODY

(test block
  "Test for `block' special operator."

  (is (equal '(syn::name foo syn::forms (1))
             (syn:parse nil (syn:find-syntax 'block) '(block foo 1))))
  (check-error-cases 'block
                     '(block))
  (check-roundtrip-cases '(block foo a b)))

(test return-from
  "Test for `return-from' special operator."

  (is (equal '(syn::name foo syn::value 1)
             (syn:parse nil (syn:find-syntax 'return-from) '(return-from foo 1))))
  (is (equal '(syn::name foo syn::value nil)
             (syn:parse nil (syn:find-syntax 'return-from) '(return-from foo))))
  #+TODO (apply #'unparse-return-from-special-operator
                (parse-return-from-special-operator
                 (lambda (&rest args) (print args))
                 '(return-from foo bla))))

(test return
  "Test for the `return' special operator"

  (syntax-test-cases (return)
    '((return (declare)) syn:invalid-syntax-error)
    '((return 1 2)       syn:invalid-syntax-error)

    '((return)           (syn::value nil))
    '((return 1)         (syn::value 1))))

(test tagbody
  "Test for `tagbody' special operator."

  (syn:parse nil (syn:find-syntax 'tagbody) '(tagbody 0 1 "bla" "bli" 2 (list 1) (list 3)))
  (syn:parse nil (syn:find-syntax 'tagbody) '(tagbody (1+ a) (1+ b) :foo))

  (check-error-cases 'tagbody
                     '((tagbody nil nil) program-error))
  (check-roundtrip-cases 'tagbody
                         '(tagbody)
                         '(tagbody nil)
                         '(tagbody nil :foo)
                         '(tagbody nil :foo :bar)
                         '(tagbody nil (1+ a))
                         '(tagbody nil :foo (1+ a))
                         '(tagbody nil (1+ a) :foo)
                         '(tagbody (1+ a))
                         '(tagbody (1+ a) (1+ b))
                         '(tagbody (1+ a) (1+ b) (1+ c))
                         '(tagbody :foo (1+ a) (1+ b))
                         '(tagbody (1+ a) :foo (1+ b))
                         '(tagbody (1+ a) (1+ b) :foo)))


(test go
  "test for `go' special operator."

  (is (equal '(syn::tag 1) (syn:parse nil (syn:find-syntax 'go) '(go 1)))))

;;;; Compiler-magic special forms
;;;; TODO test internal ones as well

(test eval-when
  "Test for `eval-when' special operator."

  (is (equal '(syn::situations (:execute) syn::forms (1))
             (syn:parse nil (syn:find-syntax 'eval-when) '(eval-when (:execute) 1))))
  ;; TODO
  )

(test load-time-value
  "Test for `load-time-value' special operator"

  (is (equal '(syn::form foo syn::read-only-p nil)
             (syn:parse nil (syn:find-syntax 'load-time-value) '(load-time-value foo nil))))

  #+no (check-error-cases 'load-time-value
                     '((load-time-value) sb-kernel::arg-count-error)
                     '((load-time-value 1 2) type-error))
  (check-roundtrip-cases 'load-time-value
                         '(load-time-value 1)
                         '(load-time-value (form))
                         '(load-time-value 1 t)
                         '(load-time-value 1 nil)))

(test quote
  "Test for `quote' special operator."

  (is (equal '(syn::material 1)
             (syn:parse nil (syn:find-syntax 'quote) '(quote 1))))
  #+no (check-error-cases 'quote
                     '((quote) sb-kernel::arg-count-error)
                     '((quote x y) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'quote
                         '(quote 1)
                         '(quote x)
                         '(quote quote)))

(test function
  "Test for `function' special operator."

  (is (equal '(syn::name foo
               syn::lambda-list ()
               documentation nil
               syn::declarations ()
               syn::forms ())
             (syn:parse nil (syn:find-syntax 'function) '(function foo))))
  (is (equal '(syn::name (setf foo)
               syn::lambda-list ()
               documentation nil
               syn::declarations ()
               syn::forms ())
             (syn:parse nil (syn:find-syntax 'function) '(function (setf foo)))))
  (is (equal '(syn::name nil
               syn::lambda-list ((a) () b () nil ())
               documentation nil
               syn::declarations nil
               syn::forms ((foo)))
             (syn:parse nil (syn:find-syntax 'function) '(function (lambda (a &rest b) (foo))))))

  #+no (check-error-cases 'function
                     '((function) sb-kernel::arg-count-error)
                     '((function 1) program-error)
                     '((function x y) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'function
                         '(function foo)
                         '(function (setf foo))
                         '(function (lambda ()))
                         '(function (lambda (x)))
                         '(function (lambda (x) x))
                         '(function (lambda (x) x y))))

;;;; SYMBOL-MACROLET, LET[*], LOCALLY and PROGV

(test symbol-macrolet
  "Test for `symbol-macrolet' special operator."

  (is (equal '(syn::names (foo bar)
               values (1 2)
               syn::declarations nil
               syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'symbol-macrolet)
                    '(symbol-macrolet ((foo 1) (bar 2)) (list foo bar)))))

  (check-error-cases 'symbol-macrolet
                     '((symbol-macrolet (foo)) program-error)
                     '((symbol-macrolet ((foo))) program-error)
                     '((symbol-macrolet ((:bla 1))) program-error))
  (check-roundtrip-cases 'symbol-macrolet
                         '(symbol-macrolet ())
                         '(symbol-macrolet ((a 1) (c 1)) (declare (type boolean a)) a)))

(test let
  "Test for `let' special operator."

  (is (equal '(syn::names (foo bar) syn::values (1 2) syn::declarations () syn::forms ((list foo bar)))
             (syn:parse nil (syn:find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar)))))
  #+no (check-error-cases 'let
                     '((let) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'let
                         '(let ())
                         '(let ((a 1) b (c 1)) (declare (type boolean a)) a)))

(test let*
  "Test for `let*' special operator."

  #+no (check-error-cases 'let*
                     '((let*) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'let*
                         '(let* ())
                         '(let* ((a 1) b (c 1)) (declare (type boolean a)) a)))

(test locally
  "Test for `locally' special operator."

  (is (equal '(syn::declarations ((type (integer x)) (type (double-float x))) syn::forms (x))
             (syn:parse nil (syn:find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x))))

  (check-roundtrip-cases 'locally
                         '(locally)
                         '(locally (declare (type integer a)))
                         '(locally (declare (type integer a)) a)
                         '(locally a)))

(test progv
  "Test for `progv' special operator."

  (check-roundtrip-cases 'progv
                         '(progv () ())
                         '(progv '(a) '(1))
                         '(progv (foo) '(1))
                         '(progv () () 1)
                         '(progv () () 1 2)))

;;;; MACROLET, FLET and LABELS

(test macrolet
  "Test for `macrolet' special operator."

  (is (equal '(syn::names (foo bar baz)
               syn::functions
               ((syn::parsed-lambda ((a b) () bla () nil ()) nil nil ((list a b)))
                (syn::parsed-lambda (() () nil () nil ()) nil nil ("not-doc-string"))
                (syn::parsed-lambda (() () nil () nil ()) "doc-string" nil (1)))
               syn::declarations nil
               syn::forms ((foo 1 2)))
             (syn:parse nil (syn:find-syntax 'macrolet) '(macrolet ((foo (a b &rest bla) (list a b))
                                                            (bar ()
                                                              "not-doc-string")
                                                            (baz ()
                                                              "doc-string"
                                                              1))
                                                   (foo 1 2)))))
  #+no (check-error-cases 'macrolet
                          '((macrolet) sb-kernel::arg-count-error)
                          '((macrolet ((f))) program-error)
                          '((macrolet ((f 1))) program-error))
  (check-roundtrip-cases 'macrolet
                         '(macrolet ())
                         '(macrolet ((f ())))
                         '(macrolet ((f (a &rest b) (declare (type string a)) a)))))

(test flet
  "Test for `flet' special operator."

  #+no (check-error-cases 'flet
                     '((flet) sb-kernel::arg-count-error)
                     '((flet ((f 1))) program-error))
  (check-roundtrip-cases 'flet
                         '(flet ())
                         '(flet ((f ())))
                         '(flet ((f (a &rest b) (declare (type string a)) a)))))

(test labels
  "Test for `labels' special operator."

  #+no (check-error-cases 'labels
                     '((labels) sb-kernel::arg-count-error)
                     '((labels ((f 1))) program-error))
  (check-roundtrip-cases 'labels
                         '(labels ())
                         '(labels ((f ())))
                         '(labels ((f (a &rest b) (declare (type string a)) a)))))

;;;; THE

(test the
  "Test for `the' special operator."

  (syn:parse nil (syn:find-syntax 'the) '(the integer x))

  (check-error-cases 'the
                     '((the) #+no sb-kernel::arg-count-error)
                     '((the symbol) #+no sb-kernel::arg-count-error)
                     '((the symbol value extra) #+no sb-kernel::arg-count-error))
  (check-roundtrip-cases 'the
                         '(the symbol 1)))

;;;; SETQ

(define-symbol-macro special-operators.setq.global
    (cdr foo))

(test setq
  "Test for `setq' special operator."

  (check-error-cases 'setq
                     '((setq a)        program-error)
                     '((setq a 1 b)    program-error)
                     '((setq 1 1)      program-error)
                     '((setq (1+ a) 1) program-error))
  (check-roundtrip-cases 'setq
                         '(setq)
                         '(setq a 1)
                         '(setq a 1 b 2)
                         '(setq special-operators.setq.global 1)))

;;;; THROW, CATCH and UNWIND-PROTECT

(test throw
  "Test for `throw' special operator."

  (syn:parse nil (syn:find-syntax 'throw) '(throw (+ 1 2) :value))
  #+no (check-error-cases 'throw
                     '((throw) sb-kernel::arg-count-error)
                     '((throw 'foo) sb-kernel::arg-count-error)
                     '((throw 'foo 1 extra) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'throw
                         '(throw 'foo 1)
                         '(throw (get-tag) 2)))

(test catch
  "Test for `catch' special operator."

  (is (equal '(syn::tag-form (+ 1 2) syn::forms (:value))
             (syn:parse nil (syn:find-syntax 'catch) '(catch (+ 1 2) :value))))
  #+no (check-error-cases 'catch
                     '((catch) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'catch
                         '(catch 'foo)
                         '(catch 'foo 1)
                         '(catch 'foo 1 2)
                         '(catch (get-tag))))

(test unwind-protect
  "Test for `unwind-protect' special operator."

  (is (equal '(syn::protected (progn 1 2) syn::cleanup (3 4))
             (syn:parse nil (syn:find-syntax 'unwind-protect) '(unwind-protect (progn 1 2) 3 4))))

  #+no (check-error-cases 'unwind-protect
                          '((unwind-protect) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'unwind-protect
                         '(unwind-protect foo)
                         '(unwind-protect foo bar)
                         '(unwind-protect foo bar baz)))

;;;; multiple-value stuff

(test multiple-value-call
  "Test for `multiple-value-call' special operator."

  (is (equal '(syn::function-form foo syn::arguments (1 2))
             (syn:parse nil (syn:find-syntax 'multiple-value-call) '(multiple-value-call foo 1 2))))
  #+no (check-error-cases 'multiple-value-call
                     '((multiple-value-call) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'multiple-value-call
                         '(multiple-value-call fun)
                         '(multiple-value-call fun 1)))

(test multiple-value-prog1
  "Test for `multiple-value-prog1' special operator."

  #+no (check-error-cases 'multiple-value-prog1
                     '((multiple-value-prog1) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'multiple-value-prog1
                         '(multiple-value-prog1 1)
                         '(multiple-value-prog1 1 2)
                         '(multiple-value-prog1 1 2 3)))

;;; Application

(test application
  "Test for the \"application\" pseudo-operator."

  (is (equal '(syn::abstraction foo syn::arguments ())
             (syn:parse nil (syn:find-syntax 'syn::application) '(foo))))
  (is (equal '(syn::abstraction foo syn::arguments (1))
             (syn:parse nil (syn:find-syntax 'syn::application) '(foo 1))))

  (is (equal '(syn::abstraction (((x) () nil () nil ()) nil () (x))
               syn::arguments (1))
             (syn:parse nil (syn:find-syntax 'syn::application) '((lambda (x) x) 1)))))
