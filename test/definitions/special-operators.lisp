(cl:in-package #:syntax.test)

(def-suite* :syntax.special-operators
  :in :syntax)

;;; Utilities

(defun check-roundtrip (special-operator form)
  (let ((syntax   (find-syntax special-operator))
         ; (parser   (sb-c::special-operator-info-parser info))
         ; (unparser (sb-c::special-operator-info-unparser info))
        )
    (finishes (parse nil syntax form))
    ; (assert (equal form (parse nil syntax form)))
    ))

(defun check-roundtrip-cases (special-operator &rest forms)
  (dolist (form forms) (check-roundtrip special-operator form)))

(defun check-error (special-operator form expected-error)
  (let ((syntax (find-syntax special-operator))
         ; (parser (sb-c::special-operator-info-parser info))
        )
    (signals invalid-syntax-error
      (parse nil syntax form))))

(defun check-error-cases (special-operator &rest specs)
  (loop :for (form expected-error) :in specs
        :do (check-error special-operator form expected-error)))

;;;; Special operators for control

(test progn
  "Test for `progn' special operator."

  (check-error-cases
   'progn
   '((progn . 1) foo))

  (is (equal '(syntax::forms ())
             (parse nil (find-syntax 'progn) '(progn)))))

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

  (is (equal '(syntax::name foo syntax::forms (1))
             (parse nil (find-syntax 'block) '(block foo 1))))
  (check-error-cases 'block
                     '(block))
  (check-roundtrip-cases '(block foo a b)))

(test return-from
  "Test for `return-from' special operator."

  (is (equal '(syntax::name foo syntax::value 1)
             (parse nil (find-syntax 'return-from) '(return-from foo 1))))
  (is (equal '(syntax::name foo syntax::value nil)
             (parse nil (find-syntax 'return-from) '(return-from foo))))
  #+TODO (apply #'unparse-return-from-special-operator
                (parse-return-from-special-operator
                 (lambda (&rest args) (print args))
                 '(return-from foo bla))))

(test tagbody
  "Test for `tagbody' special operator."

  (parse nil (find-syntax 'tagbody) '(tagbody 0 1 "bla" "bli" 2 (list 1) (list 3)))
  (parse nil (find-syntax 'tagbody) '(tagbody (1+ a) (1+ b) :foo))

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

  (is (equal '(syntax::tag 1) (parse nil (find-syntax 'go) '(go 1)))))

;;;; Compiler-magic special forms
;;;; TODO test internal ones as well

(test eval-when
  "Test for `eval-when' special operator."

  (is (equal '(syntax::situations (:execute) syntax::forms (1))
             (parse nil (find-syntax 'eval-when) '(eval-when (:execute) 1))))
  ;; TODO
  )

(test load-time-value
  "Test for `load-time-value' special operator"

  (is (equal '(syntax::form foo syntax::read-only-p nil)
             (parse nil (find-syntax 'load-time-value) '(load-time-value foo nil))))

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

  (is (equal '(syntax::material 1)
             (parse nil (find-syntax 'quote) '(quote 1))))
  #+no (check-error-cases 'quote
                     '((quote) sb-kernel::arg-count-error)
                     '((quote x y) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'quote
                         '(quote 1)
                         '(quote x)
                         '(quote quote)))

(test function
  "Test for `function' special operator."

  (is (equal '(name foo
               syntax::lambda-list ()
               syntax::docstring nil
               syntax::declarations ()
               syntax::body ())
             (parse nil (find-syntax 'function) '(function foo))))
  (is (equal '(name (setf foo)
               syntax::lambda-list ()
               syntax::docstring nil
               syntax::declarations ()
               syntax::body ())
             (parse nil (find-syntax 'function) '(function (setf foo)))))
  (is (equal '(name nil
               syntax::lambda-list ((a) nil b nil nil ; nil
                                    )
               syntax::docstring nil
               syntax::declarations nil
               syntax::body ((foo)))
             (parse nil (find-syntax 'function) '(function (lambda (a &rest b) (foo))))))

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

  (is (equal '(syntax::names (foo bar)
               values (1 2)
               syntax::declarations nil
               syntax::forms ((list foo bar)))
             (parse nil (find-syntax 'symbol-macrolet)
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

  (is (equal '(syntax::names (foo bar) syntax::values (1 2) syntax::declarations () syntax::forms ((list foo bar)))
             (parse nil (find-syntax 'let) '(let ((foo 1) (bar 2)) (list foo bar)))))
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

  (is (equal '(syntax::declarations ((type integer x) (type double-float x)) syntax::forms (x))
             (parse nil (find-syntax 'locally) '(locally (declare (type integer x) (type double-float x)) x))))

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

  (is (equal '(syntax::names (foo bar baz)
               syntax::functions
               ((syntax::parsed-lambda ((a b) nil bla nil nil #+aux nil) nil nil ((list a b)))
                (syntax::parsed-lambda (nil nil nil nil nil #+aux nil) nil nil ("not-doc-string"))
                (syntax::parsed-lambda (nil nil nil nil nil #+aux nil) "doc-string" nil (1)))
               syntax::declarations nil
               syntax::forms ((foo 1 2)))
             (parse nil (find-syntax 'macrolet) '(macrolet ((foo (a b &rest bla) (list a b))
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

  (parse nil (find-syntax 'the) '(the integer x))

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

  (parse nil (find-syntax 'throw) '(throw (+ 1 2) :value))
  #+no (check-error-cases 'throw
                     '((throw) sb-kernel::arg-count-error)
                     '((throw 'foo) sb-kernel::arg-count-error)
                     '((throw 'foo 1 extra) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'throw
                         '(throw 'foo 1)
                         '(throw (get-tag) 2)))

(test catch
  "Test for `catch' special operator."

  (is (equal '(syntax::tag-form (+ 1 2) syntax::forms (:value))
             (parse nil (find-syntax 'catch) '(catch (+ 1 2) :value))))
  #+no (check-error-cases 'catch
                     '((catch) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'catch
                         '(catch 'foo)
                         '(catch 'foo 1)
                         '(catch 'foo 1 2)
                         '(catch (get-tag))))

(test unwind-protect
  "Test for `unwind-protect' special operator."

  (is (equal '(syntax::protected (progn 1 2) syntax::cleanup (3 4))
             (parse nil (find-syntax 'unwind-protect) '(unwind-protect (progn 1 2) 3 4))))

  #+no (check-error-cases 'unwind-protect
                          '((unwind-protect) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'unwind-protect
                         '(unwind-protect foo)
                         '(unwind-protect foo bar)
                         '(unwind-protect foo bar baz)))

;;;; multiple-value stuff

(test multiple-value-call
  "Test for `multiple-value-call' special operator."

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
