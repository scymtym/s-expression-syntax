(cl:in-package #:syntax.test)

(in-suite* :syntax.standard-macros
  :in :syntax)

(test defun
 (parse t (find-syntax 'defun) (cst:cstify '(defun foo (bar baz)
                                             "bla"
                                             (declare (type integer bar baz))
                                             (+ 1 2)))))

(test defclass
  "Test for `declass' standard macro."

  (is (equal '(name foo
               syntax::superclasses (bar baz)
               syntax::slots ((foo))
               syntax::default-initargs (:bar)
               syntax::default-initforms (1)
               syntax::metaclass foo
               documentation nil
               syntax::option-names (:my-class-option)
               syntax::option-values ((1)))
             (parse t (find-syntax 'defclass)
                    '(defclass foo (bar baz)
                      ((foo :initform (+ 1) :custom-option :foo :reader bar))
                      (:metaclass foo)
                      (:default-initargs
                       :bar 1)
                      (:my-class-option 1))))))

(test defgeneric
  "Test for `defgeneric' standard macro."

  (is (equal '(name foo
               syntax::lambda-list ((b a) nil nil nil nil)
               syntax::generic-function-class nil
               syntax::argument-precedence-order nil
               documentation "bla"
               syntax::option-names (:generic-function-class)
               syntax::option-values (("bla")))
             (parse t (find-syntax 'defgeneric)
                    '(defgeneric foo (a b)
                      (:documentation "bla")
                      (:generic-function-class "bla"))))))
