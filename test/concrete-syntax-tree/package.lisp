(cl:defpackage #:syntax.concrete-syntax-tree.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:syntax.concrete-syntax-tree.test)

(def-suite :syntax.concrete-syntax-tree)

(defun run-tests ()
  (run! :syntax.concrete-syntax-tree))
