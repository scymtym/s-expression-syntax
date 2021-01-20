(asdf:defsystem "s-expression-syntax.concrete-syntax-tree"
  :description "Enables the s-expression-syntax system to process concrete syntax trees."
  :license     "MIT"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("s-expression-syntax"

                "concrete-syntax-tree")

  :components  ((:module     "concrete-syntax-tree"
                 :pathname   "code/concrete-syntax-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "client"))))

  :in-order-to ((test-op (test-op "s-expression-syntax.concrete-syntax-tree/test"))))

(asdf:defsystem "s-expression-syntax.concrete-syntax-tree/test"
  :version      (:read-file-form "version-string.sexp")
  :depends-on   ((:version "fiveam"                                   "1.4")

                 (:version "s-expression-syntax.concrete-syntax-tree" (:read-file-form "version-string.sexp"))

                 (:version "s-expression-syntax/test"                 (:read-file-form "version-string.sexp")))

  :components  ((:module     "concrete-syntax-tree"
                 :pathname   "test/concrete-syntax-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "client"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:s-expression-syntax.concrete-syntax-tree.test '#:run-tests)))
