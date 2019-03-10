(asdf:defsystem "syntax.concrete-syntax-tree"
  :description "Enables the syntax system to process concrete syntax trees."
  :license     "MIT"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("syntax"

                "concrete-syntax-tree")

  :components  ((:module     "concrete-syntax-tree"
                 :pathname   "src/concrete-syntax-tree"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "grammar"))))

  :in-order-to ((test-op (test-op "syntax.concrete-syntax-tree/test"))))

(asdf:defsystem "syntax.concrete-syntax-tree/test"
  :version      (:read-file-form "version-string.sexp")
  :depends-on   ((:version "fiveam"                      "1.4")
                 (:version "syntax.concrete-syntax-tree" (:read-file-form "version-string.sexp")))

  :components  ((:module     "concrete-syntax-tree"
                 :pathname   "test/concrete-syntax-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "grammar"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:syntax.concrete-syntax-tree.test '#:run-tests)))
