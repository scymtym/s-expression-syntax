(asdf:defsystem "syntax.concrete-syntax-tree"
  :description "This system enables the syntax system to process concrete syntax trees."
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

                              (:file       "grammar")))))
