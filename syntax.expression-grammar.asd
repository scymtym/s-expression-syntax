(asdf:defsystem "syntax.expression-grammar"
  :description "This system enables the syntax system to process non-s-expr representations."
  :license     "MIT"

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "parser.packrat" "0.1"))

  :components ((:module     "expression-grammar"
                :pathname   "src/expression-grammar"
                :serial     t
                :components ((:file       "package")

                             (:file       "protocol")
                             (:file       "stubs")
                             (:file       "grammar")))))
