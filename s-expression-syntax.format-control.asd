(defsystem "s-expression-syntax.format-control"
  :description "A declarative description of Common Lisp's format control syntax."
  :license     "MIT"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "parser.packrat"                "0.1")
                (:version "parser.packrat.grammar.string" "0.1")

                (:version "architecture.builder-protocol" "0.10"))

  :components  ((:module     "format-control"
                 :pathname   "code/format-control"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "rules"))))

  :in-order-to ((test-op (test-op "s-expression-syntax.format-control/test"))))

(defsystem "s-expression-syntax.format-control/test"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ("alexandria"

               (:version "fiveam"                             "1.4")

               (:version "s-expression-syntax.format-control" (:read-file-form "version-string.sexp"))

               (:version "s-expression-syntax/test"           (:read-file-form "version-string.sexp")))

  :components ((:module     "format-control"
                :pathname   "test/format-control"
                :serial     t
                :components ((:file       "package")
                             (:file       "rules"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:s-expression-syntax.format-control.test '#:run-tests)))
