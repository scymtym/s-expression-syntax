(asdf:defsystem "s-expression-syntax"
  :description "A declarative description of Common Lisp's s-expression syntax."
  :license     "MIT"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "utilities.print-items"                  "0.3")

                (:version "parser.packrat"                         "0.1")

                (:version "architecture.builder-protocol"          "0.10")

                (:version "s-expression-syntax.expression-grammar" (:read-file-form "version-string.sexp")))

  :components  ((:module     "base"
                 :pathname   "code"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "semantics")

                              (:file       "database")

                              (:file       "macros")))

                (:module     "definitions"
                 :pathname   "code/definitions"
                 :depends-on ("base")
                 :serial     t
                 :components (; (:file       "package")

                              (:file       "names")

                              (:file       "types")
                              (:file       "declarations")
                              (:file       "forms")

                              (:file       "lambda-lists")

                              (:file       "grammar")
                              (:file       "bindings")
                              (:file       "pseudo-operators")
                              (:file       "special-operators")

                              (:file       "definition-macros")
                              (:file       "control-macros")
                              (:file       "debug-macros")
                              (:file       "iteration-macros")))

                (:file        "classify"
                 :pathname    "code/classify"
                 :depends-on  ("definitions")))

  :in-order-to ((test-op (test-op "s-expression-syntax/test"))))

(asdf:defsystem "s-expression-syntax/test"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ("alexandria"

               (:version "fiveam"              "1.4")

               (:version "s-expression-syntax" (:read-file-form "version-string.sexp")))

  :components ((:module     "test"
                :serial     t
                :components ((:file       "package")
                             (:file       "protocol")
                             (:file       "classify")))

               (:module     "definitions"
                :pathname   "test/definitions"
                :depends-on ("test")
                :serial     t
                :components ((:file       "names")

                             (:file       "types")
                             (:file       "declarations")
                             (:file       "forms")

                             (:file       "lambda-lists")

                             (:file       "bindings")
                             (:file       "pseudo-operators")
                             (:file       "special-operators")

                             (:file       "definition-macros")
                             (:file       "control-macros")
                             (:file       "debug-macros"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:s-expression-syntax.test '#:run-tests)))
