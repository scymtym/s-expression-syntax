(defsystem "syntax"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "parser.packrat"                "0.1")

               (:version "architecture.builder-protocol" "0.10"))

  :components ((:module     "src"
                :serial     t
                :components ((:file       "package")

                             (:file       "conditions")
                             (:file       "protocol")

                             (:file       "semantics")

                             (:file       "database")

                             (:file       "macros")))

               (:module     "definitions"
                :pathname   "src/definitions"
                :depends-on ("src")
                :serial     t
                :components (; (:file       "package")

                             (:file       "lambda-lists")

                             (:file       "grammar")

                             (:file       "bindings")
                             (:file       "declarations")
                             (:file       "forms")
                             (:file       "types")

                             ; (:file       "standard-macros")
                             (:file       "special-operators"))))

  :in-order-to ((test-op (test-op "syntax/test"))))

(defsystem "syntax/test"
  :depends-on ("alexandria"

               (:version "fiveam" "1.4")

               (:version "syntax" (:read-file-form "version-string.sexp")))

  :components ((:module     "test"
                :serial     t
                :components ((:file       "package")

                             (:file       "grammar")

                             (:file       "lambda-lists")

                             ; (:file       "bindings")
                             ; (:file       "declarations")
                             (:file       "forms")
                             ; (:file       "types")

                             (:file       "special-operators"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:syntax.test '#:run-tests)))
