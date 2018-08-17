(defsystem "syntax"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ()

  :components ()

  :in-order-to ((test-op (test-op "syntax/test"))))

(defsystem "syntax/test"
  :depends-on ((:version "fiveam" "1.4")

               (:version "syntax" (:read-file-form "version-string.sexp")))

  :components ()

  :perform    (test-op (operation component)
                ))
