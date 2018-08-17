(defsystem "syntax.concrete-syntax-tree"
  :depends-on ("syntax"

               "concrete-syntax-tree")

  :components ((:module     "concrete-syntax-tree"
                :pathname   "src/concrete-syntax-tree"
                :serial     t
                :components ((:file       "package")

                             (:file       "grammar")))))
