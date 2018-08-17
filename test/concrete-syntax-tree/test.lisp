(parser:defgrammar lambda-lists
  (:class syntax.concrete-syntax-tree::cst-grammar #+normally parser.packrat.grammar.sexp:sexp-grammar))
(parser:in-grammar lambda-lists)

(parser.packrat:parse `(specialized-lambda-list ,(make-hash-table :test #'eq))
                      (cst:cstify `(,(cst:cstify '(a integer)) b c &optional d &rest r &key e)))
