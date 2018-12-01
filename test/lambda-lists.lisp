(cl:in-package #:syntax.test)

(def-suite* :syntax.lambda-lists)

;;; Ordinary lambda-list

(test ordinary-lambda-list
  "Smoke test for the `ordinary-lambda-list' rule."

  (is-true (parser.packrat:parse `(syntax::ordinary-lambda-list ,(make-hash-table :test #'eq))
                                 '(foo bar &optional (hash-table-rehash-size default)
                                   &rest x
                                   &key ((:x-kw y) 1 supplied?) b &allow-other-keys
                                   ; &aux (a 1)
                                   )
                                 :grammar 'syntax::lambda-lists))
  (is-true (parser.packrat:parse `(syntax::ordinary-lambda-list ,(make-hash-table :test #'eq))
                                 '(foo foo2 &rest pie &key ((:foo bar) :default bar-p)
                                   ; &aux (a 1) b
                                   )
                                 :grammar 'syntax::lambda-lists)))

;;; Specialized lambda-list

(test specialized-lambda-list
  "Smoke test for the `specialized-lambda-list' rule."

  (is-true (parser.packrat:parse `(syntax::specialized-lambda-list ,(make-hash-table :test #'eq))
                                 '((foo bar) (baz fez) &rest whoop)
                                 :grammar 'syntax::lambda-lists))
  (is-false (parser.packrat:parse `(syntax::specialized-lambda-list ,(make-hash-table :test #'eq))
                                  '((foo bar) (baz fez) &rest foo)
                                  :grammar 'syntax::lambda-lists)))

;;; Destructuring Lambda-list

(test destructuring-lambda-list
  "Smoke test for the `destructuring-lambda-list' rule."

  (is-true (parser.packrat:parse `(syntax::destructuring-lambda-list ,(make-hash-table :test #'eq))
                                 '((foo bar))
                                 :grammar 'syntax::lambda-lists))
  (is-true (parser.packrat:parse `(syntax::destructuring-lambda-list ,(make-hash-table))
                                 '(&whole whole (foo &key a) . (&rest fez))
                                 :grammar 'syntax::lambda-lists)))
