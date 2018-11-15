(cl:in-package #:syntax)

(define-macro defun
    (list* (:guard name symbolp)
           (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
           (:compose (docstring-body) (list documentation declarations forms)))
  ((name          1)
   (lambda-list   1)
   (documentation ?)
   (declarations  *>)
   (forms         *>)))

(define-macro defclass
    (list (:guard name symbolp)
          (list (* (<<- superclasses (:guard symbolp))))
          (list (* (<<- slots (list (* :any)))))
          (* (or (list :default-initargs (* (:seq (<<- default-initargs  (:guard symbolp))
                                                  (<<- default-initforms :any))))
                 (list :metaclass        (:guard metaclass symbolp))
                 (list :documentation    (:guard documentation stringp))
                 (list* (<<- option-names (:guard keywordp)) (<<- option-values)))))
  ((name              1)
   (superclasses      *)
   (slots             *)
   ;; Standard options
   (default-initargs  *)
   (default-initforms *)
   (metaclass         ?)
   (documentation     ?)
   ;; Non-standard options
   (option-names      *)
   (option-values     *)))

(define-macro defgeneric
    (list name
          (<- lambda-list ((ordinary-lambda-list lambda-lists) 'nil))
          (* (or (list :generic-function-class    (:guard generic-function-class symbolp))
                 (list :argument-precedence-order (<- argument-precedence-order (or :most-specific-first
                                                                                    :most-specific-last)))
                 (list :documentation             (:guard documentation stringp))
                 (list* (<<- option-names (:guard keywordp)) (<<- option-values)))))
  ((name                      1)
   (lambda-list               1)
   ;; Standard options
   (generic-function-class    ?)
   (argument-precedence-order ?)
   (documentation             ?)
   ;; Other options
   (option-names              *)
   (option-values             *)))
