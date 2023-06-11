;;;; io-macros.lisp --- Standard macros for input and output.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax)

(parser:in-grammar special-operators)

;;;

(define-macro with-standard-io-syntax
    (list (* (<<- form ((form forms)))))
  ((form * :evaluation t)))

(define-macro with-input-from-string
    (list* (must (list (<- var    ((variable-name! names)))
                       (<- string ((form! forms)))
                       (* (or (seq :index (<- index ((form! forms))))
                              (seq :start (<- start ((form! forms))))
                              (seq :end   (<- end   ((form! forms)))))))
                 "must be of the form (VAR STRING &KEY INDEX START END)")
           (<- (declaration form) ((body forms))))
  ((var         1  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil))
   (string      1  :evaluation t)
   (index       ?  :evaluation t)
   (start       ?  :evaluation t)
   (end         ?  :evaluation t)
   (declaration *>)
   (form        *> :evaluation t)))

(define-macro with-output-to-string
    (list* (must (list (<- var ((variable-name! names)))
                       (? (seq (<- string ((form! forms)))
                               (? (seq :element-type (<- element-type ((form! forms))))))))
                 "must be of the form (VAR STRING &key ELEMENT-TYPE)")
           (<- (declaration form) ((body forms))))
  ((var          1  :evaluation (make-instance 'binding-semantics
                                               :namespace 'variable
                                               :scope     :lexical
                                               :values    nil))
   (string       ?  :evaluation t)
   (element-type ?  :evaluation t)
   (declaration  *>)
   (form         *> :evaluation t)))

(define-macro with-open-file
    (list* (must (list (<- stream   ((variable-name! names)))
                       (<- filespec ((form! forms)))
                       (* (<<- option ((form forms)))))
                 "must be of the form (STREAM FILESPEC OPTION*)")
           (<- (declaration form) ((body forms))))
  ((stream      1  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil))
   (filespec    1  :evaluation t)
   (option      *  :evaluation t)
   (declaration *>)
   (form        *> :evaluation t)))

(define-macro with-open-stream
    (list* (must (list (<- var   ((variable-name! names)))
                       (<- stream ((form! forms))))
                 "must be of the form (VAR STREAM)")
           (<- (declaration form) ((body forms))))
  ((var         1  :evaluation (make-instance 'binding-semantics
                                              :namespace 'variable
                                              :scope     :lexical
                                              :values    nil))
   (stream      1  :evaluation t)
   (declaration *>)
   (form        *> :evaluation t)))

;;; Formatted output

(define-macro formatter
    (list (must (<- control-string (and (guard :any (typep 'string))
                                        ((unparsed-expression forms)
                                         :format-control)))
                "must be a format control string"))
  ((control-string 1)))

;;; Pretty printer

(define-macro pprint-logical-block
    (list* (must (list (<- stream-symbol ((variable-name! names)))
                       (<- object ((form! forms)))
                       (* (or (seq :prefix          (<- prefix          ((form! forms))))
                              (seq :per-line-prefix (<- per-line-prefix ((form! forms)))) ; TODO prefix and per-line-prefix are mutually exclusive
                              (seq :suﬃx           (<- suﬃx            ((form! forms)))))))
                 "must be of the form (STREAM-DESIGNATOR OBJECT &key PREFIX PER-LINE-PREFIX SUFFIX)")
           (<- (declaration form) ((body forms))))
  ((stream-symbol   1  :evaluation (make-instance 'binding-semantics
                                                  :namespace 'variable
                                                  :scope     :lexical
                                                  :values    nil))
   (object          1  :evaluation t)
   (prefix          ?  :evaluation t)
   (per-line-prefix ?  :evaluation t)
   (suffix          ?  :evaluation t)
   (declaration     *>)
   (form            *> :evaluation t)))

(define-macro pprint-exit-if-list-exhausted
    (list* (must (list) "no arguments allowed"))
  ())

(define-macro pprint-pop
    (list* (must (list) "no arguments allowed"))
  ())

;;;

(define-macro print-unreadable-object
    (list* (must (list (<- object ((form! forms)))
                       (<- stream ((form! forms)))
                       (* (or (seq :type     (<- type     ((form! forms))))
                              (seq :identity (<- identity ((form! forms)))))))
                 "must be of the form (OBJECT STREAM &key TYPE IDENTITY)")
           (<- form ((forms forms))))
  ((object   1  :evaluation t)
   (stream   1  :evaluation t)
   (type     ?  :evaluation t)
   (identity ?  :evaluation t)
   (form     *> :evaluation t)))
