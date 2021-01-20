;;;; rules.lisp --- Parsing of format control directives.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.format-control)

;;; The rules in this grammar describe the syntax of format directive
;;; arguments and format directives.
;;;
;;; All directive rules take an argument which describes the lexical
;;; context surrounding the directive. The context is a stack of
;;; symbols which is initially (nil). When directives such as ~{ are
;;; parsed, a symbol such as `:iteration' is pushed onto the
;;; stack. Directives encountered between ~{ and ~} are processed with
;;; this augmented stack. This allows checking problems such as ~^
;;; occurring outside of ~{…~} or ~; occurring after ~:;.
(parser:defgrammar format-control
  (:class parser.packrat.grammar.string::simple-string-grammar))

(parser:in-grammar format-control)

;;; Utilities

(defun in-context? (context stack)
  (labels ((check-one (context)
             (find context stack :test #'eq)))
    (if (consp context)
        (some #'check-one context)
        (check-one context))))

(defun in-direct-context? (context stack)
  (eq context (first stack)))

;;; Directive arguments

(defrule possibly-colon-and-at ()
    (or (seq #\: (? #\@))
        (seq #\@ (? #\:))
        (seq)))

(defrule possibly-argument ()
    (or (seq #\' :any)
        (+ (<<- digits (guard digit-char-p))) #\#
        #\v #\V))

(defrule colon-and-at (allowed)
    (:transform (or (seq (<- colon? #\:) (? (<- at?    #\@)))
                    (seq (<- at?    #\@) (? (<- colon? #\:)))
                    (seq))
     (let ((index (logior (if colon? 1 0) (if at? 2 0))))
       (unless (logbitp index allowed)
         (:fatal
          (case index
            (0 "directive requires colon (@) or at-sign (@)")
            (1 "directive does not accept colon (:)")
            (2 "directive does not accept at-sign (@)")
            (3 "directive does not accept both colon and at-sign (:@)"))))))
  (list colon? at?))

(defrule numeric-argument ()
    (or (:transform
           (bounds (start end)
             (+ (<<- digits (guard digit-char-p)))) ; TODO sign
         (let ((value (parse-integer (coerce (reverse digits) 'string))))
           (bp:node* (:numeric-argument :value  value
                                        :bounds (cons start end)))))
        (:transform
           (bounds (start end) (or #\v #\V))
         (bp:node* (:v-argument :bounds (cons start end))))
        (:transform
           (bounds (start end) #\#)
           (bp:node* (:#-argument :bounds (cons start end))))))

(defrule character-argument ()
    (or (:transform
           (bounds (start end)
             (seq #\' (must character "character must follow single quote in character argument")))
         (bp:node* (:character-argument :value  character
                                        :bounds (cons start end))))
        (:transform
           (bounds (start end) (or #\v #\V))
         (bp:node* (:v-argument :bounds (cons start end))))
        (and #\#
             (must (:transform (seq) (:fail))
                   "# cannot be used as a character argument"))
        (and (guard digit-char-p)
             (must (:transform (seq) (:fail))
                   "number literal cannot be used as a character argument"))))

;;; Directives

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *directive-rules* '()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun colon-and-at-mask (colon-allowed? at-sign-allowed? both-allowed?)
    (logior #b0001                  ; neither @ nor : is always allowed
            (if colon-allowed?   #b0010 0)
            (if at-sign-allowed? #b0100 0)
            (if both-allowed?    #b1000 0))))

;;; Each element of PARAMETERS is either
;;; 1. An assignment expression of the form (<- NAME EXPRESSION)
;;; 2. A symbol
;;; 3. The symbol `&sub-directives'
(defmacro define-directive-rule ((name character)
                                 (colon-allowed? at-sign-allowed? both-allowed?
                                  &rest parameters)
                                 (&rest body)
                                 &body body-expressions)
  (let ((rule-name     (a:symbolicate '#:directive- name))
        (character     (let ((upper-case (char-upcase character)))
                         (if (char= character upper-case)
                             character
                             `(or ,character ,upper-case))))
        (colon-at-mask (colon-and-at-mask
                        colon-allowed? at-sign-allowed? both-allowed?)))
    (multiple-value-bind (body-parameters sub-directives?)
        (a:if-let ((index (position '&sub-directives body)))
          (values (subseq body 0 index) t)
          (values body                  nil))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (pushnew ',rule-name *directive-rules*))

         (defrule ,rule-name (context)
             ;; The first leg of the `and' checks whether the
             ;; directive character in the input matches CHARACTER but
             ;; does not check argument count or syntax. If the
             ;; character matches, the arguments are parsed again,
             ;; this time with strict checking.
             ;; TODO the directive rule should do this once and dispatch based on the result
             (and (seq #\~
                       (? (possibly-argument)) (* (seq #\, (? (possibly-argument))))
                       (possibly-colon-and-at)
                       ,character)
                  (bounds (start end)
                    (seq #\~
                         ,@(when #+no unless #+no (every (a:of-type '(and symbol
                                                                      (not (eql '&sub-directives))))
                                                         parameters)
                                 parameters
                                 (list (reduce (lambda (parameter right)
                                                 (let ((expression (if (eq (first parameter) '<<-)
                                                                       `(seq (? ,parameter)
                                                                             (* (seq #\, (? ,parameter))))
                                                                       `(? ,parameter))))
                                                   (if right
                                                       `(seq ,expression (? (seq #\, ,right)))
                                                       expression)))
                                               parameters
                                               :from-end      t
                                        ; :end           (1- (length parameters))
                                        ; :initial-value `(? ,(a:lastcar parameters))
                                               :initial-value nil
                                               )))
                         (<- (colon? at?)
                             (colon-and-at ,colon-at-mask))
                                    ,character
                                    ,@body-expressions)))
             context               ; "use" context
             (bp:make+finish-node+relations*
              ',name
              (nconc ,@(a:mappend (lambda (parameter)
                                    (destructuring-bind (name &optional (expression name))
                                        (a:ensure-list parameter)
                                      (let ((keyword (a:make-keyword name)))
                                        `((when ,name
                                            (list ,keyword ,expression))))))
                                  (list* 'colon? 'at? body-parameters))
                     (list :bounds (cons start end)))
              (list ,@(when parameters
                        (a:mappend (lambda (parameter)
                                     (when (consp parameter)
                                       (destructuring-bind (kind name expression) parameter
                                         (declare (ignore expression))
                                         `((list ',(if (eq kind '<-) 'bp:? '*)
                                                 ,(a:make-keyword name)
                                                 ,name)))))
                                   parameters))
                    ,@(when sub-directives?
                        '((list '* :sub-directive (nreverse sub-directives))))))
                  #+no (bp:node* (,name :colon? colon?
                                        :at     at?
                                        :bounds (cons start end))
                         ,@(when parameters
                             (map 'list (lambda (parameter)
                                          (destructuring-bind (_ name __) parameter
                                            `(bp:? ,(a:make-keyword name) ,name)))
                                  parameters))
                         ,@(when sub-directives?
                             '((* :sub-directive (nreverse sub-directives))))))))))

;;; Basic output

(define-directive-rule (:character #\C) (t t t) ())


(macrolet ((define (name character)
             `(define-directive-rule (,name ,character)
                  (nil nil nil (<- count (numeric-argument)))
                  ())))
  (define :newline    #\%)
  (define :fresh-line #\&)
  (define :page       #\|)
  (define :tilde      #\~))

;;; Radix control

(define-directive-rule (:radix #\r) (t t t (<- radix (numeric-argument))) ())

(macrolet ((define (name character radix)
             )) )

;;; Printer operations

(define-directive-rule (:aesthetic #\a) (t t t
                                         (<- mincol (numeric-argument))
                                         (<- colinc (numeric-argument))
                                         (<- minpad (numeric-argument))
                                         (<- padchar (character-argument)))
    ())

(define-directive-rule (:standard #\s) (t t t
                                        (<- mincol (numeric-argument))
                                        (<- colinc (numeric-argument))
                                        (<- minpad (numeric-argument))
                                        (<- padchar (character-argument)))
    ())

(define-directive-rule (:write #\w) (t t t) ())

;;; Pretty printer operations

(define-directive-rule (:conditional-newline #\_) (t t t) ())

(define-directive-rule (:logical-block #\<) (t t t) (&sub-directives) ; TODO justification vs. logical block, prefix and suffix cannot contain directives, ~^ is allowed
    (* (<<- sub-directives (content context)))
  (must (seq (<- (colon2? at2?) (colon-and-at 0))#\~ #\>) "missing closing ~>"))

(define-directive-rule (:indent #\i) (t nil nil (<- count (numeric-argument)))
    ())

(define-directive-rule (:call-function #\/) (t t t
                                             (<<- argument (or (numeric-argument)
                                                               (character-argument))))
    ((name (coerce (nreverse name) 'string)))
  (must (+ (and (not #\/) (<<- name))) "function name must follow ~/")
  (must #\/ "missing closing /"))

;;; Layout control

;;; Control flow operations

(define-directive-rule (:goto #\*) (t t nil (<- count (numeric-argument))) ())

(define-directive-rule (:conditional #\[) (t t nil
                                           (<- clause-number (numeric-argument)))
    (&sub-directives)
  (and (<- new-context (:transform (seq) (list* :conditional context)))
       (* (or (:transform
               (<<- sub-directives (<- node (directive-clause-separator new-context)))
               (when (getf (bp:node-initargs* node) :colon?)
                 (setf new-context (list* :conditional/rest (rest new-context)))))
              (<<- sub-directives (content new-context)))))
  (must (seq #\~ #\]) "missing closing ~]"))

(define-directive-rule (:iteration #\{) (t t t
                                         (<- limit (numeric-argument)))
    (&sub-directives)
  (and (<- new-context (:transform (seq) (list* :iteration context)))
       (* (<<- sub-directives (content new-context))))
  (must (seq #\~ #\}) "missing closing ~}"))

(define-directive-rule (:recursive #\?) (nil t nil) ())

;;; Miscellaneous operations

(define-directive-rule (:case-conversion #\() (t t t) (&sub-directives)
  (* (<<- sub-directives (content context)))
  (must (seq #\~ #\)) "missing closing ~)"))

;;; Pseudo operations

(define-directive-rule (:clause-separator #\;) (t ; in ~[ and ~<
                                                t ; in ~<
                                                nil)
    ()
  (and (must (:transform (seq) (when (in-direct-context? :conditional/rest context)
                                 (:fail)))
             "clause separator (~;) after \"else\" clause separator (~:;)")
       (must (:transform (seq) (unless (in-direct-context? :conditional context)
                                 (:fail)))
             "clause separator (~;) not directly surrounded by conditional context (~[…~])")
       (seq)))

(define-directive-rule (:escape #\^) (t ; only in ~:{ or ~:@{
                                      nil nil
                                      (<- first  (numeric-argument))
                                      (<- second (numeric-argument))
                                      (<- third  (numeric-argument)))
    ()
  (and (must (:transform (seq) (unless (in-context? '(:iteration :justification)
                                                    context)
                                 (:fail)))
             "escape upward (~^) outside of iteration or justification context")
       (seq)))

(define-directive-rule (:ignored-newline #\Newline) (t t nil) ()
  (? (and (:transform :any (unless at? (:fail)))
          (* #\Space))))

;;; Entry point

(macrolet ((define ()
             `(defrule directive (context)
                  (or ,@(loop :for rule :in *directive-rules*
                              :collect `(,rule context))))))
  (define))

(defrule escaped-tilde () ; TODO also defined as directive
    (seq #\~ #\~)
  #\~)

(defrule simple-text ()
    (bounds (start end)
      (+ (<<- characters (or (escaped-tilde) (and (not #\~) :any)))))
  (bp:node* (:simple-text :content (coerce (nreverse characters) 'string)
                          :bounds  (cons start end))))

(defrule content (context)
    (or (directive context)
        (simple-text)))

(defrule format-control ()
    (bounds (start end)
      (* (<<- elements (content 'nil))))
  (bp:node* (:format-control :bounds (cons start end))
    (* :element (nreverse elements))))
