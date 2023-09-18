;;;; forms.lisp --- Tests for form-related rules.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.test)

(def-suite* :s-expression-syntax.forms
  :in :s-expression-syntax)

(test place
  "Smoke test for the `place' rule."
  (rule-test-cases ((syn::place syn::forms))
    ;; Invalid syntax
    '(#1=1
      nil #1# nil)
    '(#2=:foo
      nil #2# nil)
    '(#3=nil
      nil #3# nil)
    '(#4=t
      nil #4# nil)
    '(#5=(declare)
      :fatal #5# "declare is not allowed here")
    ;; Valid syntax
    '(#6=foo
      t #6# (:unparsed () :expression #6# :context :place :source #6#))
    '(#7=(foo 1)
      t #7# (:unparsed () :expression #7# :context :place :source #7#))))

(test place!
  "Smoke test for the `place!' rule."
  (rule-test-cases ((syn::place! syn::forms))
    ;; Invalid syntax
    '(#1=1
      :fatal #1# "place must be a cons or a variable name")
    '(#2=:foo
      :fatal #2# "place must not be a keyword")
    '(#3=nil
      :fatal #3# "place must not be a constant variable")
    '(#4=t
      :fatal #4# "place must not be a constant variable")
    '(#5=(declare)
      :fatal #5# "declare is not allowed here")))

(test body
  "Smoke test for the `body' rule."
  (rule-test-cases ((syn::body syn::forms))
    ;; Invalid declarations
    '(((declare #1=1))        :fatal #1# "must be a declaration specifier")
    '(((declare #2=()))       :fatal #2# "must be a declaration specifier")
    '(((declare (#3=1)))      :fatal #3# "declaration identifier must be a symbol")
    '(((declare (type #4=1))) :fatal #4# "must be a type specifier")
    ;; Empty
    '(() t nil (() ()))
    ;; No declarations
    '((#5=1)
      t nil (()
             ((:unparsed () :expression 1 :context :form :source #5#))))
    '((#6=1 #7=2)
      t nil (()
             ((:unparsed () :expression 1 :context :form :source #6#)
              (:unparsed () :expression 2 :context :form :source #7#))))
    ;; Valid declarations
    '((#8=(declare #9=(ignore #10=a)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #10#))))
                   :kind ignore :source #9#))))
               :source #8#))
             ()))
    '((#11=(declare #12=(type #13=bit)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #13#))))
                                       :source #13#))))
                   :kind type :source #12#))))
               :source #11#))
             ()))
    '((#14=(declare #15=(type #16=bit #17=a)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #16#))))
                                       :source #16#))
                                     ((:variable-name () :name a :source #17#))))
                   :kind type :source #15#))))
               :source #14#))
             ()))
    '((#18=(declare #19=(type #20=bit #21=a #22=b)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:atomic-type-specifier
                                       ((:name . 1) (((:type-name () :name bit :source #20#))))
                                       :source #20#))
                                     ((:variable-name () :name a :source #21#))
                                     ((:variable-name () :name b :source #22#))))
                   :kind type :source #19#))))
               :source #18#))
             ()))
    ;; Multiple declarations
    '((#23=(declare #24=(ignore #25=a)) #26=(declare #27=(ignore #28=b)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #25#))))
                   :kind ignore :source #24#))))
               :source #23#)
              (:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name b :source #28#))))
                   :kind ignore :source #27#))))
               :source #26#))
             ()))
    ;; Declarations and forms
    '((#29=(declare #30=(ignore #31=a)) #32=3 #33=4)
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #31#))))
                   :kind ignore :source #30#))))
               :source #29#))
             ((:unparsed () :expression 3 :context :form :source #32#)
              (:unparsed () :expression 4 :context :form :source #33#))))))

(test docstring-body
  "Smoke test for the `docstring-body' rule."
  (rule-test-cases ((syn::docstring-body syn::forms))
    ;; Empty
    '(() t nil (nil () ()))
    ;; Only forms
    '((#1="foo") t nil
      (nil () ((:unparsed () :expression "foo" :context :form :source #1#))))
    ;; Declarations and docstrings
    '(#2=(#3=(declare #4=(ignore #5=a)))
      t #2# (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #5#))))
                   :kind ignore :source #4#))))
               :source #3#))
             nil
             ()))
    '(#6=(#7=(declare #8=(ignore #9=a)) #10="foo")
      t #6# (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #9#))))
                   :kind ignore :source #8#))))
               :source #7#))
             nil
             ((:unparsed () :expression "foo" :context :form :source #10#))))
    '((#11="foo" #12=(declare #13=(ignore #14=a)))
      t nil (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #14#))))
                   :kind ignore :source #13#))))
               :source #12#))
             (:documentation () :string "foo" :source #11#)
             ()))
    ;; Forms and docstrings
    '((#15=1)
      t nil (nil () ((:unparsed () :expression 1 :context :form :source #15#))))
    '((#16="foo" #17=1)
      t nil (()
             (:documentation () :string "foo" :source #16#)
             ((:unparsed () :expression 1 :context :form :source #17#))))
    '((#18=1 #19="foo")
      t nil (nil () ((:unparsed () :expression 1     :context :form :source #18#)
                     (:unparsed () :expression "foo" :context :form :source #19#))))))

(test tagbody-body
  "Smoke test for the `tagbody-body' rule."
  (rule-test-cases ((syn::tagbody-body syn::forms))
    ;; Invalid syntax
    '(((declare #1=1))
      :fatal #1# "must be a declaration specifier")
    '((nil #2=nil)
      :fatal #2# "the tag NIL occurs more than once")
    '((2 #3=2)
      :fatal #3# "the tag 2 occurs more than once")
    ;; Valid syntax
    '(#4=()
      t #4# (() ()))
    '(#5=(#6=(declare #7=(ignore #8=a)))
      t #5# (((:declaration
               ((:declaration-specifier . *)
                (((:declaration-specifier
                   ((:argument . *) (((:variable-name () :name a :source #8#))))
                   :kind ignore :source #7#))))
               :source #6#))
             ()))
    '(#9=(#10=7)
      t #9# (()
             ((:tagbody-segment
               ((:label . 1) (((:tag () :name 7 :source #10#)
                               :evaluation (:binding :namespace syn::tag
                                                     :scope     :lexical))))
               :source #10#))))
    '(#11=(#12=(list))
      t #11# (()
             ((:tagbody-segment
               ((:statement . *) (((:unparsed
                                    ()
                                    :expression (list)
                                    :context    :form
                                    :source     #12#)
                                   :evaluation t)))
               :source #12#))))))
