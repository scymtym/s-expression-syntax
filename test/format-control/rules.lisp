;;;; rules.lisp --- Tests for format control rules.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.format-control.test)

(def-suite* :s-expression-syntax.format-control)

;;; Directive arguments

(test at-and-colon
  "Test for the `at-and-colon' rule."
  (rule-test-cases ((at-and-colon format-control))))

(test numeric-argument
  "Test for the `numeric-argument' rule."
  (rule-test-cases ((numeric-argument format-control))
    '("1234" t nil (:numeric-argument () :value 1234 :bounds (0 . 4)))
    '("v"    t nil (:v-argument       ()             :bounds (0 . 1)))
    '("#"    t nil (:#-argument       ()             :bounds (0 . 1)))))

(test character-argument
  "Test for the `character-argument' rule."
  (rule-test-cases ((character-argument format-control))
    '("'"  :fatal 1   "character must follow single quote in character argument")
    '("#"  :fatal 1   "# cannot be used as a character argument")
    '("1"  :fatal 1   "number literal cannot be used as a character argument")

    '("'A" t      nil (:character-argument () :value #\A :bounds (0 . 2)))
    '("v"  t      nil (:v-argument         ()            :bounds (0 . 1)))))

;;; Directives

;;; Basic output

(test directive-character
  "Test for the `directive-character' rule."
  (rule-test-cases ((directive-character format-control) ''(nil))
    '("~C"   t      nil (:character ()                      :bounds (0 . 2)))
    '("~:C"  t      nil (:character () :colon? #\:          :bounds (0 . 3)))
    '("~@C"  t      nil (:character ()             :at? #\@ :bounds (0 . 4)))
    '("~:@C" t      nil (:character () :colon? #\: :at? #\@ :bounds (0 . 4)))))

(test directive-newline
  "Test for the `directive-newline' rule."
  (rule-test-cases ((directive-newline format-control) ''(nil))
    '("~:%"  :fatal 2   "directive does not accept colon (:)")
    '("~@%"  :fatal 2   "directive does not accept at-sign (@)")
    '("~:@%" :fatal 3   "directive does not accept both colon and at-sign (:@)")

    '("~%"   t      nil (:newline () :bounds (0 . 2)))
    '("~2%"  t      nil (:newline
                         (:count (((:numeric-argument () :value 2 :bounds (1 . 2)))))
                         :bounds (0 . 3)))))

(test directive-fresh-line
  "Test for the `directive-fresh-line' rule."
  (rule-test-cases ((directive-fresh-line format-control) ''(nil))
    '("~:&"  :fatal 2   "directive does not accept colon (:)")
    '("~@&"  :fatal 2   "directive does not accept at-sign (@)")
    '("~:@&" :fatal 3   "directive does not accept both colon and at-sign (:@)")

    '("~&"   t      nil (:fresh-line () :bounds (0 . 2)))
    '("~2&"  t      nil (:fresh-line
                         (:count (((:numeric-argument () :value 2 :bounds (1 . 2)))))
                         :bounds (0 . 3)))))

(test directive-page
  "Test for the `directive-page' rule."
  (rule-test-cases ((directive-page format-control) ''(nil))
    '("~:|"  :fatal 2   "directive does not accept colon (:)")
    '("~@|"  :fatal 2   "directive does not accept at-sign (@)")
    '("~:@|" :fatal 3   "directive does not accept both colon and at-sign (:@)")

    '("~|"   t      nil (:page () :bounds (0 . 2)))
    '("~2|"  t      nil (:page
                         (:count (((:numeric-argument () :value 2 :bounds (1 . 2)))))
                         :bounds (0 . 3)))))

(test directive-tilde
  "Test for the `directive-tilde' tilde."
  (rule-test-cases ((directive-tilde format-control) ''(nil))
    '("~:~"  :fatal 2   "directive does not accept colon (:)")
    '("~@~"  :fatal 2   "directive does not accept at-sign (@)")
    '("~:@~" :fatal 3   "directive does not accept both colon and at-sign (:@)")

    '("~~"   t      nil (:tilde () :bounds (0 . 2)))
    '("~2~"  t      nil (:tilde
                         (:count (((:numeric-argument () :value 2 :bounds (1 . 2)))))
                         :bounds (0 . 3)))))

(test directive-conditional-newline
  "Test for the `directive-conditional-newline' rule."
  (rule-test-cases ((directive-conditional-newline format-control))))



(test directive-logical-block
  "Test for the `directive-logical-block' rule."
  (rule-test-cases ((directive-logical-block format-control))))

(test directive-indent
  "Test for the `directive-indent' rule."
  (rule-test-cases ((directive-indent format-control))))

(test directive-call-function
  "Test for the `directive-call-function' rule."
  (rule-test-cases ((directive-call-function format-control) ''(nil))
    ;; Invalid syntax
    '("~/"     :fatal 2   "function name must follow ~/")
    '("~/foo"  :fatal 5   "missing closing /")
    ;; Valid syntax
    '("~/foo/" t      nil (:call-function () :name "foo" :bounds (0 . 6)))))

(test directive-goto
  "Test for the `directive-goto' rule."
  (rule-test-cases ((directive-goto format-control) ''(nil))
    '("~*"   t nil (:goto () :bounds (0 . 2)))
    '("~1*"  t nil (:goto
                    (:count (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                    :bounds (0 . 3)))
    '("~1:*" t nil (:goto
                    (:count (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                    :colon? #\: :bounds (0 . 4)))))

(test directive-conditional
  "Test for the `directive-conditional' rule."
  (rule-test-cases ((directive-conditional format-control) ''(nil))
    '("~["                 :fatal 2   "missing closing ~]")
    '("~[~:;~;~]"          :fatal 6   "clause separator (~;) after \"else\" clause separator (~:;)")

    ; '("~:@[~]"             t      nil (:conditional () :colon? #\: :at #\@ :bounds (0 . 6)))

    ; '("~[foo~:*bar~]"      t      nil ())
    ; '("~:@[~Wfoobar~;~W~]" t      nil ())
    ; '("~2[foo~;~]"         t      nil ())
    ; '("~[~{foo~;~}~]"      t      nil ())
    ; '("~2[a~;b~:;c~;d~]"   t      nil ())

    '("~V[foo~;~]"
      t nil (:conditional
             (:sub-directive (((:simple-text () :content "foo" :bounds (3 . 6)))
                              ((:clause-separator () :bounds (6 . 8))))
              :clause-number (((:v-argument () :bounds (1 . 2)))))
             :bounds (0 . 10)))

    '("~#[foo~;~]"
      t nil (:conditional
             (:sub-directive (((:simple-text () :content "foo" :bounds (3 . 6)))
                              ((:clause-separator () :bounds (6 . 8))))
              :clause-number (((:#-argument () :bounds (1 . 2)))))
             :bounds (0 . 10)))))

(test directive-iteration
  "Test for the `directive-iteration' rule."
  (rule-test-cases ((directive-iteration format-control) ''(nil))
    '("~{"     :fatal 2   "missing closing ~}")

    '("~{~}"   t      nil (:iteration () :bounds (0 . 4)))
    '("~:@{~}" t      nil (:iteration () :colon? #\: :at? #\@ :bounds (0 . 6)))
    '("~{~W~}" t      nil (:iteration
                           (:sub-directive (((:write () :bounds (2 . 4)))))
                           :bounds (0 . 6)))
    '("~{~^~}" t      nil (:iteration
                           (:sub-directive (((:escape () :bounds (2 . 4)))))
                           :bounds (0 . 6)))))

(test directive-recursive
  "Test for the `directive-recursive' rule."

  (rule-test-cases ((directive-recursive format-control))))

(test directive-case-conversion
  "Test for the `directive-case-conversion' rule."
  (rule-test-cases ((directive-case-conversion format-control)
                    ''(nil))
    '("~("     :fatal 2   "missing closing ~)")

    '("~(~)"   t      nil (:case-conversion ()                      :bounds (0 . 4)))
    '("~:(~)"  t      nil (:case-conversion () :colon? #\:          :bounds (0 . 5)))
    '("~@(~)"  t      nil (:case-conversion ()             :at? #\@ :bounds (0 . 5)))
    '("~:@(~)" t      nil (:case-conversion () :colon? #\: :at? #\@ :bounds (0 . 6)))
    '("~@:(~)" t      nil (:case-conversion () :colon? #\: :at? #\@ :bounds (0 . 6)))))

(test directive-clause-separator
  "Test for the `directive-clause-separator' rule."
  (rule-test-cases ((directive-clause-separator format-control)
                    ''(:conditional))
    '("~;"   t nil (:clause-separator ()                      :bounds (0 . 2)))
    '("~:;"  t nil (:clause-separator () :colon? #\:          :bounds (0 . 3)))
    '("~@;"  t nil (:clause-separator ()             :at? #\@ :bounds (0 . 3)))
    '("~:@;" t nil (:clause-separator () :colon? #\: :at? #\@ :bounds (0 . 4)))
    '("~@:;" t nil (:clause-separator () :colon? #\: :at? #\@ :bounds (0 . 4)))))

(test directive-escape
  "Test for the `directive-escape' rule."
  (rule-test-cases ((directive-escape format-control) ''(nil))
    '("~^" :fatal 2 "escape upward (~^) outside of iteration or justification context"))

  (rule-test-cases ((directive-escape format-control) ''(:iteration))
    '("~^"      t nil (:escape () :bounds (0 . 2)))
    '("~1^"     t nil (:escape
                       (:first (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                       :bounds (0 . 3)))
    '("~1,2^"   t nil (:escape
                       (:second (((:numeric-argument () :value 2 :bounds (3 . 4))))
                        :first  (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                       :bounds (0 . 5)))
    '("~1,2,3^" t nil (:escape
                       (:third  (((:numeric-argument () :value 3 :bounds (5 . 6))))
                        :second (((:numeric-argument () :value 2 :bounds (3 . 4))))
                        :first  (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                       :bounds (0 . 7)))))

(test directive-ignored-newline
  "Test for the `directive-ignored-newline' rule."
  (rule-test-cases ((directive-ignored-newline format-control) ''(nil))
    '("~
"
      t nil (:ignored-newline () :bounds (0 . 2)))
    '("~@
  "
      t nil (:ignored-newline () :at? #\@ :bounds (0 . 5)))))

(test directive-aesthetic
  "Test for the `directive-aesthetic' rule."
  (rule-test-cases ((directive-aesthetic format-control) ''(nil))
    '("~A"        t nil (:aesthetic () :bounds (0 . 2)))
    '("~1A"       t nil (:aesthetic
                         (:mincol (((:numeric-argument () :value 1 :bounds (1 . 2)))))
                         :bounds (0 . 3)))
    '("~:@A"      t nil (:aesthetic
                         ()
                         :colon? #\: :at? #\@ :bounds (0 . 4)))
    '("~,,,'XA"   t nil (:aesthetic
                         (:padchar (((:character-argument () :value #\X :bounds (4 . 6)))))
                         :bounds (0 . 7)))
    '("~1,2,3:@A" t nil (:aesthetic
                         (:mincol (((:numeric-argument () :value 1 :bounds (1 . 2))))
                          :colinc (((:numeric-argument () :value 2 :bounds (3 . 4))))
                          :minpad (((:numeric-argument () :value 3 :bounds (5 . 6)))))
                         :colon? #\: :at? #\@ :bounds (0 . 9)))))

(test directive-standard
  "Test for the `directive-standard' rule."
  (rule-test-cases ((directive-standard format-control) ''(nil))
    '("~S" t nil (:standard () :bounds (0 . 2)))))

(test directive-write
  "Test for the `directive-write' rule."
  (rule-test-cases ((directive-write format-control) ''(nil))
    '("~W" t nil (:write () :bounds (0 . 2)))))

;;; Entry point

(test format-control
  "Test for the `format-control' rule."

  (rule-test-cases ((format-control format-control))
    '("foo~~bar~
baz"
      t nil
      (:format-control
       (:element (((:simple-text () :content "foo~bar" :bounds (0 . 8)))
                  ((:ignored-newline () :bounds (8 . 10)))
                  ((:simple-text () :content "baz" :bounds (10 . 13)))))
       :bounds (0 . 13)))))
