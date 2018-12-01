(cl:in-package #:syntax.expression-grammar)

(declaim (inline %%natural? %natural? %listp %null %first %rest %equal %eql))

(defun %%natural? (thing)
  (typep thing '(or list symbol string character number)))

(defun %natural? (thing)
  (or (%%natural? thing)
      (natural? *client* thing)))

(defun %listp (maybe-list)
  (cond ((listp maybe-list)      t)
        ((%%natural? maybe-list) nil)
        (t                       (listp* *client* maybe-list))))

(defun %null (maybe-list)
  (cond ((null maybe-list)       t)
        ((eq maybe-list '%null)  t)
        ((%%natural? maybe-list) nil)
        (t                       (null* *client* maybe-list))))

(defun %first (maybe-list)
  (if (listp maybe-list)
      (first maybe-list)
      (first* *client* maybe-list)))

(defun %rest (maybe-list)
  (if (listp maybe-list)
      (rest maybe-list)
      (rest* *client* maybe-list)))

(defun %equal (left right)
  (if (%natural? left)
      (equal left right)
      (equal* *client* left right)))

(defun %eql (left right)
  (cond ((eql left right) t)
        ((%natural? left) nil)
        (t                (eql* *client* left right))))
