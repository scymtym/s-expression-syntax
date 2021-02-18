;;;; stubs.lisp --- Inlinable functions for common operations.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:s-expression-syntax.expression-grammar)

(declaim (inline %%natural? %natural?
                 %listp %null %first %rest
                 %symbol-name %symbol-package
                 %typep %equal %eql))

(defun %%natural? (thing)
  (typep thing '(or list symbol package array character number)))

(defun %natural? (thing)
  (or (%%natural? thing)
      (natural? *client* thing)))

(defun %naturalize (thing)
  (if (%natural? thing)
      thing
      (naturalize *client* thing)))

;;; List operations

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

;;; Symbols

(defun %symbol-name (symbol)
  (if (%natural? symbol)
      (symbol-name symbol)
      (symbol-name* *client* symbol)))

(defun %symbol-package (symbol)
  (if (%natural? symbol)
      (symbol-package symbol)
      (symbol-package* *client* symbol))) ; TODO a lexical variable to *client* in compile-rule method

(defun %package-name (symbol)
  (if (%natural? symbol)
      (package-name symbol)
      (package-name* *client* symbol)))

;;; Comparison

(defun %typep (thing type)
  (if (%natural? thing)
      (typep thing type)
      (typep* *client* thing type)))

(defun %equal (left right)
  (if (%natural? left)
      (equal left right)
      (equal* *client* left right)))

(defun %eql (left right)
  (cond ((eql left right) t)
        ((%natural? left) nil)
        (t                (eql* *client* left right))))
