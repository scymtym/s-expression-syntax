(cl:in-package #:syntax.concrete-syntax-tree)

(defclass cst-client ()
  ())

(defvar *client* (make-instance 'cst-client))

;;;

(defmethod eg:natural? ((client cst-client) (maybe-list t))
  t)

(defmethod eg:natural? ((client cst-client) (maybe-list cst:cst))
  nil)

;;;

(defmethod eg:naturalize ((client cst-client) (maybe-list cst:cst))
  (cst:raw maybe-list))

;;;

(defmethod eg:listp* ((client cst-client) (maybe-list t))
  nil)

(defmethod eg:listp* ((client cst-client) (maybe-list cst:cons-cst))
  t)

(defmethod eg:listp* ((client cst-client) (maybe-list cst:cst))
  (cst:null maybe-list))

;;;

(defmethod eg:null* ((client cst-client) (maybe-list t))
  nil)

(defmethod eg:null* ((client cst-client) (maybe-list cst:cst))
  (cst:null maybe-list))

;;;

(defmethod eg:first* ((client cst-client) (maybe-list cst:cst))
  (cst:first maybe-list))

;;;

(defmethod eg:rest* ((client cst-client) (maybe-list cst:cst))
  (cst:rest maybe-list))

;;;

(defmethod eg:equal* ((client cst-client) (left cst:cst) (right t))
  (equal (cst:raw left) right))

;;;

(defmethod eg:eql* ((client cst-client) (left cst:cst) (right t))
  (eql (cst:raw left) right))
