;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;;; Nodes
;;; =====

(deftype node-kind ()
  '(member :nature :decision :utility :assumption))

(define-class node ()
  (name
   (parents :initform '())
   potential
   (kind :type node-kind)))

(defgeneric node-type (node)
  (:documentation "Return the type of NODE (as Lisp type)."))


(defgeneric node-values (node)
  (:documentation "Return the possible values for node's domain.")
  (:method (node)
    (values-for-type (node-type node))))

(defgeneric values-for-type (type)
  (:documentation "Return all values for type, if there are finitely many."))


(defgeneric node-cardinality (node)
  (:documentation "Return the number of possible values in the node's domain.
If the domain is infinite, return NIL.")
  (:method (node)
    (length (node-values node))))


(defgeneric node-discrete-p (node)
  (:documentation "Return true if NODE is discrete, false otherwise")
  (:method (node)
    (not (null (node-cardinality node)))))

(defun node-potential-dimensions (node)
  (cons (node-cardinality node) (mapcar #'node-cardinality (node-parents node))))

(defgeneric node-inverse-mapping (node)
  (:documentation "Return the mapping from values to indices in the node potential."))

(defgeneric node-probability (node node-value &rest parent-values)
  (:documentation "Compute the probabilitye of (NODE-VALUE PARENT-VALUES) for NODE.")
  (:method (node node-value &rest parent-values)
    (apply #'aref (node-potential node)
           (compute-node-value-index node node-value parent-values))))

(defgeneric compute-node-value-index (node node-value parent-values)
  (:documentation "Compute the index of (NODE-VALUE . PARENT-VALUES) in NODE's potential.")
  (:method (node node-value parent-values)
    (let ((node-index (gethash node-value (node-inverse-mapping node)))
          (parent-indices
            (mapcar (lambda (value node)
                      (gethash value (node-inverse-mapping node)))
                    parent-values (node-parents node))))
      (cons node-index parent-indices))))


;;; Discrete Nodes
;;; ==============

(define-class discrete-node (node)
  ((values :type sequence)
   (inverse-mapping))
  (:conc-name node))

(defmethod initialize-instance :after ((node discrete-node) &key)
  (unless (slot-boundp node 'inverse-mapping)
    (let ((table (make-hash-table :test 'equal))
          (values (node-values node)))
      (dotimes (i (length values))
        (setf (gethash (elt values i) table) i))
      (setf (node-inverse-mapping node) table))))

(defmethod node-type ((node discrete-node))
  `(member ,@(coerce (node-values node) 'list)))

(defmethod node-discrete-p ((node discrete-node))
  (declare (ignorable node))
  t)
