
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;;; a cpt contains a hashtable for the given vars
(defstruct cpt
  hashtable
  vars)

(defun cpt-contains-node (cpt node)
  "checks if the cpt contains the node's name in its vars"
  (let ((var (node-name node))
	(vars (cpt-vars cpt)))
    (member var vars :test #'equal)))

(defun build-cpt-lhs-for-given-nodes (node-list)
  "creates all possible combinations of node-name and node-value pairs -> lhs of a factor"
  (let ((named-lists '()))
    (dolist (node node-list)
      (setf named-lists (cons (node-get-named-value-list node) 
			      named-lists)))
    (if (> (length named-lists) 0) 
	(apply #'map-product #'list named-lists)
	(list *trivial-element*))))

(defun build-cpt-for-nodes (nodes init-potential-value)
  "returns a hashtable filled with all node-name and -value combinations of the given nodes as keys and with the init-potential-value as values"
  (let* ((result-cpt (make-hash-table :test #'equal))
	 (cpt-lhs (build-cpt-lhs-for-given-nodes nodes))
	 (cpt-rhs (make-array (length cpt-lhs) :initial-element init-potential-value)))
    (dotimes (i (length cpt-lhs))
      (setf (gethash (elt cpt-lhs i) result-cpt) (elt cpt-rhs i)))
    result-cpt))

(defun get-all-cpts-of-nodes (nodes)
  "returns all cpts of given nodes as a list of hash-tables"
  (let ((list-of-cpts nil))
    (dolist (node nodes)
      (setf list-of-cpts (cons (node-cpt node) list-of-cpts)))
    list-of-cpts))

(defun get-all-nodes-that-exist-in-the-cpts (node-list)
  (let ((result-list nil))
    (dolist (node node-list) 
      (setf result-list (union result-list (list node)))
      (setf result-list (union result-list (node-parents node))))
    result-list))
