
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;;; a cpt contains a hashtable for the given vars
(define-class cpt ()
  (hashtable
  (vars :type sequence)))

(defun cpt-contains-node-p (cpt node)
  "checks if the cpt contains the node"
  (let* ((vars (cpt-vars cpt))
	 (nodes (mapcar #'var-node vars)))
    (member node nodes :test #'equal)))

(defun cpt-contains-var-p (cpt var)
  "checks if the cpt contains the var"
  (member var (cpt-vars cpt) :test #'equal))

(defun cpt-containing-var (cpt var)
  "returns the cpt if it contains the given var"
  (if (cpt-contains-var-p cpt var) 
      cpt 
      nil))

(defun cpt-containing-node (cpt node)
  "returns the cpt if it contains the given var"
  (if (cpt-contains-node-p cpt node) 
      cpt 
      nil))

(defun cpts-containing-var (cpts var)
  (let ((result nil))
    (dolist (cpt cpts) 
      (when (cpt-contains-var-p cpt var)
	(setf result (cons cpt result))))
    result))

(defun cpts-containing-node (cpts node)
  "returns all cpts that contain the node"
  (let ((result nil))
    (dolist (cpt cpts)
      (when (cpt-contains-node-p cpt node)
	(setf result (cons cpt result))))
    result))

(defun build-cpt-lhs-for-given-nodes (node-list empty-element)
  "creates all possible combinations of node-name and node-value pairs -> lhs of a factor"
  (let ((named-lists '()))
    (dolist (node node-list)
      (setf named-lists (append named-lists 
				(list (node-get-named-value-list node)))))
    (if (> (length named-lists) 0) 
	(apply #'map-product #'list named-lists)
	(list empty-element))))

(defun build-cpt-for-nodes (nodes init-potential-value empty-element)
  "returns a hashtable filled with all var-name and -value combinations of the given nodes as keys and with the init-potential-value as values"
  (let* ((result-cpt-hashtable (make-hash-table :test #'equal))
	 (cpt-lhs (build-cpt-lhs-for-given-nodes nodes empty-element))
	 (cpt-rhs (make-array (length cpt-lhs) :initial-element init-potential-value))
	 (vars (mapcar (lambda (x) (node-var x)) nodes)))
    (dotimes (i (length cpt-lhs))
      (setf (gethash (elt cpt-lhs i) result-cpt-hashtable) (elt cpt-rhs i)))
    (make-cpt :hashtable result-cpt-hashtable :vars vars)))

(defun build-cpt-for-vars (vars init-potential-value empty-element)
  "this is just a wrapper function for more comfort"
  (let ((nodes (mapcar #'var-node vars)))
    (build-cpt-for-nodes nodes init-potential-value empty-element)))

(defun get-all-cpts-of-nodes (nodes)
  "returns all cpts of given nodes"
  (mapcar #'node-cpt nodes))

(defun get-all-nodes-that-exist-in-the-cpts (node-list)
  (let ((result-list nil))
    (dolist (node node-list) 
      (setf result-list (union result-list (list node)))
      (setf result-list (union result-list (node-parents node))))
    result-list))
