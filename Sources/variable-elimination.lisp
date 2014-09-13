;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(defparameter *trivial-element* '(empty 0))
(defparameter *neutral-addition-element* 0)
(defparameter *neutral-multiplication-element* 1)

#++(defun ve-pr1 (bayes-net nodes-to-keep nodes-to-eliminate-in-order)
  "Implementation of VE_PR1 of Darwiche on p. 134"
  (let ((s (cpts-of-bayes-net bayes-net)))
    (dolist (node-to-eliminate nodes-to-eliminate)
      (let ((cpts-containing-elimination-node nil)) 
        ))))

(defun cpt-contains-node-p (cpt node)
  "returns nil if the hashmap does not contain the node's name as a key"   
  (let ((result nil))
    (if (loop for key being the hash-key of cpt thereis (hashkey-contains-nodename-p key (node-name node)))
      (setf result T))
    result))

(defun hashkey-contains-nodename-p (key name)
  (let ((result nil))
    (dolist (node-name-value-pair key)
      (if (equal name (car node-name-value-pair))
          (setf result T)))
    result))

(defun cpts-of-bayes-net (bayes-net)
  "Returns a list of all CPTs (hastables containing the potentials) of the given bayes net"
  (let ((nodes (bayes-net-nodes bayes-net))
        (cpts nil))
    (dolist (node nodes)
      (setf cpts (cons (node-cpt node) 
                       cpts)))
    cpts ))

(defun build-cpt-lhs-for-given-nodes (node-list)
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
      

(defun sum-out-vars (node-with-cpt-of-interest nodes-to-eliminate)
  "implementation of SumOutVars on Darviche page 130"
  (assert (not (null nodes-to-eliminate)) () "The Node to eliminate is empty")
  ;; x = all nodes from the cpt of interest
  (let ((x (cons node-with-cpt-of-interest 
		 (node-parents node-with-cpt-of-interest)))  
	(z nodes-to-eliminate))
    (let* ((base-cpt (node-cpt node-with-cpt-of-interest))
	   (y (set-difference x z :test #'equal)) ; removes z from x; y contains the nodes of the result after elimination
	   (result-cpt (build-cpt-for-nodes y *neutral-addition-element*)))
      (loop for key being the hash-keys of result-cpt
	    using (hash-value value)
	    do 
	       (loop for base-key being the hash-keys of base-cpt
		     using (hash-value base-value)
		     do 
			(when (equal 
                               (length (intersection key base-key :test #'equal)) 
                               (length key))
			    ;; y instantiation is contained in x instatiation -> sum
			    ;; key is contained in base-key
                          (setf (gethash key result-cpt) (+ base-value (gethash key result-cpt))))
			(when (eq key *trivial-element*) 
                          (setf (gethash key result-cpt) (+ base-value (gethash key result-cpt))))
		     #+(or)(format t "The base-value associated with the base-key ~S is ~S~%" base-key base-value)
		     )
	    #+(or)(format t "The value associated with the key ~S is ~S~%" key value)
	    )
      ;; print the result
      (loop for key being the hash-keys of result-cpt
	    using (hash-value value)
	    do
	       (format t "The value associated with the key ~S is ~S~%" key value))
      result-cpt)))	

      
(defun multiply-factors (nodes-with-cpts-to-multiply)
  "implementation of MultiplyFactors on Darviche page 131"
  (assert (> (length nodes-with-cpts-to-multiply) 1) () "You must provide at least two nodes to multiply their CPTs.")
  ;; z = union-list of all nodes that exist are involved in the CPTs
  (let* ((z (get-all-nodes-that-exist-in-the-cpts nodes-with-cpts-to-multiply))
	 (result-cpt (build-cpt-for-nodes z *neutral-multiplication-element*)))
    (loop for result-key being the hash-keys of result-cpt
	  using (hash-value result-value)
	  do
	     (let ((cpts-to-multiply (get-all-cpts-of-nodes nodes-with-cpts-to-multiply)))
	       (dolist (cpt cpts-to-multiply)
		 (loop for cpt-key being the hash-keys of cpt
		     using (hash-value cpt-value)
		     do
			(if (equal 
			     (length (intersection cpt-key result-key :test #'equal)) 
			     (length cpt-key))
			    (setf (gethash result-key result-cpt) (* cpt-value (gethash result-key result-cpt)))
			    )
		       )
		 )
	       )
	  )
    result-cpt
    ))

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
