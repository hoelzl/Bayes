;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(defun build-cpt-lhs-for-given-nodes (node-list)
  (let ((named-lists '()))
    (dolist (node node-list)
      (setf named-lists (cons (node-get-named-value-list node) 
			      named-lists)))
    (if (> (length named-lists) 0) 
	(apply #'map-product #'list named-lists)
	'((trivial-factor 0)))))      

(defun sum-out-vars (node-with-cpt-of-interest nodes-to-eliminate)
  "implementation of SumOutVars on Darviche page 130"
  (assert (not (null nodes-to-eliminate)) () "The Node to eliminate is empty")
  ;; x = all nodes from the cpt of interest
  (let ((x (cons node-with-cpt-of-interest 
		 (node-parents node-with-cpt-of-interest)))  
	(z nodes-to-eliminate))
    (let ((base-cpt (node-cpt node-with-cpt-of-interest))
	  (y (set-difference x z :test #'equal)) ; removes z from x; y contains the nodes of the result after elimination
	  (result-cpt (make-hash-table :test 'equal)))
      (let* ((result-cpt-lhs (build-cpt-lhs-for-given-nodes y))
	     (result-cpt-rhs (make-array (length result-cpt-lhs) :initial-element 0)))
	(dotimes (i (length result-cpt-lhs))
	  (setf (gethash (elt result-cpt-lhs i) result-cpt) (elt result-cpt-rhs i)))
	(loop for key being the hash-keys of result-cpt
	      using (hash-value value)
	      do 
		 (loop for base-key being the hash-keys of base-cpt
		       using (hash-value base-value)
		       do 
			  (if (equal 
			       (length (intersection key base-key :test #'equal)) 
			       (length key))
			      ;; y instantiation is contained in x instatiation -> sum
			      ;; key is contained in base-key
			      (setf (gethash key result-cpt) (+ base-value (gethash key result-cpt))))
			  (if (equal key '(trivial-factor 0)) 
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
	result-cpt))))	
      
