;;; -*- Mode: Lisp; common-lisp-style: poem -*-
;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(defparameter *empty-element* '(empty 0))
(defparameter *neutral-addition-element* 0)
(defparameter *neutral-multiplication-element* 1)

#+(or)(defun ve-pr1 (bayes-net nodes-to-keep nodes-to-eliminate-in-order)
  "Implementation of VE_PR1 of Darwiche on p. 134"
  (let ((s (bn-cpts bayes-net)))
    (dolist (node-to-eliminate nodes-to-eliminate-in-order)
      (let ((cpts-containing-elimination-node (cpts-containing-node s node-to-eliminate)))
        (let ((f (multiply-factors cpts-containing-elimination-node)))
          (let ((f-at-i (sum-out-var f node-to-eliminate)))
            ;; replace all factors in S that were multiplied before by f-at-i
          
            ))))
    ;; return multiplication of all factors in S
    ))

#+-(defun sum-out-vars (cpt vars-to-sum-out)
  "implementation of SumOutVars on Darviche page 130"
  (if (null vars-to-sum-out) 
      cpt ; return cpt if there are no vars to be summed out
      (let ((x (cpt-vars cpt))  
            (z cpts-to-eliminatevars-to-sum-out))
        (let* ((y (set-difference x z :test #'equal)) ; removes z from x; y contains the nodes of the result after elimination
               (result-cpt (build-cpt-for-nodes y *neutral-addition-element* *empty-element*)))
          (loop for key being the hash-keys of result-cpt
                using (hash-value value)
                do 
                   (loop for base-key being the hash-keys of cpt
                         using (hash-value base-value)
                         do 
                            (when (equal 
                                   (length (intersection key base-key :test #'equal)) 
                                   (length key))
                              ;; y instantiation is contained in x instatiation -> sum
                              ;; key is contained in base-key
                              (setf (gethash key result-cpt) (+ base-value (gethash key result-cpt))))
                            (when (eq key *empty-element*) 
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
  
#+(or)(defun sum-out-vars (node-with-cpt-of-interest nodes-to-eliminate)
  "implementation of SumOutVars on Darviche page 130"
  (assert (not (null nodes-to-eliminate)) () "The Node to eliminate is empty")
  ;; x = all nodes from the cpt of interest
  (let ((x (cons node-with-cpt-of-interest 
		 (node-parents node-with-cpt-of-interest)))  
	(z nodes-to-eliminate))
    (let* ((base-cpt (node-cpt node-with-cpt-of-interest))
	   (y (set-difference x z :test #'equal)) ; removes z from x; y contains the nodes of the result after elimination
	   (result-cpt (build-cpt-for-nodes y *neutral-addition-element* *empty-element*)))
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
			(when (eq key *empty-element*) 
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

      
#+(or)(defun multiply-factors (nodes-with-cpts-to-multiply)
  "implementation of MultiplyFactors on Darviche page 131"
  (assert (> (length nodes-with-cpts-to-multiply) 1) () "You must provide at least two nodes to multiply their CPTs.")
  ;; z = union-list of all nodes that exist are involved in the CPTs
  (let* ((z (get-all-nodes-that-exist-in-the-cpts nodes-with-cpts-to-multiply))
	 (result-cpt (build-cpt-for-nodes z *neutral-multiplication-element* *empty-element*)))
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

