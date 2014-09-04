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
    (apply #'map-product #'list named-lists)))      

(defun sum-out-var (node-with-cpt-of-interest node-to-eliminate)
  "implementation of SumOutVars on Darviche page 130"
  (assert (not (null node-to-eliminate)) () "The Node to eliminate is empty")
  (assert (> (length (node-variables node-with-cpt-of-interest)) 1) () "The CPT of interest has only one variable")
  ;; x = all nodes from the cpt of interest
  (let ((x (cons node-with-cpt-of-interest (node-parents node-with-cpt-of-interest)))  
	(z node-to-eliminate))
    (assert (member z x) () "The Var to be eliminated is not contained in the CPT of interest")
    (let ((base-cpt (node-cpt node-with-cpt-of-interest))
	  (y (remove z x)) ; removes z from x; y contains the nodes of the result after elimination
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
			  (format t "The base-value associated with the base-key ~S is ~S~%" base-key base-value)
		       )
		 (format t "The value associated with the key ~S is ~S~%" key value)
	      )
	result-cpt))))	
      
      #+-(let ((result-cpt-lhs 
		 (/ (node-cardinality node-with-potential-of-interest) (node node-to-eliminate)))))
      
      #+-(length result-potential) ;todo: implement double loop that sums out the var from node-to-elimininate	;)
      

#+(or)(defgeneric sum-out-var (node-with-potential-of-interest node-to-eliminate)
  (:documentation "implementation of SumOutVars on Darviche page 130")
  (:method (node-with-potential-of-interest node-to-eliminate)
    (let ((x (node-get-vars node-with-potential-of-interest))
          (z (node-name node-to-eliminate)))
      (if (null z) (error 'node-to-eliminate-is-empty))
      (if (null (first x)) (error 'node-with-potential-of-interest-has-no-var))
      (if (not (member z x)) (error 'var-to-be-eliminated-is-not-contained-in-the-potential-of-interest))
      (let ((y (remove z x))
            (result-potential (make-array 
                               (/ (node-cardinality node-with-potential-of-interest) (node node-to-eliminate)))))
        ;(loop for i from 0 to '(1 2 3) do (print i))
        (length result-potential) ;todo: implement double loop that sums out the var from node-to-elimininate
        ;)
        )
      )
    )
)
