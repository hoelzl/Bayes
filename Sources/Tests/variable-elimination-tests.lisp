;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-sum-out-var ()
  (let* ((node-a (make-discrete-node
                 :domain-values '(t nil)
                 :kind :nature
                 :potential (make-array '(2) 
					:initial-contents '(0.6 0.4))
                 :name 'A))
	 (node-b (make-discrete-node
                   :domain-values '(t nil)
                   :kind :nature
                   :potential (make-array '(4) 
					  :initial-contents '(0.2 0.8 0.75 0.25)) 
                   :name 'B
                   :parents (list node-a)))
	 (node-c (make-discrete-node 
                     :domain-values '(t nil)
                     :kind :nature 
                     :potential (make-array '(4) 
					    :initial-contents '(0.8 0.2 0.1 0.9)) 
                     :name 'C
                     :parents (list node-a)))
	 (node-d (make-discrete-node 
                       :domain-values '(t nil) 
                       :kind :nature 
                       :potential (make-array '(8) 
					      :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
                       :name 'D
                       :parents (list node-b node-c)))
	 (summed-out-d (sum-out-vars node-d (list node-d)))
	 (summed-out-db (sum-out-vars node-d (list node-d node-b)))
	 (summed-out-dbc (sum-out-vars node-d (list node-d node-b node-c))))
    (is (equal (hash-table-count summed-out-d) 4))
    (is (equal (hash-table-count summed-out-db) 2))
    (is (equal (hash-table-count summed-out-dbc) 1))
    (loop for key being the hash-keys of summed-out-d
	  using (hash-value value)
	  do
	     (is (= 1.0 value)))
    (loop for key being the hash-keys of summed-out-db
	  using (hash-value value)
	  do
	     (is (= 2.0 value)))
    (loop for key being the hash-keys of summed-out-dbc
	  using (hash-value value)
	  do
	     (is (= 4.0 value)))
    (signals error (sum-out-vars node-d nil))))
