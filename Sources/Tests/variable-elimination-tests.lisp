;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-sum-out-vars ()
  (let* ((var-d (node-var *test-node-d*))
         (var-b (node-var *test-node-b*))
         (var-c (node-var *test-node-c*))
         (summed-out-d (sum-out-vars (node-cpt *test-node-d*) (list var-d)))
         (summed-out-db (sum-out-vars (node-cpt *test-node-d*) (list var-d var-b)))
         (summed-out-dbc (sum-out-vars (node-cpt *test-node-d*) (list var-d var-b var-c))))
    (is (equal (hash-table-count (cpt-hashtable summed-out-d)) 4))
    (is (equal (hash-table-count (cpt-hashtable summed-out-db)) 2))
    (is (equal (hash-table-count (cpt-hashtable summed-out-dbc)) 1))
    (loop for key being the hash-keys of (cpt-hashtable summed-out-d)
	  using (hash-value value)
	  do
	     (is (= 1.0 value)))
    (loop for key being the hash-keys of (cpt-hashtable summed-out-db)
	  using (hash-value value)
	  do
	     (is (= 2.0 value)))
    (loop for key being the hash-keys of (cpt-hashtable summed-out-dbc)
	  using (hash-value value)
	  do
	     (is (= 4.0 value)))
    (is (equal (sum-out-vars (node-cpt *test-node-d*) nil) (node-cpt *test-node-d*)))))
