;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-sum-out-vars ()
  (let ((summed-out-d (sum-out-vars *test-node-d* (list *test-node-d*)))
        (summed-out-db (sum-out-vars *test-node-d* (list *test-node-d* *test-node-b*)))
        (summed-out-dbc (sum-out-vars *test-node-d* (list *test-node-d* *test-node-b* *test-node-c*))))
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
    (signals error (sum-out-vars *test-node-d* nil))))
