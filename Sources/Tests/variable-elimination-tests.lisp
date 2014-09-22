;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-variable-elimination-suite)

(defparameter *epsilon* 0.00000000001)

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

;; testing complete bayesnet multiplication from page 128 of darviche
(deftest test-multiply-factors ()
  (let ((bn-multiplication (multiply-factors (list (node-cpt *test-node-a*) 
                                                   (node-cpt *test-node-b*) 
                                                   (node-cpt *test-node-c*) 
                                                   (node-cpt *test-node-d*) 
                                                   (node-cpt *test-node-e*)))))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C T)(D T)(E T))) 0.06384)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C T)(D T)(E nil))) 0.02736)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C T)(D nil)(E T))) 0.00336)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C T)(D nil)(E nil))) 0.00144)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C nil)(D T)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C nil)(D T)(E nil))) 0.02160)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C nil)(D nil)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B T)(C nil)(D nil)(E nil))) 0.00240)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C T)(D T)(E T))) 0.21504)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C T)(D T)(E nil))) 0.09216)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C T)(D nil)(E T))) 0.05376)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C T)(D nil)(E nil))) 0.02304)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C nil)(D T)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C nil)(D T)(E nil))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C nil)(D nil)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A T)(B nil)(C nil)(D nil)(E nil))) 0.09600)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C T)(D T)(E T))) 0.01995)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C T)(D T)(E nil))) 0.00855)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C T)(D nil)(E T))) 0.00105)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C T)(D nil)(E nil))) 0.00045)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C nil)(D T)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C nil)(D T)(E nil))) 0.24300)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C nil)(D nil)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B T)(C nil)(D nil)(E nil))) 0.02700)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C T)(D T)(E T))) 0.00560)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C T)(D T)(E nil))) 0.00240)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C T)(D nil)(E T))) 0.00140)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C T)(D nil)(E nil))) 0.00060)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C nil)(D T)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C nil)(D T)(E nil))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C nil)(D nil)(E T))) 0)) *epsilon*))
    (is (< (abs (- (cpt-probability bn-multiplication '((A nil)(B nil)(C nil)(D nil)(E nil))) 0.0900)) *epsilon*))
))
