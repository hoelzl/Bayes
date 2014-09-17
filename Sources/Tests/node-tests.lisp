;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-discrete-node-type-1 ()
  (let ((n1 (make-discrete-node :name 'list-values
                                :var (make-discrete-var :name 'A :domain-values '(a b c)) 
                                :potential-rhs '(0.1 0.2 0.7)))
        (n2 (make-discrete-node :name 'vector-values
                                :var (make-discrete-var :name 'B :domain-values #(a b c))
                                :potential-rhs '(0.4 0.2 0.4))))
    (is (equal (node-type n1) '(member a b c)))
    (is (equal (node-type n2) '(member a b c)))
))

(deftest test-node-get-named-value-lists ()
  (let* ((n (make-discrete-node :name 'N
                                :var (make-discrete-var :name 'N :domain-values '(1 2 3))
                                :potential-rhs '(0.1 0.15 0.1)))
         (domain-values-of-n '((N 1) (N 2) (N 3)))
         (expected-result-for-n (list domain-values-of-n)))
    (is (equal expected-result-for-n (node-get-named-value-lists n)))
    (let* ((m (make-discrete-node :name 'M
                                  :var (make-disrete-var :name 'M :domain-values '(6 7 8))
                                  :potential-rhs '(0.2 0.3 0.4 0.1 0.5 0.2 0.7 0.9 0.1)
                                  :parents (list n)))
           (domain-values-of-m '((M 6) (M 7) (M 8)))
           (expected-result-for-m (list domain-values-of-n domain-values-of-m)))
      (is (equal domain-values-of-m (node-get-named-value-list m)))
      (is (equal expected-result-for-m (node-get-named-value-lists m))))))


(deftest test-node-structure ()
  (is (member *test-node-a* (node-parents *test-node-b*))) ; check if a is parent of b
  (is (member *test-node-a* (node-parents *test-node-c*))) ; check if a is parent of c
  (is (member *test-node-b* (node-parents *test-node-d*))) 
  (is (member *test-node-c* (node-parents *test-node-d*)))
  (is (member *test-node-c* (node-parents *test-node-e*)))
  (is (member *test-node-a* (node-parents (first (node-parents *test-node-e*))))))
  
(deftest test-node-potential-dimensions ()
  (is (equal (node-potential-dimensions *test-node-a*) '(2)))
  (is (equal (node-potential-dimensions *test-node-b*) '(2 2)))
  (is (equal (node-potential-dimensions *test-node-c*) '(2 2)))
  (is (equal (node-potential-dimensions *test-node-d*) '(2 2 2)))
  (is (equal (node-potential-dimensions *test-node-e*) '(2 2))))

(deftest test-node-probability ()
  (is (equal 0.6 (node-probability *test-node-a* '((A t)))))
  (is (equal 0.4 (node-probability *test-node-a* '((A nil)))))
  (is (equal 0.2 (node-probability *test-node-b* '((A t) (B t)))))
  (is (equal 0.2 (node-probability *test-node-b* '((B t) (A t)))))
  (is (equal 0.8 (node-probability *test-node-b* '((A t) (B nil)))))
  (is (equal 0.8 (node-probability *test-node-b* '((B nil) (A t)))))
  (is (equal 0.75 (node-probability *test-node-b* '((A nil) (B t)))))
  (is (equal 0.75 (node-probability *test-node-b* '((B t) (A nil)))))
  (is (equal 0.25 (node-probability *test-node-b* '((A nil) (B nil)))))
  (is (equal 0.25 (node-probability *test-node-b* '((B nil) (A nil)))))
  ;; checking if probability accepts any order of inputs
  (is (equal 0.95 (node-probability *test-node-d* '((B t) (C t) (D t)))))
  (is (equal 0.95 (node-probability *test-node-d* '((B t) (D t) (C t)))))
  (is (equal 0.95 (node-probability *test-node-d* '((C t) (B t) (D t)))))
  (is (equal 0.95 (node-probability *test-node-d* '((C t) (D t) (B t)))))
  (is (equal 0.95 (node-probability *test-node-d* '((D t) (B t) (C t)))))
  (is (equal 0.95 (node-probability *test-node-d* '((D t) (C t) (B t)))))
  (is (equal 0.05 (node-probability *test-node-d* '((B t) (C t) (D nil)))))
  (is (equal 0.9 (node-probability *test-node-d* '((B t) (C nil) (D t)))))
  (is (equal 0.1 (node-probability *test-node-d* '((B t) (C nil) (D nil)))))
  (is (equal 0.1 (node-probability *test-node-d* '((B t) (D nil) (C nil)))))
  (is (equal 0.1 (node-probability *test-node-d* '((C nil) (B t) (D nil)))))
  (is (equal 0.1 (node-probability *test-node-d* '((C nil) (D nil) (B t)))))
  (is (equal 0.1 (node-probability *test-node-d* '((D nil) (B t) (C nil)))))
  (is (equal 0.1 (node-probability *test-node-d* '((D nil) (C nil) (B t)))))
  (is (equal 0.8 (node-probability *test-node-d* '((B nil) (C t) (D t)))))
  (is (equal 0.2 (node-probability *test-node-d* '((B nil) (C t) (D nil)))))
  (is (equal 0 (node-probability *test-node-d* '((B nil) (C nil) (D t)))))
  (is (equal 1 (node-probability *test-node-d* '((B nil) (C nil) (D nil))))))
