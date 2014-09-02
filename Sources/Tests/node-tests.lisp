;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-discrete-node-type-1 ()
  (let ((n1 (make-discrete-node :domain-values '(a b c)
                                :name 'n1
                                :potential '(0.1 0.2 0.7)))
        (n2 (make-discrete-node :domain-values #(a b c)
                                :name 'n2
                                :potential '(0.4 0.2 0.4))))
    (is (equal (node-type n1) '(member a b c)))
    (is (equal (node-type n2) '(member a b c)))
))

(deftest test-node-get-named-value-lists ()
  (let* ((n (make-discrete-node :domain-values '(1 2 3)
                                :name 'N
                                :potential '(0.1 0.15 0.1)))
         (domain-values-of-n '((N 1) (N 2) (N 3)))
         (expected-result-for-n (list domain-values-of-n)))
    (is (equal expected-result-for-n (node-get-named-value-lists n)))
    (let* ((m (make-discrete-node :domain-values '(6 7 8)
                                  :name 'M
                                  :potential '(0.2 0.3 0.4 0.1 0.5 0.2 0.7 0.9 0.1)
                                  :parents (list n)))
           (domain-values-of-m '((M 6) (M 7) (M 8)))
           (expected-result-for-m (list domain-values-of-n domain-values-of-m)))
      (is (equal domain-values-of-m (node-get-named-value-list m)))
      (is (equal expected-result-for-m (node-get-named-value-lists m))))))

(deftest test-bayesian-network-darwiche-page-127 ()
  (let ((node-a (make-discrete-node
                 :domain-values '(t nil)
                 :kind :nature
                 :potential (make-array '(2) :initial-contents '(0.6 0.4)) 
                 :name 'A)))
    (is (equal 0.6 (node-probability node-a '((A t)))))
    (is (equal 0.4 (node-probability node-a '((A nil)))))
    (let ((node-b (make-discrete-node
                   :domain-values '(t nil)
                   :kind :nature
                   :potential (make-array '(4) :initial-contents '(0.2 0.8 0.75 0.25)) 
                   :name 'B
                   :parents (list node-a))))
      (is (equal 0.2 (node-probability node-b '((A t) (B t)))))
      (is (equal 0.8 (node-probability node-b '((A t) (B nil)))))
      (is (equal 0.75 (node-probability node-b '((A nil) (B t)))))
      (is (equal 0.25 (node-probability node-b '((A nil) (B nil)))))
      (is (member node-a (node-parents node-b))) ; check if a is parent of b
      ;; TODO: make this work too (commutative)
      ;; (is (equal 0.25 (node-probability node-b '((B nil) (A nil)))))
      (let ((node-c (make-discrete-node 
                     :domain-values '(t nil)
                     :kind :nature 
                     :potential (make-array '(4) :initial-contents '(0.8 0.2 0.1 0.9)) 
                     :name 'C
                     :parents (list node-a))))
        (is (member node-a (node-parents node-c))) ; check if a is parent of c
        (let ((node-d (make-discrete-node 
                       :domain-values '(t nil) 
                       :kind :nature 
                       :potential (make-array '(8) :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
                       :name 'D
                       :parents (list node-b node-c))))
          (is (member node-b (node-parents node-d)))
          (is (member node-c (node-parents node-d)))
          (is (equal 0.95 (node-probability node-d '((B t) (C t) (D t)))))
          (is (equal 0.05 (node-probability node-d '((B t) (C t) (D nil)))))
          (is (equal 0.9 (node-probability node-d '((B t) (C nil) (D t)))))
          (is (equal 0.1 (node-probability node-d '((B t) (C nil) (D nil)))))
          (is (equal 0.8 (node-probability node-d '((B nil) (C t) (D t)))))
          (is (equal 0.2 (node-probability node-d '((B nil) (C t) (D nil)))))
          (is (equal 0 (node-probability node-d '((B nil) (C nil) (D t)))))
          (is (equal 1 (node-probability node-d '((B nil) (C nil) (D nil)))))

          (let ((node-e (make-discrete-node 
                         :domain-values '(t nil)
                         :kind :nature 
                         :potential (make-array '(4) :initial-contents '(0.7 0.3 0 1)) 
                         :name 'E
                         :parents (list node-c))))
            (is (member node-c (node-parents node-e)))
            (is (member node-a (node-parents (first (node-parents node-e)))))
            (is (equal (node-potential-dimensions node-a) '(2)))
            (is (equal (node-potential-dimensions node-b) '(2 2)))
            (is (equal (node-potential-dimensions node-c) '(2 2)))
            (is (equal (node-potential-dimensions node-d) '(2 2 2)))
            (is (equal (node-potential-dimensions node-e) '(2 2)))))))))




#+-(deftest test-node-index-1 ()
  (let ((node (make-discrete-node 
               :values '(a b)
               :potential #2A((0.1 0.2 0.3) (0.4 0.5 0.6))
               :parents (list (make-discrete-node
                               :values '(1 2 3) :potential '(0.1 0.2 0.3))))))
    (is (equal (compute-node-value-index node 'a '(1)) '(0 0)))
    (is (equal (compute-node-value-index node 'a '(2)) '(0 1)))
    (is (equal (compute-node-value-index node 'a '(3)) '(0 2)))
    (is (equal (compute-node-value-index node 'b '(1)) '(1 0)))
    (is (equal (compute-node-value-index node 'b '(2)) '(1 1)))
    (is (equal (compute-node-value-index node 'b '(3)) '(1 2)))))
