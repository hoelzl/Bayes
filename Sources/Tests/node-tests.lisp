;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-discrete-node-type-1 ()
  (let ((n1 (make-discrete-node :domain-values '(a b c) :name "n1" :potential '(0.1 0.2 0.7)))
        (n2 (make-discrete-node :domain-values #(a b c) :name "n2" :potential '(0.4 0.2 0.4))))
    (is (equal (node-type n1) '(member a b c)))
    (is (equal (node-type n2) '(member a b c)))
))

(deftest test-bayesian-network-darwiche-page-127 ()
  (let ((node-a (make-discrete-node 
                 :domain-values (list "t" "f") 
                 :kind :nature 
                 :potential (make-array '(2) :initial-contents '(0.6 0.4)) 
                 :name "A")))
    (is (equal 0.6 (node-probability node-a (list "A=t"))))
    (is (equal 0.4 (node-probability node-a (list "A=f"))))
    (let ((node-b (make-discrete-node 
                   :domain-values (list "t" "f") 
                   :kind :nature 
                   :potential (make-array '(4) :initial-contents '(0.2 0.8 0.75 0.25)) 
                   :name "B"
                   :parents (list node-a))))
      (is (equal 0.2 (node-probability node-b (list "A=t" "B=t"))))
      (is (equal 0.8 (node-probability node-b (list "A=t" "B=f"))))
      (is (equal 0.75 (node-probability node-b (list "A=f" "B=t"))))
      (is (equal 0.25 (node-probability node-b (list "A=f" "B=f"))))
      (is (member node-a (node-parents node-b))) ; check if a is parent of b
      ;; TODO: make this work too (commutative)
      ;; (is (equal 0.25 (node-probability node-b (list "B=f" "A=f"))))
      (let ((node-c (make-discrete-node 
                     :domain-values (list "t" "f") 
                     :kind :nature 
                     :potential (make-array '(4) :initial-contents '(0.8 0.2 0.1 0.9)) 
                     :name "C"
                     :parents (list node-a))))
        (is (member node-a (node-parents node-c))) ; check if a is parent of c
        (let ((node-d (make-discrete-node 
                       :domain-values (list "t" "f")  
                       :kind :nature 
                       :potential (make-array '(8) :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
                       :name "D"
                       :parents (list node-b node-c))))
          (is (member node-b (node-parents node-d)))
          (is (member node-c (node-parents node-d)))
          (is (equal 0.95 (node-probability node-d (list "B=t" "C=t" "D=t"))))
          (is (equal 0.05 (node-probability node-d (list "B=t" "C=t" "D=f"))))
          (is (equal 0.9 (node-probability node-d (list "B=t" "C=f" "D=t"))))
          (is (equal 0.1 (node-probability node-d (list "B=t" "C=f" "D=f"))))
          (is (equal 0.8 (node-probability node-d (list "B=f" "C=t" "D=t"))))
          (is (equal 0.2 (node-probability node-d (list "B=f" "C=t" "D=f"))))
          (is (equal 0 (node-probability node-d (list "B=f" "C=f" "D=t"))))
          (is (equal 1 (node-probability node-d (list "B=f" "C=f" "D=f"))))
          
          (let ((node-e (make-discrete-node 
                         :domain-values (list "t" "f") 
                         :kind :nature 
                         :potential (make-array '(4) :initial-contents '(0.7 0.3 0 1)) 
                         :name "E"
                         :parents (list node-c))))
            (is (member node-c (node-parents node-e)))
            (is (member node-a (node-parents (first (node-parents node-e)))))
            (is (equal (node-potential-dimensions node-a) '(2)))
            (is (equal (node-potential-dimensions node-b) '(2 2)))
            (is (equal (node-potential-dimensions node-c) '(2 2)))
            (is (equal (node-potential-dimensions node-d) '(2 2 2)))
            (is (equal (node-potential-dimensions node-e) '(2 2)))))))))



