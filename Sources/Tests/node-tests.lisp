;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-nodes-suite)

(deftest test-discrete-node-type-1 ()
  (let ((n1 (make-discrete-node :values '(a b c)))
        (n2 (make-discrete-node :values #(a b c))))
    (is (equal (node-type n1) '(member a b c)))
    (is (equal (node-type n2) '(member a b c)))
))

(deftest test-bayesian-network-darwiche-page-127 ()
  (let ((node-a (make-discrete-node 
                 :values (list "A=t" "A=f") 
                 :kind :nature 
                 :potential (make-array '(2) :initial-contents '(0.6 0.4)) 
                 :name "A: Winter?")))
    (is (equal 0.6 (node-probability node-a "A=t")))
    (is (equal 0.4 (node-probability node-a "A=f")))
    (let ((node-b (make-discrete-node 
                   :values (list (list "A=t" "B=t") (list "A=t" "B=f") (list "A=f" "B=t") (list "A=f" "B=f")) 
                   :kind :nature 
                   :potential (make-array '(4) :initial-contents '(0.2 0.8 0.75 0.25)) 
                   :name "B: Sprinkler?"
                   :parents (list node-a))))
      (is (equal 0.2 (node-probability node-b (list "A=t" "B=t"))))
      (is (equal 0.8 (node-probability node-b (list "A=t" "B=f"))))
      (is (equal 0.75 (node-probability node-b (list "A=f" "B=t"))))
      (is (equal 0.25 (node-probability node-b (list "A=f" "B=f"))))
      (is (member node-a (node-parents node-b))) ; check if a is parent of b
    ; (is (equal 0.25 (node-probability node-b (list "B=f" "A=f")))) ;TODO: make this work too (commutative)
      (let ((node-c (make-discrete-node 
                   :values (list (list "A=t" "C=t") (list "A=t" "C=f") (list "A=f" "C=t") (list "A=f" "C=f")) 
                   :kind :nature 
                   :potential (make-array '(4) :initial-contents '(0.8 0.2 0.1 0.9)) 
                   :name "C: Rain?"
                   :parents (list node-a))))
        (is (member node-a (node-parents node-c))) ; check if a is parent of c
        (let ((node-d (make-discrete-node 
                   :values (list (list "B=t" "C=t" "D=t") 
                                 (list "B=t" "C=t" "D=f") 
                                 (list "B=t" "C=f" "D=t") 
                                 (list "B=t" "C=f" "D=f")
                                 (list "B=f" "C=t" "D=t") 
                                 (list "B=f" "C=t" "D=f") 
                                 (list "B=f" "C=f" "D=t") 
                                 (list "B=f" "C=f" "D=f")) 
                   :kind :nature 
                   :potential (make-array '(8) :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
                   :name "D: Wet Grass?"
                   :parents (list node-a node-b))))
          (is (member node-a (node-parents node-d)))
          (is (member node-b (node-parents node-d)))
          (let ((node-e (make-discrete-node 
                   :values (list (list "C=t" "E=t") 
                                 (list "C=t" "E=f") 
                                 (list "C=f" "E=t") 
                                 (list "C=f" "E=f"))
                   :kind :nature 
                   :potential (make-array '(4) :initial-contents '(0.7 0.3 0 1)) 
                   :name "E: Slippery Road?"
                   :parents (list node-c))))
            (is (member node-c (node-parents node-e)))
            (is (member node-a (node-parents (first (node-parents node-e)))))
))))))



