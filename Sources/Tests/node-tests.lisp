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
    (is (equal (node-type n2) '(member a b c)))))

(deftest test-node-index-1 ()
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
