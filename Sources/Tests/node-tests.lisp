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
