;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-cpt-suite)

(deftest test-cpt-contains-node ()
  (let ((cpt-d (node-cpt *test-node-d*))
        (cpt-a (node-cpt *test-node-a*)))
    ;; a contains a
    (is (not (null (cpt-contains-node cpt-a *test-node-a*))))
    ;; d contains b c and d
    (is (not (null (cpt-contains-node cpt-d *test-node-b*))))
    (is (not (null (cpt-contains-node cpt-d *test-node-c*))))
    (is (not (null (cpt-contains-node cpt-d *test-node-d*))))
    ;; d doesn't contain a or e
    (is (null (cpt-contains-node cpt-d *test-node-a*)))
    (is (null (cpt-contains-node cpt-d *test-node-e*)))))

(deftest test-build-cpt-lhs-for-given-nodes ()
  (let ((result (build-cpt-lhs-for-given-nodes (list *test-node-a* *test-node-b* *test-node-c*)))
        (expected-result '(((A T) (B T) (C T)) 
                           ((A T) (B T) (C nil)) 
                           ((A T) (B nil) (C T)) 
                           ((A T) (B nil) (C nil))
                           ((A nil) (B T) (C T)) 
                           ((A nil) (B T) (C nil)) 
                           ((A nil) (B nil) (C T)) 
                           ((A nil) (B nil) (C nil)))))
    (loop for x in result
          for y in expected-result 
          do (dolist (el-in-x x)
                (is (member el-in-x y :test-not #'set-exclusive-or))))))

(deftest test-build-cpt-for-nodes ()
  (let ((cpt-for-nodes-bcd (build-cpt-for-nodes (list *test-node-b* *test-node-c* *test-node-d*) 0))
        (expected-cpt-for-nodes-bcd (make-cpt :hashtable (make-hash-table :test #'equal) :vars '(B C D))))
    (setf (gethash '((B T) (C T) (D T)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B T) (C T) (D nil)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B T) (C nil) (D T)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B T) (C nil) (D nil)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B nil) (C T) (D T)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B nil) (C T) (D nil)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B nil) (C nil) (D T)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (setf (gethash '((B nil) (C nil) (D nil)) (cpt-hashtable expected-cpt-for-nodes-bcd)) 0)
    (is (equalp cpt-for-nodes-bcd expected-cpt-for-nodes-bcd))))
