;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-potentials-suite)

(deftest test-combinations ()
  (let ((c1 (combinations '(1 2 3) '(4 5)))
        (c2 (combinations '(4 5) '(1 2 3))))
    (is (equal 6 (length c1)))
    (is (equal 6 (length c2)))
    (is (member (first c1) c2 :test-not #'set-exclusive-or)) ;todo: replace by loop over c1
    (is (member (second c1) c2 :test-not #'set-exclusive-or))
 ))

;(defun unordered-list-equals-p (list-a list-b)
;  (let ((sorted-a (sort list-a #'> :key #'car))
;        (sorted-b (sort list-b #'> :key #'car)))
;    (equal sorted-a sorted-b)))

;(defun set-member (set set-list)
;  (loop for set-list-set in set-list
;        when (loop for set-item in set
;                   for item = (if (member set-item set-list-set)
;                                (remove set-item set-list-set)
;                                (return nil))
;                            then (if (member set-item item)
;                                   (remove set-item item)
;                                   (return nil))
;                   finally (unless item (return set)) return it)))
