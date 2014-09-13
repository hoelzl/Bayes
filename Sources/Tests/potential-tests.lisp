;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-potentials-suite)

#++(defun set-exclusive-or-for-strings (list-a list-b)
  (set-exclusive-or list-a list-b :test #'equal))

#++(deftest test-combinations ()
  (let ((c1 (combinations '(1 2 3) '(4 5)))
        (c2 (combinations '(4 5) '(1 2 3))))
    (is (equal 6 (length c1)))
    (is (equal 6 (length c2)))
    (dotimes (i (length c1)) 
      (is (member (nth i c1) c2 :test-not #'set-exclusive-or)))))

#++(deftest test-permutations ()
  (let ((var-a (make-instance 'ra-variable :name "A" :domain '(1 2 3 4)))
        (var-b (make-instance 'ra-variable :name "B" :domain '(1 2)))
        (var-c (make-instance 'ra-variable :name "C" :domain '(1 2 3))))
    (let ((scope (make-instance 'scope :ra-variables (list var-a var-b var-c))))
      (let ((perms (permutations scope)))
        (is (equal 24 (length perms)))
        (is (member '("A=1" "B=1" "C=1") perms :test-not #'set-exclusive-or-for-strings)) ; testing
        (is (member '("A=1" "B=1" "C=2") perms :test-not #'set-exclusive-or-for-strings)) ; exemplary
        (is (member '("A=1" "B=1" "C=3") perms :test-not #'set-exclusive-or-for-strings)) ; cases
        (is (member '("A=1" "B=2" "C=1") perms :test-not #'set-exclusive-or-for-strings)) ; that
        (is (member '("A=1" "B=2" "C=2") perms :test-not #'set-exclusive-or-for-strings)) ; belong
        (is (member '("A=1" "B=2" "C=3") perms :test-not #'set-exclusive-or-for-strings)) ; to
        (is (member '("A=2" "B=1" "C=2") perms :test-not #'set-exclusive-or-for-strings)) ; the
        (is (member '("A=3" "B=1" "C=3") perms :test-not #'set-exclusive-or-for-strings)) ; allowed
        (is (member '("A=4" "B=2" "C=3") perms :test-not #'set-exclusive-or-for-strings)) ;permutations
        (is (not (member '("A=1" "B=3" "C=1") perms :test-not #'set-exclusive-or-for-strings))) ; also
        (is (not (member '("A=1" "B=2" "C=4") perms :test-not #'set-exclusive-or-for-strings))) ; not
        (is (not (member '("A=4" "B=3" "C=4") perms :test-not #'set-exclusive-or-for-strings))))))) ; allowed

;(deftest sumout ()
;  (let 
;)
