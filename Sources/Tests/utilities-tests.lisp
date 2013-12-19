;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)
(in-suite bayes-utilities-suite)

(deftest test-equals-numbers ()
  (is (equals 1 1))
  (is (not (equals 1 2)))
  (is (not (equals 1 1.0)))
  (is (equals 1.0 1.0)))

(deftest test-equals-strings ()
  (is (equals "" ""))
  (is (equals "foo" "foo"))
  (is (not (equals "foo" "Foo")))
  (is (not (equals "foo" "bar"))))

