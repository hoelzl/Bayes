;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(defgeneric equals (lhs rhs)
  (:documentation "Compare LHS and RHS for equality.
The default comparison is EQUAL.")
  (:method (lhs rhs)
    (equal lhs rhs)))
