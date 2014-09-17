;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;; a variable 
(define-class var ()
  (name 
   node))

(define-class discrete-var (var)
  ((domain-values :type sequence)
   (cpt :type cpt))
  (:conc-name var))

(defvar *random-variable-counter* -1)

(define-class random-variable ()
  ((name :initform (format-symbol *package* "RV~A" (incf *random-variable-counter*))))
  (:conc-name rv))
