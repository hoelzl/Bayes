;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *bayes-test-exports*
  '(#:compute-node-value-index
    #:node-potential-dimensions
    #:process-class-slots
    #:process-class-slot
    #:process-class-options
    #:node-value-index-table))

(defparameter *bayes-utilities-exports*
  '(#:define-class
    #:equals
    #:values-for-type))

(defparameter *bayes-exports*
  '(#:discrete-node
    #:make-discrete-node
    #:make-node
    #:node
    #:node-discrete-p
    #:node-domain
    #:node-kind
    #:node-name
    #:node-parents
    #:node-potential
    #:node-probability
    #:node-type
    #:node-values
    #:potential))
)

(defpackage #:bayes-implementation
  (:shadowing-import-from #:c2mop
                          #:defgeneric
                          #:defmethod
                          #:standard-generic-function)
  (:use #:common-lisp
        #:alexandria #:c2mop)
  (:nicknames #:bayes-impl)
  (:export . #.*bayes-test-exports*)
  (:export . #.*bayes-utilities-exports*)
  (:export . #.*bayes-exports*))

(defpackage #:bayes-utilities
  (:use #:common-lisp #:bayes-implementation)
  (:nicknames #:bayes-utils)
  (:export . #.*bayes-utilities-exports*))

(defpackage #:bayes
  (:use #:common-lisp #:bayes-implementation)
  (:export . #.*bayes-exports*))

(defpackage #:bayes-user
  (:use #:common-lisp #:common-lisp-user
        #:alexandria #:bayes-utilities #:bayes)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bu))

