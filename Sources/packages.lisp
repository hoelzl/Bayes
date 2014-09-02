;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *bayes-test-exports*
  '(#:node-potential-dimensions
    #:process-class-slots
    #:process-class-slot
    #:process-class-options))

(defparameter *bayes-utilities-exports*
  '(#:define-class
    #:equals
    #:values-for-type))

(defparameter *bayes-exports*
  '(#:*my-bayes-net*
    #:*node-a*
    #:*node-b*
    #:*node-c*
    #:*node-d*
    #:*node-e*    
    #:bayes-net    
    #:combinations    
    #:constraint
    #:discrete-node
    #:make-bayes-net
    #:make-discrete-node
    #:make-node
    #:node
    #:node-build-cpt
    #:node-cardinality
    #:node-cpt
    #:node-discrete-p
    #:node-domain-values
    #:node-get-named-value-lists
    #:node-get-named-value-list
    #:node-inverse-mapping
    #:node-kind
    #:node-name
    #:node-parents
    #:node-potential
    #:node-potential-dimensions
    #:node-probability
    #:node-type
    #:node-values
    #:node-variables
    #:permutations
    #:potential
    #:print-potential
    #:ra-variable
    #:ra-variable-domain
    #:ra-variable-name
    #:ra-variable-named-domain-list
    #:relation
    #:relationalDatabase
    #:scope
    #:scope-permutations
    #:scope-ra-variables
    #:sum-out-var
    #:tuple))
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

