
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;; using double floats so that (+ 0.9 0.95) -> 1.85, instead of 1.8499999
(setf *read-default-float-format* 'double-float)

(define-class bayes-net ()
  ((name)
   (nodes :initform '()))
  (:conc-name bn))

(defgeneric bn-cpts (bayes-net)
  (:documentation "Returns all CPT stuctures that belong to the given bayes-net.")
  (:method (bayes-net)
    (let ((nodes (bn-nodes bayes-net)))
      (mapcar #'node-cpt nodes)
      )))

;; Winter?
(defparameter *node-a* (make-discrete-node
                             :name "Winter?"
                             :var (make-discrete-var :name 'A 
                                                     :domain-values '(t nil))
                             :kind :nature
                             :potential-rhs '(0.6 0.4)))

(defparameter *node-b* (make-discrete-node
                             :name "Sprinkler?"
                             :var (make-discrete-var :name 'B 
                                                     :domain-values '(t nil))
                             :kind :nature
                             :potential-rhs '(0.2 0.8 0.75 0.25)
                             :parents (list *node-a*)))

(defparameter *node-c* (make-discrete-node 
                             :name "Rain?"
                             :var (make-discrete-var :name 'C 
                                                     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.8 0.2 0.1 0.9)
                             :parents (list *node-a*)))

(defparameter *node-d* (make-discrete-node
                             :name "Wet Grass?"
                             :var (make-discrete-var :name 'D 
                                                     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)
                             :parents (list *node-b* *node-c*)))

(defparameter *node-e* (make-discrete-node
                             :name "Slippery Road?"
                             :var (make-discrete-var :name 'E 
						     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.7 0.3 0 1)
                             :parents (list *node-c*)))

(defparameter *my-bayes-net* (make-bayes-net 
			      :name "Bayesnet from Adnan Darwiche on page 127" 
			      :nodes (list *node-a* *node-b* *node-c* *node-d* *node-e*)))
