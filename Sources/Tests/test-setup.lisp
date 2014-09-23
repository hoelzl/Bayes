;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(defparameter *test-node-a* (make-discrete-node
                             :name "Winter?"
                             :var (make-discrete-var :name 'A 
                                                     :domain-values '(t nil))
                             :kind :nature
                             :potential-rhs '(0.6 0.4)))

(defparameter *test-node-b* (make-discrete-node
                             :name "Sprinkler?"
                             :var (make-discrete-var :name 'B 
                                                     :domain-values '(t nil))
                             :kind :nature
                             :potential-rhs '(0.2 0.8 0.75 0.25)
                             :parents (list *test-node-a*)))

(defparameter *test-node-c* (make-discrete-node 
                             :name "Rain?"
                             :var (make-discrete-var :name 'C 
                                                     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.8 0.2 0.1 0.9)
                             :parents (list *test-node-a*)))

(defparameter *test-node-d* (make-discrete-node
                             :name "Wet Grass?"
                             :var (make-discrete-var :name 'D 
                                                     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)
                             :parents (list *test-node-b* *test-node-c*)))

(defparameter *test-node-e* (make-discrete-node
                             :name "Slippery Road?"
                             :var (make-discrete-var :name 'E 
                                                     :domain-values '(t nil))
                             :kind :nature 
                             :potential-rhs '(0.7 0.3 0 1)
                             :parents (list *test-node-c*)))

(defparameter *test-bayes-net* (make-bayes-net
                                :name "Test bayes net, based on Darwiche page 127"
                                :nodes (list 
                                        *test-node-a* *test-node-b* 
                                        *test-node-c* *test-node-d*
                                        *test-node-e*)))
