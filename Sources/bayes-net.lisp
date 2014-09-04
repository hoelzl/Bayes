
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;; using double floats so that (+ 0.9 0.95) -> 1.85, instead of 1.8499999
(setf *read-default-float-format* 'double-float)

(define-class bayes-net ()
  (name
  (nodes :initform '())))

; Winter?
(defparameter *node-a* (make-discrete-node 
			:domain-values '(t nil)
			:kind :nature 
			:potential (make-array 2 :initial-contents '(0.6 0.4)) 
			:name 'A))

; Sprinkler?
(defparameter *node-b* (make-discrete-node 
			:domain-values '(t nil)
			:kind :nature 
			:potential (make-array 4 :initial-contents '(0.2 0.8 0.75 0.25)) 
			:name 'B
			:parents (list *node-a*)))

; Rain?
(defparameter *node-c* (make-discrete-node 
			:domain-values '(t nil)
			:kind :nature 
			:potential (make-array 4 :initial-contents '(0.8 0.2 0.1 0.9)) 
			:name 'C
			:parents (list *node-a*)))

; Wet Grass?
(defparameter *node-d* (make-discrete-node 
			:domain-values '(t nil)
			:kind :nature 
			:potential (make-array 8 :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
			:name 'D
			:parents (list *node-b* *node-c*)))

; Slippery Road?
(defparameter *node-e* (make-discrete-node 
			:domain-values '(t nil)
			:kind :nature 
			:potential (make-array 4 :initial-contents '(0.7 0.3 0 1)) 
			:name 'E
			:parents (list *node-c*)))

(defparameter *my-bayes-net* (make-bayes-net 
			      :name "Bayesnet from Adnan Darwiche on page 127" 
			      :nodes (list *node-a* *node-b* *node-c* *node-d* *node-e*)))
