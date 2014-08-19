
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(define-class bayes-net ()
  (name
  (nodes :initform '())))

(defparameter *node-a* (make-discrete-node 
			:values (list "A=t" "A=f") 
			:kind :nature 
			:potential (make-array '(2) :initial-contents '(0.6 0.4)) 
			:name "A: Winter?"))

(defparameter *node-b* (make-discrete-node 
			:values (list (list "A=t" "B=t") 
				      (list "A=t" "B=f") 
				      (list "A=f" "B=t") 
				      (list "A=f" "B=f")) 
			:kind :nature 
			:potential (make-array '(4) :initial-contents '(0.2 0.8 0.75 0.25)) 
			:name "B: Sprinkler?"
			:parents (list *node-a*)))

(defparameter *node-c* (make-discrete-node 
			:values (list (list "A=t" "C=t") (list "A=t" "C=f") (list "A=f" "C=t") (list "A=f" "C=f")) 
			:kind :nature 
			:potential (make-array '(4) :initial-contents '(0.8 0.2 0.1 0.9)) 
			:name "C: Rain?"
			:parents (list *node-a*)))

(defparameter *node-d* (make-discrete-node 
			:values (list (list "B=t" "C=t" "D=t") 
				      (list "B=t" "C=t" "D=f") 
				      (list "B=t" "C=f" "D=t") 
				      (list "B=t" "C=f" "D=f")
				      (list "B=f" "C=t" "D=t") 
				      (list "B=f" "C=t" "D=f") 
				      (list "B=f" "C=f" "D=t") 
				      (list "B=f" "C=f" "D=f")) 
			:kind :nature 
			:potential (make-array '(8) :initial-contents '(0.95 0.05 0.9 0.1 0.8 0.2 0 1)) 
			:name "D: Wet Grass?"
			:parents (list *node-b* *node-c*)))


(defparameter *node-e* (make-discrete-node 
			:values (list (list "C=t" "E=t") 
				      (list "C=t" "E=f") 
				      (list "C=f" "E=t") 
				      (list "C=f" "E=f"))
			:kind :nature 
			:potential (make-array '(4) :initial-contents '(0.7 0.3 0 1)) 
			:name "E: Slippery Road?"
			:parents (list *node-c*)))

(defparameter *my-bayes-net* (make-bayes-net 
			      :name "Bayesnet from Adnan Darwiche on page 127" 
			      :nodes (list *node-a* *node-b* *node-c* *node-d* *node-e*)))
