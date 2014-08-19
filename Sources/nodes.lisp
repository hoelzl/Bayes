
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

;;; Nodes
;;; =====
;;; what is node-kind good for? examples?
;;; member is function as well as type specifier
(deftype node-kind ()
  '(member :nature :decision :utility :assumption))

; potential = factor = probability table
; at the beginning the list of parents is empty
; kind = nature, decision, utility, assumption
(define-class node ()
  (name
   (parents :initform '())
   potential
   (kind :type node-kind)))


(defgeneric node-type (node)
  (:documentation "Return the type of NODE (as Lisp type)."))


(defgeneric node-potential-var-set (node)
  (:documentation "returns the set of variables that the potential of the given node is defined on.")
  (:method (node)
    (let ((resultset '())
	  (instantiation (first (node-values node))))
      (cond ((not(listp instantiation)) 
	     (push (subseq instantiation 0 (position #\= instantiation)) resultset))
	    ( t (dotimes (i (length instantiation)) 
		  (push (subseq (elt instantiation i) 0 (position #\= (elt instantiation i))) resultset))))
      resultset))) ;return result

(defgeneric node-values (node)
  (:documentation "Return the possible values for node's domain.")
  (:method (node)
    (values-for-type (node-type node))))

(defgeneric values-for-type (type)
  (:documentation "Return all values for type, if there are finitely many."))


(defgeneric node-cardinality (node)
  (:documentation "Return the number of possible values in the node's domain.
If the domain is infinite, return NIL.")
  (:method (node)
    (length (node-values node))))


(defgeneric node-discrete-p (node)
  (:documentation "Return true if NODE is discrete, false otherwise")
  (:method (node)
    (not (null (node-cardinality node)))))

; returns a list: first element is cardinality of node, next elements are cardinality of parents
; e.g. (2, 3, 2) (current node: healthy has two possible states, 1st father traffic light (3 states) etc...)
(defun node-potential-dimensions (node)
  (cons (node-cardinality node) (mapcar #'node-cardinality (node-parents node))))

(defgeneric node-inverse-mapping (node)
  (:documentation "Return the mapping from values to indices in the node potential."))

(defgeneric node-probability (node node-value &rest parent-values)
  (:documentation "Compute the probability of (NODE-VALUE PARENT-VALUES) for NODE.")
  (:method (node node-value &rest parent-values)
    (apply #'aref (node-potential node)
           (compute-node-value-index node node-value parent-values))))

;;; question: mustn't lambda be quoted? #'(lambda...
(defgeneric compute-node-value-index (node node-value parent-values)
  (:documentation "Compute the index of (NODE-VALUE . PARENT-VALUES) in NODE's potential.")
  (:method (node node-value parent-values)
    (let ((node-index (gethash node-value (node-inverse-mapping node)))
          (parent-indices
            (mapcar (lambda (value node)
                      (gethash value (node-inverse-mapping node)))
                    parent-values (node-parents node))))
      (cons node-index parent-indices))))

(defgeneric print-potential (node)
  (:documentation "prints the potential / factor-function of the given node.")
  (:method (node)
    (let ((values (node-values node)))
      (format t " ~S | f ~%" (node-name node))
      (dotimes (i (length values))
        (format t " ~S | ~S~%" (elt values i) (node-probability node (elt values i)))))))

;;; Discrete Nodes
;;; ==============

(define-class discrete-node (node)
  ((values :type sequence)
   (inverse-mapping))
  (:conc-name node)) 
;;; conc-name abbreviates accessor names for a structure's attributes here: node-values would get values instead of discrete-node-values

;;; after discrete node was initialized, default inverse-mapping attribute is created as a hashmap 
(defmethod initialize-instance :after ((node discrete-node) &key)
  (unless (slot-boundp node 'inverse-mapping)
    (let ((table (make-hash-table :test 'equal))
          (values (node-values node)))
      (dotimes (i (length values))
        (setf (gethash (elt values i) table) i))
      (setf (node-inverse-mapping node) table))))

;;; returns the node type for a discrete node
;;; it casts node-values into a list and returns something like (MEMBER 1 2 3)
(defmethod node-type ((node discrete-node))
  `(member ,@(coerce (node-values node) 'list)))

(defmethod node-discrete-p ((node discrete-node))
  (declare (ignorable node))
  t)

;;; tests if given node is a discrete node
;(defmethod node-discrete-p ((node node))
;  (if (eql (type-of node) 'DISCRETE-NODE ) T nil ))

;;; how to run
; (defparameter *wetter* (make-instance 'discrete-node :values '(Sonne Regen) :potential '(0.4 0.6)) "Sonne oder Regen") -> *WETTER*
; (node-values *wetter*)    -> (sonne regen)
; (node-potential *wetter*) -> (0.4 0.6)
; (initialize-instance *wetter*) is executed implicitly when make-instance is called

; (defparameter *supiwetter2* (make-instance 'discrete-node :values '(1 2) :kind :nature :potential (make-array '(2) :initial-contents '(5 6)) :name 'supiwetter2))
; (node-probability *supiwetter2* 2)  --> 6 !! :)

;(defparameter *supiwetter* (make-instance 'discrete-node :values '(sonne regen) :kind :nature :potential (make-array '(2) :initial-contents '(0.4 0.6)) :name 'supiwetter))
;(node-probability *supiwetter* 'sonne)
