
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
; at the beginning, the list of parents is empty
; kind = nature, decision, utility, assumption
(define-class node ()
  (name
   (parents :initform '())
   potential
   (kind :type node-kind)))


(defgeneric node-type (node)
  (:documentation "Return the type of NODE (as Lisp type)."))

(defgeneric node-domain-values (node)
  (:documentation "Return the possible values for node's domain.")
  (:method (node)
    (values-for-type (node-type node))))

(defgeneric values-for-type (type)
  (:documentation "Return all values for type, if there are finitely many."))


(defgeneric node-cardinality (node)
  (:documentation "Return the number of possible values in the node's domain.
If the domain is infinite, return NIL.")
  (:method (node)
    (length (node-domain-values node))))


(defgeneric node-discrete-p (node)
  (:documentation "Return true if NODE is discrete, false otherwise")
  (:method (node)
    (not (null (node-cardinality node)))))

; returns a list: first element is cardinality of node, next elements are cardinality of parents
; e.g. (2, 3, 2) (current node: healthy has two possible states, 1st father traffic light (3 states) etc...)
(defun node-potential-dimensions (node)
  (cons (node-cardinality node) (mapcar #'node-cardinality (node-parents node))))

(defgeneric node-inverse-mapping (node)
  (:documentation "Return the mapping from domain-values to their indices. (no longer in potential)"))

#+-(defgeneric node-probability (node node-value &rest parent-values)
     (:documentation "Compute the probability of (NODE-VALUE PARENT-VALUES) for NODE.")
     (:method (node node-value &rest parent-values)
       (apply #'aref (node-potential node)
	      (compute-node-value-index node node-value parent-values))))

;; example usage:
#+-(node-probability *node-d* (list "B=t" "C=t" "D=f"))
(defgeneric node-probability (node node-values)
  (:documentation "Compute the probability of the given instatiation of a tuple of the nodes cpt. The value list is given in the form: <p1.name>=<p1.val> <p2.name>=<p2.val> ... <node.name>=<node.val>")
  (:method (node node-values)
    (let ((cpt (node-cpt node)))
      (gethash node-values cpt))))


;;; not used anymore
;;; question: mustn't lambda be quoted? #'(lambda...
#+-(defgeneric compute-node-value-index (node node-value parent-values)
  (:documentation "Compute the index of (NODE-VALUE . PARENT-VALUES) in NODE's potential.")
  (:method (node node-value parent-values)
    (let ((node-index (gethash node-value (node-inverse-mapping node)))
          (parent-indices
            (mapcar (lambda (value node)
                      (gethash value (node-inverse-mapping node)))
                    parent-values (node-parents node))))
      (cons node-index parent-indices))))

;; deprecated
#+-(defgeneric print-potential (node)
  (:documentation "prints the potential / factor-function of the given node.")
  (:method (node)
    (let ((values (node-domain-values node)))
      (format t " ~S | f ~%" (node-name node))
      (dotimes (i (length values))
        (format t " ~S | ~S~%" (elt values i) (node-probability node (elt values i)))))))

(defgeneric print-potential (node)
  (:documentation "prints the potential / factor-function of the given node.")
  (:method (node)
    (let ((cpt (node-cpt node)))
      (maphash #'(lambda (key-list potential) (format t "~S -> ~S~%" key-list potential)) cpt))))

#+-(defun print-hash-entry (key-list potential)
    (format t "~S -> ~S~%" key-list potential))

(defgeneric node-variables (node)
  (:documentation "returns all variables (names of nodes) that are involved in the potential of the given node")
  (:method (node)
    (let ((result '())
	  (parents (node-parents node)))
      (dotimes (i (length parents))
	(setf result (cons (node-name (elt parents i)) result)))
      (setf result (cons (node-name node) result))
      (reverse result))))



;;; Discrete Nodes
;;; ==============

(define-class discrete-node (node)
  ((domain-values :type sequence)
   (cpt)
   #+-(inverse-mapping))
  (:conc-name node)) 
;;; conc-name abbreviates accessor names for a structure's attributes here: node-values would get values instead of discrete-node-values

;;; after discrete node was initialized, default inverse-mapping attribute is created as a hashmap 
(defmethod initialize-instance :after ((node discrete-node) &key)
  #+-(unless (slot-boundp node 'inverse-mapping)
    (let ((table (make-hash-table :test 'equal))
          (values (node-domain-values node)))
      (dotimes (i (length values)) ; todo: go over map product of values and parent values
        (setf (gethash (elt values i) table) i))
      (setf (node-inverse-mapping node) table)))
  (unless (slot-boundp node 'cpt)
    (let ((cpt (make-hash-table :test 'equal))
	  (cpt-lhs (node-build-cpt node))
	  (cpt-rhs (node-potential node)))
      (dotimes (i (length cpt-lhs))
	(setf (gethash (elt cpt-lhs i) cpt) (elt cpt-rhs i)))
    (setf (node-cpt node) cpt))))



;;; prepare a potential table and insert the values
;(defmethod initialize-instance :before ((node discrete-node) &key)
;  (let ((potential-table-keys (map-product 'list node-get-named-value-lists(node)))))
;)

;;; e.g. executed on node B where : A -> B  => ("A=t" "A=f") ("B=t" "B=f")
(defgeneric node-get-named-value-lists (node)
  (:documentation "Returns in order a list of the parent values followed by the node's values")
  (:method (node)
    (let ((parent-list (node-parents node))
	  (result-list '()))
      (dotimes (i (length parent-list))
	(let ((current-parent (elt parent-list i)))
	  (let ((parent-name (node-name current-parent))
		(parent-values (node-domain-values current-parent)))
	    (let ((partial-result-list '()))
	      (dotimes (j (length parent-values))
		(setf partial-result-list 
		      (reverse
		      (cons 
		       (format nil "~a=~a" parent-name (elt parent-values j)) 
		       partial-result-list))))
	      (setf result-list (cons partial-result-list result-list))))))
      ;; now the parents are pushed to the result array, so we have to add the node values as well
      (let ((name (node-name node))
	    (values (node-domain-values node)))
	(let ((partial-result-list '()))
	  (dotimes (j (length values))
	    (setf partial-result-list (reverse (cons 
						(format nil "~a=~a" name (elt values j)) 
						partial-result-list))))
	  (setf result-list (reverse (cons partial-result-list result-list)))))
      (values-list result-list))))

(defgeneric node-build-cpt (node)
  (:documentation "returns the left side of a CPT for the given node")
  (:method (node)
    (multiple-value-call #'map-product 'list (node-get-named-value-lists node))))


;;; returns the node type for a discrete node
;;; it casts node-values into a list and returns something like (MEMBER 1 2 3)
(defmethod node-type ((node discrete-node))
  `(member ,@(coerce (node-domain-values node) 'list)))

(defmethod node-discrete-p ((node discrete-node))
  (declare (ignorable node))
  t)

;;; tests if given node is a discrete node
;(defmethod node-discrete-p ((node node))
;  (if (eql (type-of node) 'DISCRETE-NODE ) T nil ))

;;; how to run on existing params: switch to package bayes-user and then:
; (node-probability *node-a* "A=t")          -> 0.6
; (node-potential-var-set *node-a*)          -> ("A")
; (node-domain-values *node-a*)                     -> ("A=t" "A=f")
; (node-potential *node-a*)                  -> #(0.6 0.4)

;;; how to make new nodes:

; how to make a node with just one variable
; (defparameter *simple-node-over-A* (make-discrete-node :values ("A=t" "A=f") :kind :nature :potential (make-array '(2) :initial-contents '(0.4 0.6)) :name "node-name"))

; how to make a node over more variables: before
; (defparameter *node-over-A-and-B* (make-discrete-node :values (list (list "A=t" "B=t")(list "A=t" "B=f")(list "A=f" "B=t")(list "A=f" "B=f")) :kind :nature :potential (make-array '(4) :initial-contents '(0.6 0.1 0.2 0.1)) :name "A-and-B"))

; how to make a node over more variables: NOW
; (defparameter *node-over-A-and-B* (make-discrete-node :values '()  :kind :nature :potential (make-array '(4) :initial-contents '(0.6 0.1 0.2 0.1)) :name "A-and-B"))
