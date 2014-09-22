
;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)


;;; Nodes
;;; =====

(deftype node-kind ()
  '(member :nature :decision :utility :assumption))

; potential = factor = probability table
; at the beginning, the list of parents is empty
; kind = nature, decision, utility, assumption
(define-class node ()
  (name
   (parents :initform '())
   potential-rhs
   (kind :type node-kind)))


(defgeneric node-type (node)
  (:documentation "Return the type of NODE (as Lisp type)."))

;;; it casts node-values into a list and returns something like (MEMBER 1 2 3)

;; todo: delete?
#+(or)(defgeneric values-for-type (type)
  (:documentation "Return all values for type, if there are finitely many."))


(defgeneric node-domain-values (node)
  (:documentation "Return the possible values for node's domain.")
  #+(or)(:method (node)
    (values-for-type (node-type node))))

(defgeneric node-cardinality (node)
  (:documentation "Return the number of possible values in the node's domain.
If the domain is infinite, return NIL.")
  (:method (node)
    (length (node-domain-values node))))


(defgeneric node-discrete-p (node)
  (:documentation "Return true if NODE is discrete, false otherwise")
  (:method (node)
    ;; TODO: This is incorrect, e.g., for integers
    (not (null (node-cardinality node)))))


(defun node-potential-dimensions (node)
  "Returns a list with the cardinality of node's parents followed by its own cardinality."
  (append (mapcar #'node-cardinality (node-parents node)) (list (node-cardinality node))))

(defun sort-node-values (node-values)
  (sort (copy-list node-values) #'string<
        :key #'first))

;; example usages:
#+(or) (node-probability *node-b* '((A t)(B t))) ;; -> 0.2 -> T
;; the lookup is commutative
#+(or) (node-probability *node-b* '((B t)(A t))) ;; -> 0.2 -> T
(defgeneric node-probability (node node-values)
  (:documentation "Compute the probability of the given instantiation of a tuple of the nodes cpt. 
   The value list is an alist of node-value pairs, i.e., given in the form:
   ((p1.name p1.value) (p2.name p2.value) ... (node.name node.value))")
  (:method (node node-values)
    (let ((cpt-table (cpt-hashtable (node-cpt node)))
          (ordered-node-values (sort-node-values node-values)))
      (gethash ordered-node-values cpt-table))))


(defgeneric print-potential (node)
  (:documentation "prints the potential / factor-function of the given node.")
  (:method (node)
    (let ((cpt-table (cpt-hashtable (node-cpt node))))
      (maphash #'(lambda (key-list potential)
                   (format t "~S -> ~S~%" key-list potential)) cpt-table))))

(defgeneric node-variables (node)
  (:documentation
   "Returns all variables that are involved in the potential of the given node")
  (:method (node)
    (let ((result '())
	  (parents (node-parents node)))
      (iter (for parent in-sequence parents)
	(setf result (cons (var-name (node-var parent)) result)))
      (setf result (cons (var-name (node-var node)) result))
      (reverse result))))


;;; Discrete Nodes
;;; ==============

(define-class discrete-node (node)
  ((var :type discrete-var)
   cpt)
  (:conc-name node))

(defmethod node-type ((node discrete-node))
  `(member ,@(coerce (var-domain-values (node-var node)) 'list)))

(defmethod node-domain-values ((node discrete-node))
  (var-domain-values (node-var node)))


;;; after discrete node was initialized, default inverse-mapping attribute is created as a hashmap 
(defmethod initialize-instance :after ((node discrete-node) &key var)
  ;; create bidirectional link between node and var
  (setf (var-node var) node)
  ;; create cpt 
  (unless (slot-boundp node 'cpt)
    ;; create cpt-hashtable
    (let ((cpt (make-cpt :hashtable (make-hash-table :test 'equal) :vars nil))
	  (cpt-lhs (node-build-cpt node))
	  (cpt-rhs (node-potential-rhs node)))
      (assert (= (length cpt-lhs) (length cpt-rhs)) ()
              "Left-hand side and right-hand side of CPT have different lengths:~%~W, ~W"
              cpt-lhs cpt-rhs)
      ;; save cpt lhs with rhs
      (dotimes (i (length cpt-lhs))
	(setf (gethash (elt cpt-lhs i) (cpt-hashtable cpt)) (elt cpt-rhs i)))
      ;; sort what was just saved for quick retrieval
      (setf (node-cpt node) (sort-cpt cpt))
      ;; create cpt-vars
      (let ((vars nil)
	    (parents (node-parents node)))
	(dolist (parent parents)
	  (setf vars (cons (node-var parent) vars)))
	(setf vars (cons (node-var node) vars))
	(setf (cpt-vars (node-cpt node)) vars)))))

(defmethod node-variables ((node discrete-node))
  (cpt-vars (node-cpt node)))

(defmethod print-object ((node discrete-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A: ~A" (node-name node) (node-domain-values node))))

(defgeneric node-build-cpt (node)
  (:documentation "returns the left side of a CPT for the given node")
  (:method (node)
    (apply #'map-product #'list (node-get-named-value-lists node))))

;;; e.g. executed on node B where : A -> B  => ((A t) (A nil)) ((B t) (B nil))
(defgeneric node-get-named-value-lists (node)
  (:documentation "Returns in order a list of the parent values followed by the node's values")
  (:method (node)
    (let* ((node-list (append (node-parents node) (list node)))
           (result-list (map 'list
                             #'node-get-named-value-list
                             node-list)))
      result-list)))

;; e.g. executed on node B where : A -> B => ((B t) (B nil))
(defgeneric node-get-named-value-list (node)
  (:documentation "Returns in order a list of the node values preceded by the node's name")
  (:method (node)
    (let ((name (var-name (node-var node)))
          (values (var-domain-values (node-var node))))
      (map 'list
           (lambda (value)
             (list name value))
           values))))

(defmethod node-discrete-p ((node discrete-node))
  (declare (ignorable node))
  t)
