
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
    ;; TODO: This is incorrect, e.g., for integers
    (not (null (node-cardinality node)))))


(defun node-potential-dimensions (node)
  "Returns a list with the cardinality of node's parents followed by its own cardinality."
  (append (mapcar #'node-cardinality (node-parents node)) (list (node-cardinality node))))

#+ (or)
(defgeneric node-inverse-mapping (node)
  (:documentation "Return the mapping from domain-values to their indices. (no longer in potential)"))

#+-(defgeneric node-probability (node node-value &rest parent-values)
     (:documentation "Compute the probability of (NODE-VALUE PARENT-VALUES) for NODE.")
     (:method (node node-value &rest parent-values)
       (apply #'aref (node-potential node)
	      (compute-node-value-index node node-value parent-values))))

(defun sort-node-values (node-values)
  (sort (copy-list node-values) #'string<
        :key #'first))

;; example usage:
#+-(node-probability *node-d* (list "B=t" "C=t" "D=f"))
(defgeneric node-probability (node node-values)
  (:documentation "Compute the probability of the given instatiation of a tuple of the nodes cpt. 
   The value list is an alist of node-value pairs, i.e., given in the form:
   ((p1.name p1.value) (p2.name p2.value) ... (node.name node.value))")
  (:method (node node-values)
    (let ((cpt (node-cpt node))
          (ordered-node-values (sort-node-values node-values)))
      (gethash ordered-node-values cpt))))

;;; not used anymore
;;; question: mustn't lambda be quoted? #'(lambda...
#+-(defgeneric compute-node-value-index (node node-value parent-values)
  (:documentation "Compute the index of (NODE-VALUE . PARENT-VALUES) in NODE's potential.")
  (:method (node node-value parent-values)
    (let ((node-index (gethash node-value (node-value-index-table node)))
          (parent-indices
            (mapcar (lambda (value node)
                      (gethash value (node-value-index-table node)))
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
      (maphash #'(lambda (key-list potential)
                   (format t "~S -> ~S~%" key-list potential)) cpt))))

#+-(defun print-hash-entry (key-list potential)
    (format t "~S -> ~S~%" key-list potential))

(defgeneric node-variables (node)
  (:documentation
   "Returns all variables (names of nodes) that are involved in the potential of the given node")
  (:method (node)
    (let ((result '())
	  (parents (node-parents node)))
      (iter (for parent in-sequence parents)
	(setf result (cons (node-name parent) result)))
      (setf result (cons (node-name node) result))
      (reverse result))))



;;; Discrete Nodes
;;; ==============

(define-class discrete-node (node)
  ((domain-values :type sequence)
   (cpt))
  (:conc-name node))


;;; after discrete node was initialized, default inverse-mapping attribute is created as a hashmap 
(defmethod initialize-instance :after ((node discrete-node) &key)
  (unless (slot-boundp node 'cpt)
    (let ((cpt (make-hash-table :test 'equal))
	  (cpt-lhs (node-build-cpt node))
	  (cpt-rhs (node-potential node)))
      (assert (= (length cpt-lhs) (length cpt-rhs)) ()
              "Left-hand side and right-hand side of CPT have different lengths:~%~W, ~W"
              cpt-lhs cpt-rhs)
      (dotimes (i (length cpt-lhs))
	(setf (gethash (elt cpt-lhs i) cpt) (elt cpt-rhs i)))
      (let ((sorted-cpt (make-hash-table :test 'equal)))
        (iter (for (k v) in-hashtable cpt)
          (setf (gethash (sort-node-values k) sorted-cpt) v))
        (setf (node-cpt node) sorted-cpt)))))

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
    (let ((name (node-name node))
          (values (node-domain-values node)))
      (map 'list
           (lambda (value)
             (list name value))
           values))))
  
;;; returns the node type for a discrete node
;;; it casts node-values into a list and returns something like (MEMBER 1 2 3)
(defmethod node-type ((node discrete-node))
  `(member ,@(coerce (node-domain-values node) 'list)))

(defmethod node-discrete-p ((node discrete-node))
  (declare (ignorable node))
  t)

;;; (mappend (lambda (x) (if (< x 3) (list x) '())) '(2 3 5 9))

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
