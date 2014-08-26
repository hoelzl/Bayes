;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(define-class potential ()
  ())
    
(defgeneric sum-out-var (node-with-potential-of-interest node-to-eliminate)
  (:documentation "implementation of SumOutVars on Darviche page 130")
  (:method (node-with-potential-of-interest node-to-eliminate)
    (let ((x (node-get-vars node-with-potential-of-interest))
          (z (node-name node-to-eliminate)))
      (if (null z) (error 'node-to-eliminate-is-empty))
      (if (null (first x)) (error 'node-with-potential-of-interest-has-no-var))
      (if (not (member z x)) (error 'var-to-be-eliminated-is-not-contained-in-the-potential-of-interest))
      (let ((y (remove z x))
            (result-potential (make-array 
                               (/ (node-cardinality node-with-potential-of-interest) (node node-to-eliminate)))))
        ;(loop for i from 0 to '(1 2 3) do (print i))
        (length result-potential) ;todo: implement double loop that sums out the var from node-to-elimininate
        ;)
        )
      )
    )
)


;; todo: move the following classes and methods to a relational-algebra package and use as needed
; name: A, B, C
; domain: (1 2 3) or (T nil) 
(define-class ra-variable ()
  (name
  (domain :type sequence) ; e.g. '(1,2,3)
  (named-domain-list :initform '()) ; e.g. '(A=1 A=2 A=3) // could be used as part of the hash-key??
))

; A scope S is a set of variables
(define-class scope ()
  (ra-variables))


(defgeneric permutations (scope)
  (:documentation "renders all permutations of the values of variables in the scope.")
  (:method (scope)
    (let ((variables (scope-ra-variables scope)))
      (dotimes (i (length variables)) ; variables contains relationalAlgebra variables *A* e.g.
        (let ((var-at-i (elt variables i))) ; var-at-i = *A* 
          (let ((values-at-i (ra-variable-domain var-at-i))) ; values-at-i = e.g. (1 2 3) 
            (dotimes (j (length values-at-i))
              (if (= 0 j) (setf (ra-variable-named-domain-list var-at-i) '())) ; reset list for each permutation call
              (push   (format nil "~a=~a" (ra-variable-name var-at-i) (elt values-at-i j))
                        (ra-variable-named-domain-list var-at-i)  ;must use push, so we have call-by-reference effect
              ))))))
    (let ((variables (scope-ra-variables scope))
          (input-list nil))
      (dotimes (i (length variables))
        (push (ra-variable-named-domain-list (elt variables i)) input-list))
      (multiple-value-call #'combinations (values-list input-list))))) ;using multiple results by applying combinations function via multiple-value-call function

; FIXME: works only if input lists are not empty
; returns all combinations of the list elements 
(defun combinations (&rest lists)
  (cond ((not(car lists)) (list nil))
        (t (mapcan (lambda (x) (mapcar (lambda (y) (cons y x))
                                       (car lists)))
                   (apply #'combinations (cdr lists))))))

; A tuple t on scope S, has a value on each variable in its scope -> val(t,X) is in dom(X)
(define-class tuple ()
  (scope values  ;values is a hashtable: "variable -> value in domain of the variable"
 ))

; A Constraint c on a scope S is a set of tuples on S.
; A constraint is said to involve each of the variables in its scope.
(define-class constraint ()
  (tuples scope))

; A relation is a set of tuples, all with the same scope.
(define-class relation ()
  (name ; describes the relation
   tuples ; holds the tuples
   scope ; also known as scheme: X1,...,Xn
))

; A relational database is a set of of relations
(define-class relationalDatabase ()
  (relations))



;; example-usage:
; (defparameter *A* (make-instance 'ra-variable :name "A" :domain '(1 2 3 4) ))
; (defparameter *B* (make-instance 'ra-variable :name "B" :domain '(1 2) ))
; (defparameter *C* (make-instance 'ra-variable :name "C" :domain '(1 2 3) ))
; (defparameter *scope* (make-instance 'scope :ra-variables (list *A* *B* *C*))) ;; when doing this :ra-variables '(*A* *B* *C*)
; (permutations *scope*)
; this does the same thing:
; (map-product 'list (ra-variable-domain *A*)  (ra-variable-domain *B*)  (ra-variable-domain *C*) )

;(defgeneric tuple (scope row-index)
;  (:documentation "returns a tuple of the scope for the given row")
;  ())

;(defgeneric val-of-tuple (tuple variable)
;  (:documentation "returns the value of the variable in the tuple")
;  ())

;(defgeneric join (scope scope)
;  (:documentation "Joins two scopes")

