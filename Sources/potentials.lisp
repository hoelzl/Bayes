;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(define-class potential ()
  ())

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


;(defgeneric scope-permutations (scope) 
;  (:documentation "get all possible permutations for the given scope")
;  (:method (scope)
;    (let ((variables (scope-ra-variables))
;          (variables-length (length variables)))
;      (dotimes (i variables-length)
;        (
;      )))


;(defun find-permutations (lst i)
;  (cond ((= i (length lst)) (format t "~S~%" lst))
;        (t (let ((j i))
;	     (dotimes (j (length lst))
;               (rotatef (nth i lst) (nth j lst))
;	       (find-permutations lst (+ i 1))
;               (rotatef (nth i lst) (nth j lst)) ; cleanup return to previous condition
;)))))


;(defgeneric permutations (scope)
;  (:documentation "renders all permutations of the values of variables in the scope.")
;  (:method (scope)
;    (let ((variables (scope-ra-variables scope)))
;      (dotimes (i (length variables)) ; variables contains relationalAlgebra variables *A* e.g.
;        (let ((var-at-i (elt variables i))) ; var-at-i = *A*
;          ;(coerce var-at-i 'ra-variable) ; casting variable to variable, so that we can call object-specific functions 
;          (let ((values-at-i (ra-variable-domain var-at-i))) 
;            (dotimes (j (length values-at-i))
;              (let ((k (+ 1 j)))
;                (dotimes (k (- length values-at-i 1))
;                  (format t "~S: ~S~%" (ra-variable-name var-at-i) (elt values-at-i j))
;            )
;          )
;        )
;      )
;    )
;  )
;)

;(defgeneric permutations (scope)
;  (:documentation "renders all permutations of the values of variables in the scope.")
;  (:method (scope)
;    (let ((variables (scope-ra-variables scope)))
;      (dotimes (i (length variables)) ; variables contains relationalAlgebra variables *A* e.g.
;        (let ((var-at-i (elt variables i))) ; var-at-i = *A*
;          ;(coerce var-at-i 'ra-variable) ; casting variable to variable, so that we can call object-specific functions 
;          (let ((values-at-i (ra-variable-domain var-at-i))) 
;            (dotimes (j (length values-at-i))
;                (format t "~S= ~S~%" (ra-variable-name var-at-i) (elt values-at-i j))
;                (nconc (ra-variable-named-domain-list var-at-i)  ;must use nconc, so we have call-by-reference effect
;                       (format nil "~a=~a" (ra-variable-name var-at-i) (elt values-at-i j)) 
;                )
;            )
;          )
;        )
;      )
;    )
;  )
;)
; (setf *s* (nconc *s* (list (format nil ...))))  nasty!!!
; push!!!


(defgeneric permutations (scope)
  (:documentation "renders all permutations of the values of variables in the scope.")
  (:method (scope)
    (let ((variables (scope-ra-variables scope)))
      (dotimes (i (length variables)) ; variables contains relationalAlgebra variables *A* e.g.
        (let ((var-at-i (elt variables i))) ; var-at-i = *A*
          ;(coerce var-at-i 'ra-variable) ; casting variable to variable, so that we can call object-specific functions 
          (let ((values-at-i (ra-variable-domain var-at-i))) 
            (dotimes (j (length values-at-i))
              (if (= 0 j) (setf (ra-variable-named-domain-list var-at-i) '())) ; reset list for each permutation call
              (format t "~S= ~S~%" (ra-variable-name var-at-i) (elt values-at-i j))
              (push   (format nil "~a=~a" (ra-variable-name var-at-i) (elt values-at-i j))
                        (ra-variable-named-domain-list var-at-i)  ;must use push, so we have call-by-reference effect
              ))))))
    (let ((variables (scope-ra-variables scope))
          (input-list nil))
      (dotimes (i (length variables))
        (push (ra-variable-named-domain-list (elt variables i)) input-list))
      (multiple-value-call #'combinations (values-list input-list))))) ;using multiple results by applying combinations function via multiple-value-call function

; works only if input lists are not empty
; returns all combinations of the list elements 
(defun combinations (&rest lists)
  (if (car lists)
    (mapcan 
      (lambda (inner-val) (mapcar 
        (lambda (outer-val) (cons outer-val inner-val)) (car lists))
      )
      (apply #'combinations (cdr lists))
    )
  ; else
  (list nil))
)

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
 (defparameter *A* (make-instance 'ra-variable :name "A" :domain '(1 2 3 4) ))
 (defparameter *B* (make-instance 'ra-variable :name "B" :domain '(1 2) ))
 (defparameter *C* (make-instance 'ra-variable :name "C" :domain '(1 2 3) ))
 (defparameter *scope* (make-instance 'scope :ra-variables (list *A* *B* *C*))) ;; when doing this :ra-variables '(*A* *B* *C*)
; (permutations *scope*)

;(defgeneric tuple (scope row-index)
;  (:documentation "returns a tuple of the scope for the given row")
;  ())

;(defgeneric val-of-tuple (tuple variable)
;  (:documentation "returns the value of the variable in the tuple")
;  ())

;(defgeneric join (scope scope)
;  (:documentation "Joins two scopes")

