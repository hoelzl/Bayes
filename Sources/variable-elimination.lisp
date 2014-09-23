;;; -*- Mode: Lisp; common-lisp-style: poem -*-
;;; Copyright (c) 2014 Thomas Rogge-Solti
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(defparameter *empty-element* '(empty 0))
(defparameter *neutral-addition-element* 0)
(defparameter *neutral-multiplication-element* 1)

(defun variable-elimination (bayes-net vars-to-eliminate-in-order)
  "Implementation of VE_PR1 of Darwiche on p. 134. Returns the prior marginal Pr(vars-to-keep)"
  (let ((s (bn-cpts bayes-net)))
    (dolist (var-to-eliminate vars-to-eliminate-in-order)
      (let ((cpts-containing-elimination-var (cpts-containing-var s var-to-eliminate)))
        (let ((f (multiply-factors cpts-containing-elimination-var)))
          (let ((f-at-i (sum-out-vars f (list var-to-eliminate))))
            ;; replace all factors in S that were multiplied before by f-at-i
            ;; removes original cpts that contain the var that was just eliminated 
            (setf s (set-difference s cpts-containing-elimination-var :test #'equal))
            ;; adding new factor to S
            (setf s (cons f-at-i s))))))
    ;; return multiplication of all remaining factors in S to get the prior marginal over the remaining vars
    (multiply-factors s)))

(defun sum-out-vars (cpt vars-to-sum-out)
  "implementation of SumOutVars on Darviche page 130"
  (if (null vars-to-sum-out) 
      ;; return cpt if there are no vars to be summed out
      cpt
      ;; else: sum out!
      (let ((x (cpt-vars cpt))    ; x = all vars  
            (z vars-to-sum-out))  ; z = vars to sum out
        ;; y = vars to keep
        (let* ((y (set-difference x z :test #'equal)) ; removes z from x
               (result-cpt (build-cpt-for-vars y *neutral-addition-element* *empty-element*)))
          (loop for key being the hash-keys of (cpt-hashtable result-cpt)
                using (hash-value value)
                do 
                   (loop for base-key being the hash-keys of (cpt-hashtable cpt)
                         using (hash-value base-value)
                         do 
                            (when (equal 
                                   (length (intersection key base-key :test #'equal)) 
                                   (length key))
                              ;; y instantiation is contained in x instatiation -> sum
                              ;; key is contained in base-key
                              (setf (gethash key (cpt-hashtable result-cpt)) 
                                    (+ base-value (gethash key (cpt-hashtable result-cpt)))))
                            (when (eq key *empty-element*) 
                              (setf (gethash key (cpt-hashtable result-cpt))
                                    (+ base-value (gethash key (cpt-hashtable result-cpt)))))
                            #+(or)(format t "The base-value associated with the base-key ~S is ~S~%" base-key base-value)
                         )
                   #+(or)(format t "The value associated with the key ~S is ~S~%" key value)
                )
          ;; print the result
          #+(or)(loop for key being the hash-keys of (cpt-hashtable result-cpt)
                using (hash-value value)
                do
                   (format t "The value associated with the key ~S is ~S~%" key value))
          result-cpt))))


(defun multiply-factors (cpts-to-multiply)
  "Implementation of MultiplyFactors on Darviche page 131"
  (cond ((or (null cpts-to-multiply) 
             (= (length cpts-to-multiply) 0)) 
         (format t "Factor multiplication called without an argument. Returning nil.")
         nil)
        ((= (length cpts-to-multiply) 1)
         #+(or)(format t "Factor multiplication called with only one factor. Returning factor unchanged.")
         (car cpts-to-multiply))
        (t ;; z = union-list of all vars from the cpts to be multiplied
         (let* ((z (get-union-of-vars-from-cpts cpts-to-multiply))
                (result-cpt (build-cpt-for-vars z *neutral-multiplication-element* *empty-element*)))
           (loop for result-key being the hash-keys of (cpt-hashtable result-cpt)
                 using (hash-value result-value)
                 do
                    (dolist (cpt cpts-to-multiply)
                      (loop for cpt-key being the hash-keys of (cpt-hashtable cpt)
                            using (hash-value cpt-value)
                            do
                               (when (equal 
                                      (length (intersection cpt-key result-key :test #'equal)) 
                                      (length cpt-key))
                                 (setf (gethash result-key (cpt-hashtable result-cpt)) 
                                       (* cpt-value (gethash result-key (cpt-hashtable result-cpt))))))))
           result-cpt))))

