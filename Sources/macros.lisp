;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun process-class-slots (class-name options slots)
  (mapcar (process-class-slot class-name options) slots))

(defun process-class-slot (class-name options)
  (let* ((conc-name (or (second (assoc :conc-name options)) class-name)))
    (lambda (slot)
      (destructuring-bind (slot-name &rest args &key initarg accessor reader writer
                                     &allow-other-keys)
          (ensure-cons slot)
        (unless initarg
          (setf args
                (list* :initarg (make-keyword slot-name) args)))
        (unless (or accessor reader writer)
          (setf args 
                (list* :accessor (format-symbol *package* "~A-~A" conc-name slot-name) args)))
        (cons slot-name args)))))

;;; TODO: This does not work for inherited initargs.
(defun extract-class-initargs (slots)
  (mapcar (lambda (slot)
            (destructuring-bind (slot-name &key initarg &allow-other-keys)
                (ensure-cons slot)
              (or (symbolicate initarg) slot-name)))
          slots))

(defun process-struct-slots (struct-name options slots)
  (mapcar (process-struct-slot struct-name options) slots))

(defun process-struct-slot (struct-name options)
  (declare (ignore struct-name options))
  (lambda (slot)
    (destructuring-bind (slot-name &key type read-only initform)
        (ensure-cons slot)
      (let ((args '()))
        (when type
          (setf args (list* :type type args)))
        (when read-only
          (setf args (list* :read-only read-only args)))
        ;; defstruct takes its initform as unnamed optional parameter
        (cond (initform
               (push initform args))
              (args
               (push nil args)))
        (if args
            (cons slot-name args)
            slot-name)))))
) ;eval-when

(defmacro define-class (name supers slots &rest options)
  (let ((structp (second (assoc :structure-class options))))
    (setf options (removef options :structure-class :key 'car))
    (if structp
        ;; TODO: Process slot and class options when defining structures so that more complex
        ;; definitions can be translated both ways.
        `(defstruct (,name ,@(when supers `((:include ,@supers))) ,@options)
           ,@(process-struct-slots name options slots))
        `(progn
           (defclass ,name ,supers
             ,(process-class-slots name options slots)
             ,@options)
           ;; TODO: Define a type predicate for classes (maybe use the correct convention for
           ;; the -P postfix and explicitly define the name of the type predicate for th struct
           ;; case as well.

           ;; TODO: Process initargs correctly and build a form that does not use apply.
           (defun ,(symbolicate '#:make- name)
               (&rest args &key ,@(extract-class-initargs slots) &allow-other-keys)
             (apply #'make-instance args))))))
