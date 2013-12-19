;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE in the root directory for
;;; further information.

(in-package #:bayes-tests)

(in-suite bayes-macro-suite)

(deftest test-process-class-slots-1 ()
  (is (equal (process-class-slots 'foo '() '()) '()))
  (let* ((processed-slots (process-class-slots
                           'foo '() '((slot-1)
                                      (slot-2 :accessor s2 :initarg :my-initarg))))
         (slot-1 (first processed-slots))
         (slot-2 (second processed-slots)))
    (destructuring-bind (name &key accessor initarg) slot-1
      (is (eq name 'slot-1))
      (is accessor 'foo-slot-1)
      (is initarg :slot-1))
    (destructuring-bind (name &key accessor initarg) slot-2
      (is (eq name 'slot-2))
      (is accessor 's2)
      (is initarg :my-initarg))))

(deftest test-process-class-options-1 ()
  (is (eq (process-class-options 'foo '()) '()))
  (is (equal (process-class-options 'foo '((:metaclass my-metaclss)))
             '((:metaclass my-metaclss))))
  (is (equal (process-class-options
              'foo '((:structure-class t) (:metaclass my-metaclss)))
             '((:metaclass my-metaclss))))
  (is (equal (process-class-options 
              'foo '((:default-initargs (x 1)) (:documentation "my class")
                     (:structure-class t) (:metaclass my-metaclss)))
             '((:default-initargs (x 1)) (:documentation "my class") 
               (:metaclass my-metaclss)))))

(deftest test-define-class-1 ()
  (let ((expansion (macroexpand-1 '(define-class foo ()
                                    ()))))
    (is (equal (first expansion) 'progn))
    (is (equal (second expansion) '(defclass foo () ())))
    (destructuring-bind (_defun name (_rest args _key _aok) &body body) (third expansion)
      (is (eq _defun 'defun))
      (is (eq name 'make-foo))
      (is (eq _rest '&rest))
      (is (symbolp args))
      (is (eq _key '&key))
      (is (eq _aok '&allow-other-keys))
      (is (equal body `((declare (ignorable))
                        (apply #'make-instance 'foo ,args)))))))

(deftest test-define-class-2 ()
  (let ((expansion (macroexpand-1 '(define-class bar (my-superclass-1 my-superclass-2)
                                    (my-slot-1
                                     (my-slot-2)
                                     (my-slot-3 :reader my-slot-3)
                                     (my-slot-4 :writer set-my-slot-4)
                                     (my-slot-5 :accessor my-slot-5))
                                    (:my-class-arg my-class-value)))))
    (let ((defclass-form (second expansion))
          (maker-form (third expansion)))
      (destructuring-bind (defclass name supers
                            (slot-1 slot-2 slot-3 slot-4 slot-5)
                            (class-arg class-arg-value))
          defclass-form
        (is (eq defclass 'defclass))
        (is (eq name 'bar))
        (is (equal supers '(my-superclass-1 my-superclass-2)))
        (destructuring-bind (name &key accessor initarg) slot-1
          (is (eq name 'my-slot-1))
          (is (eq accessor 'bar-my-slot-1))
          (is (eq initarg :my-slot-1)))
        (destructuring-bind (name &key accessor initarg) slot-2
          (is (eq name 'my-slot-2))
          (is (eq accessor 'bar-my-slot-2))
          (is (eq initarg :my-slot-2)))
        (destructuring-bind (name &key reader initarg) slot-3
          (is (eq name 'my-slot-3))
          (is (eq reader 'my-slot-3))
          (is (eq initarg :my-slot-3)))
        (destructuring-bind (name &key writer initarg) slot-4
          (is (eq name 'my-slot-4))
          (is (eq writer 'set-my-slot-4))
          (is (eq initarg :my-slot-4)))
        (destructuring-bind (name &key accessor initarg) slot-5
          (is (eq name 'my-slot-5))
          (is (eq accessor 'my-slot-5))
          (is (eq initarg :my-slot-5)))
        (is (eq class-arg :my-class-arg))
        (is (eq class-arg-value 'my-class-value)))
      (destructuring-bind (defun name (_rest args _key key1 key2 key3 key4 key5 _aok) &body body)
          maker-form
        (is (eq defun 'defun))
        (is (eq name 'make-bar))
        (is (eq _rest '&rest))
        (is (symbolp args))
        (is (eq _key '&key))
        (is (eq key1 'my-slot-1))
        (is (eq key2 'my-slot-2))
        (is (eq key3 'my-slot-3))
        (is (eq key4 'my-slot-4))
        (is (eq key5 'my-slot-5))
        (is (eq _aok '&allow-other-keys))
        (is (equal body `((declare (ignorable my-slot-1 my-slot-2 my-slot-3 my-slot-4 my-slot-5))
                          (apply #'make-instance 'bar ,args))))))))

(deftest test-define-class-3 ()
  (let ((expansion (macroexpand-1 '(define-class foo-struct ()
                                    ()
                                    (:structure-class t)))))
    (destructuring-bind (defstruct (name)) expansion
      (is (eq defstruct 'defstruct))
      (is (eq name 'foo-struct)))))

(deftest test-define-class-4 ()
  (let ((expansion (macroexpand-1 '(define-class bar ()
                                    (my-slot-1
                                     (my-slot-2)
                                     (my-slot-3 :type integer)
                                     (my-slot-4 :initform 'initial-value)
                                     (my-slot-5 :initform 123 :type integer))
                                    (:structure-class t)))))
    (destructuring-bind (defstruct (name) slot-1 slot-2 slot-3 slot-4 slot-5) expansion
      (is (eq defstruct 'defstruct))
      (is (eq name 'bar))
      (is (eq slot-1 'my-slot-1))
      (is (eq slot-2 'my-slot-2))
      (is (equal slot-3 '(my-slot-3 nil :type integer)))
      (is (equal slot-4 '(my-slot-4 'initial-value)))
      (is (equal slot-5 '(my-slot-5 123 :type integer))))))
