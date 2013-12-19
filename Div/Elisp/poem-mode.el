;;; -*- Mode: Emacs-Lisp -*-

;;; Copyright (c) 2013 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

;;; The following code snippet defines the lisp indentation mode used
;;; for source code in all packages of the Iliad implementation.
;;; Place it in your Emacs init file after the (require 'slime) form,
;;; or ensure that it is evaluated in a hook.

(define-common-lisp-style "poem"
  "The indentation style for Poem/Iliad source files."
  (:inherit "basic")
  (:eval
   (whitespace-mode 1))
  (:variables
   (lisp-lambda-list-keyword-alignment t)
   (lisp-lambda-list-keyword-parameter-alignment t)
   (lisp-lambda-list-keyword-parameter-indentation 0)
   (lisp-loop-indent-subclauses t)
   (whitespace-style (face tabs trailing))
   (indent-tabs-mode nil)
   (comment-fill-column nil)
   (fill-column 96))
  (:indentation
   (make-instance (4 2))
   (make-load-form-saving-slots (4 2))
   (make-space-instance (as make-instance))
   (make-duplicate-instance (as make-instance))
   (while (as when))
   (until (as when))
   (mvbind (as multiple-value-bind))
   (mvsetq (as multiple-value-setq))
   (dbind (as destructuring-bind))
   (def-feature (as defun))))

;;; To use this indentation style put the following line at the top of
;;; your source code files.

;;; -*- Mode: Lisp; common-lisp-style: poem -*-
