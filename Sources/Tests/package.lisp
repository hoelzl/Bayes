(in-package #:common-lisp-user)

(defpackage #:bayes-test
  (:use #:common-lisp #:bayes)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bt))
