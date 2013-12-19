(in-package #:common-lisp-user)

(defpackage #:bayes-tests
  (:use #:common-lisp 
        #:alexandria #:bayes-implementation #:stefil)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bt))
