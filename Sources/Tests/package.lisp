(in-package #:common-lisp-user)

(defpackage #:bayes-tests
  ;; Resolve conflicts between GBBOpen and Alexandria
  (:shadowing-import-from #:alexandria
                          #:copy-file
                          #:ensure-list
                          #:make-keyword
                          #:set-equal
                          #:with-gensyms
                          #:xor)
  (:use #:common-lisp 
        #:alexandria #:bayes #:fiveam #:gbbopen-tools)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bt))
