(in-package #:common-lisp-user)

(defpackage #:bayes
  ;; Resolve conflicts between GBBOpen and Alexandria
  (:shadowing-import-from #:alexandria
                          #:copy-file
                          #:ensure-list
                          #:make-keyword
                          #:set-equal
                          #:with-gensyms
                          #:xor)
  (:use #:common-lisp
        #:alexandria #:gbbopen-tools)
  (:export))

(defpackage #:bayes-user
  ;; Resolve conflicts between GBBOpen and Alexandria
  (:shadowing-import-from #:alexandria
                          #:copy-file
                          #:ensure-list
                          #:make-keyword
                          #:set-equal
                          #:with-gensyms
                          #:xor)
  (:use #:common-lisp #:common-lisp-user
        #:alexandria #:bayes #:gbbopen-tools)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bu))

