(in-package #:common-lisp-user)

(defpackage #:bayes
  (:shadowing-import-from #:alexandria
                          #:copy-file
                          #:ensure-list
                          #:make-keyword
                          #:set-equal
                          #:with-gensyms
                          #:xor
                          )
  (:use #:common-lisp #:alexandria #:gbbopen-tools)
  #+ (or) (:export))

(defpackage #:bayes-user
  (:use #:common-lisp #:common-lisp-user #:bayes)
  #+define-nicknames-for-poem-packages
  (:nicknames #:bu))
