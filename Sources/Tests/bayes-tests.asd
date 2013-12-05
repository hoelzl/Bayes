;;;; bayes-tests.asd

(asdf:defsystem #:bayes
  :serial t
  :description "Tests for the Bayes package."
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
               #:gbbopen
               #:gbbopen-tools
               #:bayes)
  :components ((:file "package")))
