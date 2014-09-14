;;;; bayes-tests.asd

(asdf:defsystem #:bayes-tests
  :serial t
  :description "Tests for the Bayes package."
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
               #:bayes
               #:stefil)
  :components ((:file "packages")
               (:file "suites")
	       (:file "test-setup")
               (:file "macro-tests")
               (:file "utilities-tests")
               (:file "node-tests")
	       (:file "potential-tests")
	       (:file "cpt-tests")))
