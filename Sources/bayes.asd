;;;; bayes.asd

(asdf:defsystem #:bayes
  :serial t
  :description "Bayesian reasoning"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
               #:gbbopen
               #:gbbopen-tools)
  :components ((:file "package")))
