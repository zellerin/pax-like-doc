;;;; cz.zellerin.doc.asd

(asdf:defsystem #:cz.zellerin.doc
  :description "Describe cz.zellerin.doc here"
  :author "zellerin@gmail.com"
  :license  "Public domain"
  :version "1"
  :depends-on ("let-over-lambda")
  :components ((:file "package")
               (:file "cz.zellerin.doc")))
