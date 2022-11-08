;;;; cz.zellerin.doc.asd

(asdf:defsystem "cz.zellerin.doc"
  :description "Describe cz.zellerin.doc here"
  :author "zellerin@gmail.com"
  :license  "Public domain"
  :version "1"
  :depends-on ("let-over-lambda" "cl-ppcre")
  :components ((:file "package")
               (:file "cz.zellerin.doc"))
  :in-order-to ((test-op (test-op "cz.zellerin.doc/test"))))

(defsystem "cz.zellerin.doc/tests"
  :depends-on ("cz.zellerin.doc" "lisp-unit")
  :components ((:file "doc-tests"))
  :perform (test-op (o c)
		    (symbol-call :lisp-unit
				 '#:run-tests :all (find-package "CZ.ZELLERIN.DOC/TEST"))))
