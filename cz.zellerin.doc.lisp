;;;; cz.zellerin.doc.lisp

(in-package #:cz.zellerin.doc)

(defvar *package-sections* '(cz.zellerin.doc (@pax-like-doc @pax-like-doc-export))
  "List of sections in a package. For the doc package it is set
  manually, elsewhere it should be taken over from the defpackage.

This is used only for exporting.")

(defmacro define-section (name docstring &rest content)
  "Define a section for documentation. Section contains a documentation string and list of objects in documented interface to the section.

The objects are exported as a side effect."
  `(progn (defun ,name () ,docstring nil)
	  (setf (get ',name 'exports) ',content)
	  (mapcar 'export ',(mapcar #'car content))))

(define-section @pax-like-doc
  "MGL-PAX style documentation utilities."
  (define-section)
  (defpackage))

(defmacro defpackage (name &body defs)
  "Defpackage variant that strips (and stores elsewhere) :sections and
provides :export section with already exported symbols."
  `(progn
     (cl:defpackage ,name
       ,@(remove :sections defs :key 'car)
       (:export ,@(let (s (p (find-package name)))
		    (when p (do-external-symbols (v p) (push v s))) s)))
     (setf
      (getf *package-sections*
	    ',(intern (string name) 'cz.zellerin.doc))
      (mapcar (lambda (a) (intern (string a) ',name))
		',(cdr (assoc :sections defs))))))

(define-section @pax-like-doc-export
  "Export documentation to org mode. The structure is:
- package has sections (in org)
- sections refer to functions and other source items
- function has documentation

*Security note*: The text from docstrings is considered to be already in
org mode and inserted verbatim. This means that if you run it on some
package you did not write, have unsafe org mode settings and open it,
you may get surprises."
  (export-pkg-to-org))


(defun export-fn-to-org (out fn &optional (type 'function))
  "Print out function documentation as a level 3 section."
  (format out "*** =~a= ~60t:~(~a~):~%~a~2&" fn type
	  (documentation fn type)))

(defun export-section-to-org (out fn)
  "Print section and its functions in the org format to the stream."
  (format out "** ~a ~60t:section:~%~a~2&" fn
	  (documentation fn 'function))
  (dolist (e (get fn 'exports))
    (apply 'export-fn-to-org out e)))

(defun export-pkg-to-org (&key (pkg *package*)
			      (package-name (package-name pkg))
			    (file (format nil "~a.org" package-name)))
  "Export package documentation to a file.
Package documentation consists of:
- Package documentation string
- Sections referenced in the package :section definition
- Objects referenced in the section definition.

PACKAGE-NAME is used in text and as default for the file name."

  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "#+options: toc:t~%* ~a ~60t:package:~%~a~2%" package-name
	    (documentation pkg t))
    (dolist (sect (getf *package-sections*
			;; we need canonical name below, not provided one
			(intern (package-name pkg) 'cz.zellerin.doc)))
      (export-section-to-org out (intern (string-upcase sect))))))
