;;;; cz.zellerin.doc.lisp

(in-package #:cz.zellerin.doc)

(defvar *package-sections* '(cz.zellerin.doc (@annotate @export @internal))
  "List of sections in a package. For the doc package it is set manually,
elsewhere it should be taken over from the defpackage.

This is used only for exporting.")

(defmacro define-section (name docstring &rest content)
  "Define a section for documentation. Section contains a documentation
string and list of objects in documented interface to the section.

The objects are exported as a side effect."
  `(progn (defun ,name () ,docstring nil)
	  (setf (get ',name 'exports) ',content)
	  (mapcar 'export ',(mapcar #'car content))))

(define-section @annotate
  "MGL-PAX style documentation utilities.

The idea is that apart from package and code, the author defines
sections that verbally describes some block of functionality and lists
interface functions and variables. These are also by default exported,
see below.

The sections are defined by DEFINE-SECTION as a function and can be
jumped to using standard editor shortcut (=M-.=) from their names.

The place to jump from is the package definition. Standard
=cl:defpackage= is replaced with =defpackage= that expects section
names on the input, and passes exported functions from the
=define-section= to the CL version. Elisp function
`sly-edit-current-package' is provided to jump to the package
definition.

The documentation strings are expected to be more or less org mode
format. Using poporg-mode (and binding it to =C-\"=) makes it easier both
to write and to read."
  (define-section)
  (defpackage))

(defmacro defpackage (name &body defs)
  "Defpackage replacement. The format is same as for `cl:defpackage' with two exceptions:
- It expects =:sections= option on the input, and stores its value as
  list of sections that document the package
- The =:export= option is not expected, and is replaced by the currently
  known list of already exported symbols from the package, if the
  package already existed, or left empty otherwise. The idea is that
  sections define what is exported.
All other parameters are passed to =cl:defpackage= as is..
"
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

(define-section @export
  "Export documentation to org mode. The structure is:
- package has sections (in org)
- sections refer to functions and other source items
- function has documentation

*Security note*: The text from docstrings is considered to be already
in org mode and inserted verbatim. This means that if you export
documentation from a package someone else wrote documentation, *and*
have unsafe org mode settings, *and* open output in the emacs, you may
get surprises."
  (export-pkg-to-org))

(defun export-fn-to-org (out fn &optional (type 'function))
  "Print out function documentation as a level 3 section."
  (format out "*** =~a= ~60t:~(~a~):~%~a~2&" fn type
	  (documentation fn type))
  (when (and (eql type 'type)
	     (find-class fn))
    (dolist (slot (sb-mop:compute-slots (find-class fn)))
      (when (documentation slot t)
	(format out "~&- ~A :: ~a~%"
		(sb-mop:slot-definition-name slot)
		(documentation slot t ))))
    (format out "~2&")))

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

(define-section @internal
  "Entry points possible useful for debugging and maintenance."
  (*package-sections* variable)
  (update-readme))

(defun update-readme ()
  "Update readme file for the package. Intended to be run after
  sly-mrepl-sync, when both =*package*= and directory are synchronized."
  (export-pkg-to-org :pkg *package* :file "README.org"
		     :package-name "doc"))
