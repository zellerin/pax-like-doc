;;;; package.lisp

(cl:defpackage #:cz.zellerin.doc
  (:use #:cl #:lol)
  (:export  #:defpackage #:define-section #:export-pkg-to-org)
  (:shadow #:defpackage)
  (:documentation "Trivial documentation annotations helpers based on MGL-PAX system.

The primary difference from PAX is that org format is used for the backend.
The secondary is that this is much smaller.")

  #+nil (:sections @pax-like-doc @pax-like-doc-export))
