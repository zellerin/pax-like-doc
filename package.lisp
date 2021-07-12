;;;; This uses cl:defpackage for obvious reasons. We
;;;; still provide the :sections, but it is not valid so far.

(cl:defpackage #:cz.zellerin.doc
  (:use #:cl #:lol)
  (:export  #:defpackage #:define-section #:export-pkg-to-org  #:export-classes)
  (:shadow #:defpackage)
  #+nil (:sections @annotate @export)
  (:documentation "Trivial documentation annotations helpers based on MGL-PAX system.

The primary difference from PAX is that org format is used for the backend.
The secondary is that this is much smaller.

Last difference might be that elisp helpers would be for sly, not for
slime."))
