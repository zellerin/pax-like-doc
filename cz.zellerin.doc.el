(defun sly-edit-current-package ()
  (interactive)
  (sly-edit-definition
   (sly-eval '(cl:package-name cl:*package*))))
