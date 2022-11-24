(defun tz-documentation (fn-names)
  (dolist (fn-name fn-names)
    (insert "- [[help:" (symbol-name fn-name) "]["
            (symbol-name fn-name) "]] :: "
            (if (commandp fn-name) "=cmd=" "=fn="))
    (dolist (line (split-string  (documentation fn-name t) "\n"))
      (insert "  " line "\n"))))

;;;###autoload
(defun org-dblock-write:elisp-fns-doc (&optional params)
  (tz-documentation (plist-get params :fns)))

;;;###autoload
(define-skeleton org-dblock-create-elisp-block
  "Create a dblock documenting a function."
  nil
  "#+BEGIN: elisp-fns-doc :fns ("
  ("Function: " str " ")
  ")\n#+END:")

(defun sly-package-sections (package)
  "List sections in a PACKAGE (string with package name)"
  (sly-eval
   `(cl:getf cz.zellerin.doc:*package-sections*
             (cl:intern ,(upcase package) 'cz.zellerin.doc))))

(defun sly-edit-section (section)
  "Jump to SECTION definition. If called interactively, the
sections in current Sly package are offered for completition."
  (interactive
   (list (completing-read "Edit Section:"
                          (sly-package-sections
                           (let ((colon (cl-position ?\:  (sly-current-package))))
                             (if colon (substring (sly-current-package) (1+ colon))
                               (sly-current-package)))))))
  (sly-edit-definition section))


;;;###autoload
(defun org-dblock-create-lisp-block (package section)
  "Create a dblock documenting a function."
  (interactive
   (let ((p
          (completing-read "Package: "
                           (mapcar 'downcase
                                   (sly-eval '(cl:mapcar
                                               'cl:package-name (cl:list-all-packages)))))))
     (list p (completing-read "Section: " (sly-package-sections p)))))
  (insert "#+BEGIN: lisp-fns-doc :package " package " :section " section
          "\n#+END:"))


;;;###autoload
(with-eval-after-load "org"
  (org-dynamic-block-define "elisp-doc"
                            'org-dblock-create-elisp-block)
  (org-dynamic-block-define "lisp-doc"
                            'org-dblock-create-lisp-block))

;;;###autoload
(defun org-dblock-write:lisp-fns-doc (&optional params)
  "Update dynamic block at point with Lisp section documentation."
  (insert
   (sly-eval
    `(cl:with-output-to-string  (s)
        (cl:let ((cl:*package* (cl:find-package ',(plist-get params :package))))
           (cz.zellerin.doc:export-section-to-org s
              ',(plist-get params :section)))))))
