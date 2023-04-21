(define-short-documentation-group tz-doc
  (org-dblock-write:elisp-fns-doc)
  (sly-edit-section))

(defun emacs-docstring-to-org ()
  "Convert Emacs doc string to org format.

Replace backticked references (`princ`) with help links."
  (goto-char (point-min))
  (while (re-search-forward "‘\\([a-z-]+\\)’" nil t)
    (replace-match "[[help:\\1][\\1]]")))

(defun doc-dblock-expand-groups (group)
  "Extract names from a shortdoc groups."
  (when (stringp group)
    (setq group (intern group)))
  (let ((items (assq group shortdoc--groups)))
    (unless items (user-error "No such documentation group %s" group))
    (pop items)
    (remove nil (mapcar (lambda (item) (unless (stringp item) (car item))) items))))

(defun tz-documentation (fn-names short-doc-group custom-group)
  (dolist (fn-name (append (doc-dblock-expand-groups short-doc-group)
                           fn-names))
    (when (functionp fn-name)
      (insert "- [[help:" (symbol-name fn-name) "]["
              (symbol-name fn-name) "]] :: "
              (if (commandp fn-name) "=cmd=" "=fn="))
      (dolist (line (split-string  (documentation fn-name t) "\n"))
        (insert "  " line "\n"))))

  (dolist (var (custom-group-members custom-group nil))
    (let ((doc (documentation-property (car var) 'variable-documentation)))
      (insert "- [[help:" (symbol-name (car var)) "]["
              (symbol-name (car var)) "]] :: =" (symbol-name (second var)) "=")
      (insert
       (with-temp-buffer
         (dolist (line (split-string (or doc "Undocumented") "\n"))
           (insert "  " line "\n"))
         (emacs-docstring-to-org)
         (buffer-substring-no-properties (point-min) (point-max)))))))

;;;###autoload
(defun org-dblock-write:elisp-fns-doc (&optional params)
  "Write documentation from the elisp-fns-doc.

Used headers:
- :short-doc-group to describe a group defined by DEFINE-SHORT-DOCUMENTATION-GROUP
- :symbols for a list of symbols to describe
- :custom-group for a customization group
- :group when :custom-group and short-doc-group are same"
  (tz-documentation (plist-get params :symbols)
                    (or (plist-get params :short-doc-group) (plist-get params :group))
                    (or (plist-get params :custom-group) (plist-get params :group))))

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


(org-link-set-parameters "sly"
                         :follow (lambda (a) (sly-edit-definition a)))
