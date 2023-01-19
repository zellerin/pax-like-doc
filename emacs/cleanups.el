;;;###autoload
(define-skeleton add-cleanup
  "Add cleanup skeleton with default cleanups list"
  "Cleanup name: "
  "(with-code-cleanup-needed " str " (:document-in-code
                                    :export-needed :add-or-fix-tests
                                    :simplify
                                    :fix-readme-documentation
                                    :check-function-names
                                    :use-or-factor-utilities
                                    :use-objects
                                    :think-exceptions
                                    :simplify))")
