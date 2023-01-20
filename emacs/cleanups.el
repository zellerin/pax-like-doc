;;;###autoload
(define-skeleton add-cleanup
  "Add cleanup skeleton with default cleanups list"
  "Cleanup name: "
  "(with-code-cleanup-needed " str " (:export-needed :add-or-fix-tests
                                    :fix-readme-documentation
                                    :check-function-names
                                    :use-or-factor-utilities
                                    :use-objects
                                    :think-exceptions
                                    :simplify
                                    :no-private-data))")
