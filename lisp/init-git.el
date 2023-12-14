;; Git
;; Tell magit to automatically put us in vi-insert-mode when committing a change.
(use-package magit
  :preface
  :hook (with-editor-mode . evil-insert-state))

(defun me/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com[:/]\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(provide 'init-git)
