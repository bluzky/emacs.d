;; Git
;; Tell magit to automatically put us in vi-insert-mode when committing a change.
(use-package magit
  :hook (with-editor-mode . meow-insert-mode)
  :config
  ;; sort branch by last commit date
  (setq magit-list-refs-sortby "-committerdate")
  )

(use-package diff-hl
  :defer t
  :commands (diff-hl-mode)
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (add-hook 'prog-mode-hook #'diff-hl-mode)))
  :hook (diff-hl-mode . diff-hl-margin-mode))


;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :bind (:map smerge-mode-map
              ("C-c m n" . smerge-next)
              ("C-c m p" . smerge-prev)
              ("C-c m b" . smerge-keep-base)
              ("C-c m u" . smerge-keep-upper)
              ("C-c m l" . smerge-keep-lower)
              ("C-c m a" . smerge-keep-all)
              ("C-c m RET" . smerge-keep-current)
              ("C-c m r" . smerge-resolve)
              ("C-c m e" . smerge-ediff)))

(defun me/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (let ((repo (magit-get "remote" (magit-get-push-remote) "url")))
    (if (string-match "github\\.com" repo)
        (visit-gh-pull-request repo)
      (visit-bb-pull-request repo))))



(defun visit-gh-pull-request (repo)
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            repo)
           (magit-get-current-branch))))



;; Bitbucket pull requests are kinda funky, it seems to try to just do the
;; right thing, so there's no branches to include.
;; https://bitbucket.org/<username>/<project>/pull-request/new
(defun visit-bb-pull-request (repo)
  (browse-url
   (format "https://bitbucket.org/%s/pull-request/new"
           (replace-regexp-in-string
            "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
            repo))))

(provide 'init-git)
