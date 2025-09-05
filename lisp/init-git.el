;; Git
;; Tell magit to automatically put us in vi-insert-mode when committing a change.
(use-package transient)

(use-package magit
  :hook (with-editor-mode . evil-insert-state)
  :config
  ;; sort branch by last commit date
  (setq magit-list-refs-sortby "-committerdate")
  )

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  (diff-hl-mode . diff-hl-margin-mode))


;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  ;; :hook
  ;; (prog-mode . smerge-mode)
  :bind (:map smerge-mode-map
              ("C-c s n" . smerge-next)
              ("C-c s p" . smerge-prev)
              ("C-c s b" . smerge-keep-base)
              ("C-c s u" . smerge-keep-upper)
              ("C-c s l" . smerge-keep-lower)
              ("C-c s a" . smerge-keep-all)
              ("C-c s RET" . smerge-keep-current)
              ("C-c s r" . smerge-resolve)
              ("C-c s k" . smerge-kill-current)
              ("C-c s E" . smerge-ediff))
  )

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
