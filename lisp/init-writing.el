(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/notes")
(use-package org
  :config
  (add-to-list 'org-src-lang-modes '("elixir" . elixr-ts))

  :custom
  (org-default-notes-file (concat org-directory "/Inbox.org"))
  )

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(use-package denote
  :ensure t
  :bind
  (("C-c c" . denote)                    ;; Create new note
   ("C-c n n" . denote-open-or-create)     ;; Find or create note
   ("C-c n i" . denote-link-or-create)     ;; Insert link to note
   ("C-c n l" . denote-backlinks)          ;; Show backlinks to current note
   ;; Custom search commands using consult
   ("C-c n f" . denote-notes-find-file)    ;; Find files with fd
   ("C-c n s" . denote-notes-search))      ;; Search content with ripgrep
  :config
  ;; Set the directory where your notes will be stored
  (setq denote-directory (concat org-directory "/denote"))

  ;; Optional: Set preferred note-taking file format
  (setq denote-file-type 'org)

  ;; Automatically create the notes directory if it doesn't exist
  (make-directory denote-directory t)

  ;; Custom function to find files in denote directory using consult-fd
  (defun denote-notes-find-file ()
    "Find files in `denote-directory' using `consult-fd'."
    (interactive)
    (let ((default-directory denote-directory))
      (consult-fd)))

  ;; Custom function to search in denote directory using consult-ripgrep
  (defun denote-notes-search ()
    "Search in `denote-directory' using `consult-ripgrep'."
    (interactive)
    (consult-ripgrep denote-directory)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments")
  (add-to-list 'markdown-code-lang-modes '("elixir" . elixir-ts-mode)))

(use-package d2-mode
  :mode "\\.d2\\'"
  :config
  (setenv "D2_LAYOUT" "dagre")
  (setq d2-output-format ".png"))

(provide 'init-writing)
