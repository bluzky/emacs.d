(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/notes")


;; (require 'slash-commands)
(use-package slash-commands
  :quelpa (slash-commands :repo "bluzky/slash-commands" :fetcher github)
  :config
  (global-slash-commands-mode)
  )

(use-package org
  :config
  (add-to-list 'org-src-lang-modes '("elixir" . elixir-ts))
  :hook (org-mode . (lambda () (toggle-truncate-lines nil)))
  :custom
  (org-default-notes-file (concat org-directory "/Inbox.org"))
  )


(use-package org-bullets :hook (org-mode . org-bullets-mode))


;; Set up org-mode slash commands
(add-hook 'org-mode-hook
          (lambda ()
            (slash-commands-register-commands
             '(("todo" . (lambda () (org-todo "TODO")))
               ("done" . (lambda () (org-todo "DONE")))
               ("heading" . (submenu
                             ("H1" . (lambda () (org-insert-heading)))
                             ("H2" . (lambda () (org-insert-subheading nil)))
                             ))
               ("checkbox" . (lambda () (insert "[ ] ")))
               ("table" . (lambda () (org-table-create "3x3")))
               ("insert" .(submenu
                           ("code" . (lambda ()
                                       (insert "#+BEGIN_SRC \n\n#+END_SRC")
                                       (forward-line -1)))
                           ("quote" . (lambda ()
                                        (insert "#+BEGIN_QUOTE\n\n#+END_QUOTE")
                                        (forward-line -1)))
                           ))
               ("today" . (lambda () (insert (format-time-string "%d-%m-%Y"))))
               ("now" . (lambda () (insert (format-time-string "%H:%M:%S"))))
               ("templates" . (submenu
                               ("6 times book" . (lambda()
                                                   (insert "- 🥲 \n- 🥲 \n- ✅ \n- ✅ \n- TODOs:\n  + [ ] \n  + [ ] \n")
                                                   (forward-line -7)
                                                   (forward-char 4)))
                               ("meeting" . (lambda ()
                                              (insert "* Meeting Notes\n** Attendees\n\n** Agenda\n\n** Action Items\n\n")))
                               ("project" . (lambda ()
                                              (insert "* Project: \n** Description\n\n** Tasks\n\n** Timeline\n\n")))
                               ("weekly" . (lambda ()
                                             (insert "* Weekly Review\n** Achievements\n\n** Challenges\n\n** Next Week\n\n")))
                               ))))))

;; Set up markdown-mode slash commands
(add-hook 'markdown-mode-hook
          (lambda ()
            (slash-commands-register-commands
             '(("h1" . (lambda () (insert "# ")))
               ("h2" . (lambda () (insert "## ")))
               ("h3" . (lambda () (insert "### ")))
               ("code" . (lambda () (insert "```\n\n```") (forward-line -1)))
               ("link" . (lambda () (insert "[]()") (backward-char 3)))
               ("image" . (lambda () (insert "![]()") (backward-char 3)))
               ("quote" . (lambda () (insert "> ")))
               ("export word" . (lambda ()
                                  (shell-command (format "pandoc -s \"%s\" -o \"%s.docx\"" (buffer-file-name) (file-name-sans-extension (buffer-file-name))))
                                  (shell-command (format "open \"%s.docx\"" (file-name-sans-extension (buffer-file-name))))))))))



;; Enable the minor mode in specific major modes
;; (add-hook 'org-mode-hook 'slash-commands-mode)
;; (add-hook 'markdown-mode-hook 'slash-commands-mode)

;; Denote is a note-taking package for Emacs that focuses on simplicity and
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


(defun color-is-light-p (hex-color)
  "Calculate the luminance of a hex color string like #RRGGBB.
Returns a value between 0.0 (dark) and 1.0 (light)."
  (let* ((hex (if (string-prefix-p "#" hex-color)
                  (substring hex-color 1)
                hex-color))
         (r (string-to-number (substring hex 0 2) 16))
         (g (string-to-number (substring hex 2 4) 16))
         (b (string-to-number (substring hex 4 6) 16)))
    (>(/ (+ (* r 0.299)
            (* g 0.587)
            (* b 0.114))
         255.0) 0.5)))

;; customize markdown-mode faces based on the current theme
;; Create a function that will be called when markdown-mode loads
(defun my-markdown-theme-integration ()
  ;; Define a function to update faces based on current theme
  (defun my-update-markdown-faces ()

    (let ((bg-color (face-background 'default nil t)))
      (if (color-is-light-p bg-color)
          ;; Set the background color for light mode
          (custom-set-faces
           '(markdown-code-face ((t (:background "#FDF6E3" :extend t)))))

        ;; Set the background color for dark mode
        (custom-set-faces
         '(markdown-code-face ((t (:background "#282C34" :extend t))))))
      ))


  ;; Add it to theme hooks
  (advice-add 'load-theme :after
              (lambda (&rest _) (when (featurep 'markdown-mode) (my-update-markdown-faces))))

  ;; Run once for initial setup
  (my-update-markdown-faces))

;; Set up the hook to run when markdown-mode loads
(with-eval-after-load 'markdown-mode
  (my-markdown-theme-integration))

(provide 'init-writing)
