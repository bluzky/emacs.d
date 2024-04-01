;; Configure whichkey and setup keybindings Which-key ; Code:

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4
        which-key-allow-evil-operators t
        which-key-add-column-padding 1
        which-key-allow-multiple-replacements t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-max-display-columns 4 
        which-key-min-display-lines 4
        which-key-separator " : " 
        which-key-prevent-C-h-from-cycling t
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-use-C-h-for-paging t))

(defun setup-key-bindings()
  "Setup my custom keybindings"
  (evil-leader/set-key
   "'" 'vterm-toggle
   "e" 'treemacs
   "E" 'treemacs-select-window
   "Q" 'save-buffers-kill-emacs
   "R" 'restart-emacs

   ;; File
   "f" '("File" . (keymap))
   "ff" '("Find in project" . project-find-file)
   "fr" '("Recent files" . consult-recent-file)
   "fD" '("Delete" . me/delete-buffer-file)
   "fR" '("Rename" . rename-visited-file)
   "fS" '("Save as" . write-file)

   ;; Ask/AI
   "a" '("Search/AI" . (keymap))
   "ag" '("GPT prompt" . org-ai-prompt)
   "ar" '("GPT region" . org-ai-on-region)
   "as" '("GPT summarize" . org-ai-summarize)
   "ax" '("GPT explain code" . org-ai-explain-code)
   "af" '("GPT refactor code" . org-ai-refactor-code)
   
   ;;  Buffer
   "b" '("Buffer" . (keymap))
   ;; "bs" '("Save" . save-buffer)
   "bb" '("Switch buffer" . consult-buffer)
   "bd" '("Close current buffer" . kill-buffer)
   "bs" '("Scratch buffer" . scratch-buffer)
   "bS" '("Save all buffers" . save-some-buffers)
   "bn" '("New" . evil-buffer-new)
   "bt" '("Open in new tab" . tab-new)

   ;; Code
   "j" '("Code/jump" . (keymap))
   "jd" '("Find definition" . xref-find-definitions)
   "jf" '("Format buffer" . eglot-format-buffer)
   "jr" '("Find references" . xref-find-references)
   "ji" '("Buffer's symbols" . consult-imenu)
   "ja" '("Code action" . eglot-code-actions)
   "jj" '("avy jump" . avy-goto-char-timer)
   "jl" '("avy line" . avy-goto-line)
   "jw" '("avy word" . avy-goto-word-0)
   "je" '("next error" . flymake-goto-next-error)
   "jE" '("previous error" . flymake-goto-prev-error)

   "g" '("Magit" . (keymap))
   "gs" '("status" . magit-status)
   "gp" '("create PR" . me/visit-pull-request-url)
   "gb" '("blame" . magit-blame-addition)
   "gl" '("log current file" . magit-log-buffer-file)
   "gd" '("diff changed" . magit-diff-unstaged)
   "gr" '("PR review" . pr-review-search)
   "gR" '("PR review open" . pr-review)

   "h" '("Help" . (keymap))
   "hf" '("function" . describe-function)
   "hk" '("key" . describe-key)
   "ht" '("change theme" . consult-theme)
   "hv" '("variable" . describe-variable)
   "hl" '("absolute line number" . (lambda () (interactive) (setq display-line-numbers t)))
   "hL" '("relative line number" . (lambda () (interactive) (setq display-line-numbers 'relative)))

   "i" '("Insert" . (keymap))
   "ir" '("from kill ring" . yank-from-kill-ring)
   "iy" '("snippets" . consult-yasnippet)

   "s" '("Search" . (keymap))
   "ss" '("Search buffer" . swiper)
   "sS" '("Search buffer with input" . swiper-thing-at-point)
   "sp" '("Search project" . consult-ripgrep)
   "sP" '("Search project with input" . me/search-projectile)
   "sf" '("Search dir" . me/search-dir)
   "sF" '("Search dir with input" . me/search-dir-with-input)
   "sr" '("Resume last search" . vertico-repeat-last)

   "o" '("Open" . (keymap))
   "op" '("project" . projectile-switch-project)
   "ob" '("bookmark" . list-bookmarks)

   "l" '("Open link" . (keymap))
   "lg" '("Github opollo" . (lambda () (interactive) (browse-url "https://github.com/onpointvn/opollo")))
   "ld" '("Djadmin" . (lambda () (interactive) (browse-url "https://djadmin.onpoint.vn/")))
   "la" '("Admin" . (lambda () (interactive) (browse-url "https://admin.onpoint.vn/")))
   "ll" '("Logging" . (lambda () (interactive) (browse-url "https://logging.onpoint.vn/")))
   "lk" '("Local kk" . (lambda () (interactive) (browse-url "http://localhost:5200/api-docs")))
   "ls" '("Ducduck go" . (lambda () (interactive) (browse-url "https://duckduckgo.com")))

   "w" '("Window/tab" . (keymap))
   "wt" '("switch tab" . tab-switch)
   "ww" '("switch window" . ace-select-window)

  ;;  "u" '("UI/UX" . (keymap))
  ;;  "ul" '("Relative line number" . (lambda () (setq display-line-numbers 'relative)))
  ;;  "uL" '("Relative line number" . (lambda () (setq display-line-numbers t)))
  ))

;; Set leader key to SPC
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (setup-key-bindings))

(provide 'init-whichkey)
