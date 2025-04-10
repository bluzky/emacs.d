;; Configure whichkey and setup keybindings Which-key ; Code:

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)

(require 'functions)

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
    "fF" '("Find file at point" . me/find-file-at-point-with-line)
    "fg" '("Find file with line" . me/find-file-with-line)
    "fr" '("Recent files" . consult-recent-file)
    "fD" '("Delete" . me/delete-buffer-file)
    "fR" '("Rename" . rename-visited-file)
    "fS" '("Save as" . write-file)
    "fy" '("Copy file path" . me/copy-buffer-relative-path)
    "fY" '("Copy file path" . me/copy-buffer-abs-path)

    ;; Ask/AI
    "a" '("AI/Application" . (keymap))
    "aa" '("Ask Gptel" . gptel)
    "am" '("Gptel Menu" . gptel-menu)


    "e" '("Relysium" . (keymap))
    "ea" '("Ask about code" . relysium-ask)
    "ee" '("Complete at point" . relysium-edit-dwim)
    "eb" '("Add context" . relysium-buffer-add-context)
    "ec" '("Clear chat buffer" . relysium-buffer-clear)
    "ed" '("Show debug log" . relysium-debug-log)
    "eD" '("Toggle debug mode" . relysium-toggle-debug-mode)
    "eg" '("Generate from comments" . relysium-generate-from-comments)
    "es" '("Suggest code" . relysium-suggest)
    "et" '("Toggle chat window" . relysium-buffer-toggle-window)


    ;;  Buffer
    "b" '("Buffer" . (keymap))
    ;; "bs" '("Save" . save-buffer)
    "bb" '("Switch buffer" . consult-buffer)
    "bd" '("Close current buffer" . kill-buffer)
    "bs" '("Open scratch" . (lambda () (interactive) (find-file "~/.scratch")))
    "bS" '("Save all buffers" . save-some-buffers)
    "bn" '("New" . evil-buffer-new)
    "bt" '("Open in new tab" . tab-new)

    ;; Code
    "c" '("Code" . (keymap))
    "cf" '("Format buffer" . lsp-bridge-code-format)
    "ca" '("Code action" . lsp-bridge-code-action)
    "cx" '("Execute code" . quickrun)
    "cX" '("Execute code region" . quickrun-region)
    "ci" '("Function outline" . imenu-list-smart-toggle)
    "ce" '("next error" . flymake-goto-next-error)
    "cE" '("previous error" . flymake-goto-prev-error)

    ;; Jump
    "j" '("Jump" . (keymap))
    "jd" '("Find definition" . citre-jump)
    "jr" '("Find references" . xref-find-references)
    "ji" '("Buffer's symbols" . consult-imenu)
    "jj" '("avy jump" . avy-goto-char-timer)
    "jl" '("avy line" . avy-goto-line)
    "jw" '("avy word" . avy-goto-word-0)

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

    "n" '("Note/Writing" . (keymap))
    "nn" '("Open or create" . denote-open-or-create)
    "nc" '("New note" . denote)
    "nf" '("Find file" . denote-notes-find-file)
    "nj" '("Journal" . denote-journal-extras-new-or-existing-entry)
    "ns" '("Search note" . denote-notes-search)


    "i" '("Insert" . (keymap))
    "ir" '("from kill ring" . yank-from-kill-ring)
    "iy" '("snippets" . consult-yasnippet)

    "s" '("Search" . (keymap))
    "ss" '("Search buffer" . swiper)
    "sS" '("Search buffer with input" . swiper-thing-at-point)
    "sp" '("Search project" . consult-ripgrep)
    "sP" '("Search project with input" . me/search-project)
    "sf" '("Search dir" . me/search-dir)
    "sF" '("Search dir with input" . me/search-dir-with-input)
    "sr" '("Resume last search" . vertico-repeat)
    "sw" '("Search web" . me/search-web)

    "o" '("Open" . (keymap))
    "op" '("project" . project-switch-project)
    "ob" '("bookmark" . list-bookmarks)

    "l" '("Open link" . (keymap))
    "la" '("Github action" . (lambda () (interactive) (browse-url "https://github.com/onpointvn/opollo/actions")))
    "lp" '("My PRs" . (lambda () (interactive) (browse-url "https://github.com/onpointvn/opollo/pulls?q=sort%3Aupdated-desc+is%3Apr+is%3Aopen+author%3Abluzky")))
    "ls" '("Ducduck go" . (lambda () (interactive) (browse-url "https://duckduckgo.com")))

    "x" '("Extra/execute" . (keymap))
    "xm" '("Macro" . consult-kmacro)


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
