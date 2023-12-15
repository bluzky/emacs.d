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
   "e" 'treemacs
   "o" 'treemacs-select-window
   "Q" 'save-buffers-kill-emacs
   "R" 'restart-emacs

   ;; File
   "f" '("File" . (keymap))
   "ff" '("Find in project" . project-find-file)
   "fr" '("Recent files" . consult-recent-file)
   ;; "fD" '("Delete" . )
   "fR" '("Rename" . rename-visited-file)
   "fS" '("Save as" . write-file)

   ;;  Buffer
   "b" '("Buffer" . (keymap))
   ;; "bs" '("Save" . save-buffer)
   "bb" '("Switch buffer" . consult-buffer-other-window)
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

   "g" '("Magit" . (keymap))
   "gs" '("status" . magit-status)
   "gp" '("create PR" . me/visit-pull-request-url)
   "gb" '("blame" . magit-blame-addition)
   "gl" '("log current file" . magit-log-buffer-file)
   "gd" '("diff changed" . magit-diff-unstaged)

   "h" '("Help" . (keymap))
   "hf" '("function" . describe-function)
   "hk" '("key" . describe-key)
   "ht" '("change theme" . consult-theme)
   "hv" '("variable" . describe-variable)
   "hl" '("absolute line number" . (lambda () (interactive) (setq display-line-numbers t)))
   "hL" '("relative line number" . (lambda () (interactive) (setq display-line-numbers 'relative)))

   "i" '("Insert" . (keymap))
   "ir" '("from kill ring" . yank-from-kill-ring)
   "is" '("snippets" . consult-yasnippet)

   "s" '("Search" . (keymap))
   "ss" '("Search buffer" . swiper)
   "sS" '("Search buffer" . swiper-thing-at-point)
   "sp" '("Search project" . consult-ripgrep)
   "sP" '("Search project" . me/consult-ripgrep-symbol-at-point)
   "sr" '("Resume last search" . vertico-repeat-last)

   "p" '("Project" . (keymap))
   "pp" '("Switch project" . projectile-switch-project)

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
