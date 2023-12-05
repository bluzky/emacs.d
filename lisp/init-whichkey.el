;; Configure whichkey and setup keybindings
;; Which-key
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
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-prevent-C-h-from-cycling t
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-use-C-h-for-paging t))

(defun setup-key-bindings()
  "Setup my custom keybindings"
  (evil-leader/set-key
   "." 'find-file
   "," 'consult-buffer
   "'" 'execute-extended-command

   "e" 'treemacs
   "Q" 'save-buffers-kill-emacs
   "R" 'restart-emacs

   ;; File
   "f" '("File" . (keymap))
   "ff" '("Find in project" . project-find-file)
   "fr" '("Recent files" . consult-recent-file)
   ;; "fD" '("Delete" . )
   "fR" '("Rename" . rename-visited-file)
   "fS" '("Save as" . write-buffer)

   ;;  Buffer
   "b" '("Buffer" . (keymap))
   ;; "bs" '("Save" . save-buffer)
   "bs" '("Scratch buffer" . scratch-buffer)
   "bS" '("Save all" . save-some-buffers)
   "bn" '("New" . evil-buffer-new)

   ;; Code
   "c" '("Code" . (keymap))
   "cd" '("Find definition" . xref-find-definitions)
   "cf" '("Format buffer" . eglot-format-buffer)
   "cr" '("Find references" . xref-find-references)
   "cs" '("Buffer's symbols" . consult-imenu)
   "ca" '("Code action" . eglot-code-actions)

   "g" '("Magit" . (keymap))
   "gs" '("status" . magit-status)
   "gp" '("create PR" . me/visit-pull-request-url)
   "gb" '("blame" . magit-blame-addition)
   "gl" '("log current file" . magit-log-current)
   "gd" '("diff changed" . magit-diff-unstaged)

   "h" '("Help" . (keymap))
   "hf" '("function" . describe-function)
   "hk" '("key" . describe-key)
   "ht" '("Change theme" . consult-theme)
   "hv" '("variable" . describe-variable)

   "i" '("Insert" . (keymap))
   "ir" '("from kill ring" . yank-from-kill-ring)
   "is" '("snippets" . consult-yasnippet)

   "s" '("Search" . (keymap))
   "ss" '("Search buffer" . consult-line)
   "sp" '("Search project" . consult-git-grep)
   "sr" '("Resume last search" . vertico-repeat-last)

   "p" '("Project" . (keymap))
   "pp" '("Switch project" . project-switch-project)

   "w" '("window" . (keymap))
   "wd" '("delete" . delete-window)
   "wo" '("delete other" . delete-other-windows)
   "ww" '("ace-window" . aw-show-dispatch-help))
  )

;; Set leader key to SPC
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (setup-key-bindings))

(provide 'init-whichkey)
