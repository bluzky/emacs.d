;; Configure whichkey and setup keybindings
;; Which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))


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

   "g" '("magit" . (keymap))
   "gc" '("commit" . magit-commit)
   "gf" '("fetch" . magit-fetch)
   "gg" '("status" . magit-status)

   "h" '("Help" . (keymap))
   "hf" '("function" . describe-function)
   "hk" '("key" . describe-key)
   "ht" '("Change theme" . consult-theme)
   "hv" '("variable" . describe-variable)

   "i" '("Insert" . (keymap))
   "ir" '("From kill ring" . yank-from-kill-ring)

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
