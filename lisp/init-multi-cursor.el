;; (use-package evil-mc :after evil :config (global-evil-mc-mode 1))

(use-package evil-mc
  :after evil
  :init
  (which-key-add-keymap-based-replacements evil-motion-state-map "gr" "evil-mc")
  :hook
  (prog-mode . turn-on-evil-mc-mode)
  (text-mode . turn-on-evil-mc-mode)
  :config
  (define-key evil-motion-state-map (kbd "gr") evil-mc-key-map)
  (add-hook 'magit-mode-hook 'turn-off-evil-mc-mode)
  (setq-default evil-mc-one-cursor-show-mode-line-text nil)
  (evil-define-key '(normal insert) evil-mc-key-map
    (kbd "C-<down>") #'evil-mc-make-cursor-move-next-line
    (kbd "C-<up>") #'evil-mc-make-cursor-move-prev-line
    (kbd "M-n") #'evil-mc-make-and-goto-next-match
    (kbd "M-N") #'evil-mc-make-and-goto-prev-match))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(provide 'init-multi-cursor)
