;; Enable mouse
(xterm-mouse-mode 1)

;; Don’t bother confirming killing processes and don’t let backup~ files scatter around.
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil))

;; Enable recentf mode
(use-package recentf
  :init
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :hook
  (after-init . recentf-mode)
  )

;; Automatically refreshes the buffer for changes outside of Emacs
;; Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package markdown-mode)

;; Slightly shorten eldoc display delay.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))


;; Show matching parentheses
;; Reduce the highlight delay to instantly.
(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

;; Enter ediff with side-by-side buffers to better compare the differences.
(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

;; Auto-pairing quotes and parentheses etc.
;; Electric-pair-mode has improved quite a bit in recent Emacs versions. No longer need an extra package for this. It also takes care of the new-line-and-push-brace feature.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Clean up whitespace on save
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))


;; Syntax highlighting improvement
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))



;; Flycheck
;; A modern on-the-fly syntax checking extension – absolute essential
(use-package flycheck :config (global-flycheck-mode +1))

;; move line up/down, duplicate line/region
(use-package move-dup
  :bind (("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)
         ("C-c D" . move-dup-duplicate-up)
         ("C-c d" . move-dup-duplicate-down)))

(provide 'init-editor)
