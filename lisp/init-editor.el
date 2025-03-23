;; Enable mouse
(use-package emacs
  :ensure nil
  :hook (after-init . global-hl-line-mode)
  :config
  (xterm-mouse-mode 1)
  (setq initial-major-mode 'elixir-ts-mode)
  (setq initial-scratch-message "")

  ;; disable auto-save
  (auto-save-mode -1)
  (setq auto-save-default nil)

  ;; config answer y/n
  (setq use-short-answers t)

  ;; auto select help window
  (setq help-window-select t)

  ;; prefer vertical split
  (setq split-height-threshold 60)
  (setq split-width-threshold 106)
  )

;; improve the default help system
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)   ; Describe function
         ("C-h v" . helpful-variable)   ; Describe variable
         ("C-h k" . helpful-key)        ; Describe keybinding
         ("C-h x" . helpful-command)))  ; Describe command


;; enable system clipboard
(use-package xclip
  :config
  (xclip-mode 1))

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
  (run-at-time nil (* 5 60) 'recentf-save-list) ;; auto save every 5 minutes
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

;; (use-package highlight-escape-sequences
;;   :hook (prog-mode . hes-mode))


;; move line up/down, duplicate line/region
(use-package move-dup
  :bind (("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)))



(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; (use-package blamer
;;   :quelpa (blamer :fetcher github :repo "artawower/blamer.el")
;;   :bind (("s-i" . blamer-show-commit-info)
;;          ("C-c i" . blamer-show-posframe-commit-info))
;;   :custom
;;   (blamer-idle-time 0.3)
;;   (blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :background nil
;;                     :height 140
;;                     :italic t)))
;;   :config
;;   (global-blamer-mode 1))

;; (use-package color-rg
;;   :quelpa (color-rg :fetcher github :repo "manateelazycat/color-rg")
;;   )

(provide 'init-editor)
